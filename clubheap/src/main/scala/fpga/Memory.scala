package fpga

import chisel3._
import chisel3.util._

// The memory module of one level of the heap
//     including a sister memory for nodes at the same level
// read port:
// - raddr_in: the address of the entry to read
//             if the address is a null pointer, then allocate a new entry in the sister memory
// - rdata_lc_out: the data of the left child (Cluster)
// - rdata_rc_out: the data of the right child (Cluster)
// - raddr_actual_out: the address of the entry actually read
//             if raddr_in is a valid pointer, then raddr_actual_out = raddr_in
//             if raddr_in is a null pointer, then raddr_actual_out is the address of the allocated entry
// write port:
// - waddr_in: the address of the entry to write
// - wdata_lc_in: the data of the left child (Cluster)
// - wdata_rc_in: the data of the right child (Cluster)
// memory data:
// although rdata/wdata are Cluster (with a larger width), the actual data stored in the memory is UInt
// - dynamic memory model: all data in Cluster are stored in UInt
// - static memory model: all data (excluding next) in Cluster are stored in UInt
// - the last level: only entries in Cluster are stored in UInt
class Memory(
    val level: Int, // the level of the memory (starting from 1)
    val use_ffmem: Boolean = false, // if true, use FFMem, otherwise use Sram
) extends Module {

    val K = Const.count_of_elements_in_each_cluster
    val is_the_last_level = level == Const.count_of_levels
    val is_dynamic = Const.is_dynamic(level)
    def invalid_addr(addr: UInt): Bool = addr(Const.link_width(level)-1)

    val io = IO(new Bundle {
        // read port
        val raddr_in = Input(UInt(Const.link_width(level).W))
        val rdata_lc_out = Output(new Cluster(level))
        val rdata_rc_out = Output(new Cluster(level))
        val raddr_actual_out = Output(UInt(Const.link_width(level).W))

        // write port
        val waddr_in = Input(UInt(Const.link_width(level).W))
        val wdata_lc_in = Input(new Cluster(level))
        val wdata_rc_in = Input(new Cluster(level))
    })

    // the sister memory
    val mem = Module(new SisterMem(
        data_depth = Const.data_depth(level),
        data_width = Const.data_width(level, is_dynamic),
        use_ffmem = use_ffmem,
        no_sister = level == 1,
    ))

    // in the dynamic memory model:
    // - valid_addr : the address to write is valid (has been allocated), calculated in READ stage
    // - valid_data : the data to write is valid (lc or rc is valid), calculated in WRITE stage
    // (valid_addr, valid_data) =
    //   - (0, 0) no operation (actually, allocate and then free)
    //   - (0, 1) require allocation
    //   - (1, 0) require free
    //   - (1, 1) normal update
    // therefore, we check invalid_addr(io.raddr_in) to determine if we need to allocate a new entry
    // we use a free list to record the first free entry
    // when we need to allocate a new entry, we read the node at the free list, and set the free list to the next field of the node
    val free_list = if (is_dynamic) RegInit(0.U(Const.link_width(level).W)) else DontCare
    val require_allocation = invalid_addr(io.raddr_in)
    val require_allocation_delay = RegNext(require_allocation)
    val require_free = io.wdata_lc_in.is_empty && io.wdata_rc_in.is_empty

    // determine the address to read
    // - no allocation : raddr_in
    // - allocation and no free : free_list
    //   NOTE: if we have just allocated a new entry in the last cycle, the free_list has not been updated yet
    //         so we need to use the next field of the (just allocated) entry to get the next address to allocate
    //         we use the next field of the left child to build the free list
    def link(data: UInt): UInt = {
        if (is_the_last_level) {
            data.asTypeOf(Vec(K-1, new Entry))(0).metadata
        } else {
            if (is_dynamic) {
                data.asTypeOf(new Cluster(level)).next
            } else {
                data.asTypeOf(new StaticCluster(level)).entries(0).metadata
            }
        }
    }
    val next_of_just_allocated = link(mem.io.rdata_lc_out)
    val next_addr_to_allocate = Mux(require_allocation_delay, next_of_just_allocated, free_list)
    
    // the address to read in actual, it is calculated in the READ stage (before the memory is read)
    val actual_raddr = if (is_dynamic) (Mux(require_allocation,
        Mux(require_free, 
            io.waddr_in, 
            next_addr_to_allocate),
        io.raddr_in
    )) else io.raddr_in
    io.raddr_actual_out := actual_raddr
    
    // update the free list
    // - allocate and free : no update (the newly freed entry is immediately allocated)
    // - no allocation and no free : no update
    // - allocate and no free : update the free list to the next field of the newly allocated entry, i.e. next_of_just_allocated
    // - no allocation and free : update the free list to the newly freed entry
    if (is_dynamic) {
        when (require_allocation && !require_free) {
            free_list := next_of_just_allocated
        }
        when (!require_allocation && require_free) {
            free_list := io.waddr_in
        }
    }

    // below is the read / write logic
    val raddr_delay = RegNext(io.raddr_in)

    // read logic
    mem.io.raddr_in := actual_raddr
    io.rdata_lc_out := DontCare
    io.rdata_rc_out := DontCare
    if (is_the_last_level) {
        io.rdata_lc_out.entries := mem.io.rdata_lc_out.asTypeOf(io.rdata_lc_out.entries)
        io.rdata_rc_out.entries := mem.io.rdata_rc_out.asTypeOf(io.rdata_rc_out.entries)
    } else {
        if (is_dynamic) {
            io.rdata_lc_out := mem.io.rdata_lc_out.asTypeOf(io.rdata_lc_out)
            io.rdata_rc_out := mem.io.rdata_rc_out.asTypeOf(io.rdata_rc_out)
            // a new leaf node is allocated, set the left and right children to null
            //       otherwise, use the next field recorded
            when (invalid_addr(raddr_delay)) {
                io.rdata_lc_out.next := (-1).S.asUInt // -1 is the null pointer
                io.rdata_rc_out.next := (-1).S.asUInt
            }
        } else {
            // in the static memory model, we do not need to check the empty status
            io.rdata_lc_out := mem.io.rdata_lc_out.asTypeOf(new StaticCluster(level)).to_dynamic
            io.rdata_rc_out := mem.io.rdata_rc_out.asTypeOf(new StaticCluster(level)).to_dynamic
            io.rdata_lc_out.next := Cat(raddr_delay, false.B)
            io.rdata_rc_out.next := Cat(raddr_delay, true.B)
        }
    }

    // write logic
    mem.io.waddr_in := io.waddr_in
    val wdata_lc = Wire(new Cluster(level))
    val wdata_rc = Wire(new Cluster(level))
    wdata_lc := io.wdata_lc_in
    wdata_rc := io.wdata_rc_in
    // if the node is to be freed, we add it to the front of the free list
    // - in the last level, use the metadata of the first entry to record the next address to allocate
    // - in other levels, use the next field to record the next address to allocate
    if (is_dynamic) {
        when (require_free) {
            if (is_the_last_level) {
                wdata_lc.entries(0).metadata := next_addr_to_allocate
            } else {
                wdata_lc.next := next_addr_to_allocate
            }
        }
    }
    if (is_the_last_level) { // only entries are written
        mem.io.wdata_lc_in := wdata_lc.entries.asUInt
        mem.io.wdata_rc_in := wdata_rc.entries.asUInt
    } else {
        if (is_dynamic) { // dynamic cluster is written
            mem.io.wdata_lc_in := wdata_lc.asUInt
            mem.io.wdata_rc_in := wdata_rc.asUInt
        } else { // static cluster is written
            mem.io.wdata_lc_in := wdata_lc.to_static.asUInt
            mem.io.wdata_rc_in := wdata_rc.to_static.asUInt
        }
    }

}

