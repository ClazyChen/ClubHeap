package fpga

import chisel3._
import chisel3.util._
import fpga.UIntExt._

// The memory module of one level of the heap
//     including a sister memory for nodes at the same level
// read port:
// - ren_in: read enable signal
// - raddr_in: the address of the entry to read
//             if the address is invalid, then allocate a new entry in the sister memory
// - rdata_lc_out: the data of the left child (Cluster)
// - rdata_rc_out: the data of the right child (Cluster)
// - raddr_out: the address of the entry actually read
// write port:
// - wen_in: write enable signal
// - waddr_in: the address of the entry to write
// - wdata_lc_in: the data of the left child (Cluster)
// - wdata_rc_in: the data of the right child (Cluster)
//
// [ClubHeap+] introduces no hazard design as follows:
// - we ensure that 1) if the data of an address is still being used, then it will not be overwritten
//                  2) if the data of an address is already being used, then it will not be read
//                  3) if we allocate and free an address in the same cycle, then we directly give the address for allocation
//                     and cancel the write operation

class Memory(
    val level: Int, // the level of the memory (starting from 1)
    val use_ffmem: Boolean = false, // if true, use FFMem, otherwise use Sram
) extends Module {

    val is_last_level = level == HeapConst.count_of_levels
    val is_dynamic_memory = HeapConst.is_dynamic_memory(level)

    // the address width of the sister memory is the link width of the previous level
    val addr_width = HeapConst.link_width(level-1)

    val io = IO(new Bundle {
        // read port
        val ren_in = Input(Bool()) // read enable signal
        val raddr_in = Input(UInt(addr_width.W)) // the address to read
        val rdata_lc_out = Output(new Cluster(level)) // the data of the left child (Cluster)
        val rdata_rc_out = Output(new Cluster(level)) // the data of the right child (Cluster)
        val raddr_out = Output(UInt(addr_width.W)) // the address actually read

        // write port 
        val wen_in = Input(Bool()) // write enable signal
        val waddr_in = Input(UInt(addr_width.W)) // the address to write
        val wdata_lc_in = Input(new Cluster(level)) // the data of the left child (Cluster)
        val wdata_rc_in = Input(new Cluster(level)) // the data of the right child (Cluster)
    })

    // the sister memory
    val mem = Module(new SisterMem(
        level = level,
        use_ffmem = use_ffmem,
    ))

    // read ports of the sister memory
    val ren = Wire(Bool())
    val raddr = Wire(UInt(addr_width.W))
    val rdata_lc = Wire(new Cluster(level))
    val rdata_rc = Wire(new Cluster(level))
    ren := io.ren_in
    raddr := io.raddr_in
    rdata_lc := mem.io.rdata_lc_out
    rdata_rc := mem.io.rdata_rc_out

    // write ports of the sister memory
    val wen = Wire(Bool())
    val wdata_lc = Wire(new Cluster(level))
    val wdata_rc = Wire(new Cluster(level))
    wen := io.wen_in
    wdata_lc := io.wdata_lc_in
    wdata_rc := io.wdata_rc_in

    if (is_dynamic_memory) {

        // is dynamic memory, use the free list, and allocation and free
        // free list is initialized to 0
        val free_list = RegInit(0.U(addr_width.W))

        // allocation and free
        val alloc = io.raddr_in.select_highest
        val free = io.wdata_lc_in.is_empty && io.wdata_rc_in.is_empty

        // directly give the address for allocation and free, cancel the read and write operation
        when (alloc && free) {
            ren := false.B
            wen := false.B
        }

        // determine the address to read
        // - if last cycle is allocation, then the next address is its next field 
        // - otherwise, use the free list
        val free_list_invalid = RegNext(alloc && !free)
        val alloc_addr = Mux(free_list_invalid, mem.io.rdata_lc_out.next, free_list)

        // determine the actual address to read
        when (alloc) {
            raddr := Mux(free, io.waddr_in, alloc_addr)
        }

        // if the cluster is just allocated, return an empty cluster
        val alloc_delay = RegNext(alloc)
        when (alloc_delay) {
            rdata_lc := Cluster.empty(level)
            rdata_rc := Cluster.empty(level)
        }

        // determine the actual data to write
        when (free && !alloc) {
            if (is_last_level) {
                wdata_lc.entries(0).metadata := alloc_addr
            } else {
                wdata_lc.next := alloc_addr
            }
        }

        // update the free list
        free_list := Mux(free && !alloc, io.waddr_in, alloc_addr)
    } 
    
    // read logic
    mem.io.ren_in := ren
    mem.io.raddr_in := raddr
    io.rdata_lc_out := rdata_lc
    io.rdata_rc_out := rdata_rc
    io.raddr_out := RegNext(raddr)

    // write logic
    mem.io.wen_in := wen
    mem.io.waddr_in := io.waddr_in
    mem.io.wdata_lc_in := wdata_lc
    mem.io.wdata_rc_in := wdata_rc
}

