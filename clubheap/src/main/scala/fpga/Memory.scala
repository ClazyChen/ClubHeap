package fpga

import chisel3._
import chisel3.util._
import fpga.UIntExt._

// The memory module of one level of the heap
//     including a sister memory for nodes at the same level
//
// [ClubHeap+] introduces no hazard design as follows:
// - we ensure that 1) if the data of an address is still being used, then it will not be overwritten
//                  2) if the data of an address is already being used, then it will not be read
//                  3) if we allocate and free an address in the same cycle, then we directly give the address for allocation
//                     and cancel the write operation

class Memory(val level: Int) extends Module {

    val is_last_level = level == HeapConst.count_of_levels
    val is_dynamic_memory = HeapConst.is_dynamic_memory(level)
    val addr_width = HeapConst.addr_width(level)

    val io = IO(new Bundle {
        val read = new ReadMemIO(level)
        val write = new WriteMemIO(level)
    })

    // The sister memory
    val mem = Module(new SisterMem(level))

    // Read port connections
    val ren = Wire(Bool())
    val raddr = Wire(UInt(addr_width.W))
    val rdata_lc = Wire(new Cluster(level))
    val rdata_rc = Wire(new Cluster(level))
    ren := io.read.ren
    raddr := io.read.raddr_raw
    rdata_lc := mem.io.rdata_lc_out
    rdata_rc := mem.io.rdata_rc_out

    // Write port connections
    val wen = Wire(Bool())
    val wdata_lc = Wire(new Cluster(level))
    val wdata_rc = Wire(new Cluster(level))
    wen := io.write.wen
    wdata_lc := io.write.wdata_lc
    wdata_rc := io.write.wdata_rc

    if (is_dynamic_memory) {

        // For dynamic memory, use free list, allocation and deallocation
        // Free list is initialized to 0
        val free_list = RegInit(0.U(addr_width.W))

        // Allocation and deallocation
        val alloc = io.read.raddr_raw.select_highest
        val free = io.write.wdata_lc.is_empty && io.write.wdata_rc.is_empty

        // Directly allocate address and free, cancel read and write operations
        when (alloc && free) {
            ren := false.B
            wen := false.B
        }

        // Determine the address to read
        // - If the last cycle was allocation, then the next address is its next field
        // - Otherwise, use the free list
        val free_list_invalid = RegNext(alloc && !free)
        val alloc_addr = Mux(free_list_invalid, mem.io.rdata_lc_out.next, free_list)

        // Determine the actual address to read
        when (alloc) {
            raddr := Mux(free, io.write.waddr, alloc_addr)
        }

        // If the cluster was just allocated, return an empty cluster
        val alloc_delay = RegNext(alloc)
        when (alloc_delay) {
            rdata_lc := Cluster.empty(level)
            rdata_rc := Cluster.empty(level)
        }

        // Determine the actual data to write
        when (free && !alloc) {
            if (is_last_level) {
                wdata_lc.entries(0).metadata := alloc_addr
            } else {
                wdata_lc.next := alloc_addr
            }
        }

        // Update the free list
        free_list := Mux(free && !alloc, io.write.waddr, alloc_addr)
    } 
    
    // Read logic
    mem.io.ren_in := ren
    mem.io.raddr_in := raddr
    io.read.rdata_lc := rdata_lc
    io.read.rdata_rc := rdata_rc
    io.read.raddr := RegNext(raddr)

    // Write logic
    mem.io.wen_in := wen
    mem.io.waddr_in := io.write.waddr
    mem.io.wdata_lc_in := wdata_lc
    mem.io.wdata_rc_in := wdata_rc
}
