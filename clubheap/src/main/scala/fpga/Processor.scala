package fpga

import chisel3._
import chisel3.util._


// a processor in a level of the clubheap
// -----------------------------------
// [ClubHeap+] containing 4 stages:
// - READ: read the data from the memory
// - COMPARE: compare the data with the current data
// - PREPARE: prepare the data for write
// - WRITE: write the data to the memory

class Processor(val level: Int) extends Module {

    val addr_width = HeapConst.addr_width(level)
    val paddr_width = HeapConst.paddr_width(level)

    val io = IO(new Bundle {
        val prev = new InterLevelIO(level)
        val next = Flipped(new InterLevelIO(level+1))
    })

    // stages, memory, and forwarding
    val read = new Read(level)
    val compare = new Compare(level)
    val prepare = new Prepare(level)
    val write = new Write(level)
    val mem = new Memory(level)
    val fwd = new Forwarding(level)

    // inter-level connections (to previous level)
    read.io.addr_prev_in <> io.prev.addr
    read.io.paddr_prev_in <> io.prev.paddr
    compare.io.op_prev_in <> io.prev.op
    compare.io.sc_prev_in <> io.prev.sc
    compare.io.new_min_prev_out <> io.prev.new_min
    compare.io.next_prev_out <> io.prev.next

    // inter-level connections (to next level)
    compare.io.addr_next_out <> io.next.addr
    compare.io.paddr_next_out <> io.next.paddr
    prepare.io.op_next_out <> io.next.op
    prepare.io.sc_next_out <> io.next.sc
    prepare.io.new_min_next_in <> io.next.new_min
    prepare.io.next_next_in <> io.next.next

    // intra-level connections
    read.io.rc <> compare.io.rc
    compare.io.cp <> prepare.io.cp
    prepare.io.pw <> write.io.pw

    mem.io.read <> read.io.mem
    mem.io.write <> write.io.mem

    // forwarding
    fwd.io.r <> read.io.r_fwd
    fwd.io.c <> compare.io.c_fwd
    fwd.io.p <> prepare.io.p_fwd
    fwd.io.w <> write.io.w_fwd
    fwd.io.ren_out <> read.io.ren_in
    fwd.io.wen_out <> write.io.wen_in
    fwd.io.fwd_c <> compare.io.fwd_c
    
}


