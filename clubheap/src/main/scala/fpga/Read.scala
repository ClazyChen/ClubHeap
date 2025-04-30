package fpga

import chisel3._
import chisel3.util._

// The READ stage of a processor
//    this module is the first stage of the ClubHeap+ pipeline

class Read(val level: Int) extends Module {

    val io = IO(new Bundle { 

        // ports from the CMP stage at the previous level
        val addr_prev_in = Input(UInt(HeapConst.addr_width(level).W))
        val paddr_prev_in = Input(UInt(HeapConst.paddr_width(level).W))

        // ports to forwarding
        val r_fwd = Output(new Stash(level))
        val ren_in = Input(Bool())

        // ports to memory
        val mem = Flipped(new ReadMemIO(level))

        // ports to the CMP stage at this level
        val rc = Output(new Stash(level))

    })

    io.mem.ren <> io.ren_in
    io.mem.raddr_raw <> io.addr_prev_in
    io.mem.rdata_lc <> io.rc.data_lc
    io.mem.rdata_rc <> io.rc.data_rc

    io.rc.addr <> io.mem.raddr
    io.rc.paddr := RegNext(io.paddr_prev_in)

    io.r_fwd := DontCare // has no use
    io.r_fwd.paddr := io.paddr_prev_in

}
