package fpga

import chisel3._
import chisel3.util._

// the WRITE stage of a processor
//    this module is the fourth stage of the ClubHeap+ pipeline

class Write(val level: Int) extends Module {
    val io = IO(new Bundle {

        // ports to forwarding
        val w_fwd = Output(new Stash(level))
        val wen_in = Input(Bool())

        // ports from the PREPARE stage at this level
        val pw = Input(new Stash(level))

        // ports to the memory
        val mem = Flipped(new WriteMemIO)
    })

    io.w_fwd <> io.pw

    io.mem.wen <> io.wen_in
    io.mem.waddr_raw <> io.pw.addr
    io.mem.wdata_lc <> io.pw.data_lc
    io.mem.wdata_rc <> io.pw.data_rc
}

