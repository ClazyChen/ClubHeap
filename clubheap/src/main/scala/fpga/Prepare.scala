package fpga

import chisel3._
import chisel3.util._

// The PREPARE stage of a processor
//    this module is the third stage of the ClubHeap+ pipeline

class Prepare(val level: Int) extends Module {

    val io = IO(new Bundle {
        
        // ports to forwarding
        val p_fwd = Output(new Stash(level))

        // ports from the COMPARE stage at this level
        val cp = Input(new CPStash(level))

        // ports to the WRITE stage at this level
        val pw = Output(new Stash(level))

        // ports from the COMPARE stage at the next level
        val new_min_next_in = Input(new Entry)
        val next_next_in = Input(UInt(HeapConst.addr_width(level+1).W))

        // ports to the COMPARE stage at the next level
        val op_next_out = Output(new Operator)
        val sc_next_out = Output(Bool())

    })

    io.p_fwd <> io.cp.data

    io.op_next_out := io.cp.op_next
    io.sc_next_out := io.cp.sc_next

    val pw = Reg(new Stash(level))
    pw := io.cp.data

    // update fields based on the signal from the COMPARE stage at the next level
    when (io.cp.op_next.pop) { // pop operation, indicating new_min is promoted from the next level
        when (!io.cp.sc && !io.cp.sc_next) {
            pw.data_lc.min_lc := io.new_min_next_in
        }
        when (!io.cp.sc && io.cp.sc_next) {
            pw.data_lc.min_rc := io.new_min_next_in
        }
        when (io.cp.sc && !io.cp.sc_next) {
            pw.data_rc.min_lc := io.new_min_next_in
        }
        when (io.cp.sc && io.cp.sc_next) {
            pw.data_rc.min_rc := io.new_min_next_in
        }
    }

    when (!io.cp.sc) {
        pw.data_lc.next := io.next_next_in
    }
    when (io.cp.sc) {
        pw.data_rc.next := io.next_next_in
    }

    io.pw := pw
}

