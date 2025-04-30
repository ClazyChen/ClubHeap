package fpga

import chisel3._
import chisel3.util._

// [ClubHeap+] The forwarding module
// This module is used to forward the data and ensure:
// 1. the data is forwarded correctly
// 2. if the data is still being used, it will not be written back
// 3. if the data is already used, it will not be read again

class Forwarding(val level: Int) extends Module {

    val io = IO(new Bundle {
        
        // data to forward
        val r = Input(new Stash(level))
        val c = Input(new Stash(level))
        val p = Input(new Stash(level))
        val w = Input(new Stash(level))

        // cancel read or write operation
        val ren_out = Output(Bool())
        val wen_out = Output(Bool())

        // the forwarded data
        val fwd_c = Output(new Stash(level))
    
    })

    // cancel read operation
    val rc_same = io.r.paddr === io.c.paddr
    val rp_same = io.r.paddr === io.p.paddr
    val rw_same = io.r.paddr === io.w.paddr
    io.ren_out := !(rc_same || rp_same || rw_same)

    // cancel write operation
    val cw_same = io.c.paddr === io.w.paddr
    val pw_same = io.p.paddr === io.w.paddr
    io.wen_out := !(rw_same || cw_same || pw_same)

    // forward data
    val rp_forward = RegNext(rc_same)
    val rw_forward = RegNext(rp_same)
    val rw_forward_delay = RegNext(rw_same)

    when (rp_forward) {
        io.fwd_c := io.p
    } .elsewhen (rw_forward) {
        io.fwd_c := io.w
    } .elsewhen (rw_forward_delay) {
        io.fwd_c := RegNext(io.w)
    } .otherwise { // no forwarding
        io.fwd_c := io.c
    }

}
