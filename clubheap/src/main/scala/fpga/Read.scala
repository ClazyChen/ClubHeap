package fpga

import chisel3._
import chisel3.util._

// The READ stage of a processor
//    this module build a bridge between the COMPARE.PRE stage and the memory
// -----------------------------------
// ports for the READ stage:
// horizontal (within a level):
// - raddr_out: the address of the entry actually read (to the COMPARE.PRE stage)
// - rdata_lc_out: the data of the left child (Cluster) (to the COMPARE.PRE stage)
// - rdata_rc_out: the data of the right child (Cluster) (to the COMPARE.PRE stage)
// vertical (between levels):
// - raddr_prev_in: the address of the entry to read (from the COMPARE.PRE stage on the previous level)
// - link_prev_out: the link field of the previous level (to the COMPARE stage on the previous level)
//                  literally, it is the address of the entry read on this level, i.e. raddr_out
// -----------------------------------
// ports between the READ stage and the memory:
// - raddr_mem: the address of the entry to read (to the memory)
// - raddr_actual_mem: the address of the entry actually read (from the memory)
// - rdata_lc_mem: the data of the left child (Cluster) (from the memory)
// - rdata_rc_mem: the data of the right child (Cluster) (from the memory)
class Read(val level: Int) extends Module {
    val io = IO(new Bundle { 
        // ports for the READ stage
        // horizontal ports
        val raddr_out = Output(UInt(Const.link_width(level).W))
        val rdata_lc_out = Output(new Cluster(level))
        val rdata_rc_out = Output(new Cluster(level))

        // vertical ports
        val raddr_prev_in = Input(UInt(Const.link_width(level).W))
        val link_prev_out = Output(UInt(Const.link_width(level).W))

        // ports between the READ stage and the memory
        val raddr_mem = Output(UInt(Const.link_width(level).W))
        val raddr_actual_mem = Input(UInt(Const.link_width(level).W))
        val rdata_lc_mem = Input(new Cluster(level))
        val rdata_rc_mem = Input(new Cluster(level))
    })

    // the address to read from the memory
    io.raddr_mem := io.raddr_prev_in

    // the data read from the memory
    io.rdata_lc_out := io.rdata_lc_mem
    io.rdata_rc_out := io.rdata_rc_mem

    // the address actually read from the memory
    // 1. to update the link field of the previous level
    io.link_prev_out := io.raddr_actual_mem
    // 2. to determine whether to forward the data
    io.raddr_out := RegNext(io.raddr_actual_mem)
}
