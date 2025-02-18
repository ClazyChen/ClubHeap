package fpga

import chisel3._
import chisel3.util._

// the WRITE stage of a processor
//    this module build a bridge between the COMPARE.POST stage and the memory
// -----------------------------------
// ports for the WRITE stage:
// horizontal (within a level):
// - waddr_in: the address of the entry to write (from the COMPARE.POST stage)
// - wdata_lc_in: the data of the left child of the entry to write (from the COMPARE.POST stage)
// - wdata_rc_in: the data of the right child of the entry to write (from the COMPARE.POST stage)
// no vertical ports
// -----------------------------------
// ports between the WRITE stage and the memory:
// - waddr_mem: the address of the entry to write (to the memory)
// - wdata_lc_mem: the data of the left child (Cluster) (to the memory)
// - wdata_rc_mem: the data of the right child (Cluster) (to the memory)

class Write(val level: Int) extends Module {
    val io = IO(new Bundle {
        // ports for the WRITE stage
        val waddr_in = Input(UInt(Const.link_width(level).W))
        val wdata_lc_in = Input(new Cluster(level))
        val wdata_rc_in = Input(new Cluster(level))

        // ports between the WRITE stage and the memory
        val waddr_mem = Output(UInt(Const.link_width(level).W))
        val wdata_lc_mem = Output(new Cluster(level))
        val wdata_rc_mem = Output(new Cluster(level))
    })

    // directly forward the address to the memory
    io.waddr_mem := io.waddr_in
    io.wdata_lc_mem := io.wdata_lc_in
    io.wdata_rc_mem := io.wdata_rc_in
}

