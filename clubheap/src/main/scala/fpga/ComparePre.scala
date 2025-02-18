package fpga

import chisel3._
import chisel3.util._
import fpga.UIntExt._

// the preprocessing module of the COMPARE stage
//    this module is used to preprocess the read data
// -----------------------------------
// ports for the COMPARE.PRE stage:
// horizontal (within a level):
// - raddr_in/out: the address of the entry actually read (from the READ stage, to the COMPARE stage)
// - rdata_lc_in: the data of the left child of the entry read (from the READ stage)
// - rdata_rc_in: the data of the right child of the entry read (from the READ stage)
// - forward_data_lc_in: the data of the left child of the entry forwarded (from the COMPARE.POST stage)
// - forward_data_rc_in: the data of the right child of the entry forwarded (from the COMPARE.POST stage)
// - rdata_lc_out: the data of the left child of the entry read (to the COMPARE stage)
// - rdata_rc_out: the data of the right child of the entry read (to the COMPARE stage)
// - cdata_out: the data of the child to be compared (to the COMPARE stage)
//              if the processor subsequently read the same address, then we use the forwarding data
//              otherwise, we use the read data
// - select_child_out: the selection of the child to be compared (to the COMPARE stage)
//                     same as the select_child_prev_in of the previous level
// vertical (between levels):
// - select_child_prev_in: the selection of the child to be compared (from the COMPARE stage on the previous level)
//                         cdata_out is selected from rdata_lc_out or rdata_rc_out by this signal
//                         0: select rdata_lc_out; 1: select rdata_rc_out
// - raddr_next_out: the address of the entry to be read on the next level (to the READ stage on the next level)
class ComparePre(val level: Int) extends Module {

    val io = IO(new Bundle {
        val raddr_in = Input(UInt(Const.link_width(level).W))
        val raddr_out = Output(UInt(Const.link_width(level).W))
        val rdata_lc_in = Input(new Cluster(level))
        val rdata_rc_in = Input(new Cluster(level))

        val forward_data_lc_in = Input(new Cluster(level))
        val forward_data_rc_in = Input(new Cluster(level))

        val rdata_lc_out = Output(new Cluster(level))
        val rdata_rc_out = Output(new Cluster(level))
        val cdata_out = Output(new Cluster(level))
        val select_child_out = Output(Bool())

        // vertical ports
        val select_child_prev_in = Input(Bool())
        val raddr_next_out = Output(UInt(Const.link_width(level).W))
    })

    // the address actually read from the memory
    io.raddr_out := io.raddr_in

    // forwarding
    // if the processor subsequently read the same address,
    //      we should use the forwarding data to replace the read data
    // NOTE: the highest bit is used to indicate if the address is valid
    //       so we do not need to check the highest bit when comparing the address
    val raddr_delay = Reg(UInt(Const.link_width(level).W))
    raddr_delay := io.raddr_in
    val require_forwarding = raddr_delay.drop_highest === io.raddr_in.drop_highest
    
    // select the forwarding data
    io.rdata_lc_out := Mux(require_forwarding && !io.select_child_prev_in, io.forward_data_lc_in, io.rdata_lc_in)
    io.rdata_rc_out := Mux(require_forwarding && io.select_child_prev_in, io.forward_data_rc_in, io.rdata_rc_in)

    // the data of the child to be compared
    val cdata_out = Mux(io.select_child_prev_in, 
        Mux(require_forwarding, io.forward_data_rc_in, io.rdata_rc_in),
        Mux(require_forwarding, io.forward_data_lc_in, io.rdata_lc_in)
    )
    io.cdata_out := cdata_out

    // select the child to be compared
    io.select_child_out := io.select_child_prev_in

    // the address of the entry to be read on the next level
    io.raddr_next_out := cdata_out.next

}
