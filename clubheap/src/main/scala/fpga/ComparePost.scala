package fpga

import chisel3._
import chisel3.util._

// the postprocessing module of the COMPARE stage
//    this module is used to postprocess the read data
// -----------------------------------
// ports for the COMPARE.POST stage:
// horizontal (within a level):
// - raddr_in: the address of the entry actually read (from the COMPARE stage)
// - rdata_lc_in: the data of the left child of the entry read (from the COMPARE stage)
// - rdata_rc_in: the data of the right child of the entry read (from the COMPARE stage)
// - select_child_in: the selection of the child to be compared (from the COMPARE stage)
// - select_child_next_level_in: the selection of the child to be compared on the next level (from the COMPARE stage)
// - op_next_level_in: the operation to be performed on the next level (from the COMPARE stage)
// - new_min_valid_in: the validity of the promoted new_min (from the COMPARE stage)
// - forward_data_lc_out: the data of the left child of the entry forwarded (to the COMPARE.PRE stage)
// - forward_data_rc_out: the data of the right child of the entry forwarded (to the COMPARE.PRE stage)
// - waddr_out: the address of the entry to write (to the WRITE stage) (same as raddr_in)
// - wdata_lc_out: the data of the left child of the entry read (to the WRITE stage)
// - wdata_rc_out: the data of the right child of the entry read (to the WRITE stage)
// vertical (between levels):
// - new_min_next_in: the promoted new_min (from the COMPARE stage on the next level)
// - op_next_out: the operation to be performed on the next level (to the COMPARE stage on the next level)
// - select_child_next_out: the selection of the child to be compared on the next level (to the COMPARE.PRE stage on the next level)

class ComparePost(val level: Int) extends Module {

    val io = IO(new Bundle {
        val raddr_in = Input(UInt(Const.link_width(level).W))
        val rdata_lc_in = Input(new Cluster(level))
        val rdata_rc_in = Input(new Cluster(level))
        val select_child_in = Input(Bool())
        val select_child_next_level_in = Input(Bool())
        val op_next_level_in = Input(new Operator)
        val new_min_valid_in = Input(Bool())

        val forward_data_lc_out = Output(new Cluster(level))
        val forward_data_rc_out = Output(new Cluster(level))

        val waddr_out = Output(UInt(Const.link_width(level).W))
        val wdata_lc_out = Output(new Cluster(level))
        val wdata_rc_out = Output(new Cluster(level))
        
        val new_min_next_in = Input(new Entry)
        val op_next_out = Output(new Operator)
        val select_child_next_out = Output(Bool())
        
    })

    // directly forward the address to the WRITE stage
    io.waddr_out := io.raddr_in

    val data_lc = Wire(new Cluster(level))
    val data_rc = Wire(new Cluster(level))
    data_lc := io.rdata_lc_in
    data_rc := io.rdata_rc_in

    // replace the min_lc/min_rc of the selected child with the promoted new_min
    when (io.new_min_valid_in) {
        when (io.select_child_in) {
            when (io.select_child_next_level_in) {
                data_rc.min_rc := io.new_min_next_in
            } .otherwise {
                data_rc.min_lc := io.new_min_next_in
            }
        } .otherwise {
            when (io.select_child_next_level_in) {
                data_lc.min_rc := io.new_min_next_in
            } .otherwise {
                data_lc.min_lc := io.new_min_next_in
            }
        }
    }

    // if the selected child has no valid child, set the next field to -1
    val valid_link_lc = data_lc.min_lc.existing || data_lc.min_rc.existing
    val valid_link_rc = data_rc.min_lc.existing || data_rc.min_rc.existing

    when (!valid_link_lc) {
        data_lc.next := (-1).S.asUInt
    }

    when (!valid_link_rc) {
        data_rc.next := (-1).S.asUInt
    }

    io.forward_data_lc_out := data_lc
    io.forward_data_rc_out := data_rc
    io.wdata_lc_out := data_lc
    io.wdata_rc_out := data_rc

    io.op_next_out := io.op_next_level_in
    io.select_child_next_out := io.select_child_next_level_in
}

