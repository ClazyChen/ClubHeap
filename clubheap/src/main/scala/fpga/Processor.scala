package fpga

import chisel3._
import chisel3.util._


// a processor in a level of the clubheap
// -----------------------------------
// containing 3 stages:
// - READ: read the data from the memory
// - COMPARE: compare the data with the current data
// - WRITE: write the data to the memory
// -------------------------------------------------------------------------------------------------------
// within three cycles:
//     (previous level)         (this level)                                 (next level)
//                           |================|===========================|=================|====== clock 0
// --- raddr_prev_in ------> |                |
//                           |      READ      |
// <-- link_prev_out ------- |                |
//                           |================|===========================|=================|====== clock 1
// - select_child_prev_in -> |   COMPARE.PRE  | --- raddr_next_out -----> |                 |
//                           |................|                           |                 |
// ------ op_prev_in ------> |                | <-- link_next_in -------- |      READ       |
//                           |    COMPARE     |                           |                 |
// <--- new_min_prev_out --- |                |                           |                 |
//                           |================|===========================|=================|====== clock 2
//                           |                | - select_child_next_out > |    COMPARE.PRE  |
//                           |                |                           |.................|
//                           |  COMPARE.POST  | ------ op_next_out -----> |                 |
//                           |                |                           |     COMPARE     |
//                           |                | <--- new_min_next_in ---- |                 |                           |                 |
//                           |................|                           |                 |
//                           |   WRITE        |                           |                 |
//                           |================|===========================|=================|====== clock 3
// -------------------------------------------------------------------------------------------------------
// ports (only vertical ports):
// raddr : the address of the entry to read (upper -> lower)
// link : the link field of the previous level (lower -> upper)
// op : the operation to be performed (upper -> lower)
// select_child : the selection of the child to be compared (upper -> lower)
// new_min : the new minimum ranked element to be promoted (lower -> upper)

class Processor(val level: Int) extends Module {
    val io = IO(new Bundle {
        val raddr_prev_in = Input(UInt(Const.link_width(level).W))
        val link_prev_out = Output(UInt(Const.link_width(level).W))
        val select_child_prev_in = Input(Bool())
        val op_prev_in = Input(new Operator)
        val new_min_prev_out = Output(new Entry)

        val raddr_next_out = Output(UInt(Const.link_width(level).W))
        val link_next_in = Input(UInt(Const.link_width(level).W))
        val select_child_next_out = Output(Bool())
        val op_next_out = Output(new Operator)
        val new_min_next_in = Input(new Entry)
    })

    val read = Module(new Read(level))
    val compare_pre = Module(new ComparePre(level))
    val compare = Module(new Compare(level))
    val compare_post = Module(new ComparePost(level))
    val write = Module(new Write(level))
    val mem = Module(new Memory(level))

    // read - compare.pre
    read.io.raddr_out <> compare_pre.io.raddr_in
    read.io.rdata_lc_out <> compare_pre.io.rdata_lc_in
    read.io.rdata_rc_out <> compare_pre.io.rdata_rc_in

    // compare.pre - compare
    compare_pre.io.raddr_out <> compare.io.raddr_in
    compare_pre.io.rdata_lc_out <> compare.io.rdata_lc_in
    compare_pre.io.rdata_rc_out <> compare.io.rdata_rc_in
    compare_pre.io.cdata_out <> compare.io.cdata_in
    compare_pre.io.select_child_out <> compare.io.select_child_in

    // compare - compare.post
    compare.io.raddr_out <> compare_post.io.raddr_in
    compare.io.rdata_lc_out <> compare_post.io.rdata_lc_in
    compare.io.rdata_rc_out <> compare_post.io.rdata_rc_in
    compare.io.select_child_out <> compare_post.io.select_child_in
    compare.io.select_child_next_level_out <> compare_post.io.select_child_next_level_in
    compare.io.op_next_level_out <> compare_post.io.op_next_level_in
    compare.io.new_min_valid_out <> compare_post.io.new_min_valid_in

    // compare.post - write
    compare_post.io.waddr_out <> write.io.waddr_in
    compare_post.io.wdata_lc_out <> write.io.wdata_lc_in
    compare_post.io.wdata_rc_out <> write.io.wdata_rc_in

    // compare.post - compare.pre (forwarding)
    compare_post.io.forward_data_lc_out <> compare_pre.io.forward_data_lc_in
    compare_post.io.forward_data_rc_out <> compare_pre.io.forward_data_rc_in

    // read - memory
    read.io.raddr_mem <> mem.io.raddr_in
    read.io.raddr_actual_mem <> mem.io.raddr_actual_out
    read.io.rdata_lc_mem <> mem.io.rdata_lc_out
    read.io.rdata_rc_mem <> mem.io.rdata_rc_out

    // memory - write
    mem.io.waddr_in <> write.io.waddr_mem
    mem.io.wdata_lc_in <> write.io.wdata_lc_mem
    mem.io.wdata_rc_in <> write.io.wdata_rc_mem

    // vertical ports
    // previous level <> this level
    io.raddr_prev_in <> read.io.raddr_prev_in
    io.link_prev_out <> read.io.link_prev_out
    io.select_child_prev_in <> compare_pre.io.select_child_prev_in
    io.op_prev_in <> compare.io.op_prev_in
    io.new_min_prev_out <> compare.io.new_min_prev_out

    // this level <> next level
    io.raddr_next_out <> compare_pre.io.raddr_next_out
    io.link_next_in <> compare.io.link_next_in
    io.select_child_next_out <> compare_post.io.select_child_next_out
    io.op_next_out <> compare_post.io.op_next_out
    io.new_min_next_in <> compare_post.io.new_min_next_in

}


