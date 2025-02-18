package fpga

import chisel3._
import chisel3.util._

// the clubheap
class ClubHeap extends Module {
    
    val io = IO(new Bundle {
        val op_in = Input(new Operator)
        val queue_id_in = Input(UInt(log2Ceil(Const.count_of_partitions).W))
        val entry_out = Output(new Entry)
    })

    // the top processor
    val top_proc = Module(new TopProcessor)
    val proc = for (i <- 1 until Const.count_of_levels + 1) yield {
        Module(new Processor(i))
    }

    // connect the processors
    top_proc.io.queue_id_in <> io.queue_id_in
    top_proc.io.op_in <> io.op_in
    top_proc.io.entry_out <> io.entry_out

    // top to first processor
    top_proc.io.raddr_next_out <> proc(0).io.raddr_prev_in
    top_proc.io.link_next_in <> proc(0).io.link_prev_out
    top_proc.io.select_child_next_out <> proc(0).io.select_child_prev_in
    top_proc.io.op_next_out <> proc(0).io.op_prev_in
    top_proc.io.new_min_next_in <> proc(0).io.new_min_prev_out

    // between processors
    for (i <- 0 until Const.count_of_levels - 1) {
        proc(i).io.raddr_next_out <> proc(i+1).io.raddr_prev_in
        proc(i).io.link_next_in <> proc(i+1).io.link_prev_out
        proc(i).io.select_child_next_out <> proc(i+1).io.select_child_prev_in
        proc(i).io.op_next_out <> proc(i+1).io.op_prev_in
        proc(i).io.new_min_next_in <> proc(i+1).io.new_min_prev_out
    }

    // the last processor
    proc(Const.count_of_levels - 1).io.link_next_in := (-1).S.asUInt
    proc(Const.count_of_levels - 1).io.new_min_next_in := Entry.empty
}
