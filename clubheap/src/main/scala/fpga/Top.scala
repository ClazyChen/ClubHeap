package fpga

import chisel3._
import chisel3.util._

// the top processor in the clubheap
// it acts as "processor 0"
// only has COMPARE and PREPARE stages
class Top extends Module {
    val io = IO(new Bundle {
        val top = new PQIO
        val next = Flipped(new InterLevelIO(1))
    })

    val mins = RegInit(VecInit(Seq.fill(Const.count_of_partitions)(Entry.empty)))

    // COMPARE stage
    io.next.addr := io.top.queue_id_in
    io.next.paddr := io.top.queue_id_in
    val data = mins(io.top.queue_id_in)
    val cmp = io.top.op_in.push < data
    val min_data = Mux(cmp, io.top.op_in.push, data)
    val max_data = Mux(cmp, data, io.top.op_in.push)
    mins(io.top.queue_id_in) := min_data

    // PREPARE stage
    val addr_delay = RegNext(io.top.queue_id_in)
    io.next.op.push := RegNext(max_data)
}