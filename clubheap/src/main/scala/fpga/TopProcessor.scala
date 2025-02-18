package fpga

import chisel3._
import chisel3.util._

// the top processor in the clubheap
// it acts as "processor 0"

class TopProcessor extends Module {
    val level = 1
    val io = IO(new Bundle {
        // ports from the top
        val queue_id_in = Input(UInt(log2Ceil(Const.count_of_partitions).W))
        val op_in = Input(new Operator)
        val entry_out = Output(new Entry)

        // it shares the same interface as the other processors
        val raddr_next_out = Output(UInt(Const.link_width(level).W))
        val link_next_in = Input(UInt(Const.link_width(level).W))
        val select_child_next_out = Output(Bool())
        val op_next_out = Output(new Operator)
        val new_min_next_in = Input(new Entry)
    })

    val mins = Reg(Vec(Const.count_of_partitions, new Entry))

    // virtual read module
    val raddr = RegNext(io.queue_id_in)
    val rdata = RegNext(mins(raddr))

    // virtual compare.pre module
    val op = RegNext(io.op_in)
    val op_delay = RegNext(op)
    val raddr_delay = RegNext(raddr)
    val new_data = Reg(new Entry)

    val require_forwarding = raddr_delay === raddr
    val wdata = Mux(op_delay.pop, io.new_min_next_in, new_data)
    val cdata = Mux(require_forwarding, wdata, rdata) // forwarding data = new_min_next_in
    io.raddr_next_out := raddr

    // virtual compare module
    val cmp = op.push.rank < cdata.rank

    // new_min of the top level is the scheduled entry
    io.entry_out := Mux(op.pop,
        Mux(cmp, op.push, cdata),
        Entry.empty
    )

    // update the data of the top level
    new_data := cdata
    when (cmp && !op.pop) {
        new_data := op.push
    }

    // generate the next operator
    val op_next = RegNext(op)
    when (cmp) {
        op_next.push := cdata
    }

    // virtual compare.post module
    io.op_next_out := op_next
    io.select_child_next_out := false.B

    // virtual write module
    val waddr = RegNext(raddr_delay)
    mins(waddr) := wdata

}