package fpga

import chisel3._
import chisel3.util._

// Read-Memory IO
class ReadMemIO(val level: Int) extends Bundle {
    val ren = Input(Bool())
    val raddr_raw = Input(UInt(HeapConst.addr_width(level).W))
    val rdata_lc = Output(new Cluster(level))
    val rdata_rc = Output(new Cluster(level))
    val raddr = Output(UInt(HeapConst.addr_width(level).W))
}

// Write-Memory IO
class WriteMemIO(val level: Int) extends Bundle {
    val wen = Input(Bool())
    val waddr = Input(UInt(HeapConst.addr_width(level).W))
    val wdata_lc = Input(new Cluster(level))
    val wdata_rc = Input(new Cluster(level))
}

// Pipeline stash
class Stash(val level: Int) extends Bundle {
    val addr = UInt(HeapConst.addr_width(level).W)
    val paddr = UInt(HeapConst.paddr_width(level).W)
    val data_lc = new Cluster(level)
    val data_rc = new Cluster(level)
}

// C-P Pipeline stash
class CPStash(val level: Int) extends Bundle {
    val op_next = new Operator
    val sc_next = Bool()
    val sc = Bool()
    val data = new Stash(level)
}

// Inter-level ports
// processor(level-1) to processor(level)
class InterLevelIO(val level: Int) extends Bundle {
    val addr = Input(UInt(HeapConst.addr_width(level).W))
    val paddr = Input(UInt(HeapConst.paddr_width(level).W))
    val op = Input(new Operator)
    val sc = Input(Bool())
    val new_min = Output(new Entry)
    val next = Output(UInt(HeapConst.addr_width(level).W))
}

// Top-level ports
class PQIO extends Bundle {
    val queue_id_in = Input(UInt(log2Ceil(HeapConst.count_of_partitions).W))
    val op_in = Input(new Operator)
    val entry_out = Output(new Entry)
}
