package fpga

import chisel3._
import chisel3.util._

// The read port of a memory
class ReadPort(
    val addr_width: Int,
    val data_width: Int,
) extends Bundle {
    val en = Input(Bool())
    val addr = Input(UInt(addr_width.W))
    val data = Output(UInt(data_width.W))
}

// The write port of a memory
class WritePort(
    val addr_width: Int,
    val data_width: Int,
) extends Bundle {
    val en = Input(Bool())
    val addr = Input(UInt(addr_width.W))
    val data = Input(UInt(data_width.W))
}

// The trait of a memory
trait MemoryTrait {
    def write(addr: UInt, data: UInt): Unit
    def no_write(): Unit
    def read(addr: UInt): UInt
}

// The SRAM module
class Sram(
    val data_depth: Int,
    val data_width: Int,
) extends Module with MemoryTrait {
    val addr_width = log2Ceil(data_depth)

    val io = IO(new Bundle {
        val r = new ReadPort(addr_width, data_width)
        val w = new WritePort(addr_width, data_width)
    })

    val mem = SyncReadMem(data_depth, UInt(data_width.W))

    // write the data to the memory
    when (io.w.en) {
        mem.write(io.w.addr, io.w.data)
    }

    // read the data from the memory
    val rdata = mem.read(io.r.addr, io.r.en)

    // if the read / write port access the same address
    //    then the read data is the same as the write data
    val same_addr = RegNext(io.r.addr === io.w.addr && io.r.en && io.w.en)
    val wdata = RegNext(io.w.data)
    io.r.data := Mux(same_addr, wdata, rdata)

    // the trait of the memory
    // write the data to the memory
    def write(addr: UInt, data: UInt): Unit = {
        io.w.en := true.B
        io.w.addr := addr
        io.w.data := data
    }

    // do not write the data to the memory
    def no_write(): Unit = {
        io.w.en := false.B
        io.w.addr := DontCare
        io.w.data := DontCare
    }

    // read the data from the memory
    def read(addr: UInt): UInt = {
        io.r.en := true.B
        io.r.addr := addr
        io.r.data
    }
}