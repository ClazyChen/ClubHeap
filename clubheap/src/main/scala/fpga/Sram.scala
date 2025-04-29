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
    def write(addr: UInt, data: UInt, en: Bool): Unit
    def read(addr: UInt, en: Bool): UInt
}

// [ClubHeap+] We have a no hazard design for the memory, so it will never happen that
//             we read and write the same address at the same time
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

    // Read data from memory when read enable is active
    io.r.data := mem.read(io.r.addr, io.r.en)

    // Write data to memory when write enable is active
    when (io.w.en) {
        mem.write(io.w.addr, io.w.data)
    }

    // Memory trait implementation
    def write(addr: UInt, data: UInt, en: Bool): Unit = {
        io.w.en := en
        io.w.addr := addr
        io.w.data := data
    }

    def read(addr: UInt, en: Bool): UInt = {
        io.r.en := en
        io.r.addr := addr
        io.r.data
    }
}