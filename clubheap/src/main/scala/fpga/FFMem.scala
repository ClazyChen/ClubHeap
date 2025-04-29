package fpga

import chisel3._
import chisel3.util._

// NOTE: this module is not used in ClubHeap because we need more scalability
//       FFMem needs too much flip-flops in FPGA

// A psuedo SRAM memory implemented by a register file
class FFMem(
    val data_depth: Int,
    val data_width: Int,
) extends Module with MemoryTrait {
    val addr_width = log2Ceil(data_depth)

    val io = IO(new Bundle {
        val r = new ReadPort(addr_width, data_width)
        val w = new WritePort(addr_width, data_width)
    })

    // the memory
    val mem = Reg(Vec(data_depth, UInt(data_width.W)))

    // write the data to the memory
    when (io.w.en) {
        mem(io.w.addr) := io.w.data
    }

    // read the data from the memory
    val rdata = RegNext(mem(io.r.addr))
    val ren = RegNext(io.r.en)
    io.r.data := Mux(ren, rdata, DontCare)

    // the trait of the memory
    // write the data to the memory
    def write(addr: UInt, data: UInt, en: Bool): Unit = {
        io.w.en := en
        io.w.addr := addr
        io.w.data := data
    }

    // read the data from the memory
    def read(addr: UInt, en: Bool): UInt = {
        io.r.en := en
        io.r.addr := addr
        io.r.data
    }

}
