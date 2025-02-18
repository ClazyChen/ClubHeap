package fpga

import chisel3._
import chisel3.util._

// A memory module with two memories
//     (storing the sister nodes in the heap)
// - read (always read the sister nodes at the same time, but only one of them is actually used in the compare stage)
//   - one addr port (no enable port, because the read is always enabled)
//   - two data ports (one for each sister node)
// - write (always write the sister nodes at the same time, but only one of them is actually modified)
//   - one addr port (no enable port, because the write is always enabled)
//   - two data ports (one for each sister node)
class SisterMem(
    val data_depth: Int,
    val data_width: Int,
    val use_ffmem: Boolean = false, // if true, use FFMem, otherwise use Sram
    val no_sister: Boolean = false, // if true, no sister nodes (i.e. only one memory, representing the root node)
) extends Module {

    // the address width
    val addr_width = log2Ceil(data_depth)

    // the IO interface
    val io = IO(new Bundle {
        // read port
        val raddr_in = Input(UInt(addr_width.W))
        val rdata_lc_out = Output(UInt(data_width.W))
        val rdata_rc_out = Output(UInt(data_width.W))

        // write port
        val waddr_in = Input(UInt(addr_width.W))
        val wdata_lc_in = Input(UInt(data_width.W))
        val wdata_rc_in = Input(UInt(data_width.W))
    })

    // generate one of the memories
    def generate_memory() = if (use_ffmem) {
        Module(new FFMem(data_depth, data_width))
    } else {
        Module(new Sram(data_depth, data_width))
    }

    // the memory of the left child
    val mem_lc = generate_memory()

    // if there is no sister nodes, then only the left child memory is used
    if (no_sister) {
        io.rdata_lc_out := mem_lc.read(io.raddr_in)
        io.rdata_rc_out := DontCare
        mem_lc.write(io.waddr_in, io.wdata_lc_in)
    } else {
        // the memory of the right child
        val mem_rc = generate_memory()
        io.rdata_lc_out := mem_lc.read(io.raddr_in)
        io.rdata_rc_out := mem_rc.read(io.raddr_in)
        mem_lc.write(io.waddr_in, io.wdata_lc_in)
        mem_rc.write(io.waddr_in, io.wdata_rc_in)
    }
}
