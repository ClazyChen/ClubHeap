package fpga

import chisel3._
import chisel3.util._

// A memory module with two memories
//     (storing the sister nodes in the heap)
// - read (always read the sister nodes at the same time, but only one of them is actually used in the compare stage)
//   - one addr port (no enable port, because the read is always enabled)
//   - two data ports (one for each sister node)
//   - one ren port (if false, no read operation is performed)
// - write (can choose to not write)
//   - one addr port
//   - two data ports (one for each sister node)
//   - one wen port (if false, no write operation is performed)
// The "right" memory at the first level is not used
class SisterMem(
    val level: Int,
    val use_ffmem: Boolean = false, // if true, use FFMem, otherwise use Sram
) extends Module {

    // the address width
    val addr_width = HeapConst.link_width(level-1)
    val data_width = HeapConst.data_width(level)
    val data_depth = HeapConst.data_depth(level)

    // the IO interface
    val io = IO(new Bundle {
        // read port
        val ren_in = Input(Bool()) // Read enable signal
        val raddr_in = Input(UInt(addr_width.W))
        val rdata_lc_out = Output(new Cluster(level))
        val rdata_rc_out = Output(new Cluster(level))

        // write port
        val wen_in = Input(Bool()) // Write enable signal
        val waddr_in = Input(UInt(addr_width.W))
        val wdata_lc_in = Input(new Cluster(level))
        val wdata_rc_in = Input(new Cluster(level))
    })

    // generate one of the memories
    def generate_memory() = if (use_ffmem) {
        Module(new FFMem(data_depth, data_width))
    } else {
        Module(new Sram(data_depth, data_width))
    }

    val mem_lc = generate_memory()
    val mem_rc = generate_memory()

    // memory structure
    // - last level: only Vec(K-1, new Entry)
    // - static cluster: StaticCluster(level)
    // - dynamic cluster: Cluster(level)
    val K = HeapConst.count_of_elements_in_each_cluster
    val is_last_level = level == HeapConst.count_of_levels
    val is_dynamic_cluster = HeapConst.is_dynamic_cluster(level)

    // read logic
    io.rdata_lc_out := DontCare
    io.rdata_rc_out := DontCare
    val rdata_lc = mem_lc.read(io.raddr_in, io.ren_in)
    val rdata_rc = mem_rc.read(io.raddr_in, io.ren_in)
    if (is_last_level) {
        io.rdata_lc_out.entries := rdata_lc.asTypeOf(Vec(K-1, new Entry))
        io.rdata_rc_out.entries := rdata_rc.asTypeOf(Vec(K-1, new Entry))
        io.rdata_lc_out.next := rdata_lc.asTypeOf(Vec(K-1, new Entry))(0).metadata // reuse this field for the next pointer for allocation
    } else {
        if (is_dynamic_cluster) {
            io.rdata_lc_out := rdata_lc.asTypeOf(new Cluster(level))
            io.rdata_rc_out := rdata_rc.asTypeOf(new Cluster(level))
        } else { // static cluster
            io.rdata_lc_out := rdata_lc.asTypeOf(new StaticCluster(level)).to_dynamic
            io.rdata_rc_out := rdata_rc.asTypeOf(new StaticCluster(level)).to_dynamic
            io.rdata_lc_out.next := Cat(io.raddr_in, false.B) // static cluster has no next pointer
            io.rdata_rc_out.next := Cat(io.raddr_in, true.B)
        }
    }

    // write logic
    if (is_last_level) {
        mem_lc.write(io.waddr_in, io.wdata_lc_in.entries.asUInt, io.wen_in)
        mem_rc.write(io.waddr_in, io.wdata_rc_in.entries.asUInt, io.wen_in)
    } else {
        if (is_dynamic_cluster) {
            mem_lc.write(io.waddr_in, io.wdata_lc_in.asUInt, io.wen_in)
            mem_rc.write(io.waddr_in, io.wdata_rc_in.asUInt, io.wen_in)
        } else { // static cluster
            mem_lc.write(io.waddr_in, io.wdata_lc_in.to_static.asUInt, io.wen_in)
            mem_rc.write(io.waddr_in, io.wdata_rc_in.to_static.asUInt, io.wen_in)
        }
    }

    // for the first level, the right memory will be optimized out
    if (level == 1) {
        io.rdata_rc_out := DontCare
    }
}
