package fpga

import chisel3._
import chisel3.util._

// the clubheap
class ClubHeap extends Module {
    
    val io = IO(new Bundle {
        val out = new InterLevelIO(1)
    })

    // the number of levels
    val count_of_levels = HeapConst.count_of_levels
    
    val proc = for (i <- 1 until count_of_levels + 1) yield {
        Module(new Processor(i))
    }

    // connect the processors
    for (i <- 1 until count_of_levels) {
        proc(i).io.prev <> proc(i-1).io.next
    }

    // connect the output
    io.out <> proc(0).io.prev

    // the last processor
    proc(count_of_levels - 1).io.next.new_min := Entry.empty
    proc(count_of_levels - 1).io.next.next := (-1).S.asUInt
}
