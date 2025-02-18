package fpga

import chisel3._
import chisel3.util._

object UIntExt {
    implicit class UIntExtensions(x: UInt) {
        // drop the highest bit
        def drop_highest: UInt = {
            x(x.getWidth-2, 0)
        }

        // select the highest bit
        def select_highest: Bool = {
            if (x.getWidth == 0) {
                false.B
            } else {
                x(x.getWidth-1)
            }
        }
    }
}
