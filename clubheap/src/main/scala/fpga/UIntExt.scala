package fpga

import chisel3._
import chisel3.util._

object UIntExt {
    implicit class UIntExtensions(x: UInt) {

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
