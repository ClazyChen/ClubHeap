package fpga

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SisterMemSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  // Test parameters
  val dataDepth = 16
  val dataWidth = 32

  // Test case for no_sister = true
  "SisterMem with no_sister=true" should "only use left child memory" in {
    test(new SisterMem(dataDepth, dataWidth, use_ffmem = false, no_sister = true)) { dut =>
      // Initialize all signals
      dut.io.wen_in.poke(false.B)
      dut.io.waddr_in.poke(0.U)
      dut.io.wdata_lc_in.poke(0.U)
      dut.io.wdata_rc_in.poke(0.U)
      dut.io.raddr_in.poke(0.U)
      dut.io.ren_in.poke(false.B)
      dut.clock.step(1)

      // Write test
      dut.io.wen_in.poke(true.B)
      dut.io.waddr_in.poke(5.U)
      dut.io.wdata_lc_in.poke(0x12345678L.U)
      dut.io.wdata_rc_in.poke(0x87654321L.U) // This should be ignored
      dut.clock.step(1)

      // Read test with read enable
      dut.io.wen_in.poke(false.B)
      dut.io.raddr_in.poke(5.U)
      dut.io.ren_in.poke(true.B)
      dut.clock.step(1)
      dut.io.rdata_lc_out.expect(0x12345678L.U)
      // Right child data should be don't care
      dut.io.rdata_rc_out.peek() // Just peek to verify it exists

      // Test read disable
      dut.io.ren_in.poke(false.B)
      dut.clock.step(1)
      // Data should be don't care when read is disabled
      dut.io.rdata_lc_out.peek()
      dut.io.rdata_rc_out.peek()

      // Test simultaneous read and write to different addresses
      dut.io.wen_in.poke(true.B)
      dut.io.waddr_in.poke(7.U)
      dut.io.wdata_lc_in.poke(0xAAAAAAAAL.U)
      dut.io.raddr_in.poke(5.U) // Read from previous address
      dut.io.ren_in.poke(true.B)
      dut.clock.step(1)
      dut.io.rdata_lc_out.expect(0x12345678L.U) // Should read the old data
    }
  }

  // Test case for no_sister = false
  "SisterMem with no_sister=false" should "use both left and right child memories" in {
    test(new SisterMem(dataDepth, dataWidth, use_ffmem = false, no_sister = false)) { dut =>
      // Initialize all signals
      dut.io.wen_in.poke(false.B)
      dut.io.waddr_in.poke(0.U)
      dut.io.wdata_lc_in.poke(0.U)
      dut.io.wdata_rc_in.poke(0.U)
      dut.io.raddr_in.poke(0.U)
      dut.io.ren_in.poke(false.B)
      dut.clock.step(1)

      // Write test
      dut.io.wen_in.poke(true.B)
      dut.io.waddr_in.poke(5.U)
      dut.io.wdata_lc_in.poke(0x12345678L.U)
      dut.io.wdata_rc_in.poke(0x87654321L.U)
      dut.clock.step(1)

      // Read test with read enable
      dut.io.wen_in.poke(false.B)
      dut.io.raddr_in.poke(5.U)
      dut.io.ren_in.poke(true.B)
      dut.clock.step(1)
      dut.io.rdata_lc_out.expect(0x12345678L.U)
      dut.io.rdata_rc_out.expect(0x87654321L.U)

      // Test read disable
      dut.io.ren_in.poke(false.B)
      dut.clock.step(1)
      // Data should be don't care when read is disabled
      dut.io.rdata_lc_out.peek()
      dut.io.rdata_rc_out.peek()

      // Test write disable
      dut.io.wen_in.poke(false.B)
      dut.io.waddr_in.poke(6.U)
      dut.io.wdata_lc_in.poke(0xAAAAAAAAL.U)
      dut.io.wdata_rc_in.poke(0xBBBBBBBBL.U)
      dut.clock.step(1)

      // Verify data was not written
      dut.io.raddr_in.poke(6.U)
      dut.io.ren_in.poke(true.B)
      dut.clock.step(1)
      dut.io.rdata_lc_out.expect(0.U) // Should be 0 as it was not written
      dut.io.rdata_rc_out.expect(0.U) // Should be 0 as it was not written

      // Test simultaneous read and write to different addresses
      dut.io.wen_in.poke(true.B)
      dut.io.waddr_in.poke(8.U)
      dut.io.wdata_lc_in.poke(0xCCCCCCCCL.U)
      dut.io.wdata_rc_in.poke(0xDDDDDDDDL.U)
      dut.io.raddr_in.poke(5.U) // Read from previous address
      dut.io.ren_in.poke(true.B)
      dut.clock.step(1)
      dut.io.rdata_lc_out.expect(0x12345678L.U) // Should read the old data
      dut.io.rdata_rc_out.expect(0x87654321L.U) // Should read the old data
    }
  }
} 