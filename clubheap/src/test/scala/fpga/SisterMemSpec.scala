package fpga

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SisterMemSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  // Test parameters
  val level = 2 // Test with level 2 (not the last level)
  val K = HeapConst.count_of_elements_in_each_cluster
  val is_dynamic_memory = HeapConst.is_dynamic_memory(level)

  // Test case for level 1 (root level)
  "SisterMem at level 1" should "only use left child memory" in {
    test(new SisterMem(level = 1)) { dut =>
      // Initialize all signals
      dut.io.wen_in.poke(false.B)
      dut.io.waddr_in.poke(0.U)
      dut.io.raddr_in.poke(0.U)
      dut.io.ren_in.poke(false.B)
      dut.clock.step(1)

      // Write test
      dut.io.wen_in.poke(true.B)
      dut.io.waddr_in.poke(5.U)
      // Set left child data
      dut.io.wdata_lc_in.entries(0).rank.poke(1.U)
      dut.io.wdata_lc_in.entries(0).metadata.poke(0x1234.U)
      dut.io.wdata_lc_in.entries(0).existing.poke(true.B)
      dut.clock.step(1)

      // Read test with read enable
      dut.io.wen_in.poke(false.B)
      dut.io.raddr_in.poke(5.U)
      dut.io.ren_in.poke(true.B)
      dut.clock.step(1)
      dut.io.rdata_lc_out.entries(0).rank.expect(1.U)
      dut.io.rdata_lc_out.entries(0).metadata.expect(0x1234.U)
      dut.io.rdata_lc_out.entries(0).existing.expect(true.B)
      // Right child data should be don't care
      dut.io.rdata_rc_out.peek() // Just peek to verify it exists
    }
  }

  // Test case for normal levels
  "SisterMem at normal levels" should "use both left and right child memories" in {
    test(new SisterMem(level = level)) { dut =>
      // Initialize all signals
      dut.io.wen_in.poke(false.B)
      dut.io.waddr_in.poke(0.U)
      dut.io.raddr_in.poke(0.U)
      dut.io.ren_in.poke(false.B)
      dut.clock.step(1)

      // Write test
      dut.io.wen_in.poke(true.B)
      dut.io.waddr_in.poke(5.U)
      // Set left child data
      dut.io.wdata_lc_in.entries(0).rank.poke(1.U)
      dut.io.wdata_lc_in.entries(0).metadata.poke(0x1234.U)
      dut.io.wdata_lc_in.entries(0).existing.poke(true.B)
      // Set right child data
      dut.io.wdata_rc_in.entries(0).rank.poke(2.U)
      dut.io.wdata_rc_in.entries(0).metadata.poke(0x5678.U)
      dut.io.wdata_rc_in.entries(0).existing.poke(true.B)
      dut.clock.step(1)

      // Read test with read enable
      dut.io.wen_in.poke(false.B)
      dut.io.raddr_in.poke(5.U)
      dut.io.ren_in.poke(true.B)
      dut.clock.step(1)
      dut.io.rdata_lc_out.entries(0).rank.expect(1.U)
      dut.io.rdata_lc_out.entries(0).metadata.expect(0x1234.U)
      dut.io.rdata_lc_out.entries(0).existing.expect(true.B)
      dut.io.rdata_rc_out.entries(0).rank.expect(2.U)
      dut.io.rdata_rc_out.entries(0).metadata.expect(0x5678.U)
      dut.io.rdata_rc_out.entries(0).existing.expect(true.B)

      // Test read disable
      dut.io.ren_in.poke(false.B)
      dut.clock.step(1)
      // Data should be don't care when read is disabled
      dut.io.rdata_lc_out.peek()
      dut.io.rdata_rc_out.peek()

      // Test simultaneous read and write to different addresses
      dut.io.wen_in.poke(true.B)
      dut.io.waddr_in.poke(7.U)
      // Set new left child data
      dut.io.wdata_lc_in.entries(0).rank.poke(3.U)
      dut.io.wdata_lc_in.entries(0).metadata.poke(0x9ABC.U)
      dut.io.wdata_lc_in.entries(0).existing.poke(true.B)
      // Set new right child data
      dut.io.wdata_rc_in.entries(0).rank.poke(4.U)
      dut.io.wdata_rc_in.entries(0).metadata.poke(0xDEF0.U)
      dut.io.wdata_rc_in.entries(0).existing.poke(true.B)
      dut.io.raddr_in.poke(5.U) // Read from previous address
      dut.io.ren_in.poke(true.B)
      dut.clock.step(1)
      dut.io.rdata_lc_out.entries(0).rank.expect(1.U) // Should read the old data
      dut.io.rdata_lc_out.entries(0).metadata.expect(0x1234.U)
      dut.io.rdata_rc_out.entries(0).rank.expect(2.U)
      dut.io.rdata_rc_out.entries(0).metadata.expect(0x5678.U)
    }
  }

  // Test case for last level
  "SisterMem at last level" should "only store entries" in {
    test(new SisterMem(level = HeapConst.count_of_levels)) { dut =>
      // Initialize all signals
      dut.io.wen_in.poke(false.B)
      dut.io.waddr_in.poke(0.U)
      dut.io.raddr_in.poke(0.U)
      dut.io.ren_in.poke(false.B)
      dut.clock.step(1)

      // Write test
      dut.io.wen_in.poke(true.B)
      dut.io.waddr_in.poke(5.U)
      // Set left child data
      dut.io.wdata_lc_in.entries(0).rank.poke(1.U)
      dut.io.wdata_lc_in.entries(0).metadata.poke(0x1234.U)
      dut.io.wdata_lc_in.entries(0).existing.poke(true.B)
      // Set right child data
      dut.io.wdata_rc_in.entries(0).rank.poke(2.U)
      dut.io.wdata_rc_in.entries(0).metadata.poke(0x5678.U)
      dut.io.wdata_rc_in.entries(0).existing.poke(true.B)
      dut.clock.step(1)

      // Read test with read enable
      dut.io.wen_in.poke(false.B)
      dut.io.raddr_in.poke(5.U)
      dut.io.ren_in.poke(true.B)
      dut.clock.step(1)
      dut.io.rdata_lc_out.entries(0).rank.expect(1.U)
      dut.io.rdata_lc_out.entries(0).metadata.expect(0x1234.U)
      dut.io.rdata_lc_out.entries(0).existing.expect(true.B)
      dut.io.rdata_rc_out.entries(0).rank.expect(2.U)
      dut.io.rdata_rc_out.entries(0).metadata.expect(0x5678.U)
      dut.io.rdata_rc_out.entries(0).existing.expect(true.B)
    }
  }
} 