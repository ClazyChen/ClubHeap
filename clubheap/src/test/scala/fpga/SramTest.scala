package fpga

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SramTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Sram"

  // Test basic read/write operations
  it should "perform basic read/write operations" in {
    test(new Sram(data_depth = 16, data_width = 32)) { dut =>
      // Write some data
      dut.io.w.en.poke(true.B)
      dut.io.w.addr.poke(0.U)
      dut.io.w.data.poke(0xdeadbeefL.U)
      dut.clock.step()
      
      // Read the data back
      dut.io.r.en.poke(true.B)
      dut.io.r.addr.poke(0.U)
      dut.clock.step()
      dut.io.r.data.expect(0xdeadbeefL.U)
    }
  }

  // Test simultaneous read and write to different addresses
  it should "handle simultaneous read and write to different addresses" in {
    test(new Sram(data_depth = 16, data_width = 32)) { dut =>
      // Write initial data
      dut.io.w.en.poke(true.B)
      dut.io.w.addr.poke(1.U)
      dut.io.w.data.poke(0x11111111L.U)
      dut.clock.step()
      
      // Simultaneously write to addr 2 and read from addr 1
      dut.io.w.en.poke(true.B)
      dut.io.w.addr.poke(2.U)
      dut.io.w.data.poke(0x22222222L.U)
      
      dut.io.r.en.poke(true.B)
      dut.io.r.addr.poke(1.U)
      dut.clock.step()
      
      dut.io.r.data.expect(0x11111111L.U)
    }
  }

  // Test disabled read/write ports
  it should "handle disabled read/write ports correctly" in {
    test(new Sram(data_depth = 16, data_width = 32)) { dut =>
      // Write initial data
      dut.io.w.en.poke(true.B)
      dut.io.w.addr.poke(5.U)
      dut.io.w.data.poke(0x55555555L.U)
      dut.clock.step()
      
      // Disable both read and write
      dut.io.w.en.poke(false.B)
      dut.io.r.en.poke(false.B)
      dut.clock.step()
      
      // Enable only read
      dut.io.r.en.poke(true.B)
      dut.io.r.addr.poke(5.U)
      dut.clock.step()
      
      // Should still read the initial data
      dut.io.r.data.expect(0x55555555L.U)
    }
  }

  // Test read-during-write at consecutive addresses
  it should "handle read-during-write at consecutive addresses" in {
    test(new Sram(data_depth = 16, data_width = 32)) { dut =>
      // Write to address 0
      dut.io.w.en.poke(true.B)
      dut.io.w.addr.poke(0.U)
      dut.io.w.data.poke(0xAAAAAAAAL.U)
      dut.clock.step()
      
      // Simultaneously write to address 1 and read from address 0
      dut.io.w.addr.poke(1.U)
      dut.io.w.data.poke(0xBBBBBBBBL.U)
      dut.io.r.en.poke(true.B)
      dut.io.r.addr.poke(0.U)
      dut.clock.step()
      
      dut.io.r.data.expect(0xAAAAAAAAL.U)
      
      // Read from address 1
      dut.io.r.addr.poke(1.U)
      dut.clock.step()
      
      dut.io.r.data.expect(0xBBBBBBBBL.U)
    }
  }
} 