package dev.oates.control

import chiseltest._
import chisel3._
import dev.oates.control.Utils.BusUtils
import org.scalatest.flatspec.AnyFlatSpec

class UtilsTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "split a bus into a list of bits" in {
    test(new Module {
      val io = IO(new Bundle {
        val bus = Input(UInt(9.W))
        val bits1 = Output(UInt(2.W))
        val bits2 = Output(UInt(3.W))
        val bits3 = Output(UInt(4.W))
      })

      val Seq(
        b1, b2, b3
      ) = io.bus.split(io.bits1.getWidth, io.bits2.getWidth, io.bits3.getWidth)

      io.bits1 := b1
      io.bits2 := b2
      io.bits3 := b3
    }) { c =>
      c.io.bus.poke(311.U)
      c.io.bits1.expect(2.U)
      c.io.bits2.expect(3.U)
      c.io.bits3.expect(7.U)
    }
  }
}
