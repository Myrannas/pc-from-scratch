package dev.oates.control

import Chisel.{Cat, Fill}
import chisel3._

object Utils {
  implicit class BusUtils(val bus: Bits) {
    def split(widths: Int*): Seq[Bits] = {
      val size = bus.getWidth

      widths
        .scanLeft(0)(_ + _)
        .sliding(2)
        .map {
          case Seq(end, start) => {
            bus(size - end - 1, size - start)
          }
        }
        .toSeq
    }
  }

  implicit class UIntUtils(val input: UInt) {
    def extendSign(toWidth: Int): UInt = {
      require(toWidth > input.getWidth, s"Cannot extend to a smaller width. $toWidth <= ${input.getWidth}")

      val signBit = input.head(1)

      Cat(Fill(toWidth - input.getWidth, signBit), input)
    }
  }
}
