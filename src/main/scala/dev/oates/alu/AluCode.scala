package dev.oates.alu

import chisel3._
import chisel3.experimental.ChiselEnum

object AluCode extends ChiselEnum {
  val noop = Value(0.U)
  val add = Value(1.U)
  val sub = Value(2.U)
  val a = Value(3.U)
  val and = Value(4.U)
  val or = Value(5.U)
  val not = Value(6.U)
  val xor = Value(7.U)
}
