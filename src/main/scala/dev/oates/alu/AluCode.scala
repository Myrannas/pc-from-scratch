package dev.oates.alu

import chisel3._
import chisel3.experimental.ChiselEnum

object AluCode extends ChiselEnum {
  val noop: AluCode.Type = Value(0.U)
  val add: AluCode.Type = Value(1.U)
  val sub: AluCode.Type = Value(2.U)
  val a: AluCode.Type = Value(3.U)
  val and: AluCode.Type = Value(4.U)
  val or: AluCode.Type = Value(5.U)
  val not: AluCode.Type = Value(6.U)
  val xor: AluCode.Type = Value(7.U)
}
