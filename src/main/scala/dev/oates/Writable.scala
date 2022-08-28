package dev.oates

import chisel3._

class Writable[T <: Data](gen: T) extends Bundle {
  val value = Input(gen)
  val enable = Input(Bool())
}
