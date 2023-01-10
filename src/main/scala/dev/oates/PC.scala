package dev.oates

import chisel3._

class PC(val width: Int, val debug: Boolean = false) extends Module {
  val io = IO(new Bundle() {
    val in = Input(UInt(width.W))
    val out = Output(UInt(width.W))
    val write = Input(Bool())
    val writeRelative = Input(Bool())
    val writeRelativeAddr = Input(UInt(width.W))
  })

  val register = RegInit(0.U(width.W))

  val nextPc = Wire(UInt(width.W))
  when(io.write && io.writeRelative) {
    nextPc := io.writeRelativeAddr + io.in
  }.elsewhen(io.write && !io.writeRelative) {
      nextPc := io.in
    }
    .otherwise {
      nextPc := register + 1.U
    }

  register := nextPc
  io.out := register

  if (debug) {
    printf(p"PC: [${Hexadecimal(io.out)}] Next: [${Hexadecimal(nextPc)}]\n")
  }
}
