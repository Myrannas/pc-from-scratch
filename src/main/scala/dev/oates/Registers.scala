package dev.oates

import chisel3._
import chisel3.util.{log2Ceil, isPow2}

class Registers(count: Int, width: Int, debug: Boolean = false) extends Module {
  require(count >= 4, "Register count must be > 4")
  require(isPow2(count), "Register count must be a power of 2")
  require(isPow2(width), "Register count must be a power of 2")

  val io = IO(new Bundle {
    val in: UInt = Input(UInt(width.W))
    val outA: UInt = Output(UInt(width.W))
    val outB: UInt = Output(UInt(width.W))
    val inSelect: UInt = Input(UInt(log2Ceil(count).W))
    val write: Bool = Input(Bool())
    val outSelectA: UInt = Input(UInt(log2Ceil(count).W))
    val outSelectB: UInt = Input(UInt(log2Ceil(count).W))
  })

  private val registers = RegInit(VecInit(Seq.fill(count)(0.U(width.W))))
  private val outA = Wire(UInt(width.W))
  private val outB = Wire(UInt(width.W))

  io.outA := RegNext(outA)
  io.outB := RegNext(outB)

  when(io.write === true.B && io.inSelect === io.outSelectA) {
    outA := io.in
  } otherwise {
    outA := registers(io.outSelectA)
  }

  when(io.write === true.B && io.inSelect === io.outSelectB) {
    outB := io.in
  } otherwise {
    outB := registers(io.outSelectB)
  }

  when(io.write === true.B) {
    registers(io.inSelect) := io.in
  }

  if (debug) {
    printf("Registers:\n");

    for (i <- 0 until registers.length) {
      printf(p"\tRegister $i: ${registers(i)}\n")
    }
    printf(
      p"\tOutputs [${io.outSelectA}]: ${io.outA} [${io.outSelectB}]: ${io.outB}\n")

    when(io.write) {
      printf(p"\tInputs [${io.inSelect}]: ${io.in}\n")
    }
  }
}
