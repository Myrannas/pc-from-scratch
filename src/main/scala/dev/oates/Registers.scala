package dev.oates

import chisel3._
import chisel3.util.{log2Ceil, isPow2}

class Registers(count: Int, width: Int, debug: Boolean = false) extends Module {
  require(count >= 4, "Register count must be > 4")
  require(isPow2(count), "Register count must be a power of 2")
  require(isPow2(width), "Register count must be a power of 2")

  val io = IO(new Bundle {
    val in = Input(UInt(width.W))
    val outA = Output(UInt(width.W))
    val outB = Output(UInt(width.W))
    val inSelect = Input(UInt(log2Ceil(count).W))
    val write = Input(Bool())
    val outSelectA = Input(UInt(log2Ceil(count).W))
    val outSelectB = Input(UInt(log2Ceil(count).W))
  })

  private val registers = RegInit(VecInit(Seq.fill(count)(0.U(width.W))))

  when(io.write === true.B && io.inSelect === io.outSelectA) {
    io.outA := io.in
  } otherwise {
    io.outA := registers(io.outSelectA)
  }

  when(io.write === true.B && io.inSelect === io.outSelectB) {
    io.outB := io.in
  } otherwise {
    io.outB := registers(io.outSelectB)
  }

  when(io.write === true.B) {
    registers(io.inSelect) := io.in
  }

  if (debug) {
    printf("Registers:\n");

    for (i <- 0 until registers.length) {
      printf(p"\tRegister $i: ${registers(i)}\n")
    }
    printf(p"\tOutputs [${io.outSelectA}]: ${io.outA} [${io.outSelectB}]: ${io.outB}\n")
    printf(p"\tInputs [${io.inSelect}]: ${io.in}\n")
  }
}
