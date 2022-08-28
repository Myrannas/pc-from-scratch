package dev.oates

import chisel3._
import chisel3.util.{log2Ceil, isPow2}



class Registers(count: Int, width: Int) extends Module {
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
    val pc = Output(UInt(width.W))
  })


  private val registers = RegInit(VecInit(Seq.fill(count)(0.U(width.W))))
  private val pcRegister = (count - 1).U

  io.outA := registers(io.outSelectA)
  io.outB := registers(io.outSelectB)

  when(io.write === true.B) {
    registers(io.inSelect) := io.in
  }

  when(!(io.inSelect === pcRegister && io.write === true.B)) {
    registers(pcRegister) := registers(pcRegister) + 1.U
  }

  io.pc := registers(pcRegister)
}
