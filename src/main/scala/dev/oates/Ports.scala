package dev.oates

import Chisel.isPow2
import chisel3._
import chisel3.util.log2Ceil

class Ports(ports: Int, width: Int) extends Module {
  require(isPow2(ports), "The number of ports must be a power of 2")
  require(isPow2(width), "The data width must be a power of 2")

  val io = IO(new Bundle {
    val writeValue = Input(UInt(width.W))
    val writeE = Input(Bool())
    val writePortSelect = Input(UInt(log2Ceil(ports).W))

    val outputPorts = Output(Vec(ports, UInt(width.W)))
  })

  private val internalRegisters = Reg(Vec(ports, UInt(width.W)))

  io.outputPorts := internalRegisters

  when(io.writeE === true.B) {
    internalRegisters(io.writePortSelect) := io.writeValue
  }
}
