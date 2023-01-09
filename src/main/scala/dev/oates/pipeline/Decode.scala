package dev.oates.pipeline

import Chisel.log2Ceil
import chisel3._
import chisel3.util.experimental.loadMemoryFromFileInline
import dev.oates.control.{DecodeUnit, OpCode}

class Decode(registerCount: Int,
             memoryFile: String = "",
             width: Int,
             debug: Boolean)
    extends Module {

  private val instructions =
    SyncReadMem(1024, UInt((OpCode.getWidth + log2Ceil(registerCount) * 3).W))
  if (memoryFile.trim().nonEmpty) {
    loadMemoryFromFileInline(instructions, memoryFile)
  }

  val io = IO(new Bundle {
    val pc = Input(UInt(width.W))
    val pcE = Input(Bool())
  })

  private val pc = Input(UInt(width.W))
  private val controlUnit = Module(new DecodeUnit(registerCount, width, debug))

  when(io.pcE === true.B) {
    pc := io.pc
  } .otherwise {
    pc := pc + 1.U
  }

  controlUnit.io.instruction := instructions(io.pc)
}
