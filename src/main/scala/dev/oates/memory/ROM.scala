package dev.oates.memory

import chisel3._
import chisel3.util.experimental.loadMemoryFromFileInline

class ROM(width: Int, size: Int, memoryFile: String) extends Module {
  val io = IO(new Bundle {
    val address = Input(UInt(10.W))
    val data = Output(UInt(32.W))
  })

  private val instructions = SyncReadMem(size, UInt(width.W))
  if (memoryFile.trim().nonEmpty) {
    loadMemoryFromFileInline(instructions, memoryFile)
  }

  io.data := instructions.read(io.address)
}
