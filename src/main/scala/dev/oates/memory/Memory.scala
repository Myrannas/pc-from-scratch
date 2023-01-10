package dev.oates.memory

import chisel3._

class Memory(dataMemory: Int, width: Int, debug: Boolean = false)
    extends Module {
  private val memory: SyncReadMem[UInt] = SyncReadMem(
    dataMemory,
    UInt(width.W)
  )

  val io = IO(new Bundle() {
    val writeAddress: UInt = Input(UInt(10.W))
    val write: Bool = Input(Bool())
    val writeData: UInt = Input(UInt(width.W))

    val readData: UInt = Output(UInt(width.W))
    val readAddress: UInt = Input(UInt(10.W))
  })

  when(io.write) {
    memory.write(io.writeAddress, io.writeData)

    if (debug) {
      printf(
        p"Memory write [${Hexadecimal(io.writeData)}] to [${Hexadecimal(io.writeAddress)}]\n")
    }
  }

  io.readData := memory.read(io.readAddress)

  if (debug) {
    printf(
      p"Memory read [${Hexadecimal(io.readData)}] from [${Hexadecimal(io.readAddress)}]\n")
  }
}
