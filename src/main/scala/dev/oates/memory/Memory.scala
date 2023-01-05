package dev.oates.memory

import chisel3._

class Memory(width: Int) extends Module {
  val memory: SyncReadMem[UInt] = SyncReadMem(
    1000, UInt(width.W)
  )
}
