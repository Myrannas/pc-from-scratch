package dev.oates.alu

import chisel3._
import chisel3.util.MuxLookup

class ALU(width: Int, debug: Boolean = false) extends Module {
  var io = IO(new Bundle {
    val inA = Input(UInt(width.W))
    val inB = Input(UInt(width.W))
    val inC = Input(UInt(width.W))
    val op = Input(AluCode())

    val constant = Input(Bool())

    val out = Output(UInt(width.W))
    val zero = Output(Bool())
  })

  val result = Wire(UInt(width.W))

  var inB = Mux(
    io.constant === true.B,
    io.inC,
    io.inB
  )

  result := MuxLookup(
    io.op.asUInt,
    inB,
    Seq(
      AluCode.add.asUInt -> (io.inA + inB),
      AluCode.sub.asUInt -> (io.inA - inB),
      AluCode.a.asUInt -> io.inA,
      AluCode.not.asUInt -> ~io.inA,
      AluCode.or.asUInt -> (io.inA | inB),
      AluCode.and.asUInt -> (io.inA & inB),
      AluCode.xor.asUInt -> (io.inA ^ inB),
    )
  )

  io.out := result
  io.zero := result === 0.U

  if (debug) {
    printf(
      p"ALU op [${Hexadecimal(io.op.asUInt)}] inA [${Hexadecimal(io.inA)}] inB [${Hexadecimal(
        io.inB)}] out [${Hexadecimal(io.out)}]\n")
  }
}
