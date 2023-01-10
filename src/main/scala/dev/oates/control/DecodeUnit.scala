package dev.oates.control

import Chisel.{Cat, log2Ceil, switch}
import chisel3._
import chisel3.util.{Fill, is}
import dev.oates.alu.AluCode
import dev.oates.control.Utils.{BusUtils, UIntUtils}

/**
  * Instruction formats
  *
  * [regB] [regA] [regW] [op]
  * [constant|constant] [regW] [op]
  *
  * @param registers The register count
  * @param width     The width of data buses
  */
class DecodeUnit(registers: Int, width: Int, debug: Boolean = false)
    extends Module {
  private val registersWidth = log2Ceil(registers)
  private val opsWidth = OpCode.getWidth
  private val pcRegister = (registers - 1).U

  var io = IO(new Bundle {
    val instruction: Bits =
      Input(Bits((OpCode.getWidth + registersWidth * 3).W))

    val pc = Input(UInt(width.W))

    val decoded: ControlUnitBundle =
      Output(new ControlUnitBundle(registersWidth, width))
  })

  private val zeroN = Reg(Bool())
  val Seq(
    regB,
    regA,
    regC,
    op
  ) = io.instruction.split(registersWidth,
                           registersWidth,
                           registersWidth,
                           opsWidth)

  io.decoded.regWrite := regC
  io.decoded.regReadA := regA
  io.decoded.regReadB := regB

  io.decoded.regReadAE := false.B
  io.decoded.regReadBE := false.B
  io.decoded.portWriteE := false.B
  io.decoded.regWriteE := false.B
  io.decoded.constant := Cat(Seq(regB, regA))
  io.decoded.aluOp := AluCode.noop
  io.decoded.regBConstant := false.B
  io.decoded.branchNZero := false.B
  io.decoded.branchZero := false.B
  io.decoded.branchRelative := false.B
  io.decoded.memoryWrite := false.B
  io.decoded.memoryRead := false.B
  io.decoded.flushed := false.B
  io.decoded.pc := io.pc

  switch(OpCode(op.asUInt)) {
    is(OpCode.add) {
      io.decoded.regWriteE := true.B
      io.decoded.aluOp := AluCode.add
      io.decoded.regReadAE := true.B
      io.decoded.regReadBE := true.B
    }

    is(OpCode.sub) {
      io.decoded.regWriteE := true.B
      io.decoded.aluOp := AluCode.sub
      io.decoded.regReadAE := true.B
      io.decoded.regReadBE := true.B
    }

    is(OpCode.constant) {
      io.decoded.regWriteE := true.B
      io.decoded.aluOp := AluCode.noop
      io.decoded.regBConstant := true.B
    }

    is(OpCode.xor) {
      io.decoded.regWriteE := true.B
      io.decoded.aluOp := AluCode.xor
      io.decoded.regReadAE := true.B
      io.decoded.regReadBE := true.B
    }

    is(OpCode.not) {
      io.decoded.regWriteE := true.B
      io.decoded.aluOp := AluCode.not
      io.decoded.regReadAE := true.B
    }

    is(OpCode.and) {
      io.decoded.regWriteE := true.B
      io.decoded.aluOp := AluCode.and
      io.decoded.regReadAE := true.B
      io.decoded.regReadBE := true.B
    }

    is(OpCode.or) {
      io.decoded.regWriteE := true.B
      io.decoded.aluOp := AluCode.or
      io.decoded.regReadAE := true.B
      io.decoded.regReadBE := true.B
    }

    is(OpCode.output) {
      io.decoded.portWriteE := true.B
      io.decoded.aluOp := AluCode.a
      io.decoded.regReadAE := true.B
    }

    is(OpCode.jumpC) {
      io.decoded.aluOp := AluCode.noop

      io.decoded.regWrite := pcRegister
      io.decoded.regBConstant := true.B
      io.decoded.branchZero := true.B
      io.decoded.branchNZero := true.B
      io.decoded.branchRelative := false.B

      io.decoded.constant := Cat(
        Seq(io.decoded.regReadB, io.decoded.regReadA, regC))
    }

    is(OpCode.bz) {
      io.decoded.aluOp := AluCode.add

      io.decoded.branchZero := true.B
      io.decoded.regWrite := pcRegister
      io.decoded.regReadA := pcRegister
      io.decoded.regBConstant := true.B
      io.decoded.branchRelative := true.B

      io.decoded.constant := Cat(Seq(regB, regA, regC)).extendSign(width)
    }

    is(OpCode.bnz) {
      io.decoded.aluOp := AluCode.add

      io.decoded.branchNZero := true.B
      io.decoded.regWrite := pcRegister
      io.decoded.regReadA := pcRegister
      io.decoded.regBConstant := true.B
      io.decoded.branchRelative := true.B

      io.decoded.constant := Cat(Seq(regB, regA, regC)).extendSign(width)
    }

    is(OpCode.load) {
      io.decoded.regWriteE := true.B
      io.decoded.memoryRead := true.B
    }

    is(OpCode.store) {
      io.decoded.memoryWrite := true.B
      io.decoded.regReadAE := true.B
      io.decoded.regReadBE := true.B
    }
  }

  if (debug) {
    printf(
      p"instruction [${Hexadecimal(io.instruction)}]\n" +
        p"op [$op] constant [${Hexadecimal(io.decoded.constant)}] " +
        p"regs [${Hexadecimal(regA)}, ${Hexadecimal(regB)}, ${Hexadecimal(regC)}] \n" +
        p"flags [Z ${Binary(zeroN)}, RWE ${Binary(io.decoded.regWriteE)}, PWE ${Binary(
          io.decoded.portWriteE)} RBC ${Binary(io.decoded.regBConstant)} BZ ${Binary(
          io.decoded.branchZero)} BNZ ${Binary(io.decoded.branchNZero)}]\n")
  }
}
