package dev.oates.control

import Chisel.{Cat, MuxLookup, log2Ceil, switch}
import chisel3._
import chisel3.util.{Fill, is}
import dev.oates.alu.AluCode

/**
 * Instruction formats
 *
 * [regB] [regA] [regW] [op]
 * [constant|constant] [regW] [op]
 *
 * @param registers The register count
 * @param width     The width of data buses
 */
class ControlUnit(registers: Int, width: Int, debug: Boolean = false) extends Module {
  private val registersWidth = log2Ceil(registers)
  private val opsWidth = OpCode.getWidth
  private val pcRegister = (registers - 1).U

  var io = IO(new Bundle {
    val instruction = Input(Bits((OpCode.getWidth + registersWidth * 3).W))
    val zero = Input(Bool())

    val aluOp = Output(AluCode())
    val regReadA = Output(UInt(registersWidth.W))
    val regReadB = Output(UInt(registersWidth.W))
    val regWrite = Output(UInt(registersWidth.W))
    val regWriteE = Output(Bool())
    val portWriteE = Output(Bool())

    val constant = Output(UInt(width.W))
    val regBConstant = Output(Bool())
  })

  private val zeroN = Reg(Bool())
  private val op = io.instruction(opsWidth - 1, 0)
  zeroN := io.zero

  val regC = io.instruction(opsWidth + registersWidth - 1, opsWidth)
  io.regWrite := regC
  io.regReadA := io.instruction(opsWidth + registersWidth * 2 - 1, opsWidth + registersWidth)
  io.regReadB := io.instruction(opsWidth + registersWidth * 3 - 1, opsWidth + registersWidth * 2)

  io.portWriteE := false.B
  io.regWriteE := false.B
  io.constant := Cat(Seq(io.regReadB, io.regReadA))
  io.aluOp := AluCode.noop
  io.regBConstant := false.B

  switch (OpCode(op)) {
    is(OpCode.add) {
      io.regWriteE := true.B
      io.aluOp := AluCode.add
      zeroN := io.zero
    }

    is(OpCode.sub) {
      io.regWriteE := true.B
      io.aluOp := AluCode.sub
      zeroN := io.zero
    }

    is(OpCode.load) {
      io.regWriteE := true.B
      zeroN := io.zero
      io.aluOp := AluCode.noop
      io.regBConstant := true.B
    }

    is(OpCode.xor) {
      io.regWriteE := true.B
      io.aluOp := AluCode.xor
      zeroN := io.zero
    }

    is(OpCode.not) {
      io.regWriteE := true.B
      io.aluOp := AluCode.not
      zeroN := io.zero
    }

    is(OpCode.and) {
      io.regWriteE := true.B
      io.aluOp := AluCode.and
      zeroN := io.zero
    }

    is(OpCode.or) {
      io.regWriteE := true.B
      io.aluOp := AluCode.or
      zeroN := io.zero
    }

    is(OpCode.output) {
      io.portWriteE := true.B
      io.aluOp := AluCode.a
    }

    is(OpCode.jumpC) {
      io.aluOp := AluCode.noop

      io.regWriteE := true.B
      io.regWrite := pcRegister
      io.regBConstant := true.B

      io.constant := Cat(Seq(io.regReadB, io.regReadA, regC))
    }

    is(OpCode.bz) {
      io.aluOp := AluCode.add

      io.regWriteE := zeroN === true.B
      io.regWrite := pcRegister
      io.regReadA := pcRegister
      io.regBConstant := true.B

      io.constant := extendSign(Cat(Seq(io.regReadB, io.regReadA, regC)), width)
    }

    is(OpCode.bnz) {
      io.aluOp := AluCode.add

      io.regWriteE := zeroN === false.B
      io.regWrite := pcRegister
      io.regReadA := pcRegister
      io.regBConstant := true.B

      io.constant := extendSign(Cat(Seq(io.regReadB, io.regReadA, regC)), width)
    }
  }

  if (debug) {
    printf(p"instruction [${Hexadecimal(io.instruction)}]\n" +
      p"op [${op}] constant [${Hexadecimal(io.constant)}] " +
      p"regs [${Hexadecimal(io.regReadB)}, ${Hexadecimal(io.regReadA)}, ${Hexadecimal(io.regWrite)}] \n" +
      p"flags [Z ${Binary(zeroN)}, RWE ${Binary(io.regWriteE)}, PWE ${Binary(io.portWriteE)} RBC ${Binary(io.regBConstant)}]\n")
  }

  def extendSign(input: UInt, toWidth: Int): UInt = {
    require(toWidth > input.getWidth)

    val signBit = input.head(1)

    Cat(Fill(toWidth - input.getWidth, signBit), input)
  }
}
