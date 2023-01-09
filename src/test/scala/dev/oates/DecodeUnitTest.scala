package dev.oates

import chiseltest._
import chisel3._
import chisel3.util.log2Ceil
import dev.oates.alu.AluCode
import dev.oates.control.{DecodeUnit, OpCode}
import org.scalatest.flatspec.AnyFlatSpec

class DecodeUnitTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "ControlUnit"

  private val width = 16
  private val registers = 4
  private val registerWidth = log2Ceil(registers)
  private val encode3 = OpCode.encode3(registerWidth, _, _, _, _)
  private val encode1C = OpCode.encode1C(registerWidth, _, _, _)
  private val encodeC = OpCode.encodeC(registerWidth, _, _)
  private val encode2 = OpCode.encode2(registerWidth, _, _, _)

  it should "Set the ALU op if if input is an ALU operation" in {
    test(new DecodeUnit(registers, width)) {
      c =>
        c.io.instruction.poke(encode3(OpCode.noop, 1, 1, 1))
        c.io.decoded.aluOp.expect(AluCode.noop)

        c.io.instruction.poke(encode3(OpCode.add, 1, 1, 1))
        c.io.decoded.aluOp.expect(AluCode.add)

        c.io.instruction.poke(encode3(OpCode.sub, 1, 1, 1))
        c.io.decoded.aluOp.expect(AluCode.sub)
    }
  }

  it should "Set the write enable flag for alu ops" in {
    test(new DecodeUnit(registers, width)) {
      c =>
        c.io.instruction.poke(encode3(OpCode.noop, 1, 1, 1))
        c.io.decoded.regWriteE.expect(false.B)

        c.io.instruction.poke(encode3(OpCode.add, 1, 1, 1))
        c.io.decoded.regWriteE.expect(true.B)

        c.io.instruction.poke(encode3(OpCode.sub, 1, 1, 1))
        c.io.decoded.regWriteE.expect(true.B)

        c.io.instruction.poke(encode3(OpCode.bz, 1, 1, 1))
        c.io.decoded.regWriteE.expect(false.B)
    }
  }

  it should "Set the correct registers for opcodes" in {
    test(new DecodeUnit(registers, width)) {
      c =>
        c.io.instruction.poke(encode3(OpCode.add, 1, 2, 3))
        c.io.decoded.aluOp.expect(AluCode.add)
        c.io.decoded.regWrite.expect(1.U)
        c.io.decoded.regReadA.expect(2.U)
        c.io.decoded.regReadB.expect(3.U)
    }
  }

  it should "OpCode.load: Set the noop op for the ALU and write to correct register" in {
    test(new DecodeUnit(registers, width)) {
      c =>
        c.io.instruction.poke(encode1C(OpCode.constant, 1, 2))
        c.io.decoded.aluOp.expect(AluCode.noop)
        c.io.decoded.regWriteE.expect(true.B)
        c.io.decoded.portWriteE.expect(false.B)
        c.io.decoded.portWriteE.expect(false.B)
        c.io.decoded.regWrite.expect(1.U)
        c.io.decoded.constant.expect(2.U)
    }
  }

  it should "Opcode.output: Sets the noop op for the ALB and writes to a port" in {
    test(new DecodeUnit(registers, width)) {
      c =>
        c.io.instruction.poke(encode2(OpCode.output, 1, 2))
        c.io.decoded.aluOp.expect(AluCode.a)
        c.io.decoded.regWriteE.expect(false.B)
        c.io.decoded.portWriteE.expect(true.B)
        c.io.decoded.regWrite.expect(1.U)
        c.io.decoded.regReadA.expect(2.U)
    }
  }

  it should "Opcode.jump: Sets the noop op for the ALB and writes to the PC" in {
    test(new DecodeUnit(registers, width, true)) {
      c =>
        c.io.instruction.poke(encodeC(OpCode.jumpC, 2))
        c.io.decoded.aluOp.expect(AluCode.noop)
        c.io.decoded.portWriteE.expect(false.B)
        c.io.decoded.regWriteE.expect(true.B)
        c.io.decoded.constant.expect(2.U)

    }
  }

  it should "Opcode.bz: Sets the addition opcode and the constant flag. Sets the pcWriteE if the previous zero was 1" in {
    test(new DecodeUnit(registers, width, true)) {
      c =>
        c.io.instruction.poke(encodeC(OpCode.bz, -1))
        c.io.decoded.constant.expect(((1 << width) - 1).asUInt)
        c.io.decoded.branchZero.expect(true.B)
        c.io.decoded.aluOp.expect(AluCode.add)
    }
  }


  it should "Opcode.bz: Branch to a positive location" in {
    test(new DecodeUnit(registers, width, true)) {
      c =>
        c.io.instruction.poke(encodeC(OpCode.bz, 2))
        c.clock.step();

        c.io.decoded.constant.expect(2)
        c.io.decoded.branchZero.expect(true.B)
        c.io.decoded.aluOp.expect(AluCode.add)
    }
  }

  it should "Opcode.bnz: Sets the addition opcode and the constant flag. Sets the pcWriteE if the previous zero was 0" in {
    test(new DecodeUnit(registers, width)) {
      c =>
        c.io.instruction.poke(encodeC(OpCode.bnz, -1))
        c.io.decoded.aluOp.expect(AluCode.add)
        c.io.decoded.constant.expect(((1 << width) - 1).asUInt)
        c.io.decoded.branchNZero.expect(true.B)
    }
  }

  it should "Opcode.store: Correctly decodes a load instruction" in {
    test(new DecodeUnit(registers, width, true)) {
      c =>
        c.io.instruction.poke(encode3(OpCode.store, 0, 1, 0))
        c.io.decoded.aluOp.expect(AluCode.noop)
        c.io.decoded.memoryWrite.expect(true.B)
        c.io.decoded.regWrite.expect(0)
        c.io.decoded.regReadB.expect(0)
        c.io.decoded.regReadA.expect(1)
    }
  }
}
