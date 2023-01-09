package dev.oates

import chiseltest._
import dev.oates.control.{ControlUnitBundle, OpCode, PipelinedControlUnit}
import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import chisel3.util.log2Ceil
import dev.oates.alu.AluCode

class PipelinedControlUnitTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "PipelinedControlUnit"

  private val width = 16
  private val registers = 4
  private val registerWidth = log2Ceil(registers)
  private val encode3 = OpCode.encode3(registerWidth, _, _, _, _)
  private val encode1C = OpCode.encode1C(registerWidth, _, _, _)

  it should "Emits read registers 1 cycle after input" in {
    test(new PipelinedControlUnit(registers, width, true)) { c =>
      c.io.instruction.poke(encode3(OpCode.add, 1, 2, 3))

      c.io.regReadA.expect(0.U)
      c.io.regReadB.expect(0.U)

      c.clock.step(1)

      c.io.regReadA.expect(2.U)
      c.io.regReadB.expect(3.U)
    }
  }

  it should "Emits ALU op 2 cycle after input" in {
    test(new PipelinedControlUnit(registers, width, true)) { c =>
      c.io.instruction.poke(encode1C(OpCode.add, 1, 3))

      for (_ <- 0 until 2) {
        c.io.aluOp.expect(AluCode.noop)
        c.clock.step()
      }

      c.io.aluOp.expect(AluCode.add)
    }
  }

  it should "Emits register write 3 cycle after input" in {
    test(new PipelinedControlUnit(registers, width, true)) { c =>
      c.io.instruction.poke(encode3(OpCode.add, 1, 2, 3))

      for (_ <- 0 until 3) {
        c.io.regWrite.expect(0.U)
        c.io.regWriteE.expect(false.B)
        c.clock.step()
      }

      c.io.regWrite.expect(1.U)
      c.io.regWriteE.expect(true.B)
    }
  }

  it should "Emits a loopback signal for A if directly using a written register" in {
    test(new PipelinedControlUnit(registers, width, true)) { c =>
      c.io.instruction.poke(encode3(OpCode.add, 1, 1, 1))
      c.io.loopBackA0.expect(false.B)
      c.clock.step(2)
      c.io.loopBackA0.expect(false.B)
      c.io.instruction.poke(encode3(OpCode.add, 2, 2, 2))

      c.clock.step()
      c.io.loopBackA0.expect(true.B)

      c.clock.step()
      c.io.loopBackA0.expect(false.B)
    }
  }
}
