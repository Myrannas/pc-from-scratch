package dev.oates

import chiseltest._
import chisel3._
import dev.oates.compiler.Parser
import org.scalatest.flatspec.AnyFlatSpec

class CPUIntegrationTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "CPU"

  it should "Can add values" in {
    test(CPU.program(4, 16, 4, true, "add", {
      cpu =>
        cpu
          .load(1, 5)
          .add(2, 1, 1)
          .output(1, 2)
    })) {
      c =>
        c.clock.step(3)
        c.io.outputPorts(1).expect(10.U)
    }
  }

  it should "Can subtract values" in {
    test(CPU.program(4, 16, 4, true, "sub", {
      cpu =>
        cpu
          .load(0, 5)
          .load(1, 3)
          .sub(2, 0, 1)
          .output(1, 2)
    })) {
      c =>
        c.clock.step(4)
        c.io.outputPorts(1).expect(2.U)
    }
  }

  it should "adjust the PC when encountering a loop" in {
    test(CPU.program(4, 16, 4, true, "loop", {
      cpu =>
        new Parser(cpu).compile(
          """
            | load r1 5
            |
            | start:
            | add r2 r2 r1
            | out o1 r2
            | jump start
            |""".stripMargin.trim)
    })) {
      c =>
        c.clock.step(10)
        c.io.outputPorts(1).expect(15.U)
    }
  }

  it should "Set branch on zero if bz is set" in {
    test(CPU.program(4, 16, 4, true, "branch", {
      cpu =>
        cpu
          .load(0, 5)
          .load(1, 1)
          .load(2, 15)
          .sub(0, 0, 1)
          .output(1, 0)
          .bnz(-2)
          .output(1, 2)
    })) {
      c =>
        c.clock.step(19)
        c.io.outputPorts(1).expect(15.U)
    }
  }
}
