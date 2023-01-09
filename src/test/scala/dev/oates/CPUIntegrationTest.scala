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
        c.clock.step(8)
        c.io.outputPorts(1).expect(10.U)
    }
  }

  it should "Can subtract values" in {
    test(CPU.program(4, 16, 4, true, "sub", {
      cpu =>
        new Parser(cpu).compile(
          """
            | r0 = 5
            | r1 = 3
            |
            | r2 = r0 - r1
            | o1 = r2
            |""".stripMargin.trim)
    })) {
      c =>
        c.clock.step(8)
        c.io.outputPorts(1).expect(2.U)
    }
  }

  it should "adjust the PC when encountering a loop" in {
    test(CPU.program(4, 16, 4, true, "loop", {
      cpu =>
        new Parser(cpu).compile(
          """
            | r1 = 5
            |
            | start:
            | r2 = r2 + r1
            | o1 = r2
            | jump start
            |""".stripMargin.trim)
    })) {
      c =>
        c.clock.step(18)
        c.io.outputPorts(1).expect(15.U)
    }
  }

  it should "Set branch on zero if bz is set" in {
    test(CPU.program(4, 16, 4, true, "branch", {
      cpu =>
        new Parser(cpu).compile(
          """
            | r0 = 1
            | r1 = 0
            |
            | bz end
            | o1 = r0
            |
            | end:
            | o2 = r0
            |""".stripMargin.trim)
    })) {
      c =>
        c.clock.step(10)
        c.io.outputPorts(1).expect(0.U)
        c.io.outputPorts(2).expect(1.U)
    }
  }
}
