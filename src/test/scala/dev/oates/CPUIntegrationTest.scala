package dev.oates

import chiseltest._
import chisel3._
import dev.oates.compiler.Parser
import org.scalatest.flatspec.AnyFlatSpec

class CPUIntegrationTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "CPU"

  it should "Can add values" in {
    test(
      CPU
        .builder()
        .withProgram(
          """
            | r0 = 5
            | r1 = r0 + r0
            | o1 = r1
            |""".stripMargin.trim
        )
        .build()) { c =>
      c.clock.step(8)
      c.io.outputPorts(1).expect(10.U)
    }
  }

  it should "Can subtract values" in {
    test(
      CPU
        .builder()
        .withProgram("""
            | r0 = 5
            | r1 = 3
            |
            | r2 = r0 - r1
            | o1 = r2
            |""".stripMargin.trim)
        .build()) { c =>
      c.clock.step(8)
      c.io.outputPorts(1).expect(2.U)
    }
  }

  it should "Can use the result of a previous computation immediately" in {
    test(
      CPU
        .builder()
        .withProgram("""
            | r0 = 5
            | r1 = r0 + r0
            | r1 = r1 + r0
            | o1 = r1
            |""".stripMargin.trim)
        .build()) { c =>
      c.clock.step(8)
      c.io.outputPorts(1).expect(15.U)
    }
  }

  it should "adjust the PC when encountering a loop" in {
    test(
      CPU
        .builder()
        .withProgram("""
            | r1 = 5
            |
            | start:
            | r2 = r2 + r1
            | o1 = r2
            | jump start
            |""".stripMargin.trim)
        .build()) { c =>
      c.clock.step(19)
      c.io.outputPorts(1).expect(15.U)
    }
  }

  it should "Set branch on zero if bz is set" in {
    test(
      CPU
        .builder()
        .withProgram("""
            | r0 = 1
            | r1 = 0
            |
            | bz end
            | o1 = r0
            |
            | end:
            | o2 = r0
            |""".stripMargin.trim)
        .build()) { c =>
      c.clock.step(10)
      c.io.outputPorts(1).expect(0.U)
      c.io.outputPorts(2).expect(1.U)
    }
  }

  it should "allow reading and writing memory" in {
    test(
      CPU
        .builder()
        .withProgram("""
            | r0 = 5
            | r1 = 13
            |
            | [ r0 ] = r1
            | r2 = [ r0 ]
            |
            | o1 = r2
            |""".stripMargin.trim)
        .build()) { c =>
      c.clock.step(10)
      c.io.outputPorts(1).expect(13.U)
    }
  }

  it should "can write and read at the same time" in {
    test(
      CPU
        .builder()
        .withDebugging()
        .withProgram(
          """
        | r0 = 1
        | r1 = 2
        | r2 = 3
        |
        | [r0] = r1
        | r3 = [r0]
        | [r0] = r2
        |
        | o1 = r3
        |""".stripMargin.trim
        )
        .build()) { c =>
      c.clock.step(11)
      c.io.outputPorts(1).expect(2.U)
    }
  }
}
