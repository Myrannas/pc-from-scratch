package dev.oates.compiler

import org.scalatest.flatspec.AnyFlatSpec
import chisel3._
import dev.oates.ProgramBuilder

class ParserTest extends AnyFlatSpec {
  "a program" should "parse correctly" in {
    val output = new Parser(ProgramBuilder(2)).compile(
      """
        | load r0 1
        | load r1 1
        |""".stripMargin.trim)

    assert(output.instructions.map(_.litValue) === ProgramBuilder(2, Array())
      .load(0, 1)
      .load(1, 1)
      .instructions.map(_.litValue))
  }

  "programs with loads and adds" should "parse correctly" in {
    val output = new Parser(ProgramBuilder(2)).compile(
      """
        | load r0 1
        | add r1 r0 r0
        |""".stripMargin.trim)

    assert(output.instructions.map(_.litValue) === ProgramBuilder(2, Array())
      .load(0, 1)
      .add(1, 0, 0)
      .instructions.map(_.litValue))
  }

  "program with labels" should "parse correctly" in {
    val output = new Parser(ProgramBuilder(2)).compile(
      """
        | load r0 1
        | start:
        | add r1 r0 r0
        |""".stripMargin.trim)

    assert(output.labels === Map("start" -> 1))
  }
}
