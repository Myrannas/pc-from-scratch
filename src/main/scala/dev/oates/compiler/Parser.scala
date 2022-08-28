package dev.oates.compiler

import Chisel.log2Ceil
import fastparse._
import ScriptWhitespace._
import chisel3.UInt
import dev.oates.ProgramBuilder

class Parser(programBuilder: ProgramBuilder) {
  type Instruction = ProgramBuilder => ProgramBuilder

  private def parseReg[_: P] =
    P("r" ~ CharsWhileIn("0-9").!.map {
      _.toInt
    })

  private def outputPort[_: P] =
    P("o" ~ CharsWhileIn("0-9").!.map {
      _.toInt
    })

  private def parseNum[_: P] = P(CharIn("0-9").rep(1).!.map(_.toInt))

  private def parseIdentifier[_: P] = P(CharsWhileIn("a-z").!)

  private def parseLabel[_: P] = P(parseIdentifier ~ ":").map {
    label => (programBuilder: ProgramBuilder) =>
      programBuilder.label(label)
  }

  private def parseLoad[_: P] =
    P("load" ~ parseReg ~ parseNum)
      .map({
        case (r, v) =>
          (programBuilder: ProgramBuilder) =>
            programBuilder.load(r, v)
      })

  private def parseAdd[_: P] =
    P("add" ~ parseReg ~ parseReg ~ parseReg)
      .map({
        case (rw, ra, rb) =>
          (programBuilder: ProgramBuilder) =>
            programBuilder.add(rw, ra, rb)
      })

  private def parseOut[_: P] =
    P("out" ~ outputPort ~ parseReg)
      .map({
        case (port, ra) =>
          (programBuilder: ProgramBuilder) =>
            programBuilder.output(port, ra)
      })

  private def parseJump[_: P] =
    P("jump" ~ parseIdentifier)
      .map({ constant => (programBuilder: ProgramBuilder) =>
        programBuilder.jumpLabel(constant)
      })

  private def parseStatement[_: P] =
    P(parseLoad | parseAdd | parseOut | parseJump | parseLabel)

  private def parseProgram[_: P] = P(parseStatement.rep(1) ~ End)

  def compile(input: String): ProgramBuilder = {
    val parseResult = parse(input, parseProgram(_))

    require(parseResult.isSuccess)

    parseResult.get.value.foldLeft(programBuilder) {
      (programBuilder, instruction) =>
        instruction(programBuilder)
    }
  }
}
