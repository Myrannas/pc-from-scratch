package dev.oates.compiler

import Chisel.{log2Ceil, switch}
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
    P(parseReg ~ "=" ~ parseNum)
      .map({
        case (r, v) =>
          (programBuilder: ProgramBuilder) =>
            programBuilder.load(r, v)
      })

  private def parseStoreAddr[_: P] =
    P("[" ~ parseReg ~ "]" ~ "=" ~ parseReg)
      .map({
        case (r, v) =>
          (programBuilder: ProgramBuilder) =>
            programBuilder.storeAddr(r, v)
      })

  private def parseLoadAddr[_: P] =
    P(parseReg ~ "=" ~ "[" ~ parseReg ~ "]")
      .map({
        case (r, v) =>
          (programBuilder: ProgramBuilder) =>
            programBuilder.loadAddr(r, v)
      })


  private def parseOperator[_: P] = P(("+" | "-" | "&" | "|").!).map({
    operator => (programBuilder: ProgramBuilder) => {
          operator match {
            case "+" => programBuilder.add _
            case "-" => programBuilder.sub _
            case "&" => programBuilder.and _
            case "|" => programBuilder.or _
      }
    }
  })

  private def parseOperation[_: P] = P(parseReg ~ "=" ~ parseReg ~ parseOperator ~ parseReg).map({
    case (r1, r2, op, r3) => {
      (programBuilder: ProgramBuilder) => op(programBuilder)(r1, r2, r3)
    }
  })

  private def parseOut[_: P] =
    P(outputPort ~ "=" ~ parseReg)
      .map({
        case (port, ra) =>
          (programBuilder: ProgramBuilder) =>
            programBuilder.output(port, ra)
      })

  private def parseJump[_: P] =
    P(("jump" | "bnz" | "bz").! ~ parseIdentifier)
      .map({
        case ("jump", constant) =>
          (programBuilder: ProgramBuilder) =>
            println(constant)
            programBuilder.jumpLabel(constant)
        case ("bnz", constant) =>
          (programBuilder: ProgramBuilder) =>
            println(constant)
            programBuilder.bnz(constant)
        case ("bz", constant) =>
          (programBuilder: ProgramBuilder) =>
            println(constant)
            programBuilder.bz(constant)
      })

  private def parseStatement[_: P] =
    P(parseLoadAddr | parseLoad | parseStoreAddr | parseOperation | parseOut | parseJump | parseLabel)

  private def parseProgram[_: P] = P(parseStatement.rep(1) ~ End)

  def compile(input: String): ProgramBuilder = {
    val parseResult = parse(input, parseProgram(_))

    require(parseResult.isSuccess, s"Program failed to parse")

    parseResult.get.value.foldLeft(programBuilder) {
      (programBuilder, instruction) =>
        instruction(programBuilder)
    }
  }
}
