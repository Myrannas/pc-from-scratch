package dev.oates

import chisel3.UInt
import dev.oates.control.OpCode

case class ProgramBuilder(registerWidth: Int,
                          instructions: Array[UInt] = Array(),
                          labels: Map[String, Int] = Map(),
                          linkTasks: Array[ProgramBuilder => ProgramBuilder] =
                            Array()) {
  def jumpLabel(label: String): ProgramBuilder = {
    val offset = instructions.length

    ProgramBuilder(
      registerWidth,
      instructions :+ OpCode.encode3(registerWidth, OpCode.noop, 0, 0, 0),
      labels,
      linkTasks :+ { (programBuilder: ProgramBuilder) =>
        programBuilder.replace(
          offset,
          OpCode.encodeC(registerWidth, OpCode.jumpC, programBuilder.labels(label)))
      }
    )
  }

  def bz(label: String): ProgramBuilder = {
    val instr = instructions.length

    ProgramBuilder(
    registerWidth,
    instructions :+ OpCode.encodeC(registerWidth, OpCode.bz, 0),
    labels,
    linkTasks :+ { (programBuilder: ProgramBuilder) =>
      programBuilder.replace(
        instr,
        OpCode.encodeC(registerWidth, OpCode.bz, programBuilder.labels(label) - instr))
    }
  )}

  def bnz(label: String): ProgramBuilder = {
    val instr = instructions.length

    ProgramBuilder(
      registerWidth,
      instructions :+ OpCode.encodeC(registerWidth, OpCode.bz, 0),
      labels,
      linkTasks :+ { (programBuilder: ProgramBuilder) =>
        programBuilder.replace(
          instr,
          OpCode.encodeC(registerWidth, OpCode.bz, programBuilder.labels(label) - instr))
      }
    )
  }

  def noop(): ProgramBuilder = ProgramBuilder(
    registerWidth,
    instructions :+ OpCode.encode3(registerWidth, OpCode.noop, 0, 0, 0),
    labels,
    linkTasks
  )

  def output(destination: Int, regA: Int): ProgramBuilder = ProgramBuilder(
    registerWidth,
    instructions :+ OpCode.encode2(registerWidth,
                                   OpCode.output,
                                   destination,
                                   regA),
    labels,
    linkTasks
  )

  def add(destination: Int, regA: Int, regB: Int): ProgramBuilder =
    ProgramBuilder(
      registerWidth,
      instructions :+ OpCode.encode3(registerWidth,
                                     OpCode.add,
                                     destination,
                                     regA,
                                     regB),
      labels,
      linkTasks
    )

  def sub(destination: Int, regA: Int, regB: Int): ProgramBuilder =
    ProgramBuilder(
      registerWidth,
      instructions :+ OpCode.encode3(registerWidth,
                                     OpCode.sub,
                                     destination,
                                     regA,
                                     regB),
      labels,
      linkTasks
    )

  def and(destination: Int, regA: Int, regB: Int): ProgramBuilder =
    ProgramBuilder(
      registerWidth,
      instructions :+ OpCode.encode3(registerWidth,
        OpCode.and,
        destination,
        regA,
        regB),
      labels,
      linkTasks
    )

  def or(destination: Int, regA: Int, regB: Int): ProgramBuilder =
    ProgramBuilder(
      registerWidth,
      instructions :+ OpCode.encode3(registerWidth,
        OpCode.or,
        destination,
        regA,
        regB),
      labels,
      linkTasks
    )

  def load(register: Int, value: Int): ProgramBuilder = {
    ProgramBuilder(registerWidth,
                   instructions :+ OpCode.encode1C(registerWidth,
                                                   OpCode.constant,
                                                   register,
                                                   value),
                   labels,
                   linkTasks)
  }

  def loadAddr(destination: Int, addressRegister: Int): ProgramBuilder = {
    ProgramBuilder(registerWidth,
      instructions :+ OpCode.encode2(registerWidth,
        OpCode.load,
        destination,
        addressRegister),
      labels,
      linkTasks)
  }

  def storeAddr(destination: Int, addressRegister: Int): ProgramBuilder = {
    ProgramBuilder(registerWidth,
      instructions :+ OpCode.encode3(registerWidth,
        OpCode.store,
        0,
        addressRegister,
        destination),
      labels,
      linkTasks)
  }

  def label(label: String): ProgramBuilder = {
    ProgramBuilder(registerWidth,
                   instructions,
                   labels + (label -> instructions.length),
                   linkTasks)
  }

  def replace(index: Int, withValue: UInt): ProgramBuilder = {
    ProgramBuilder(registerWidth,
                   instructions.updated(index, withValue),
                   labels,
                   linkTasks)
  }

  def link(): ProgramBuilder = {
    linkTasks.foldLeft(this) { (left, right) =>
      right(left)
    }
  }

  //  def jump(register: Int, value: Int): ProgramBuilder = {
  //    ProgramBuilder(registerWidth, instructions :+ OpCode.encode1C(registerWidth, OpCode.load, register, value))
  //  }
}
