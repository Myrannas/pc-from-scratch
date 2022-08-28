package dev.oates

import chisel3.UInt
import dev.oates.control.OpCode

case class ProgramBuilder(registerWidth: Int, instructions: Array[UInt] = Array(), labels: Map[String, Int] = Map(), linkTasks: Array[ProgramBuilder => ProgramBuilder] = Array()) {
  def jump(address: Int): ProgramBuilder = ProgramBuilder(
    registerWidth,
    instructions :+ OpCode.encodeC(registerWidth, OpCode.jumpC, address),
    labels,
    linkTasks
  )

  def jumpLabel(label: String): ProgramBuilder = {
    val offset = instructions.length

    ProgramBuilder(
      registerWidth,
      instructions :+ OpCode.encode3(registerWidth, OpCode.noop, 0, 0, 0),
      labels,
      linkTasks :+ {
        (programBuilder: ProgramBuilder) => programBuilder.replace(offset, OpCode.encodeC(registerWidth, OpCode.jumpC, labels(label)))
      }
    )
  }

  def bz(offset: Int): ProgramBuilder = ProgramBuilder(
    registerWidth,
    instructions :+ OpCode.encodeC(registerWidth, OpCode.bz, offset),
    labels,
    linkTasks
  )

  def bnz(offset: Int): ProgramBuilder = ProgramBuilder(
    registerWidth,
    instructions :+ OpCode.encodeC(registerWidth, OpCode.bnz, offset),
    labels,
    linkTasks
  )

  def noop(): ProgramBuilder = ProgramBuilder(
    registerWidth,
    instructions :+ OpCode.encode3(registerWidth, OpCode.noop, 0, 0, 0),
    labels,
    linkTasks
  )

  def output(destination: Int, regA: Int): ProgramBuilder = ProgramBuilder(
    registerWidth,
    instructions :+ OpCode.encode2(registerWidth, OpCode.output, destination, regA),
    labels,
    linkTasks
  )

  def add(destination: Int, regA: Int, regB: Int): ProgramBuilder = ProgramBuilder(
    registerWidth,
    instructions :+ OpCode.encode3(registerWidth, OpCode.add, destination, regA, regB),
    labels,
    linkTasks
  )

  def sub(destination: Int, regA: Int, regB: Int): ProgramBuilder = ProgramBuilder(
    registerWidth,
    instructions :+ OpCode.encode3(registerWidth, OpCode.sub, destination, regA, regB),
    labels,
    linkTasks
  )

  def load(register: Int, value: Int): ProgramBuilder = {
    ProgramBuilder(registerWidth, instructions :+ OpCode.encode1C(registerWidth, OpCode.load, register, value), labels, linkTasks)
  }

  def label(label: String): ProgramBuilder = {
    ProgramBuilder(registerWidth, instructions, labels + (label -> instructions.length), linkTasks)
  }

  def replace(index: Int, withValue: UInt): ProgramBuilder = {
    ProgramBuilder(registerWidth, instructions.updated(index, withValue), labels, linkTasks)
  }

  def link(): ProgramBuilder = {
    linkTasks.foldLeft(this) {
      (left, right) => right(left)
    }
  }

  //  def jump(register: Int, value: Int): ProgramBuilder = {
  //    ProgramBuilder(registerWidth, instructions :+ OpCode.encode1C(registerWidth, OpCode.load, register, value))
  //  }
}
