package dev.oates

import dev.oates.compiler.Parser

case class CPUBuilder(
    registersCount: Int = 4,
    width: Int = 16,
    ports: Int = 4,
    instructionMemory: Int = 1024,
    dataMemory: Int = 1024,
    name: String = "CPU",
    target: CPUTarget.Target = CPUTarget.Simulation,
    program: String = "",
    debug: Boolean = false
) {
  def withRegisters(count: Int): CPUBuilder = copy(registersCount = count)
  def withWidth(width: Int): CPUBuilder = copy(width = width)
  def withPorts(ports: Int): CPUBuilder = copy(ports = ports)
  def withProgram(program: String, name: Option[String] = None): CPUBuilder =
    copy(program = program, name = name.getOrElse(this.name))
  def withTarget(target: CPUTarget.Target): CPUBuilder = copy(target = target)
  def withInstructionMemory(size: Int): CPUBuilder =
    copy(instructionMemory = size)
  def withDataMemory(size: Int): CPUBuilder = copy(dataMemory = size)
  def withDebugging(): CPUBuilder = copy(debug = true)
  def build(): CPU =
    CPU.program(registersCount,
                width,
                ports,
                debug,
                dataMemory,
                instructionMemory,
                name,
                target,
                new Parser(_).compile(program))
}
