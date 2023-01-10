package dev.oates

import Chisel.log2Ceil
import chisel3._
import dev.oates.alu.ALU
import dev.oates.control.{DecodeUnit, OpCode, PipelinedControlUnit}
import dev.oates.memory.{Memory, ROM}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

class CPU(
    registerCount: Int,
    width: Int,
    ports: Int,
    dataMemory: Int,
    instructionMemory: Int,
    debug: Boolean = false,
    memoryFile: String = ""
) extends Module {
  require(ports <= registerCount,
          "The number of ports must be less than the number of registers")

  println(s"Synthesising a CPU with $registerCount registers, $ports ports")

  if (debug) {
    printf("---------------------------\n");
  }

  val io = IO(new Bundle {
    val outputPorts: Vec[UInt] = Output(Vec(ports, UInt(width.W)))
  })

  private val control = Module(
    new PipelinedControlUnit(registerCount, width, debug))
  private val registers = Module(new Registers(registerCount, width, debug))
  private val alu = Module(new ALU(width, debug))
  private val outputPorts = Module(new Ports(ports, width))
  private val pc = Module(new PC(width, true))
  private val memory = Module(new Memory(dataMemory, width, debug))
  private val instructions = Module(
    new ROM(OpCode.getWidth + log2Ceil(registerCount) * 3,
            instructionMemory,
            memoryFile))

  // Stage 1
  // Instruction Fetch
  pc.io.stall := control.io.stall
  instructions.io.address := pc.io.out
  instructions.io.readEnabled := !control.io.stall

  // Stage 2 - Decode
  control.io.instruction := instructions.io.data
  control.io.pc := RegNext(pc.io.out)

  // Stage 3
  // Register Selection
  registers.io.outSelectA := control.io.regReadA
  registers.io.outSelectB := control.io.regReadB
  registers.io.inSelect := control.io.regWrite

  // Stage 4
  // ALU inputs
  alu.io.inA := registers.io.outA
  alu.io.inB := registers.io.outB
  alu.io.inC := control.io.constant
  alu.io.op := control.io.aluOp
  alu.io.constant := control.io.regBConstant
  control.io.zero := alu.io.zero

  // Memory inputs
  memory.io.readAddress := registers.io.outB

  // Write PC
  pc.io.write := control.io.pcWriteE
  pc.io.in := control.io.constant
  pc.io.writeRelative := control.io.pcWriteRelativeE
  pc.io.writeRelativeAddr := control.io.pcWriteRelativeAddr

  // Stage 5
  // Write outputs
  outputPorts.io.writeValue := alu.io.out
  outputPorts.io.writeE := control.io.portWriteE
  outputPorts.io.writePortSelect := control.io.regWrite

  // Write registers
  registers.io.in := Mux(control.io.memReadE, memory.io.readData, alu.io.out)
  registers.io.write := control.io.regWriteE

  // Write Memory
  memory.io.writeAddress := registers.io.outB
  memory.io.writeData := registers.io.outA
  memory.io.write := control.io.memWriteE

  // Outputs
  io.outputPorts := outputPorts.io.outputPorts
}

object CPUTarget extends Enumeration {
  type Target = Value
  val FPGA, Simulation = Value
}

object CPU {
  def builder(): CPUBuilder = new CPUBuilder()

  def program(registerCount: Int,
              width: Int,
              ports: Int,
              debug: Boolean = false,
              dataMemory: Int,
              instructionMemory: Int,
              name: String,
              target: CPUTarget.Target,
              builder: (ProgramBuilder) => ProgramBuilder): CPU = {
    val registerWidth = log2Ceil(registerCount)
    val program = builder(ProgramBuilder(registerWidth, Array()))
    val linkedProgram = program.link()
    val instructions = linkedProgram.instructions.toSeq
    println(instructions, registerWidth)
    val memoryWidth = ((registerWidth * 3 + OpCode.getWidth) / 4).ceil.toInt

    val allValues = instructions
      .padTo(instructionMemory, 0.U)
      .map(i => {
        String.format(s"%0${memoryWidth}X", i.litValue.toInt)
      })
      .mkString("\n")

    val outDir = Paths.get("./out")
    if (!Files.exists(outDir)) {
      Files.createDirectory(outDir)
    };

    Files.write(Paths.get("out", s"$name.mem"),
                allValues.getBytes(StandardCharsets.UTF_8))

    val memoryFileName = if (target == CPUTarget.Simulation) {
      s"out/$name.mem"
    } else {
      s"$name.mem"
    }
    new CPU(registerCount,
            width,
            ports,
            instructionMemory,
            dataMemory,
            debug = debug,
            memoryFileName)
  }
}
