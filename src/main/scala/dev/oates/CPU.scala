package dev.oates

import Chisel.log2Ceil
import chisel3._
import chisel3.util.experimental.loadMemoryFromFileInline
import dev.oates.alu.ALU
import dev.oates.control.{DecodeUnit, OpCode, PipelinedControlUnit}
import dev.oates.memory.Memory

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

class CPU(
    registerCount: Int,
    width: Int,
    ports: Int,
    debug: Boolean = false,
    memoryFile: String = ""
) extends Module {
  require(ports <= registerCount,
          "The number of ports must be less than the number of registers")

  if (debug) {
    printf("---------------------------\n");
  }

  val io = IO(new Bundle {
    val outputPorts = Output(Vec(ports, UInt(width.W)))
  })

  private val control = Module(new PipelinedControlUnit(registerCount, width, debug))
  private val registers = Module(new Registers(registerCount, width, true))
  private val alu = Module(new ALU(width, debug))
  private val outputPorts = Module(new Ports(ports, width))
  private val pc = Module(new PC(width, true))
  private val memory = Module(new Memory(width, true))

  private val instructions =
    Mem(1024, UInt((OpCode.getWidth + log2Ceil(registerCount) * 3).W))
  if (memoryFile.trim().nonEmpty) {
    loadMemoryFromFileInline(instructions, memoryFile)
  }

  // Stage 1
  // Decode
  control.io.instruction := instructions(pc.io.out)

  // Stage 2
  // Register Selection
  registers.io.outSelectA := control.io.regReadA
  registers.io.outSelectB := control.io.regReadB
  registers.io.inSelect := control.io.regWrite
  registers.io.loopbackA := control.io.loopBackA0
  registers.io.loopbackB := control.io.loopBackB0

  // Stage 3
  // ALU inputs
  alu.io.inA := registers.io.outA
  alu.io.inB := registers.io.outB
  alu.io.inC := control.io.constant
  alu.io.op := control.io.aluOp
  alu.io.constant := control.io.regBConstant
  control.io.zero := alu.io.zero

  // Memory inputs
  memory.io.readAddress := registers.io.outB

  // Stage 4
  // Write outputs
  outputPorts.io.writeValue := alu.io.out
  outputPorts.io.writeE := control.io.portWriteE
  outputPorts.io.writePortSelect := control.io.regWrite

  // Write registers
  registers.io.in := Mux(control.io.memReadE, memory.io.readData, alu.io.out)
  registers.io.write := control.io.regWriteE

  // Write PC
  pc.io.write := control.io.pcWriteE
  pc.io.in := control.io.constant
  pc.io.writeRelative := control.io.pcWriteRelativeE

  // Write Memory
  memory.io.writeAddress := registers.io.outB
  memory.io.writeData := registers.io.outA
  memory.io.write := control.io.memWriteE

  // Outputs
  io.outputPorts := outputPorts.io.outputPorts
}

object CPU {
  def program(registerCount: Int,
              width: Int,
              ports: Int,
              debug: Boolean = false,
              name: String,
              builder: (ProgramBuilder) => ProgramBuilder): CPU = {
    val registerWidth = log2Ceil(registerCount)
    val program = builder(ProgramBuilder(registerWidth, Array()))
    val linkedProgram = program.link()
    val intructions = linkedProgram.instructions.toSeq
    println(intructions, registerWidth)
    val memoryWidth = ((registerWidth * 3 + OpCode.getWidth) / 4).ceil.toInt

    val allValues = intructions
      .padTo(1024, 0.U)
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
    new CPU(registerCount, width, ports, debug = debug, s"./out/$name.mem")
  }
}
