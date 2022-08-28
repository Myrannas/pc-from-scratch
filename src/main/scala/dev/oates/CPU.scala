package dev.oates

import Chisel.log2Ceil
import chisel3._
import chisel3.util.experimental.loadMemoryFromFileInline
import dev.oates.alu.ALU
import dev.oates.control.{ControlUnit, OpCode}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import javax.print.attribute.standard.Destination

class CPU(
           registerCount: Int,
           width: Int,
           ports: Int,
           debug: Boolean = false,
           memoryFile: String = ""
         ) extends Module {
  require(ports <= registerCount, "The number of ports must be less than the number of registers")

  val io = IO(new Bundle {
    val outputPorts = Output(Vec(ports, UInt(width.W)))
  })

  private val registers = Module(new Registers(registerCount, width))
  private val alu = Module(new ALU(width, debug))
  private val controlUnit = Module(new ControlUnit(registerCount, width, debug))
  private val outputPorts = Module(new Ports(ports, width))

  private val instructions = Mem(1024, UInt((OpCode.getWidth + log2Ceil(registerCount) * 3).W))
  if (memoryFile.trim().nonEmpty) {
    loadMemoryFromFileInline(instructions, memoryFile)
  }

  // ALU Input
  alu.io.inA := registers.io.outA
  alu.io.inB := registers.io.outB
  alu.io.inC := controlUnit.io.constant
  alu.io.op := controlUnit.io.aluOp
  alu.io.constant := controlUnit.io.regBConstant

  // Control Unit Input
  controlUnit.io.instruction := instructions(registers.io.pc)
  controlUnit.io.zero := alu.io.zero


  // Output Ports
  outputPorts.io.writeValue := alu.io.out
  outputPorts.io.writeE := controlUnit.io.portWriteE
  outputPorts.io.writePortSelect := controlUnit.io.regWrite

  //Registers Input
  registers.io.in := alu.io.out
  registers.io.outSelectA := controlUnit.io.regReadA
  registers.io.outSelectB := controlUnit.io.regReadB
  registers.io.inSelect := controlUnit.io.regWrite
  registers.io.write := controlUnit.io.regWriteE

  // Outputs
  io.outputPorts := outputPorts.io.outputPorts

  if (debug) {
    //    printf(p"Control signals: ${controlUnit.io}\n")
    //    printf(p"Registers: ${registers.io}\n")
    printf(p"\nPC: [${Hexadecimal(registers.io.pc)}]\n")
  }
}


object CPU {
  def program(registerCount: Int, width: Int, ports: Int, debug: Boolean = false, name: String, builder: (ProgramBuilder) => ProgramBuilder): CPU = {
    val registerWidth = log2Ceil(registerCount)
    val intructions = builder(ProgramBuilder(registerWidth, Array())).link().instructions.toSeq
    println(intructions, registerWidth)
    val memoryWidth = ((registerWidth * 3 + OpCode.getWidth) / 4).ceil.toInt

    val allValues = intructions.padTo(1024, 0.U).map(i => {
      String.format(s"%0${memoryWidth}X", i.litValue.toInt)
    }).mkString("\n")

    val outDir = Paths.get("./out")
    if (!Files.exists(outDir)) {
      Files.createDirectory(outDir)
    };

    Files.write(Paths.get("out", s"$name.mem"), allValues.getBytes(StandardCharsets.UTF_8))
    new CPU(registerCount, width, ports, debug = debug, s"./out/$name.mem")
  }
}
