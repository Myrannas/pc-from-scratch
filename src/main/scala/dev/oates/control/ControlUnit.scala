package dev.oates.control

import Chisel.log2Ceil
import chisel3._
import dev.oates.alu.AluCode

class ControlUnitBundle(registersWidth: Int, width: Int) extends Bundle {
  val aluOp: AluCode.Type = AluCode()
  val regReadA: UInt = UInt(registersWidth.W)
  val regReadB: UInt = UInt(registersWidth.W)
  val regWrite: UInt = UInt(registersWidth.W)
  val regWriteE: Bool = Bool()
  val portWriteE: Bool = Bool()
  val branchZero: Bool = Bool()
  val branchNZero: Bool = Bool()
  val branchRelative: Bool = Bool()
  val memoryWrite: Bool = Bool()
  val memoryRead: Bool = Bool()

  val constant: UInt = UInt(width.W)
  val regBConstant: Bool = Bool()
}

object ControlUnitBundle {
  def wire(registers: Int, width: Int) = {
    val w = Wire(new ControlUnitBundle(registers, width))

    w.regReadA := 0.U
    w.regReadB := 0.U
    w.regWrite := 0.U
    w.regWriteE := false.B
    w.portWriteE := false.B
    w.constant := 0.U
    w.regBConstant := false.B
    w.branchZero := false.B
    w.branchNZero := false.B
    w.branchRelative := false.B
    w.aluOp := AluCode.noop
    w.memoryWrite := false.B
    w.memoryRead := false.B

    w
  }
}

class PipelinedControlUnit(registersCount: Int, width: Int, debug: Boolean = false) extends Module {
  private val registersWidth = log2Ceil(registersCount)

  val io = IO(new Bundle {
    val instruction = Input(UInt((OpCode.getWidth + log2Ceil(registersCount) * 3).W))
    val zero = Input(Bool())

    val regReadA = Output(UInt(registersWidth.W))
    val regReadB = Output(UInt(registersWidth.W))

    val loopBackA0 = Output(Bool())
    val loopBackB0 = Output(Bool())

    val aluOp = Output(AluCode())
    val constant = Output(UInt(width.W))
    val regBConstant = Output(Bool())

    val regWrite = Output(UInt(registersWidth.W))
    val regWriteE = Output(Bool())
    val portWriteE = Output(Bool())
    val pcWriteE = Output(Bool())
    val pcWriteRelativeE = Output(Bool())
    val memWriteE = Output(Bool())
    val memReadE = Output(Bool())
  })

  private val controlUnit = Module(new DecodeUnit(registersCount, width, debug))
  private val stages = RegInit(VecInit(Seq.fill(3) { ControlUnitBundle.wire(registersWidth, width) }))
  private val prevZero = io.zero
  private val flush = Wire(Bool())

  for (i <- 0 until stages.length - 1) {
    stages(i + 1) := Mux(flush, ControlUnitBundle.wire(registersWidth, width), stages(i))
  }

  controlUnit.io.instruction := io.instruction
  stages(0) := controlUnit.io.decoded
  io.regReadA := stages(0).regReadA
  io.regReadB := stages(0).regReadB

  io.aluOp := stages(1).aluOp
  io.constant := stages(1).constant
  io.regBConstant := stages(1).regBConstant
  io.memWriteE := stages(1).memoryWrite

  io.regWrite := stages(2).regWrite
  io.regWriteE := stages(2).regWriteE
  io.portWriteE := stages(2).portWriteE
  io.memReadE := stages(2).memoryRead

  io.loopBackA0 := stages(1).regReadA === stages(2).regWrite && stages(2).regWriteE === true.B
  io.loopBackB0 := stages(1).regReadB === stages(2).regWrite && stages(2).regWriteE === true.B

  io.pcWriteE := (stages(1).branchZero && prevZero) || (stages(1).branchNZero && !prevZero)
  io.pcWriteRelativeE := stages(1).branchRelative

  flush := io.pcWriteE

  if (debug) {
    printf(p"Control Unit:\n")

    for (i <- 0 until stages.length) {
      printf(p"\tStage $i: ${stages(i)}\n")
    }

    printf(p"\tZero [${Binary(prevZero)}]\n")

    when(flush) {
      printf(p"\tFlushing control state\n")
    }

  }
}
