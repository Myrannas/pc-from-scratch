package dev.oates.control

import Chisel.log2Ceil
import chisel3._
import dev.oates.alu.AluCode

class ControlUnitBundle(registersWidth: Int, width: Int) extends Bundle {
  val aluOp: AluCode.Type = AluCode()
  val regReadA: UInt = UInt(registersWidth.W)
  val regReadB: UInt = UInt(registersWidth.W)
  val regWrite: UInt = UInt(registersWidth.W)

  val regReadAE: Bool = Bool()
  val regReadBE: Bool = Bool()
  val regWriteE: Bool = Bool()
  val portWriteE: Bool = Bool()
  val branchZero: Bool = Bool()
  val branchNZero: Bool = Bool()
  val branchRelative: Bool = Bool()
  val memoryWrite: Bool = Bool()
  val memoryRead: Bool = Bool()
  val flushed: Bool = Bool()
  val pc: UInt = UInt(width.W)

  val constant: UInt = UInt(width.W)
  val regBConstant: Bool = Bool()

  override def toPrintable: Printable = {
    p"PC [${Hexadecimal(pc)}] Flags [${Binary(flushed)}${Binary(branchZero)}${Binary(
      branchNZero)}${Binary(branchRelative)}${Binary(memoryWrite)}${Binary(
      memoryRead)}${Binary(regWriteE)}${Binary(portWriteE)}${Binary(
      regBConstant)}] Regs [${Hexadecimal(regReadA)}|${Hexadecimal(regReadB)}|${Hexadecimal(
      regWrite)}] Constant [${Hexadecimal(constant)}]"
  }

  def flush(flushed: Bool) = {
    val w = Wire(new ControlUnitBundle(registersWidth, width))

    w.regReadA := this.regReadA
    w.regReadB := this.regReadB
    w.regWrite := this.regWrite
    w.constant := this.constant
    w.regBConstant := this.regBConstant
    w.aluOp := this.aluOp
    w.pc := this.pc

    when(flushed) {
      w.regWriteE := false.B
      w.regReadAE := false.B
      w.regReadBE := false.B
      w.portWriteE := false.B
      w.branchZero := false.B
      w.branchNZero := false.B
      w.branchRelative := false.B
      w.memoryWrite := false.B
      w.memoryRead := false.B
      w.flushed := true.B
    } otherwise {
      w.regWriteE := this.regWriteE
      w.regReadAE := this.regReadAE
      w.regReadBE := this.regReadBE
      w.portWriteE := this.portWriteE
      w.branchZero := this.branchZero
      w.branchNZero := this.branchNZero
      w.branchRelative := this.branchRelative
      w.memoryWrite := this.memoryWrite
      w.memoryRead := this.memoryRead
      w.flushed := this.flushed
    }

    w
  }
}

object ControlUnitBundle {
  def wire(registers: Int, width: Int): ControlUnitBundle = {
    val w = Wire(new ControlUnitBundle(registers, width))

    w.regReadA := 0.U
    w.regReadB := 0.U
    w.regWrite := 0.U
    w.regWriteE := false.B
    w.regReadAE := false.B
    w.regReadBE := false.B
    w.portWriteE := false.B
    w.constant := 0.U
    w.regBConstant := false.B
    w.branchZero := false.B
    w.branchNZero := false.B
    w.branchRelative := false.B
    w.aluOp := AluCode.noop
    w.memoryWrite := false.B
    w.memoryRead := false.B
    w.flushed := false.B
    w.pc := 0.U

    w
  }
}

class PipelinedControlUnit(registersCount: Int,
                           width: Int,
                           debug: Boolean = false)
    extends Module {
  private val registersWidth = log2Ceil(registersCount)

  val io = IO(new Bundle {
    val instruction =
      Input(UInt((OpCode.getWidth + log2Ceil(registersCount) * 3).W))
    val pc = Input(UInt(width.W))
    val zero = Input(Bool())

    val regReadA = Output(UInt(registersWidth.W))
    val regReadB = Output(UInt(registersWidth.W))

    val aluOp = Output(AluCode())
    val constant = Output(UInt(width.W))
    val regBConstant = Output(Bool())

    val regWrite = Output(UInt(registersWidth.W))
    val regWriteE = Output(Bool())
    val portWriteE = Output(Bool())

    val pcWriteE = Output(Bool())
    val pcWriteRelativeE = Output(Bool())
    val pcWriteRelativeAddr = Output(UInt(width.W))

    val memWriteE = Output(Bool())
    val memReadE = Output(Bool())

    val stall = Output(Bool())
  })

  private val decoder = Module(new DecodeUnit(registersCount, width, debug))
  private val stages = RegInit(VecInit(Seq.fill(3) {
    ControlUnitBundle.wire(registersWidth, width)
  }))
  private val prevZero = io.zero
  private val flush = Wire(Bool())

  private val isStalledFromRegisterA = stages(0).regReadAE && stages(1).regWriteE && stages(
    1).regWrite === stages(0).regReadA
  private val isStalledFromRegisterB = stages(0).regReadBE && stages(1).regWriteE && stages(
    1).regWrite === stages(0).regReadB
  private val isStalling = Wire(Bool())
  private val wasStalling = RegNext(isStalling)
  isStalling := (isStalledFromRegisterA | isStalledFromRegisterB) & !wasStalling
  io.stall := isStalling

  // Pipeline stage 2 - decode
  decoder.io.instruction := io.instruction
  private val nextPc = Reg(UInt(width.W))
  when(!isStalling) {
    nextPc := io.pc
  }

  decoder.io.pc := nextPc

  stages(0) := Mux(isStalling, stages(0), decoder.io.decoded)
  stages(1) := stages(0).flush(flush || isStalling)
  stages(2) := stages(1).flush(flush)

  // Pipeline stage 3 - register fetch
  io.regReadA := stages(0).regReadA
  io.regReadB := stages(0).regReadB

  // Pipeline stage 4 - ALU
  io.aluOp := stages(1).aluOp
  io.constant := stages(1).constant
  io.regBConstant := stages(1).regBConstant
  io.memWriteE := stages(1).memoryWrite

  // Pipeline stage 5 - Write Back
  io.regWrite := stages(2).regWrite
  io.regWriteE := stages(2).regWriteE
  io.portWriteE := stages(2).portWriteE
  io.memReadE := stages(2).memoryRead

  io.pcWriteE := (stages(1).branchZero && prevZero) || (stages(1).branchNZero && !prevZero)
  io.pcWriteRelativeE := stages(1).branchRelative
  io.pcWriteRelativeAddr := stages(1).pc

  flush := io.pcWriteE

  if (debug) {
    printf(p"Control Unit:\n")

    printf(p"\tStage 0: ${decoder.io.decoded}\n")

    for (i <- 0 until stages.length) {
      printf(p"\tStage ${i + 1}: ${stages(i)}\n")
    }

    printf(p"\tZero [${Binary(prevZero)}]\n")

    when(flush) {
      printf(p"\tFlushing control state\n")
    }

  }
}
