package dev.oates.control

import chisel3._
import chisel3.experimental.ChiselEnum

object OpCode extends ChiselEnum {
  val noop = Value(0.U)
  val load = Value(1.U)
  val add = Value(2.U)
  val sub = Value(3.U)
  val output = Value(4.U)
  val jumpC = Value(5.U)
  val and = Value(6.U)
  val or = Value(7.U)
  val not = Value(8.U)
  val xor = Value(9.U)
  val bz = Value(10.U)
  val bnz = Value(11.U)

  def encode3(registersWidth: Int, instruction: OpCode.Type, registerA: Int, registerB: Int, registerC: Int): UInt = {
    val instructionCode = instruction.litValue.toInt

    val maxRegisterValue = math.pow(2, registersWidth)
    if (registerA >= maxRegisterValue) {
      throw new Error(s"registerA > than allowed value of ${maxRegisterValue}")
    }

    if (registerB >= maxRegisterValue) {
      throw new Error(s"registerB > than allowed value ${maxRegisterValue}")
    }

    if (registerC >= maxRegisterValue) {
      throw new Error(s"registerC > than allowed value ${maxRegisterValue}")
    }

    val iValue = (((((registerC << registersWidth) | registerB) << registersWidth) | registerA) << OpCode.getWidth) | instructionCode

    iValue.U
  }

  /**
   * Encode a 1 register + constant instruction
   *
   * Format:
   *
   * OpCode | Reg | Constant Double
   *
   * @param registersWidth
   * @param instruction
   * @param registerA
   * @param value
   * @return
   */
  def encode1C(registersWidth: Int, instruction: OpCode.Type, registerA: Int, value: Int): UInt = {
    val instructionCode = instruction.litValue.toInt

    val maxRegisterValue = math.pow(2, registersWidth)
    if (registerA >= maxRegisterValue) {
      throw new Error(s"registerA > than allowed value of ${maxRegisterValue}")
    }

    val maxConstantValue = math.pow(2, registersWidth * 2)
    if (value >= maxConstantValue) {
      throw new Error(s"constant > than allowed value of $maxConstantValue")
    }

    val iValue = (((value << registersWidth) | registerA) << OpCode.getWidth) | instructionCode

    iValue.U
  }

  /**
   * Encode a 1 register + constant instruction
   *
   * Format:
   *
   * OpCode | Reg | Constant Double
   *
   * @param registersWidth
   * @param instruction
   * @param registerA
   * @param value
   * @return
   */
  def encodeC(registersWidth: Int, instruction: OpCode.Type, value: Int): UInt = {
    val instructionCode = instruction.litValue.toInt

    val maxConstantValue = math.pow(2, registersWidth * 3)
    if (value >= maxConstantValue) {
      throw new Error(s"constant > than allowed value of $maxConstantValue")
    }

    val mask = (1 << ((registersWidth * 3) + OpCode.getWidth)) - 1
    val iValue = (value << OpCode.getWidth) & mask | instructionCode

    print(s"Encode C $iValue")

    iValue.asUInt
  }

  /**
   * Encode a 1 register + constant instruction
   *
   * Format:
   *
   * OpCode | Reg | Constant Double
   *
   * @param registersWidth
   * @param instruction
   * @param registerA
   * @param value
   * @return
   */
  def encode2(registersWidth: Int, instruction: OpCode.Type, registerA: Int, registerB: Int): UInt = {
    val instructionCode = instruction.litValue.toInt

    val maxRegisterValue = math.pow(2, registersWidth)
    if (registerA >= maxRegisterValue) {
      throw new Error(s"registerA > than allowed value of ${maxRegisterValue}")
    }

    if (registerB >= maxRegisterValue) {
      throw new Error(s"registerB > than allowed value ${maxRegisterValue}")
    }

    val iValue = (((registerB << registersWidth) | registerA) << OpCode.getWidth) | instructionCode

    iValue.U
  }
}
