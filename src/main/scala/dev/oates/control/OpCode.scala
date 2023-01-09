package dev.oates.control

import chisel3._
import chisel3.experimental.ChiselEnum

object OpCode extends ChiselEnum {
  val noop: OpCode.Type = Value(0.U)
  val constant: OpCode.Type = Value(1.U)
  val add: OpCode.Type = Value(2.U)
  val sub: OpCode.Type = Value(3.U)
  val output: OpCode.Type = Value(4.U)
  val jumpC: OpCode.Type = Value(5.U)
  val and: OpCode.Type = Value(6.U)
  val or: OpCode.Type = Value(7.U)
  val not: OpCode.Type = Value(8.U)
  val xor: OpCode.Type = Value(9.U)
  val bz: OpCode.Type = Value(10.U)
  val bnz: OpCode.Type = Value(11.U)
  val load: OpCode.Type = Value(12.U)
  val store: OpCode.Type = Value(13.U)

  def encode3(registersWidth: Int,
              instruction: OpCode.Type,
              registerA: Int,
              registerB: Int,
              registerC: Int): UInt = {
    val instructionCode = instruction.litValue.toInt

    val maxRegisterValue = math.pow(2, registersWidth)
    require(registerA < maxRegisterValue, s"registerA > than allowed value of $maxRegisterValue")
    require(registerB < maxRegisterValue, s"registerB > than allowed value of $maxRegisterValue")
    require(registerC < maxRegisterValue, s"registerC > than allowed value of $maxRegisterValue")

    val iValue = (((((registerC << registersWidth) | registerB) << registersWidth) | registerA) << OpCode.getWidth) | instructionCode

    println(s"Encode 3 $iValue")

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
  def encode1C(registersWidth: Int,
               instruction: OpCode.Type,
               registerA: Int,
               value: Int): UInt = {
    val instructionCode = instruction.litValue.toInt

    val maxRegisterValue = math.pow(2, registersWidth)
    require(registerA < maxRegisterValue, s"registerA > than allowed value of $maxRegisterValue")

    val maxConstantValue = math.pow(2, registersWidth * 2)
    require(value < maxConstantValue, s"constant > than allowed value of $maxConstantValue")

    val iValue = (((value << registersWidth) | registerA) << OpCode.getWidth) | instructionCode

    println(s"Encode 1C $iValue")

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
  def encodeC(registersWidth: Int,
              instruction: OpCode.Type,
              value: Int): UInt = {
    val instructionCode = instruction.litValue.toInt

    val maxConstantValue = math.pow(2, registersWidth * 3)
    require(value < maxConstantValue, s"constant >= than allowed value of $maxConstantValue")

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
    * @return
    */
  def encode2(registersWidth: Int,
              instruction: OpCode.Type,
              registerA: Int,
              registerB: Int): UInt = {
    val instructionCode = instruction.litValue.toInt

    val maxRegisterValue = math.pow(2, registersWidth)
    require(registerA < maxRegisterValue, s"registerA > than allowed value of $maxRegisterValue")
    require(registerB < maxRegisterValue, s"registerN > than allowed value of $maxRegisterValue")

    val iValue = (((registerB << registersWidth) | registerA) << OpCode.getWidth) | instructionCode

    iValue.U
  }
}
