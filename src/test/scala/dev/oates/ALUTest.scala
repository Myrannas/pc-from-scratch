package dev.oates

import chisel3._
import chiseltest._
import dev.oates.alu.{ALU, AluCode}
import org.scalatest.flatspec.AnyFlatSpec

class ALUTest  extends AnyFlatSpec with ChiselScalatestTester{
 behavior of "ALU"

  it should "Output the sum of A and B when opcode 0 is used" in {
    test(new ALU(16)) {
      c =>
        c.io.inA.poke(1.U)
        c.io.inB.poke(1.U)
        c.io.op.poke(AluCode.add)
        c.io.out.expect(2.U)
    }
  }

  it should "Output the diff of A and B when opcode 1 is used" in {
    test(new ALU(16)) {
      c =>
        c.io.inA.poke(2.U)
        c.io.inB.poke(1.U)
        c.io.op.poke(AluCode.sub)
        c.io.out.expect(1.U)
    }
  }

  it should "Output inB if noop" in {
    test(new ALU(16)) {
      c =>
        c.io.inA.poke(2.U)
        c.io.inB.poke(1.U)
        c.io.inC.poke(3.U)
        c.io.op.poke(AluCode.noop)
        c.io.out.expect(1.U)
    }
  }

  it should "Output inC if noop and constant" in {
    test(new ALU(16)) {
      c =>
        c.io.inA.poke(2.U)
        c.io.inB.poke(1.U)
        c.io.inC.poke(3.U)
        c.io.op.poke(AluCode.noop)
        c.io.constant.poke(true.B)
        c.io.out.expect(3.U)
    }
  }

  it should "Set the zero bit if the result is zero" in {
    test(new ALU(16)) {
      c =>
        c.io.inA.poke(2.U)
        c.io.inB.poke(1.U)
        c.io.op.poke(AluCode.sub)
        c.io.zero.expect(false.B)

        c.io.inB.poke(2.U)
        c.io.zero.expect(true.B)
    }
  }
}
