package dev.oates

import chisel3._
import chiseltest._

import org.scalatest.flatspec.AnyFlatSpec;

class RegisterTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Register"

  it should "should return the value of the selected register in either port" in {
    test(new Registers(4, 16)) { c =>
      // Select register 1 and write 5
      c.io.in.poke(5.U)
      c.io.inSelect.poke(1.U)
      c.io.write.poke(true)

      // Commit
      c.clock.step()

      c.io.outA.expect(0.U)
      c.io.outB.expect(0.U)
      // Select register 1 for read and expect 5
      c.io.outSelectA.poke(1.U)
      c.io.outSelectB.poke(1.U)
      c.io.outA.expect(5.U)
      c.io.outB.expect(5.U)
    }
  }

  it should "outputs should follow inputs by one clock cycle" in {
    test(new Registers(4, 16)) { c =>
      c.io.inSelect.poke(1.U)
      c.io.outSelectA.poke(1.U)
      c.io.outSelectB.poke(1.U)
      c.io.write.poke(true)

      for (x <- 1 to 5) {
        c.io.in.poke(x.U)
        c.io.outA.expect((x-1).U)
        c.io.outB.expect((x-1).U)
        c.clock.step()
      }
    }
  }
}
