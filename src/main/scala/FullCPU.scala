import chisel3._
import chisel3.stage.ChiselStage
import dev.oates.control.{DecodeUnit, OpCode}
import dev.oates.CPU
import dev.oates.compiler.Parser

object Main extends App {
  println(
    new ChiselStage().emitVerilog(
      CPU.program(
        16,
        16,
        4,
        debug = false,
        "program", { cpu =>
          new Parser(cpu).compile("""
            | r1 = 5
            | r3 = 2
            |
            | r2 = r2 + r1
            | [r3] = r2
            | r0 = [r3]
            | o1 = r0
            |
            |""".stripMargin.trim)
        }
      ),
      Array(
        "--emission-options=disableMemRandomization,disableRegisterRandomization",
        "--target-dir=./out"
      )
    )
  )
}
