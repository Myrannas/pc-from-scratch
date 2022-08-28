import chisel3._
import chisel3.stage.ChiselStage
import dev.oates.control.{ControlUnit, OpCode}
import dev.oates.CPU
import dev.oates.compiler.Parser

object Main extends App {
  println(
    new ChiselStage().emitVerilog(
      CPU.program(16, 16, 4, debug = false, "program", {
        cpu => new Parser(cpu).compile(
          """
            | load r1 5
            |
            | start:
            | add r2 r2 r1
            | out o1 r2
            | jump start
            |""".stripMargin.trim)
      }),
      Array(
        "--emission-options=disableMemRandomization,disableRegisterRandomization",
        "--target-dir=./out"
      )
    )
  )
}