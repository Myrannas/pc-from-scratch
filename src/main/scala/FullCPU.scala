import chisel3.stage.ChiselStage
import dev.oates.{CPU, CPUTarget}

object Main extends App {
  println(
    new ChiselStage().emitVerilog(
      CPU
        .builder()
        .withRegisters(16)
        .withWidth(16)
        .withPorts(1)
        .withDataMemory(1024)
        .withInstructionMemory(1024)
        .withTarget(CPUTarget.FPGA)
        .withProgram(
          name = Some("test"),
          program = """
            | r1 = 5
            | r3 = 2
            |
            | r2 = r2 + r1
            | [r3] = r2
            | r0 = [r3]
            | o1 = r0
            |
            |""".stripMargin.trim
        )
        .build(),
      Array(
        "--emission-options=disableMemRandomization,disableRegisterRandomization",
        "--target:fpga",
        "--target-dir=./out",
        "--emit-modules=verilog"
      )
    )
  )
}
