package alu

import chisel3._
import chisel3.util._


class ALU_int[UInt]() extends chisel3.Module {
  val io = IO(new Bundle {
    val opcode = Input(UInt(5.W))
    val op_A = Input(UInt(32.W))
    val op_B = Input(UInt(32.W))
    val output = Output(UInt(32.W))
  })

  val opcode = io.opcode
  val op_A = io.op_A
  val op_B = io.op_B
  val output = Wire(UInt(32.W))
  output := 0.U

  switch(opcode) {
    is(0.U) { output := op_A + op_B }
    is(1.U) { output := op_A - op_B }
    is(2.U) { output := op_A * op_B }
    is(3.U) { output := op_A / op_B }
    is(4.U) { output := op_A % op_B }
    is(5.U) { output := op_A ^ op_B }
    is(6.U) { output := op_A | op_B }
    is(7.U) { output := op_A & op_B }
  }

  io.output := output
}


object Main extends App {
  println("Tianshu's ALU implementation")
  (new chisel3.stage.ChiselStage).emitVerilog(new ALU_int())
}






// package alu

// import chisel3._
// import chisel3.util._


// class ALU_int[UInt](bitWidth: UInt) extends chisel3.Module {
//   val io = IO(new Bundle {
//     val opcode = Input(UInt(5.W))
//     val op_A = Input(UInt(bitWidth.W))
//     val op_B = Input(UInt(bitWidth.W))
//     val output = Output(UInt(bitWidth.W))
//   })

//   val opcode = io.opcode
//   val op_A = io.op_A
//   val op_B = io.op_B
//   val output = Wire(UInt(32.W))
//   output := 0.U

//   switch(opcode) {
//     is(0.U) { output := op_A + op_B }
//     is(1.U) { output := op_A - op_B }
//     is(2.U) { output := op_A * op_B }
//     is(3.U) { output := op_A / op_B }
//     is(4.U) { output := op_A % op_B }
//     is(5.U) { output := op_A ^ op_B }
//     is(6.U) { output := op_A | op_B }
//     is(7.U) { output := op_A & op_B }
//   }

//   io.output := output
// }


// object Main extends App {
//   println("Tianshu's ALU implementation")
//   (new chisel3.stage.ChiselStage).emitVerilog(new ALU_int(bitWidth: UInt))
// }
