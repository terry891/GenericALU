//sbt "runMain alu.MainGeneric"

package alu

import chisel3._
import chisel3.util._
import chisel3.internal.firrtl.Width

trait GenericNumberType[T] extends Bundle:
  def :=(b: T): Unit
  def +(b: T): T
  def -(b: T): T
  def *(b: T): T
  def /(b: T): T
  def <<(b: T): T
  def >>(b: T): T
  def zero: T


class UInt(val fixed_width: Width) extends GenericNumberType[UInt]:
  val operand_A = chisel3.UInt(fixed_width)
  val output = Wire(new UInt(fixed_width))

  override def :=(a: UInt): UInt = {
    operand_A := a
  }
  override def +(b: UInt): UInt = {
    val operand_B = chisel3.UInt(fixed_width)
    operand_B := b
    output := operand_A + b
    output
  }
  override def -(b: UInt): UInt = {
    val operand_B = chisel3.UInt(fixed_width)
    operand_B := b
    output := operand_A - operand_B
    output
  }
  override def *(b: UInt): UInt = {
    val operand_B = chisel3.UInt(fixed_width)
    operand_B := b
    output := operand_A * operand_B
    output
  }
  override def /(b: UInt): UInt = {
    output := operand_A / operand_B
    output
  }
  override def <<(b: UInt): UInt = {
    output := operand_A << operand_B
    output
  }
  override def >>(b: UInt): UInt = {
    output := operand_A >> operand_B
    output
  }
  override def zero: UInt = 0.U

class SInt(val fixed_width: Width) extends GenericNumberType[SInt]:
  val operand_A = chisel3.SInt(fixed_width)
  val operand_B = chisel3.SInt(fixed_width)
  val output = Wire(new SInt(fixed_width))

  override def store(a: SInt, b: SInt): SInt = {
    operand_A := a
    operand_B := b
  }
  override def add(): SInt = {
    output := operand_A + operand_B
    output
  }
  override def sub(): SInt = {
    output := operand_A - operand_B
    output
  }
  override def mul(): SInt = {
    output := operand_A * operand_B
    output
  }
  override def div(): SInt = {
    output := operand_A / operand_B
    output
  }
  override def shiftLeft(): SInt = {
    output := operand_A << operand_B
    output
  }
  override def shiftRight(): SInt = {
    output := operand_A >> operand_B
    output
  }
  override def zero: SInt = 0.S

class GenericALU[T](val width: Width) extends chisel3.Module {
  val io = IO(new Bundle {
    val opcode = Input(UInt(5.W))
    val op_A = Input(new T(Width(width)))
    val op_B = Input(new T(Width(width)))
    val output = Output(new T(Width(width)))
  })

  val opcode = io.opcode
  val op_A = io.op_A
  val op_B = io.op_B
  val output = Wire(new T(Width(width)))  
  output := op_A.zero
  output := op_A.store(io.op_A, io.op_B)

  switch(opcode) {
    is(0.U) { output := op_A.add(io.op_B) }
    is(1.U) { output := op_A.sub(io.op_B) }
    is(2.U) { output := op_A.mul(io.op_B) }
    is(3.U) { output := op_A.div(io.op_B) }
    is(4.U) { output := op_A.shiftLeft(io.op_B) }
    is(5.U) { output := op_A.shiftRight(io.op_B) }
  }

  io.output := output
}


object MainGeneric extends App {
  println("Tianshu's Generic ALU implementation")
  (new chisel3.stage.ChiselStage).emitVerilog(new GenericALU[UInt](32.W))
  (new chisel3.stage.ChiselStage).emitVerilog(new GenericALU[SInt](32.W))
  (new chisel3.stage.ChiselStage).emitVerilog(new GenericALU[SInt](16.W))
}
