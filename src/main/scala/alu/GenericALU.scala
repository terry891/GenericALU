//sbt "runMain alu.MainGeneric"

package alu

import chisel3._
import chisel3.util._
import chisel3.internal.firrtl.Width
import hardfloat._

trait GenericNumberType[T] extends Bundle:
  def :=(that: T): Unit
  def +(that: T): T
  def -(that: T): T
  def *(that: T): T
  def /(that: T): T
  def <<(that: T): T
  def >>(that: T): T
  def zero: T

class UInt(val fixed_width: Width) extends GenericNumberType[UInt]:
  val operand_A = chisel3.UInt(fixed_width)
  val output = Wire(new UInt(fixed_width))

  override def :=(that: UInt): UInt = {
    operand_A := that.data
  }
  override def +(that: UInt): UInt = {
    val operand_B = chisel3.UInt(fixed_width)
    operand_B := that.data
    output := operand_A + operand_B
    output
  }
  override def -(that: UInt): UInt = {
    val operand_B = chisel3.UInt(fixed_width)
    operand_B := that.data
    output := operand_A - operand_B
    output
  }
  override def *(that: UInt): UInt = {
    val operand_B = chisel3.UInt(fixed_width)
    operand_B := that.data
    output := operand_A * operand_B
    output
  }
  override def /(that: UInt): UInt = {
    val operand_B = chisel3.UInt(fixed_width)
    operand_B := that.data
    output := operand_A / operand_B
    output
  }
  override def <<(b: UInt): UInt = {
    output := operand_A << that.data
    output
  }
  override def >>(that: UInt): UInt = {
    output := operand_A >> that.data
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

class Float(val expWidth: Int, val sigWidth: Int) extends GenericNumberType[Float] {
  val sign = Bool()
  val exp = UInt(expWidth.W - 1)
  val sig = UInt(sigWidth.W)

  def :=(that: Float): Unit = {
    sign := that.sign
    exp := that.exp
    sig := that.sig
    val this_raw = rawFloatFromFN(expWidth, sigWidth, this.inBits)
  }

  def inBits(): Bits = {
    val bits = Wire(Bits((expWidth + sigWidth).W))
    bits := Cat(sign, exp, sig)
    bits
  }

  def +(that: Float): Float = {
    val that_raw = rawFloatFromFN(expWidth, sigWidth, that.inBits)
    val output = Wire(new Float(exp_width, sig_width))
    val adder = chisel3.Module(new AddRawFN(exp_width, sig_width))
    adder.io.a := this_raw
    adder.io.b := that_raw
    val out_raw = adder.io.rawOut
    output.sign := out_raw.sign
    output.exp := out_raw.sExp.asUInt
    output.sig := out_raw.sig(out_raw.expWidth + 1, out_raw.expWidth + 2 - sig_width + 1)
    output
  }

  def -(that: Float): Float = {
    val that_raw = rawFloatFromFN(expWidth, sigWidth, that.inBits)
    val output = Wire(new Float(exp_width, sig_width))
    val adder = chisel3.Module(new AddRawFN(exp_width, sig_width))
    adder.io.subOp := 1.U
    adder.io.a := this.this_raw
    adder.io.b := that_raw
    val out_raw = adder.io.rawOut
    output.sign := out_raw.sign
    output.exp := out_raw.sExp.asUInt
    output.sig := out_raw.sig(out_raw.expWidth + 1, out_raw.expWidth + 2 - sig_width + 1)
    output
  }

  def *(that: Float): Float = {
    val that_raw = rawFloatFromFN(expWidth, sigWidth, that.inBits)
    val output = Wire(new Float(exp_width, sig_width))
    val multiplier = chisel3.Module(new MulRawFN(exp_width, sig_width, 0))
    multiplier.io.a := this.this_raw
    multiplier.io.b := that_raw
    val out_raw = multiplier.io.rawOut
    output.sign := out_raw.sign
    output.exp := out_raw.sExp.asUInt
    output.sig := out_raw.sig(out_raw.expWidth + 1, out_raw.expWidth + 2 - sig_width + 1)
    output
  }

  def /(that: Float): Float = {
    val that_raw = rawFloatFromFN(expWidth, sigWidth, that.inBits)
    val output = Wire(new Float(exp_width, sig_width))
    val divider = chisel3.Module(new DivRawFN(exp_width, sig_width, 0))
    divider.io.a := this.this_raw
    divider.io.b := that_raw
    val out_raw = divider.io.rawOut
    output.sign := out_raw.sign
    output.exp := out_raw.sExp.asUInt
    output.sig := out_raw.sig(out_raw.expWidth + 1, out_raw.expWidth + 2 - sig_width + 1)
    output
  }

  def <<(that: UInt): Float = {
    val output = Wire(new Float(exp_width, sig_width))
    output := this
    output.sig := output.sig << that
    output
  }

  def >>(that: UInt): Float = {
    val output = Wire(new Float(exp_width, sig_width))
    output := this
    output.sig := output.sig >> that
    output
  }

  def zero: Float = {
    val output = Wire(new Float(exp_width, sig_width))
    output.sign := 0.U
    output.exp := 0.U
    output.sig := 0.U
    output
  }

}

class FixedWidth()

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
  (new chisel3.stage.ChiselStage).emitVerilog(new GenericALU[Float](16.W))
}
