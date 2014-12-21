package mjis.asm

import firm.{Relation, TargetValue}
import AMD64Registers._

case class Register(name: String, sizeBytes: Int)

object AMD64Registers {
  def RSP = -1
  def RBP = -2
  def RAX = -3
  def RBX = -4
  def RDI = -5
  def RSI = -6
  def RDX = -7
  def RCX = -8
  def R8  = -9
  def R9  = -10

  val Registers = Map[Int, Register] (
    RSP -> Register("rsp", 8),
    RBP -> Register("rbp", 8),
    RAX -> Register("rax", 8),
    RBX -> Register("rbx", 8),
    RDI -> Register("rdi", 8),
    RSI -> Register("rsi", 8),
    RDX -> Register("rdx", 8),
    RCX -> Register("rcx", 8),
    R8  -> Register("r8", 8),
    R9  -> Register("r9", 8)
  )

  // Registers to pass parameters in (in order)
  val ParamRegisters = List(RDI, RSI, RDX, RCX, R8, R9)
}

abstract class Operand(val sizeBytes: Int)
case class RegisterOperand(var regNo: Int, override val sizeBytes: Int) extends Operand(sizeBytes)
case class RegisterOffsetOperand(var regNr: Int, var offset: Int, override val sizeBytes: Int) extends Operand(sizeBytes)
case class ConstOperand(value: Int, override val sizeBytes: Int) extends Operand(sizeBytes)
case class LabelOperand(name: String) extends Operand(0) {
  def this(name: Int) = this(s".L$name")
}

abstract class Instruction {
  var comment = ""
  def operands: Seq[Operand] = Seq()
  def opcode: String = this.getClass.getSimpleName.toLowerCase
  def withComment(comment: String) = { this.comment = comment; this }
  var stackPointerDisplacement: Int = 0
}

abstract class ZeroOperandsInstruction extends Instruction
abstract class OneOperandInstruction(op1: Operand) extends Instruction {
  override def operands = Seq(op1)
}
abstract class TwoOperandsInstruction(op1: Operand, op2: Operand) extends Instruction {
  override def operands = Seq(op1, op2)
}

case class Addq(left: Operand, rightAndResult: RegisterOperand) extends TwoOperandsInstruction(left, rightAndResult)
case class Call(method: LabelOperand) extends OneOperandInstruction(method)
case class Cmpq(left: Operand, right: Operand) extends TwoOperandsInstruction(left, right)
case class Movq(src: Operand, dest: Operand) extends TwoOperandsInstruction(src, dest)
case class Mulq(left: Operand) extends OneOperandInstruction(left)
case class Popq(dest: RegisterOperand) extends OneOperandInstruction(dest)
case class Jmp(dest: LabelOperand) extends OneOperandInstruction(dest)
case class JmpConditional(dest: LabelOperand, relation: Relation, negate: Boolean) extends OneOperandInstruction(dest) {
  override def opcode: String = relation match {
    case Relation.Less => if (negate) "jge" else "jl"
    case Relation.GreaterEqual => if (negate) "jl" else "jge"

    case Relation.Greater => if (negate) "jle" else "jg"
    case Relation.LessEqual => if (negate) "jg" else "jle"

    case Relation.Equal => if (negate) "je" else "jne"
    case Relation.UnorderedLessGreater => if (negate) "jne" else "je"

    case _ => ???
  }
}
case class Pushq(src: Operand) extends OneOperandInstruction(src)
case class Ret() extends ZeroOperandsInstruction
case class Subq(subtrahend: Operand, minuendAndResult: RegisterOperand) extends TwoOperandsInstruction(subtrahend, minuendAndResult)
