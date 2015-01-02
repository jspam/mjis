package mjis.asm

import firm.Relation
import mjis.asm.OperandSpec._

import scala.collection.mutable.ListBuffer

case class Register(name: String, sizeBytes: Int)

object AMD64Registers {
  val RSP = -1
  val RBP = -2
  val RAX = -3
  val RBX = -4
  val RDI = -5
  val RSI = -6
  val RDX = -7
  val RCX = -8
  val R8  = -9
  val R9  = -10

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

sealed trait Operand
case class RegisterOperand(regNo: Int, sizeBytes: Int) extends Operand
case class RegisterOffsetOperand(regNr: Int, offset: Int, sizeBytes: Int) extends Operand
case class ConstOperand(value: Int, sizeBytes: Int) extends Operand
case class LabelOperand(name: String) extends Operand {
  def this(name: Int) = this(s".L$name")
}

case class OperandSpec(value: Int) extends AnyVal {
  def |(that: OperandSpec) = OperandSpec(this.value | that.value)
  def &(that: OperandSpec) = OperandSpec(this.value & that.value)
  def contains(that: OperandSpec) = (this & that) == that
}

object OperandSpec {
  def apply(mods: OperandSpec*): OperandSpec = mods.fold(OperandSpec.NONE)(_ | _)

  final val NONE = OperandSpec(1 << 0)
  final val READ = OperandSpec(1 << 1)
  final val WRITE = OperandSpec(1 << 2)

  final val MEMORY = OperandSpec(1 << 3) // operand can be a memory location
  final val CONST = OperandSpec(1 << 4) // operand can be a constant
}

abstract class Instruction(private val operandsWithSpec: (Operand, OperandSpec)*) {
  var comment = ""
  def opcode: String = this.getClass.getSimpleName.toLowerCase
  def withComment(comment: String) = { this.comment = comment; this }
  var stackPointerDisplacement: Int = 0
  val operands = ListBuffer[Operand](operandsWithSpec.map(_._1):_*)
  val operandSpecs = Seq[OperandSpec](operandsWithSpec.map(_._2):_*)
}

case class Addq(left: Operand, rightAndResult: RegisterOperand) extends Instruction((left, READ | CONST), (rightAndResult, READ | WRITE))
case class Call(method: LabelOperand) extends Instruction((method, READ))
case class Cmpq(left: Operand, right: Operand) extends Instruction((left, READ | CONST), (right, READ | MEMORY))
// Forget: the register contents are not needed any more -- forced end of liveness interval
case class Forget(reg: RegisterOperand) extends Instruction((reg, NONE)) {
  override def opcode: String = "# forget"
}
case class Movq(src: Operand, dest: Operand) extends Instruction((src, READ | CONST), (dest, WRITE))
case class Mulq(left: Operand) extends Instruction((left, READ))
case class Popq(dest: RegisterOperand) extends Instruction((dest, WRITE))
case class Jmp(dest: LabelOperand) extends Instruction((dest, READ))
case class JmpConditional(dest: LabelOperand, relation: Relation, negate: Boolean) extends Instruction((dest, READ)) {
  override def opcode: String = relation match {
    case Relation.Less => if (!negate) "jl" else "jge"
    case Relation.GreaterEqual => if (!negate) "jge" else "jl"

    case Relation.Greater => if (!negate) "jg" else "jle"
    case Relation.LessEqual => if (!negate) "jle" else "jg"

    case Relation.Equal => if (!negate) "je" else "jne"
    case Relation.UnorderedLessGreater => if (!negate) "jne" else "je"

    case _ => ???
  }
}
case class Pushq(src: Operand) extends Instruction((src, READ))
case class Ret() extends Instruction
case class Subq(subtrahend: Operand, minuendAndResult: RegisterOperand) extends Instruction((subtrahend, READ), (minuendAndResult, READ | WRITE))
