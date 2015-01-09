package mjis.asm

import firm.Relation
import mjis.asm.OperandSpec._
import mjis.asm.Instruction._

import scala.collection.mutable.ListBuffer

case class Register(subregs: Map[Int, String])

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
    RSP -> Register(Map(8 -> "rsp", 4 -> "esp", 2 -> "sp", 1 -> "spl")),
    RBP -> Register(Map(8 -> "rbp", 4 -> "ebp", 2 -> "bp", 1 -> "bpl")),
    RAX -> Register(Map(8 -> "rax", 4 -> "eax", 2 -> "ax", 1 -> "al")),
    RBX -> Register(Map(8 -> "rbx", 4 -> "ebx", 2 -> "bx", 1 -> "bl")),
    RDI -> Register(Map(8 -> "rdi", 4 -> "edi", 2 -> "di", 1 -> "dil")),
    RSI -> Register(Map(8 -> "rsi", 4 -> "esi", 2 -> "si", 1 -> "sil")),
    RDX -> Register(Map(8 -> "rdx", 4 -> "edx", 2 -> "dx", 1 -> "dl")),
    RCX -> Register(Map(8 -> "rcx", 4 -> "ecx", 2 -> "cx", 1 -> "cl")),
    R8  -> Register(Map(8 -> "r8",  4 -> "r8d", 2 -> "r8w", 1 -> "r8b")),
    R9  -> Register(Map(8 -> "r9",  4 -> "r9d", 2 -> "r9w", 1 -> "r9b"))
  )

  // Registers to pass parameters in (in order)
  val ParamRegisters = List(RDI, RSI, RDX, RCX, R8, R9)
}

object Instruction {
  def suffixForSize(sizeBytes: Int): String = sizeBytes match {
    case 8 => "q"
    case 4 => "l"
    case 2 => "w"
    case 1 => "b"
    case 0 => ""
  }
}

sealed abstract class Operand(val sizeBytes: Int)
case class RegisterOperand(regNr: Int, override val sizeBytes: Int) extends Operand(sizeBytes)
case class RegisterOffsetOperand(base: RegisterOperand, offset: Int, override val sizeBytes: Int) extends Operand(sizeBytes)
case class ConstOperand(value: Int, override val sizeBytes: Int) extends Operand(sizeBytes)
case class LabelOperand(name: String) extends Operand(0) {
  def this(name: Int) = this(s".L$name")
}
/** equivalent to RegisterOffsetOperand(RBP, offset, sizeBytes), but we use RBP as a general purpose
  * register and instead convert these operands to RSP relative operands once the AR size is known. */
case class ActivationRecordOperand(offset: Int, override val sizeBytes: Int) extends Operand(sizeBytes)

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
  def suffix = {
    val (constOperands, nonConstOperands) = operands.partition(_.isInstanceOf[ConstOperand])
    if (nonConstOperands.isEmpty) {
      suffixForSize(0)
    } else {
      assert(nonConstOperands.tail.forall(_.sizeBytes == nonConstOperands.head.sizeBytes))
      assert(constOperands.forall(_.sizeBytes <= nonConstOperands.head.sizeBytes))
      suffixForSize(nonConstOperands.head.sizeBytes)
    }
  }
}

// Forget: the register contents are not needed any more -- forced end of liveness interval
case class Forget(reg: RegisterOperand) extends Instruction((reg, NONE)) {
  override def opcode: String = "# forget"
}
case class Add(left: Operand, rightAndResult: Operand) extends Instruction((left, READ | CONST | MEMORY), (rightAndResult, READ | WRITE | MEMORY))
case class Sub(subtrahend: Operand, minuendAndResult: Operand) extends Instruction((subtrahend, READ | CONST | MEMORY), (minuendAndResult, READ | WRITE | MEMORY))
case class Mul(left: Operand) extends Instruction((left, READ | MEMORY))
case class Shl(shift: ConstOperand, valueAndResult: Operand) extends Instruction((shift, CONST), (valueAndResult, READ | WRITE | MEMORY))
case class Cmp(left: Operand, right: Operand) extends Instruction((left, READ | CONST | MEMORY), (right, READ | MEMORY))
case class Call(method: LabelOperand) extends Instruction((method, READ))
case class Mov(src: Operand, dest: Operand) extends Instruction((src, READ | CONST | MEMORY), (dest, WRITE | MEMORY)) {
  override def suffix = src match {
    case _ : ConstOperand => suffixForSize(dest.sizeBytes)
    case _ =>
      assert(src.sizeBytes <= dest.sizeBytes)
      if (src.sizeBytes < dest.sizeBytes) "s" + suffixForSize(src.sizeBytes) + suffixForSize(dest.sizeBytes)
      else suffixForSize(dest.sizeBytes)
  }
}
case class Pop(dest: Operand) extends Instruction((dest, WRITE | MEMORY))
case class Push(src: Operand) extends Instruction((src, READ | MEMORY))
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
case class Ret() extends Instruction
