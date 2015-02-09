package mjis.asm

import firm.Relation
import mjis.asm.OperandSpec._
import mjis.asm.Instruction._

import scala.collection.mutable.ListBuffer

import mjis.asm.AMD64Registers._

object Instruction {
  def suffixForSize(sizeBytes: Int): String = sizeBytes match {
    case 8 => "q"
    case 4 => "l"
    case 2 => "w"
    case 1 => "b"
    case 0 => ""
  }

  def unapply0(opcode: String)(instr: Instruction): Boolean = instr.opcode == opcode
  def unapply1(opcode: String)(instr: Instruction): Option[Operand] = if (instr.opcode != opcode) None else Some(instr.operands(0))
  def unapply2(opcode: String)(instr: Instruction): Option[(Operand, Operand)] = if (instr.opcode != opcode) None else Some((instr.operands(0), instr.operands(1)))

  def unorderedMasked(relation: Relation) = Relation.fromValue(relation.value & ~Relation.Unordered.value)
}

sealed abstract class Operand(val sizeBytes: Int)
case class RegisterOperand(regNr: Int, override val sizeBytes: Int) extends Operand(sizeBytes)
case class SSERegisterOperand(regNr: Int, override val sizeBytes: Int) extends Operand(sizeBytes)
// base + index * scale + offset
case class AddressOperand(base: Option[RegisterOperand] = None, indexAndScale: Option[(RegisterOperand, Int)] = None, offset: Int = 0, override val sizeBytes: Int) extends Operand(sizeBytes)
case class ConstOperand(value: Int, override val sizeBytes: Int) extends Operand(sizeBytes)
case class LabelOperand(name: String) extends Operand(0) {
  def this(nr: Int) = this(s".L$nr")
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
  final val WRITE_BEFORE = OperandSpec(1 << 6) // make the liveness interval begin before, not at the instruction

  final val MEMORY = OperandSpec(1 << 3) // operand can be a memory location
  final val CONST = OperandSpec(1 << 4) // operand can be a constant

  final val IMPLICIT = OperandSpec(1 << 5) // implicit operand, does not occur in textual representation
}

sealed class Instruction(val opcode: String, operandsWithSpec: (Operand, OperandSpec)*) {
  var comment = ""
  def withComment(comment: String) = { this.comment += comment; this }
  var stackPointerOffset: Int = 0
  def withStackPointerOffset(offset: Int): Instruction = {
    this.stackPointerOffset = offset
    this
  }
  val operands = ListBuffer[Operand](operandsWithSpec.map(_._1):_*)
  val operandSpecs = Seq[OperandSpec](operandsWithSpec.map(_._2):_*)
  def suffix = this.opcode match {
    case "call" | "cdq" | "ret" | "movd" | "movq" => ""
    case _ =>
      val (constOperands, nonConstOperands) = operands.partition(_.isInstanceOf[ConstOperand])
      if (nonConstOperands.isEmpty) {
        suffixForSize(0)
      } else {
        assert(nonConstOperands.tail.forall(_.sizeBytes == nonConstOperands.head.sizeBytes),
          s"$this: not all operands have the same size")
        assert(constOperands.forall(_.sizeBytes <= nonConstOperands.head.sizeBytes),
          s"$this: A const operand is bigger than the rest of the operands")
        suffixForSize(nonConstOperands.head.sizeBytes)
      }
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Instruction]

  override def equals(other: Any): Boolean = other match {
    case that: Instruction =>
      (that canEqual this) &&
        operands == that.operands &&
        operandSpecs == that.operandSpecs &&
        opcode == that.opcode
    case _ => false
  }

  override def hashCode(): Int = Seq(operands, operandSpecs, opcode).hashCode()

  override def toString = this.opcode + " " +
    this.operands.zip(this.operandSpecs).filter { case (_, spec) => !spec.contains(OperandSpec.IMPLICIT) }.map(_._1).mkString(", ")
}

object And {
  def apply(left: Operand, rightAndResult: Operand): Instruction =
    new Instruction("and", (left, READ | CONST | MEMORY), (rightAndResult, READ | WRITE | MEMORY))
  def unapply(instr: Instruction) = unapply2("and")(instr)
}

object Add {
  def apply(left: Operand, rightAndResult: Operand): Instruction =
    new Instruction("add", (left, READ | CONST | MEMORY), (rightAndResult, READ | WRITE | MEMORY))
  def unapply(instr: Instruction) = unapply2("add")(instr)
}

object Sub {
  def apply(subtrahend: Operand, minuendAndResult: Operand): Instruction =
    new Instruction("sub", (subtrahend, READ | CONST | MEMORY), (minuendAndResult, READ | WRITE | MEMORY))
  def unapply(instr: Instruction) = unapply2("sub")(instr)
}

object Neg {
  def apply(valueAndResult: Operand): Instruction =
    new Instruction("neg", (valueAndResult, READ | WRITE | MEMORY))
  def unapply(instr: Instruction) = unapply1("neg")(instr)
}

object Not {
  def apply(valueAndResult: Operand): Instruction =
    new Instruction("not", (valueAndResult, READ | WRITE | MEMORY))
  def unapply(instr: Instruction) = unapply1("not")(instr)
}

object Lea {
  def apply(value: AddressOperand, result: RegisterOperand): Instruction =
    new Instruction("lea", (value, MEMORY), (result, WRITE))
  def unapply(instr: Instruction) = unapply2("lea")(instr)
}

object Mul {
  def apply(left: Operand): Instruction =
    new Instruction("mul", (left, READ | MEMORY),
      (RegisterOperand(RAX, left.sizeBytes), READ | WRITE | IMPLICIT), (RegisterOperand(RDX, left.sizeBytes), WRITE | IMPLICIT))
  def unapply(instr: Instruction) = unapply1("mul")(instr)
}
object IMul {
  def apply(left: Operand): Instruction =
    new Instruction("imul", (left, READ | MEMORY),
      (RegisterOperand(RAX, left.sizeBytes), READ | WRITE | IMPLICIT), (RegisterOperand(RDX, left.sizeBytes), WRITE | IMPLICIT))
  def unapply(instr: Instruction) = unapply1("imul")(instr)
}

object IDiv {
  def apply(divisor: Operand): Instruction =
    new Instruction("idiv", (divisor, READ | MEMORY),
      (RegisterOperand(RDX, divisor.sizeBytes), READ | WRITE | IMPLICIT), (RegisterOperand(RAX, divisor.sizeBytes), READ | WRITE | IMPLICIT))
  def unapply(instr: Instruction) = unapply1("idiv")(instr)
}

object DivMod {
  def apply(dividend: Operand, divisor: Operand): Instruction =
    new Instruction("DivMod", (dividend, READ | MEMORY), (divisor, READ | MEMORY),
      (RegisterOperand(RDX, divisor.sizeBytes), WRITE_BEFORE | IMPLICIT),
      (RegisterOperand(RAX, dividend.sizeBytes), WRITE_BEFORE | IMPLICIT))
  def unapply(instr: Instruction) = unapply2("DivMod")(instr)
}

object Cdq {
  def apply(operandSize: Int) : Instruction =
    new Instruction("cdq",
      (RegisterOperand(RAX, operandSize), READ | IMPLICIT), (RegisterOperand(RDX, operandSize), WRITE | IMPLICIT))
  def unapply(instr: Instruction) = unapply0("cdq")(instr)
}

object Shl {
  def apply(shift: ConstOperand, valueAndResult: Operand): Instruction =
    new Instruction("shl", (shift, READ | CONST), (valueAndResult, READ | WRITE | MEMORY))
  def unapply(instr: Instruction) = unapply2("shl")(instr)
}
object Shr {
  def apply(shift: ConstOperand, valueAndResult: Operand): Instruction =
    new Instruction("shr", (shift, READ | CONST), (valueAndResult, READ | WRITE | MEMORY))
  def unapply(instr: Instruction) = unapply2("shr")(instr)
}
object Sar {
  def apply(shift: ConstOperand, valueAndResult: Operand): Instruction =
    new Instruction("sar", (shift, READ | CONST), (valueAndResult, READ | WRITE | MEMORY))
  def unapply(instr: Instruction) = unapply2("sar")(instr)
}

object Cmp {
  def apply(left: Operand, right: Operand): Instruction =
    new Instruction("cmp", (right, READ | CONST | MEMORY), (left, READ | MEMORY))
  def unapply(instr: Instruction) = unapply2("cmp")(instr)
}

object Call {
  def apply(method: LabelOperand, registerParams: Seq[RegisterOperand]) : Instruction =
    new Instruction("call", Seq((method, NONE)) ++ registerParams.map((_, READ | IMPLICIT)):_*)
  def apply(method: LabelOperand, returnValueSizeBytes: Int, registerParams: Seq[RegisterOperand]) : Instruction =
    new Instruction("call",
      Seq((method, NONE)) ++ registerParams.map((_, READ | IMPLICIT)) ++
      Seq((RegisterOperand(RAX, returnValueSizeBytes), WRITE | IMPLICIT)):_*)
  def unapply(instr: Instruction) = if (instr.opcode != "call") None else Some(instr.operands(0).asInstanceOf[LabelOperand])
}

/* Instruction for creating an intra-block jump destination. */
object Label {
  def apply(label: LabelOperand): Instruction = {
    assert(label.name.startsWith(".T"))
    new Instruction(label.name + ":", (label, NONE))
  }
  def unapply(instr: Instruction) = if (instr.opcode.startsWith(".T")) Some(instr.operands(0).asInstanceOf[LabelOperand]) else None
}

object Mov {
  def apply(src: Operand, dest: Operand) : Instruction =
    new Instruction("mov", (src, READ | CONST | MEMORY), (dest, WRITE | MEMORY))
  def unapply(instr: Instruction) = unapply2("mov")(instr)
}

object MovConditional {
  def apply(src: Operand, dest: Operand, relation: Relation): Instruction = {
    val opcode = unorderedMasked(relation) match {
      case Relation.Less => "cmovl"
      case Relation.GreaterEqual => "cmovge"

      case Relation.Greater => "cmovg"
      case Relation.LessEqual => "cmovle"

      case Relation.Equal => "cmove"
      case Relation.LessGreater => "cmovne"

      case _ => ???
    }
    new Instruction(opcode, (src, READ | CONST | MEMORY), (dest, WRITE | MEMORY))
  }

  def unapply(instr: Instruction) = instr.opcode match {
    case "cmovl" | "cmovge" | "cmovg" | "cmovle" | "cmove" | "cmovne" => Some((instr.operands(0), instr.operands(1)))
    case _ => None
  }
}

object MovSSE {
  def apply(src: Operand, dest: Operand, sizeBytes: Int) : Instruction =
    new Instruction(if (sizeBytes == 4) "movd" else "movq", (src, READ | CONST | MEMORY), (dest, WRITE | MEMORY))
  def unapply(instr: Instruction) =
    if (instr.opcode == "movd" || instr.opcode == "movq") Some((instr.operands(0), instr.operands(1)))
    else None
}

object Jmp {
  def apply(dest: LabelOperand) : Instruction = new Instruction("jmp", (dest, READ))
  def unapply(instr: Instruction) = unapply1("jmp")(instr)
}

object JmpConditional {
  def apply(dest: LabelOperand, relation: Relation): Instruction = {
    val opcode = unorderedMasked(relation) match {
      case Relation.Less => "jl"
      case Relation.GreaterEqual => "jge"

      case Relation.Greater => "jg"
      case Relation.LessEqual => "jle"

      case Relation.Equal => "je"
      case Relation.LessGreater => "jne"

      case _ => ???
    }
    new Instruction(opcode, (dest, READ))
  }
  def unapply(instr: Instruction) = instr.opcode match {
    case "jl" | "jge" | "jg" | "jle" | "je" | "jne" => Some(instr.operands(0))
    case _ => None
  }
}

object Test {
  def apply(left: Operand, right: Operand): Instruction =
    new Instruction("test", (right, READ | CONST | MEMORY), (left, READ | MEMORY))
  def unapply(instr: Instruction) = unapply2("test")(instr)
}

object Ret {
  def apply() = new Instruction("ret")
  def apply(returnValueSizeBytes: Int) = new Instruction("ret", (RegisterOperand(RAX, returnValueSizeBytes), READ | IMPLICIT))
  def unapply(instr: Instruction) = unapply0("ret")(instr)
}

object Xor {
  def apply(left: Operand, rightAndResult: Operand): Instruction =
    new Instruction("xor", (left, READ | CONST | MEMORY), (rightAndResult, READ | WRITE))
  def unapply(instr: Instruction) = unapply2("xor")(instr)
}

case class Phi(srcs: Seq[Operand], dest: RegisterOperand)

