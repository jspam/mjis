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

  def unapply0(opId: Int)(instr: Instruction): Boolean = instr.opId == opId
  def unapply1(opId: Int)(instr: Instruction): Option[Operand] = if (instr.opId != opId) None else Some(instr.operands(0))
  def unapply2(opId: Int)(instr: Instruction): Option[(Operand, Operand)] = if (instr.opId != opId) None else Some((instr.operands(0), instr.operands(1)))
}

object OpId {
  val ADD  = 1
  val AND  = 2
  val CALL = 3
  val CDQ  = 4
  val CMP  = 5
  val DEC  = 6
  val IDIV = 7
  val INC  = 8
  val JMP  = 9
  val SUB  = 10
  val NEG  = 11
  val LEA  = 12
  val MUL  = 13
  val SHL  = 14
  val MOV  = 15
  val RET  = 16
  val JL   = 17
  val JLE  = 18
  val JGE  = 19
  val JG   = 20
  val JE   = 21
  val JNE  = 22
}

sealed abstract class Operand(val sizeBytes: Int)
case class RegisterOperand(regNr: Int, override val sizeBytes: Int) extends Operand(sizeBytes)
// base + offset * scale + displacement
case class AddressOperand(base: Option[RegisterOperand] = None, offset: Option[RegisterOperand] = None, scale: Int = 1, displacement: Int = 0, override val sizeBytes: Int) extends Operand(sizeBytes)
case class ConstOperand(value: Int, override val sizeBytes: Int) extends Operand(sizeBytes)
case class LabelOperand(name: String) extends Operand(0)
case class BasicBlockOperand(basicBlock: AsmBasicBlock) extends Operand(0)
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

  final val IMPLICIT = OperandSpec(1 << 5) // implicit operand, does not occur in textual representation
}

sealed class Instruction(val opId: Int, val opcode: String, operandsWithSpec: (Operand, OperandSpec)*) {
  var comment = ""
  def withComment(comment: String) = { this.comment += comment; this }
  var stackPointerDisplacement: Int = 0
  def withStackPointerDisplacement(displacement: Int): Instruction = {
    this.stackPointerDisplacement = displacement
    this
  }
  val operands = ListBuffer[Operand](operandsWithSpec.map(_._1):_*)
  val operandSpecs = Seq[OperandSpec](operandsWithSpec.map(_._2):_*)
  def suffix = this.opcode match {
    case "call" | "cdq" | "ret" => ""
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
        opId == that.opId
    case _ => false
  }

  override def hashCode(): Int = Seq(operands, operandSpecs, opId).hashCode()

  override def toString = this.opcode + " " +
    this.operands.zip(this.operandSpecs).filter { case (_, spec) => !spec.contains(OperandSpec.IMPLICIT) }.map(_._1).mkString(", ")
}

object And {
  def apply(left: Operand, rightAndResult: Operand): Instruction =
    new Instruction(OpId.AND, "and", (left, READ | CONST | MEMORY), (rightAndResult, READ | WRITE | MEMORY))
  def unapply(instr: Instruction) = unapply2(OpId.AND)(instr)
}

object Add {
  def apply(left: Operand, rightAndResult: Operand): Instruction =
    new Instruction(OpId.ADD, "add", (left, READ | CONST | MEMORY), (rightAndResult, READ | WRITE | MEMORY))
  def unapply(instr: Instruction) = unapply2(OpId.ADD)(instr)
}

object Inc {
  def apply(valueAndResult: Operand): Instruction =
    new Instruction(OpId.INC, "inc", (valueAndResult, READ | WRITE | MEMORY))
  def unapply(instr: Instruction) = unapply1(OpId.INC)(instr)
}

object Sub {
  def apply(subtrahend: Operand, minuendAndResult: Operand): Instruction =
    new Instruction(OpId.SUB, "sub", (subtrahend, READ | CONST | MEMORY), (minuendAndResult, READ | WRITE | MEMORY))
  def unapply(instr: Instruction) = unapply2(OpId.SUB)(instr)
}

object Dec {
  def apply(valueAndResult: Operand): Instruction =
    new Instruction(OpId.DEC, "dec", (valueAndResult, READ | WRITE | MEMORY))
  def unapply(instr: Instruction) = unapply1(OpId.DEC)(instr)
}

object Neg {
  def apply(valueAndResult: Operand): Instruction =
    new Instruction(OpId.NEG, "neg", (valueAndResult, READ | WRITE | MEMORY))
  def unapply(instr: Instruction) = unapply1(OpId.NEG)(instr)
}

object Lea {
  def apply(value: AddressOperand, result: RegisterOperand): Instruction =
    new Instruction(OpId.LEA, "lea", (value, MEMORY), (result, WRITE))
  def unapply(instr: Instruction) = unapply2(OpId.LEA)(instr)
}

object Mul {
  def apply(left: Operand): Instruction =
    new Instruction(OpId.MUL, "mul", (left, READ | MEMORY),
      (RegisterOperand(RAX, left.sizeBytes), READ | WRITE | IMPLICIT), (RegisterOperand(RDX, left.sizeBytes), WRITE | IMPLICIT))
  def unapply(instr: Instruction) = unapply1(OpId.MUL)(instr)
}

object IDiv {
  def apply(left: Operand): Instruction =
    new Instruction(OpId.IDIV, "idiv", (left, READ | MEMORY),
      (RegisterOperand(RDX, left.sizeBytes), READ | WRITE | IMPLICIT), (RegisterOperand(RAX, left.sizeBytes), READ | WRITE | IMPLICIT))
  def unapply(instr: Instruction) = unapply1(OpId.IDIV)(instr)
}

object Cdq {
  def apply(operandSize: Int) : Instruction =
    new Instruction(OpId.CDQ, "cdq",
      (RegisterOperand(RAX, operandSize), READ | IMPLICIT), (RegisterOperand(RDX, operandSize), WRITE | IMPLICIT))
  def unapply(instr: Instruction) = unapply0(OpId.CDQ)(instr)
}

object Shl {
  def apply(shift: ConstOperand, valueAndResult: Operand): Instruction =
    new Instruction(OpId.SHL, "shl", (shift, READ | CONST), (valueAndResult, READ | WRITE | MEMORY))
  def unapply(instr: Instruction) = unapply2(OpId.SHL)(instr)
}

object Cmp {
  def apply(left: Operand, right: Operand): Instruction =
    new Instruction(OpId.CMP, "cmp", (left, READ | CONST | MEMORY), (right, READ | MEMORY))
  def unapply(instr: Instruction) = unapply2(OpId.CMP)(instr)
}

object Call {
  def apply(method: LabelOperand, registerParams: Seq[RegisterOperand]) : Instruction =
    new Instruction(OpId.CALL, "call", Seq((method, NONE)) ++ registerParams.map((_, READ | IMPLICIT)):_*)
  def apply(method: LabelOperand, returnValueSizeBytes: Int, registerParams: Seq[RegisterOperand]) : Instruction =
    new Instruction(OpId.CALL, "call",
      Seq((method, NONE)) ++ registerParams.map((_, READ | IMPLICIT)) ++
      Seq((RegisterOperand(RAX, returnValueSizeBytes), WRITE | IMPLICIT)):_*)
  def unapply(instr: Instruction) = unapply1(OpId.CALL)(instr)
}

object Mov {
  def apply(src: Operand, dest: Operand) : Instruction =
    new Instruction(OpId.MOV, "mov", (src, READ | CONST | MEMORY), (dest, WRITE | MEMORY))
  def unapply(instr: Instruction) = unapply2(OpId.MOV)(instr)
}

object Jmp {
  def apply(dest: BasicBlockOperand) : Instruction = new Instruction(OpId.JMP, "jmp", (dest, READ))
  def unapply(instr: Instruction) = unapply1(OpId.JMP)(instr)
}

object JmpConditional {
  def apply(dest: BasicBlockOperand, relation: Relation, negate: Boolean): Instruction = {
    val (opId, opcode) = relation match {
      case Relation.Less => if (!negate) (OpId.JL, "jl") else (OpId.JGE, "jge")
      case Relation.GreaterEqual => if (!negate) (OpId.JGE, "jge") else (OpId.JL, "jl")

      case Relation.Greater => if (!negate) (OpId.JG, "jg") else (OpId.JLE, "jle")
      case Relation.LessEqual => if (!negate) (OpId.JLE, "jle") else (OpId.JG, "jg")

      case Relation.Equal => if (!negate) (OpId.JE, "je") else (OpId.JNE, "jne")
      case Relation.UnorderedLessGreater => if (!negate) (OpId.JNE, "jne") else (OpId.JE, "je")

      case _ => ???
    }
    new Instruction(opId, opcode, (dest, READ))
  }
  def unapply(instr: Instruction) = instr.opId match {
    case OpId.JL | OpId.JLE | OpId.JGE | OpId.JG | OpId.JE | OpId.JNE => Some(instr.operands(0))
    case _ => None
  }
}

object Ret {
  def apply() = new Instruction(OpId.RET, "ret")
  def apply(returnValueSizeBytes: Int) = new Instruction(OpId.RET, "ret", (RegisterOperand(RAX, returnValueSizeBytes), READ | IMPLICIT))
  def unapply(instr: Instruction) = unapply0(OpId.RET)(instr)
}

case class Phi(srcs: Seq[Operand], dest: RegisterOperand)

