package mjis

import mjis.asm._
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class PeepholeOptimizer(input: AsmProgram) extends Phase[AsmProgram] {
  override def findings: List[mjis.Finding] = List()
  def getResult(): AsmProgram = {
    optimize(input)
    input
  }

  def optimize(input: AsmProgram): Unit = {
    input.functions foreach optimizeFunction
  }

  def optimizeFunction(input: AsmFunction): Unit = {
    input.basicBlocks.foreach(b => optimizeSequence(b.instructions))
  }

  def optimizeSequence(instructions: ListBuffer[Instruction]): Unit = {
    var index = 0
    while (index < instructions.length) {
      tryAllPatterns(instructions, index) match {
        case None => index += 1
        case Some((newInstructions, patternSize)) =>
          instructions.remove(index, patternSize)
          instructions.insert(index, newInstructions: _*)
          index = 0 max (index-3)
      }
    }
  }

  def tryAllPatterns(list: ListBuffer[Instruction], startIndex: Int): Option[(Seq[Instruction], Int)] = {
    val remainingInstructions = list.length - startIndex
    if (remainingInstructions >= 3) {
      tryPatternsOfLength3.lift((list(startIndex), list(startIndex + 1), list(startIndex + 2))) match {
        case Some(result) => return Some((result, 3))
        case None =>
      }
    }
    if (remainingInstructions >= 2) {
      tryPatternsOfLength2.lift((list(startIndex), list(startIndex + 1))) match {
        case Some(result) => return Some((result, 2))
        case None =>
      }
    }
    if (remainingInstructions >= 1) {
      tryPatternsOfLength1.lift(list(startIndex)) match {
        case Some(result) => return Some((result, 1))
        case None =>
      }
    }
    None
  }

  private def tryPatternsOfLength1: PartialFunction[Instruction, Seq[Instruction]] = {
    case Mov(a1, a2) if a1 == a2 => Seq()
    case Mov(ConstOperand(0, _), a: RegisterOperand) => Seq(Xor(a, a))
    case Shl(ConstOperand(1, _), a: RegisterOperand) => Seq(Add(a, a))
    case Lea(AddressOperand(Some(a1), Some((b, 1)), 0, _), a2) if a1 == a2 => Seq(Add(b, a1))
    case Lea(AddressOperand(Some(a1), None, c, _), a2) if a1 == a2 => Seq(Add(ConstOperand(c, a1.sizeBytes), a1))
  }

  private def tryPatternsOfLength2: PartialFunction[(Instruction, Instruction), Seq[Instruction]] = {
    case (Mov(_, a1),
      Mov(b, a2)) if noDependency(a1, b) && a1 == a2 => Seq(Mov(b, a1))
    case (Mov(a1, b1),
      Mov(b2, a2)) if a1 == a2 && b1 == b2 => Seq(Mov(a1, b1))
    case (
      Cmp(ConstOperand(0, _), a: RegisterOperand),
      condInstr@(JmpConditional(_) | MovConditional(_, _))
    ) => Seq(
      Test(a, a),
      condInstr
    )
  }

  private def tryPatternsOfLength3: PartialFunction[(Instruction, Instruction, Instruction), Seq[Instruction]] = {
    case (Mov(a, b1),
      IDiv(b2),
      Mov(c, b3)) if !a.isInstanceOf[ConstOperand] && b1 == b2 && b2 == b3
      => Seq(IDiv(a), Mov(c, b1))
  }

  /*
   * Checks if the value of operand b cannot depend on the value of a
   */
  private def noDependency(a: Operand, b: Operand) : Boolean = (a, b) match {
    case (ConstOperand(_, _), _) => true
    case (_, ConstOperand(_, _)) => true
    case (RegisterOperand(nr1, _), RegisterOperand(nr2, _)) if nr1 != nr2 => true
    case (RegisterOperand(nr1, _), AddressOperand(base, offset, _, _)) =>
      // Neither base nor offset may be the same register as nr1
      (base match {
        case Some(RegisterOperand(nr2, _)) => nr1 != nr2
        case None => true
      }) && (offset match {
        case Some((RegisterOperand(nr2, _), _)) => nr1 != nr2
        case None => true
      })
    case (AddressOperand(_, _, _, _), RegisterOperand(_, _)) => true
    case _ => false
  }
}