package mjis

import scala.collection.mutable
import mjis.asm._
import scala.collection.mutable.AbstractBuffer
import scala.language.existentials
import scala.collection.mutable.ListBuffer

class PeepholeOptimizer(input: AsmProgram) extends Phase[AsmProgram] {
  var num_replacements = 0

  override def findings: List[mjis.Finding] = List()
  def getResult(): AsmProgram = {
    optimize(input)
    input
  }

  def optimize(input: AsmProgram): Unit = {
    input.functions foreach optimizeFunction
  }

  def optimizeFunction(input: AsmFunction): Unit = {
    input.basicBlocks foreach optimizeBasicBlock
  }

  def optimizeBasicBlock(input: AsmBasicBlock): Unit = {
    val newInstructions = optimizeSequence(input.instructions)
    input.instructions.clear()
    input.instructions ++= newInstructions
  }

  def optimizeSequence(instructions: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    var newInstructions = ListBuffer[Instruction]()
    var oldInstructions = instructions
    var changed = false
    // Repeat "peepholeing" until one pass doesn't change any instruction
    do {
      changed = false
      var index = 0;
      while (index < oldInstructions.length) {
        tryAllPatterns(oldInstructions, index) match {
          case None =>
            newInstructions += oldInstructions(index)
            index += 1
          case Some((instructions, patternSize)) =>
            num_replacements += (if (patternSize == 1) 0 else 1)
            newInstructions ++= instructions
            index += patternSize
            changed = true
        }
      }
      oldInstructions = newInstructions
      newInstructions = ListBuffer()
    } while (changed)
    oldInstructions
  }

  def tryAllPatterns(list: AbstractBuffer[Instruction], startIndex: Int): Option[(Seq[Instruction], Int)] = {
    val remainingInstructions = list.length - startIndex;
    if (remainingInstructions >= 3) {
      tryPatternsOfLength3((list(startIndex), list(startIndex + 1), list(startIndex + 2))) match {
        case Some(result) => return Some((result, 3))
        case None =>
      }
    }
    if (remainingInstructions >= 2) {
      tryPatternsOfLength2((list(startIndex), list(startIndex + 1))) match {
        case Some(result) => return Some((result, 2))
        case None =>
      }
    }
    if (remainingInstructions >= 1) {
      tryPatternsOfLength1(list(startIndex)) match {
        case Some(result) => return Some((result, 1))
        case None =>
      }
    }
    None
  }

  private def tryPatternsOfLength1(instruction: Instruction): Option[Seq[Instruction]] = instruction match {
    case Mov(a1, a2) if a1 == a2 => Some(Seq())
    case _ => None
  }

  private def tryPatternsOfLength2(tpl: (Instruction, Instruction)): Option[Seq[Instruction]] = tpl match {
    case (Mov(_, a1),
      Mov(b, a2)) if noDependecy(a1, b) && a1 == a2 => Some(Seq(Mov(b, a1)))
    case (Mov(a1, b1),
      Mov(b2, a2)) if a1 == a2 && b1 == b2 => Some(Seq(Mov(a1, b1)))
    case _ => None
  }

  private def tryPatternsOfLength3(tpl: (Instruction, Instruction, Instruction)): Option[Seq[Instruction]] = tpl match {
    case (Mov(a, b1),
      IDiv(b2),
      Mov(c, b3)) if !a.isInstanceOf[ConstOperand] && b1 == b2 && b2 == b3
      => Some(Seq(IDiv(a), Mov(c, b1)))
    case (Mov(a1, tmp1),
      Mov(b1, a2),
      Mov(tmp2, b2)) if a1 == a2 && b1 == b2 && tmp1 == tmp2
      => Some(Seq(Mov(a1, tmp1), Xchg(a1, b1)))
    case _ => None
  }

  /*
   * Checks if the value of operand b cannot depend on the value of a
   */
  private def noDependecy(a: Operand, b: Operand) : Boolean = (a, b) match {
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