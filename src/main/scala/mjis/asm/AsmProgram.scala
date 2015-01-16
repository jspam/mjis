package mjis.asm

import scala.collection.mutable.ListBuffer
import scala.collection.mutable

class AsmBasicBlock(val nr: Int = -1) {
  val instructions = ListBuffer[Instruction]()
  /** Maps register for destination Phi node to the value to be copied there.
    * ListMap for determinism */
  val phi = mutable.ListMap[RegisterOperand, Operand]()

  val controlFlowInstructions = ListBuffer[Instruction]()
  val successors = ListBuffer[AsmBasicBlock]()
}

class AsmFunction(val name: String) {
  val prologue = new AsmBasicBlock()
  val epilogue = new AsmBasicBlock()
  var basicBlocks = List[AsmBasicBlock]()
  def allBlocks: Seq[AsmBasicBlock] = Seq(prologue) ++ basicBlocks ++ Seq(epilogue)
  var activationRecordSize = 0
}

class AsmProgram {
  val functions = ListBuffer[AsmFunction]()
}
