package mjis.asm

import scala.collection.mutable.ListBuffer
import scala.collection.mutable

class AsmBasicBlock(val nr: Int = -1) {
  val instructions = ListBuffer[Instruction]()
  val phi = mutable.ListMap[Int, Operand]() // maps destination Phi node to the operand with the value to be copied there
                                            // ListMap for determinism
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