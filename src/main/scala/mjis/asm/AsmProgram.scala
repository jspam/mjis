package mjis.asm

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class AsmBasicBlock(val nr: Int = -1) {
  /* Phi functions at the beginning of this block, with parallel copy semantics */
  val phis = ListBuffer[Phi]()
  val instructions = ListBuffer[Instruction]()
  val controlFlowInstructions = ListBuffer[Instruction]()
  def allInstructions = instructions.iterator ++ controlFlowInstructions.iterator

  val successors = ArrayBuffer[AsmBasicBlock]()
  // `predecessors` contains None entries for blocks that are not in the function's list of basic blocks
  // (because they are unreachable) and Bad predecessors.
  val predecessors = ArrayBuffer[Option[AsmBasicBlock]]()
  var comment = ""
}

class AsmFunction(val name: String) {
  // `prologue` and `epilogue` are also contained in `basicBlocks`.
  val prologue = new AsmBasicBlock()
  val epilogue = new AsmBasicBlock()
  var basicBlocks = List[AsmBasicBlock]()
  var activationRecordSize = 0

  def controlFlowEdges: Seq[(AsmBasicBlock, AsmBasicBlock)] =
    this.basicBlocks.flatMap(b => b.predecessors.flatten.map((_, b)))
}

class AsmProgram {
  val functions = ListBuffer[AsmFunction]()
}
