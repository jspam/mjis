package mjis.asm

import firm.Relation

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class AsmBasicBlock(val nr: Int = -1) {
  /* Phi functions at the beginning of this block, with parallel copy semantics */
  val phis = ListBuffer[Phi]()
  val instructions = ListBuffer[Instruction]()

  /** For successors.size == 2: Jump to successors(0) iff relation is fulfilled */
  var relation: Relation = null
  val successors = ArrayBuffer[AsmBasicBlock]()
  // `predecessors` contains None entries for blocks that are not in the function's list of basic blocks
  // (because they are unreachable) and Bad predecessors.
  val predecessors = ArrayBuffer[Option[AsmBasicBlock]]()
  var comment = ""
}

class AsmFunction(val name: String) {
  // `prologue` and `epilogue` are also contained in `basicBlocks`.
  val prologue = new AsmBasicBlock()
  var epilogue = new AsmBasicBlock()
  var basicBlocks = List[AsmBasicBlock]()
  var activationRecordSize = 0

  def controlFlowEdges: Seq[(AsmBasicBlock, AsmBasicBlock)] =
    this.basicBlocks.flatMap(b => b.predecessors.flatten.map((_, b)))

  def isLoopHeader(b: AsmBasicBlock) =
    basicBlocks.drop(basicBlocks.indexOf(b)).exists(_.successors.contains(b))

  def getLoopEnd(b: AsmBasicBlock) = basicBlocks.filter(_.successors.contains(b)).last
}

class AsmProgram {
  val functions = ListBuffer[AsmFunction]()
}
