package mjis

import mjis.asm._

import scala.collection.mutable

/** Creates Mov instructions for a map of parallel moves. */
class PhiCodeGenerator(inputPhi: Map[Operand, Operand], cycleTempRegNr: Int, memMoveTempRegNr: Int) {
  // A phi permutation interpreted as a graph looks as follows: Each node has indegree <= 1
  // (because each register is written to at most once) and arbitrary outdegree.
  // Each of the connected subgraphs is thus either acyclic (and therefore a tree) or contains
  // exactly one cycle (else there would be a node with indegree >= 2).
  // Each of the nodes in the cycle can optionally be the root of a tree.

  val originalDestOp = mutable.Map[Operand, Operand]()

  val unifiedPhi = inputPhi.map { case (dest, src) =>
    originalDestOp(unifyOp(dest)) = dest
    unifyOp(dest) -> unifyOp(src)
  }.toMap

  // Inverse of the Phi map: source operand -> list of registers its value is written to
  private val invertedUnifiedPhi: Map[Operand, Iterable[Operand]] =
    unifiedPhi.groupBy(_._2).mapValues(_.map(_._1)).withDefaultValue(Seq())

  // Cycles
  private val cycles = mutable.ArrayBuffer[List[Operand]]()

  // Roots of standalone trees
  private val roots = mutable.ArrayBuffer[Operand]()

  /* Returns a unified version (all sizeBytes stripped out) of an operand.
   * Only these unified operands are used in the dependency graph construction
   * so that permutations like "$0 -> eax, rax -> rbx" are correctly ordered. */
  private def unifyOp: Operand => Operand = {
    case r: RegisterOperand => r.copy(sizeBytes = 0)
    case a: ActivationRecordOperand => a.copy(sizeBytes = 0)
    case op => op
  }

  private def buildCyclesAndRoots() = {
    @annotation.tailrec
    def buildCyclesAndRootsRec(current: Operand, stack: List[Operand],
        visited: mutable.Set[Operand]): Unit = {
      if (visited(current)) {
        if (stack.nonEmpty && stack.contains(current)) {
          // found cycle
          cycles += current :: stack.takeWhile(_ != current)
        }
      } else {
        visited += current
        unifiedPhi.get(current) match {
          case None =>
            // no predecessor -> root of a tree
            if (!roots.contains(current))
              roots += current
          case Some(r) =>
            buildCyclesAndRootsRec(r, current :: stack, visited)
        }
      }
    }

    val visited = mutable.Set[Operand]()
    unifiedPhi.keys.foreach(buildCyclesAndRootsRec(_, Nil, visited))
  }

  private def mov(srcOp: Operand, destOp: Operand) = (srcOp, destOp) match {
    case (_: ActivationRecordOperand, _: ActivationRecordOperand) => Seq(
      Mov(srcOp, RegisterOperand(memMoveTempRegNr, srcOp.sizeBytes)),
      Mov(RegisterOperand(memMoveTempRegNr, destOp.sizeBytes), destOp)
    )
    case _ => Seq(Mov(srcOp, destOp))
  }

  /** Writes the acyclic register permutation (= tree) starting at `rootOp`, skipping the successor
    * `cycleSuccessor` of `rootOp`. */
  private def getInstructionsForTree(rootOp: Operand, cycleSuccessor: Operand): Seq[Instruction] = {
    val workList = mutable.Queue[Operand]()
    workList ++= invertedUnifiedPhi(rootOp).filter(_ != cycleSuccessor)

    var ordered = List[Operand]()
    while (workList.nonEmpty) {
      val op = workList.dequeue()
      ordered = op :: ordered
      workList ++= invertedUnifiedPhi(op)
    }

    ordered.flatMap { destOpUnified =>
      val destOp = originalDestOp(destOpUnified)
      val srcOp = inputPhi(destOp)
      mov(srcOp, destOp)
    } map(_.withComment(s" - acyclic permutation starting at $rootOp"))
  }

  /** Writes the given cyclic permutation using a temporary register. */
  private def getInstructionsForCycle(cycle: List[Operand]): Seq[Instruction] = {
    assert(cycle.length >= 2)
    val comment = " - cyclic permutation " + cycle.mkString(" -> ")

    val tempRegister = RegisterOperand(cycleTempRegNr, originalDestOp(cycle.head).sizeBytes)

    (Mov(inputPhi(originalDestOp(cycle.head)), tempRegister) +:
    cycle.tail.flatMap { destOpUnified =>
      val destOp = originalDestOp(destOpUnified)
      val srcOp = inputPhi(destOp)
      mov(srcOp, destOp)
    }.toSeq :+
    Mov(tempRegister, originalDestOp(cycle.head))) map(_.withComment(comment))
  }

  def getInstructions(): Seq[Instruction] = {
    buildCyclesAndRoots()

    // Stand-alone roots
    roots.flatMap(rootOp => getInstructionsForTree(rootOp, null)) ++
    // Cycle nodes can themselves be the root of a tree
    cycles.flatMap(cycle => (cycle.last :: cycle).sliding(2).flatMap{
      case Seq(op, cycleSuccessor) => getInstructionsForTree(op, cycleSuccessor)
    }) ++
    // Cycles, using a temporary register
    cycles.flatMap(c => getInstructionsForCycle(c.reverse))
  }
}
