package mjis

import mjis.asm._

import scala.collection.mutable

/** Creates Mov instructions for the contents of `phi` with parallel move semantics. */
class PhiCodeGenerator(phi: Map[RegisterOperand, Operand]) {
  // A phi permutation interpreted as a graph looks as follows: Each node has indegree <= 1
  // (because each register is written to at most once) and arbitrary outdegree.
  // Each of the connected subgraphs is thus either acyclic (and therefore a tree) or contains
  // exactly one cycle (else there would be a node with indegree >= 2).
  // Each of the nodes in the cycle can optionally be the root of a tree.

  // Inverse of the Phi map: source operand -> list of registers its value is written to
  private val invertedPhi: Map[Operand, Iterable[RegisterOperand]] =
    phi.groupBy(_._2).mapValues(_.map(_._1)).withDefaultValue(Seq())

  // Cycles
  private val cycles = mutable.ArrayBuffer[List[RegisterOperand]]()

  // Roots of standalone trees
  private val roots = mutable.ArrayBuffer[Operand]()

  private def buildCyclesAndRoots() = {
    @annotation.tailrec
    def buildCyclesAndRootsRec(reg: RegisterOperand, stack: List[RegisterOperand],
        visited: mutable.Set[RegisterOperand]): Unit = {
      if (visited(reg)) {
        if (stack.nonEmpty && stack.contains(reg)) {
          // found cycle
          cycles += reg :: stack.takeWhile(_ != reg)
        }
      } else {
        visited += reg
        phi.get(reg) match {
          case None =>
            // no predecessor -> root of a tree
            roots += reg
          case Some(r : RegisterOperand) =>
            buildCyclesAndRootsRec(r, reg :: stack, visited)
          case Some(op) =>
            // non-register operand -> has no predecessor -> root of a tree
            if (!roots.contains(op))
              roots += op
        }
      }
    }

    val visited = mutable.Set[RegisterOperand]()
    phi.keys.foreach(buildCyclesAndRootsRec(_, Nil, visited))
  }

  /** Writes the acyclic register permutation (= tree) starting at `rootOp`, skipping the successor
    * `cycleSuccessor` of `rootOp`. */
  private def getInstructionsForTree(rootOp: Operand, cycleSuccessor: RegisterOperand): Seq[Instruction] = {
    val workList = mutable.Queue[RegisterOperand]()
    workList ++= invertedPhi(rootOp).filter(_ != cycleSuccessor)

    var ordered = List[RegisterOperand]()
    while (workList.nonEmpty) {
      val op = workList.dequeue()
      ordered = op :: ordered
      workList ++= invertedPhi(op)
    }

    ordered.map { destOp =>
      val srcOp = phi(destOp)
      Mov(srcOp, destOp).withComment(s" - acyclic permutation starting at $rootOp")
    }
  }

  /** Writes the given cyclic permutation using a temporary register. */
  private def getInstructionsForCycle(cycle: List[RegisterOperand]): Seq[Instruction] = {
    assert(cycle.length >= 2)
    val comment = " - cyclic permutation " + cycle.mkString(" -> ")

    val destRegOp = cycle.last
    val tempRegister = RegisterOperand(0, destRegOp.sizeBytes)

    (tempRegister :: cycle).sliding(2).map { case Seq(destOp, srcOp) =>
      Mov(srcOp, destOp).withComment(comment)
    }.toSeq ++ Seq(Mov(tempRegister, destRegOp).withComment(comment))
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
