package mjis

import mjis.asm._

import scala.collection.mutable

object PhiCodeGenerator {
  val MaxTempRegisters = 2
}

/** Creates Mov instructions for a map of parallel moves. */
class PhiCodeGenerator(inputPhi: Map[Operand, Operand]) {
  // A phi permutation interpreted as a graph looks as follows: Each node has indegree <= 1
  // (because each register is written to at most once) and arbitrary outdegree.
  // Each of the connected subgraphs is thus either acyclic (and therefore a tree) or contains
  // exactly one cycle (else there would be a node with indegree >= 2).
  // Each of the nodes in the cycle can optionally be the root of a tree.

  var tempRegNrs = Seq[Int]()

  val originalDestOp = mutable.Map[Operand, Operand]()

  val unifiedPhi = inputPhi.map { case (dest, src) =>
    val unifiedDest = unifyOp(dest)
    originalDestOp(unifiedDest) = dest
    unifiedDest -> unifyOp(src)
  }.toMap

  // Inverse of the Phi map: source operand -> list of registers its value is written to
  private val invertedUnifiedPhi: Map[Operand, Iterable[Operand]] =
    unifiedPhi.groupBy(_._2).mapValues(_.map(_._1)).withDefaultValue(Seq())

  // Cycles
  private val cycles = mutable.ArrayBuffer[List[Operand]]()

  // Roots of standalone trees
  private val roots = mutable.ArrayBuffer[Operand]()

  buildCyclesAndRoots()

  /* Returns a unified version (all sizeBytes stripped out) of an operand.
   * Only these unified operands are used in the dependency graph construction
   * so that permutations like "$0 -> eax, rax -> rbx" are correctly ordered. */
  private def unifyOp: Operand => Operand = {
    case r: RegisterOperand => r.copy(sizeBytes = 0)
    case s: SSERegisterOperand => s.copy(sizeBytes = 0)
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

  private def mov(srcOp: Operand, destOp: Operand, inCycle: Boolean) = {
    def getTempRegNr(purpose: String) = if (inCycle) {
      assert(tempRegNrs.size >= 2, s"Need two temporary registers to create $purpose moves in cycles.")
      tempRegNrs(1)
    } else {
      assert(tempRegNrs.size >= 1, s"Need a temporary register to create $purpose moves.")
      tempRegNrs(0)
    }

    (srcOp, destOp) match {
      case (_: ActivationRecordOperand, _: ActivationRecordOperand) =>
        val tempRegNr = getTempRegNr("memory-to-memory")
        Seq(
          Mov(srcOp, RegisterOperand(tempRegNr, srcOp.sizeBytes)),
          Mov(RegisterOperand(tempRegNr, destOp.sizeBytes), destOp)
        )
      case (_: SSERegisterOperand, _: SSERegisterOperand) =>
        val tempRegNr = getTempRegNr("SSE-to-SSE")
        Seq(
          MovSSE(srcOp, RegisterOperand(tempRegNr, 8), 8),
          MovSSE(RegisterOperand(tempRegNr, 8), destOp, 8)
        )
      case (_: ConstOperand, _: SSERegisterOperand) =>
        val tempRegNr = getTempRegNr("const-to-SSE")
        Seq(
          Mov(srcOp, RegisterOperand(tempRegNr, 8)),
          MovSSE(RegisterOperand(tempRegNr, 8), destOp, 8)
        )
      case (_: SSERegisterOperand, _) => Seq(MovSSE(srcOp, destOp, destOp.sizeBytes max 4))
      case (_, _: SSERegisterOperand) => Seq(MovSSE(srcOp, destOp, srcOp.sizeBytes max 4))
      case _ => Seq(Mov(srcOp, destOp))
    }
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
      mov(srcOp, destOp, inCycle = false)
    } map(_.withComment(s" - acyclic permutation starting at $rootOp"))
  }

  /** Writes the given cyclic permutation using a temporary register. */
  private def getInstructionsForCycle(cycle: List[Operand]): Seq[Instruction] = {
    assert(cycle.length >= 2)
    assert(tempRegNrs.size >= 1, "Need a temporary register to create instructions for cycles.")
    val comment = " - cyclic permutation " + cycle.mkString(" -> ")

    val tempRegister = RegisterOperand(tempRegNrs(0), originalDestOp(cycle.head).sizeBytes)

    mov(inputPhi(originalDestOp(cycle.head)), tempRegister, inCycle = true) ++
    cycle.tail.flatMap { destOpUnified =>
      val destOp = originalDestOp(destOpUnified)
      val srcOp = inputPhi(destOp)
      mov(srcOp, destOp, inCycle = true)
    } ++
    mov(tempRegister, originalDestOp(cycle.head), inCycle = true) map(_.withComment(comment))
  }

  lazy val neededTempRegs = {
    def moveNeedsTempReg(ops: (Operand, Operand) /* dest, src */): Boolean = ops match {
      case (_: SSERegisterOperand, _: SSERegisterOperand | _: ConstOperand) => true
      case (_: ActivationRecordOperand | _: AddressOperand, _: ActivationRecordOperand | _: AddressOperand) => true
      case _ => false
    }
    def moveNeedsTempRegList(ops: List[Operand]): Boolean = moveNeedsTempReg((ops(0), ops(1)))

    if (cycles.nonEmpty)
      // Need one temp register for the cycle,
      // and another one if there are memory-to-memory/SSE-to-SSE/const-to-SSE moves in the cycle.
      if (cycles.exists(_.sliding(2).exists(moveNeedsTempRegList))) 2 else 1
    else
      // Memory-to-memory/SSE-to-SSE/const-to-SSE moves need a temp register
      if (inputPhi.exists(moveNeedsTempReg)) 1 else 0
  }

  def getInstructions(): Seq[Instruction] = {
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
