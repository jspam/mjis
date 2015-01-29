package mjis

import java.io.BufferedWriter

import mjis.asm.OperandSpec._
import mjis.asm._
import mjis.asm.AMD64Registers._
import mjis.util.MapExtensions._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class RegisterAllocator(input: AsmProgram) extends Phase[AsmProgram] {
  override def getResult(): AsmProgram = {
    // R15 and RBP are reserved for swap operations for now. TODO: Don't reserve two whole registers for that
    for (f <- input.functions) new FunctionRegisterAllocator(f,
      Seq(RAX, RBX, RDI, RSI, RDX, RCX, R8, R9, R10, R11, R12, R13, R14/*, R15, RBP */),
      AMD64Registers.CallerSaveRegisters,
      R15,
      RBP
    ).allocateRegs()
    input
  }

  override def dumpResult(writer: BufferedWriter): Unit = {}

  override def findings = List[Finding]()
}

/**
 * Linear scan register allocator for programs in SSA form with interval splitting.
 *
 * The following papers were used as reference:
 *
 * Christian Wimmer, Hanspeter Mössenböck: Optimized Interval Splitting in a Linear Scan Register Allocator.
 * In Proceedings of the International Conference on Virtual Execution Environments, pages 132–141.
 * ACM Press, 2005. doi:10.1145/1064979.1064998
 *
 * Christian Wimmer, Michael Franz: Linear Scan Register Allocation on SSA Form.
 * In Proceedings of the International Symposium on Code Generation and Optimization, pages 170–179.
 * ACM Press, 2010. doi:10.1145/1772954.1772979
 */
class FunctionRegisterAllocator(function: AsmFunction,
  val PhysicalRegisters: Seq[Int],
  val CallerSaveRegisters: Set[Int] = Set(),
  val tempRegNo1: Int = 0, val tempRegNo2: Int = 0) {
  // TODO: Use the ordering defined by PhysicalRegisters; e.g. in functions without calls, caller-save registers
  // should be used first.

  // activationRecordSize takes into account only the values written to the AR by the function itself,
  // but not the parameters and return address.
  var activationRecordSize: Int = 0
  val activationRecord = mutable.HashMap[Int, Int]()

  private def activationRecordOperand(sizeBytes: Int): ActivationRecordOperand = {
    activationRecordSize = CodeGenerator.align(activationRecordSize + sizeBytes, sizeBytes)
    ActivationRecordOperand(-activationRecordSize, sizeBytes)
  }

  private def activationRecordOperand(reg: RegisterOperand): ActivationRecordOperand =
    activationRecord.get(reg.regNr) match {
      case Some(offset) => ActivationRecordOperand(offset, reg.sizeBytes)
      case None =>
        val result = activationRecordOperand(reg.sizeBytes)
        activationRecord(reg.regNr) = result.offset
        result
    }

  private val INFINITY = Int.MaxValue

  /** The lifetime interval for each (virtual and physical) register, indexed by register number.
    * In case of interval splitting, only the parent interval is stored here. */
  private val intervals = mutable.Map[Int, LivenessInterval]()

  /** Preferred physical register for each liveness interval. */
  private val registerHints = mutable.Map[LivenessInterval, Int]()

  /** Helper function that creates an interval for a register operand if one doesn't exist. */
  private def interval(regOp: RegisterOperand) = {
    intervals.get(regOp.regNr) match {
      case Some(existingInterval) => existingInterval
      case None =>
        val result = new LivenessInterval(regOp)
        intervals(regOp.regNr) = result
        result
    }
  }

  /** Returns all (virtual and physical) registers used by the given instructions
    * together with their read/write specification. Does not return physical registers
    * that aren't contained in `PhysicalRegisters` passed to the constructor, e.g. RSP. */
  private def getRegisterUsages(instr: Instruction): Map[RegisterOperand, OperandSpec] = {
    val result = mutable.Map[RegisterOperand, OperandSpec]().withDefaultValue(OperandSpec.NONE)
    instr.operands.zip(instr.operandSpecs) foreach { case (op, spec) => op match {
      case r: RegisterOperand if r.regNr >= 0 || PhysicalRegisters.contains(r.regNr) =>
        result(r) |= spec
      case a: AddressOperand => Seq(a.base, a.offset) foreach {
        case Some(r: RegisterOperand) if r.regNr >= 0 || PhysicalRegisters.contains(r.regNr) =>
          result(r) |= OperandSpec.READ
        case _ =>
      }
      case _ =>
    }}
    result.toMap
  }

  /** Position handling: The instructions of a block are numbered in increments of two starting at two.
    * The phi functions are viewed as starting at zero (at the starting position of the block).
    * The odd instruction numbers are reserved for inserting move instructions e.g. because of
    * interval splitting. */

  private val blockStartPos = function.basicBlocks.zip(
    function.basicBlocks.map(b => (b.allInstructions.length + 1) * 2).scan(0)(_ + _)
  ).toMap

  private val blockControlFlowStartPos =
    function.basicBlocks.map(b => b -> (blockStartPos(b) + (b.instructions.length + 1) * 2)).toMap

  private val blockEndPos =
    function.basicBlocks.map(b => b -> (blockStartPos(b) + (b.allInstructions.length + 1) * 2)).toMap

  private def instrsWithPos(b: AsmBasicBlock) =
    b.allInstructions.zip(Range(blockStartPos(b) + 2, blockEndPos(b), 2).iterator)


  /** Additional move instructions with parallel move semantics. The keys must be
    * odd numbers. */
  val insertedInstrs = mutable.Map[Int, mutable.ListBuffer[(Operand, Operand)]]().
    withPersistentDefault(_ => ListBuffer[(Operand, Operand)]())


  private def buildLivenessIntervals() = {
    // Records for each block which variables are live at its beginning.
    val liveIn = mutable.Map[AsmBasicBlock, Set[RegisterOperand]]().withDefaultValue(Set())

    for (b <- function.basicBlocks.reverse) {
      val live = mutable.Set[RegisterOperand]()

      for (succ <- b.successors) {
        live ++= liveIn(succ)

        val predIdx = succ.predecessors.indexOf(Some(b))
        for (phi <- succ.phis) {
          phi.srcs(predIdx) match {
            case r: RegisterOperand => live += r
            case _: ConstOperand => /* nothing to do */
            case _ => ???
          }
        }
      }

      // For each variable live at the end of the block, add a range spanning the whole block.
      // It might be shortened later if a definition is encountered.
      for (op <- live) interval(op).addRange(blockStartPos(b), blockEndPos(b))

      for ((instr, instrPos) <- instrsWithPos(b).toSeq.reverse) {
        instr match {
          case Call(_) =>
            // Block caller save registers
            for (reg <- PhysicalRegisters.filter(CallerSaveRegisters)) {
              // Assign size 8 to the register operand so that debug output will
              // work nicely.
              interval(RegisterOperand(reg, 8)).addRange(instrPos, instrPos)
            }
          case Mov(src: RegisterOperand, dest: RegisterOperand) if dest.regNr >= 0
            && (PhysicalRegisters.contains(src.regNr) || src.regNr >= 0) =>
              registerHints(interval(dest)) = src.regNr
          case _ =>
        }

        for ((op, spec) <- getRegisterUsages(instr)) {
          // Physical registers need no usage information since they cannot be spilled
          if (op.regNr >= 0) interval(op).addUsage(instrPos, spec)

          if (spec.contains(WRITE)) {
            // definition -- shorten live range
            interval(op).setFrom(instrPos)
            live -= op
          }
          if (spec.contains(READ)) {
            // usage
            interval(op).addRange(blockStartPos(b), instrPos)
            live += op
          }
        }
      }

      // Phi functions are a definition for their destination operands
      live --= b.phis.map(_.dest)

      if (function.isLoopHeader(b)) {
        val loopEnd = function.getLoopEnd(b)
        for (op <- live) interval(op).addRange(blockStartPos(b), blockEndPos(loopEnd))
      }

      liveIn(b) = live.toSet
    }
  }

  /** Performs the Linear Scan algorithm and returns a physical register operand or an activation
    * record operand for each liveness interval */
  private def linearScan(): Map[LivenessInterval, Operand] = {
    // Unhandled: Intervals which start after `position` and do not have a register assigned.
    // Using a priority queue because interval splitting might insert new intervals into unhandled.
    // Order by `-start` because the priority queue dequeues entries with higher values first.
    val unhandled = mutable.PriorityQueue[LivenessInterval]()(Ordering.by(-_.start))

    val (physicalRegIntervals, virtualRegIntervals) = intervals.values.partition(_.regOp.regNr < 0)
    unhandled ++= virtualRegIntervals

    // Active: Intervals which are currently alive and have a register assigned.
    val active = mutable.Set[LivenessInterval]()

    // Inactive: Intervals which have a lifetime hole at `position` and have a register assigned.
    val inactive = mutable.Set[LivenessInterval]()
    inactive ++= physicalRegIntervals

    // Physical register numbers currently assigned to each liveness interval that is not spilled.
    val physReg = mutable.Map[LivenessInterval, Int]()
    physReg ++= physicalRegIntervals.map(i => i -> i.regOp.regNr)

    val result = mutable.Map[LivenessInterval, Operand]()

    while (unhandled.nonEmpty) {
      val current = unhandled.dequeue()
      val position = current.start

      def optimalSplitPos(maxSplitPos: Int) = {
        // TODO: possible optimization: select optimal splitting position
        // Splitting may occur only at a block boundary (the resolve phase will insert the
        // appropriate move instructions) or at an odd position (because the move instructions
        // are inserted there).
        if (blockStartPos.valuesIterator.contains(maxSplitPos)) maxSplitPos
        else if (maxSplitPos % 2 == 0) maxSplitPos - 1
        else maxSplitPos
      }

      def splitInterval(it: LivenessInterval, maxSplitPos: Int, appendToUnhandled: Boolean = true): LivenessInterval = {
        assert(maxSplitPos >= position)

        val splitPos = optimalSplitPos(maxSplitPos)
        val itSplit = it.splitAt(splitPos)
        if (appendToUnhandled && itSplit != it) unhandled += itSplit
        itSplit
      }

      def spillInterval(interval: LivenessInterval) = {
        val spilledInterval = splitInterval(interval, position, appendToUnhandled = false)
        // do not append to unhandled -- it has been handled by assigning an AR operand!
        result(spilledInterval) = activationRecordOperand(interval.regOp)

        spilledInterval.nextUsage(position) match {
          // TODO: Possible optimization: split at first use position *that requires a register*
          case Some(nextUsage) => splitInterval(spilledInterval, nextUsage.getKey)
          case None =>
        }
      }

      /* Tries to allocate a register for (at least a part of) current without spilling an interval.
       * Returns true on success. */
      def tryAllocateWithoutSpilling(): Boolean = {
        val freeUntilPos = mutable.Map[Int, Int]()
        freeUntilPos ++= PhysicalRegisters.map(_ -> INFINITY)

        for (it <- active) freeUntilPos(physReg(it)) = 0 /* not free at all */

        for (it <- inactive) {
          it.nextIntersectionWith(current, position) match {
            case Some(intersectionPos) => freeUntilPos(physReg(it)) = intersectionPos min freeUntilPos(physReg(it))
            case None =>
          }
        }

        if (freeUntilPos.values.max <= position + (position % 2) /* next even instruction pos */) {
          // No register available without spilling
          false
        } else {
          val regHint = registerHints.get(current) match {
            case Some(regNr) if regNr < 0 => regNr
            case Some(regNr) if regNr >= 0 => result(intervals(regNr)) match {
              case r: RegisterOperand => r.regNr
              case _ => 0
            }
            case None => 0
          }
          val reg =
            // Take the preferred register even if it is not optimal
            if (regHint != 0 && freeUntilPos(regHint) > position + (position % 2)) regHint
            // else take the register with highest freeUntilPos. Additionally sort by register number for determinism.
            else freeUntilPos.toSeq.maxBy(t => (t._2, t._1))._1

          // `reg` is available ...
          physReg(current) = reg
          result(current) = current.regOp.copy(regNr = reg)

          if (freeUntilPos(reg) < current.end) {
            // ... but not for the whole interval, so we need to split
            splitInterval(current, freeUntilPos(reg))
          }

          true
        }
      }

      /* Allocate a register for current by spilling an interval (possibly current itself).
       * Returns whether current was assigned a physical register (instead of being spilled). */
      def allocateWithSpilling(): Boolean = {
        // Spill the interval with the furthest next use.
        val nextUsePos = mutable.Map[Int, Int]()
        nextUsePos ++= PhysicalRegisters.map(_ -> INFINITY)

        // Fixed intervals block physical registers and cannot be spilled.
        val nextBlockedPos = mutable.Map[Int, Int]()
        nextBlockedPos ++= PhysicalRegisters.map(_ -> INFINITY)

        for (it <- active) {
          if (it.regOp.regNr < 0) {
            nextUsePos(physReg(it)) = 0
            nextBlockedPos(physReg(it)) = nextUsePos(physReg(it))
          } else {
            nextUsePos(physReg(it)) = it.nextUsagePos(position)
          }
        }

        for (it <- inactive if it.intersects(current)) {
          if (it.regOp.regNr < 0) {
            nextUsePos(physReg(it)) = nextUsePos(physReg(it)) min it.nextIntersectionWith(current, position).get
            nextBlockedPos(physReg(it)) = nextUsePos(physReg(it))
          } else {
            nextUsePos(physReg(it)) = nextUsePos(physReg(it)) min it.nextUsagePos(position)
          }
        }

        // reg = register with highest nextUsePos
        // Sort by nextUsePos, then by register number for determinism
        val reg = nextUsePos.toSeq.maxBy(t => (t._2, t._1))._1

        val regAssigned = if (current.nextUsagePos(position) > nextUsePos(reg)) {
          // current itself is the interval with the furthest next use
          spillInterval(current)
          false
        } else {
          // spill another interval
          physReg(current) = reg
          result(current) = current.regOp.copy(regNr = reg)

          // The register might still be partially blocked by a fixed interval
          if (nextBlockedPos(reg) < current.end) {
            splitInterval(current, nextBlockedPos(reg))
          }

          // Split and spill intervals that currently block `reg`
          for (it <- active if it.regOp.regNr >= 0 && physReg(it) == reg) {
            spillInterval(it)
            active -= it
          }
          for (it <- inactive if it.regOp.regNr >= 0 && physReg(it) == reg) {
            // TODO: only if next usage is before end of current?
            splitInterval(it, it.nextUsagePos(position))
          }
          true
        }

        intervals.get(reg) match {
          case Some(fixedIntervalForReg) =>
            fixedIntervalForReg.nextIntersectionWith(current, position) match {
              case Some(intersectionPos) => splitInterval(current, intersectionPos)
              case None =>
            }
          case None =>
        }

        regAssigned
      }

      // Check for `active` intervals that have become `inactive` or have expired.
      // Make a copy of `active` first because we are modifying it.
      for (it <- active.toArray if !it.contains(position)) {
        active -= it
        if (it.end >= position) inactive += it
      }

      // Check for `inactive` intervals that have become `active` or have expired.
      for (it <- inactive.toArray if it.contains(position) || it.end < position) {
        inactive -= it
        if (it.end >= position) active += it
      }

      if (current.regOp.regNr < 0 || tryAllocateWithoutSpilling() || allocateWithSpilling())
        active += current
    }

    result.toMap
  }

  /** Inserts moves between adjacent split intervals and deconstructs the phi functions. */
  private def resolveAndDeconstructSSA(mapping: Map[LivenessInterval, Operand]): Unit = {
    // Insert moves at block boundaries (also deconstructs phi functions).
    for ((pred, succ) <- function.controlFlowEdges) {
      val insertionPos =
        if (succ.predecessors.flatten.length == 1)
          blockStartPos(succ) + 1
        else {
          assert(pred.successors.size == 1, s"Critical edge from ${pred.nr} (successors " +
            pred.successors.map(_.nr).mkString(", ") + s") to ${succ.nr} (predecessors " +
            succ.predecessors.flatten.map(_.nr).mkString(", ") + ")")
          // insert before the control flow instructions
          blockControlFlowStartPos(pred) - 1
        }

      for (it <- intervals.values if it.regOp.regNr >= 0 && it.andChildren.exists(_.contains(blockStartPos(succ)))) {
        val moveFrom =
          if (it.andChildren.map(_.start).min == blockStartPos(succ)) {
            // interval is defined by a phi function
            succ.phis.find(_.dest.regNr == it.regOp.regNr) match {
              case Some(phi) => phi.srcs(succ.predecessors.indexOf(Some(pred))) match {
                case c: ConstOperand => c
                case r: RegisterOperand => mapping(interval(r).childAt(blockEndPos(pred)).get)
                case _ => ???
              }
              case None =>
                assert(false, s"Cannot find phi function defining interval ${it.regOp.regNr}")
                ???
            }
          } else mapping(it.childAtExcl(blockEndPos(pred)).get)

        val moveTo = mapping(it.childAt(blockStartPos(succ)).get)

        if (moveTo != moveFrom) insertedInstrs(insertionPos) += (moveTo -> moveFrom)
      }
    }

    // Prevent the AssemblerFileGenerator from outputting the Phis
    for (b <- function.basicBlocks) b.phis.clear()

    // Insert move instructions for intervals that have been split inside a block.
    for (interval <- intervals.values if interval.regOp.regNr >= 0) {
      for (Seq(pred, succ) <- interval.andChildren.sortBy(_.start).sliding(2)) {
        if (pred.end == succ.start && !blockStartPos.valuesIterator.contains(pred.end)) {
          assert(pred.end % 2 == 1)
          val (succOp, predOp) = (mapping(succ), mapping(pred))
          if (succOp != predOp) insertedInstrs(pred.end) += ((mapping(succ), mapping(pred)))
        }
      }
    }
  }

  private def replaceRegisterOperands(instr: Instruction, mapping: RegisterOperand => Operand) = {
    instr.operands.zipWithIndex.foreach {
      case (r: RegisterOperand, idx) if r.regNr >= 0 =>
        instr.operands(idx) = mapping(r)
      case (a: AddressOperand, idx) =>
        instr.operands(idx) = a.copy(
          base = a.base.map(mapping(_).asInstanceOf[RegisterOperand]),
          offset = a.offset.map(mapping(_).asInstanceOf[RegisterOperand])
        )
      case _ =>
    }
  }

  /** Replaces virtual registers by their assigned operand in all instructions. */
  private def applyMapping(mapping: Map[LivenessInterval, Operand]) = {
    def replaceRegOp(pos: Int)(regOp: RegisterOperand): Operand = {
      if (regOp.regNr < 0) regOp
      else mapping(interval(regOp).childAt(pos).get)
    }

    for (b <- function.basicBlocks) {
      for ((instr, pos) <- instrsWithPos(b)) {
        replaceRegisterOperands(instr, replaceRegOp(pos))
      }
    }
  }

  /** Inserts the additional move instructions generated by the resolve phase. */
  private def insertInstrs() = {
    for (b <- function.basicBlocks) {
      for ((instrList, posRange) <- Seq(
        (b.instructions, blockStartPos(b) + 1 until blockControlFlowStartPos(b) + 1 by 2),
        (b.controlFlowInstructions, blockControlFlowStartPos(b) + 1 until blockEndPos(b) + 1 by 2))) {

        var numInsertedInstrs = 0
        for ((pos, idx) <- posRange.zipWithIndex) {
          val nonIdentityMoves = insertedInstrs(pos).filter(t => t._1 != t._2)
          assert(nonIdentityMoves.toMap.size == nonIdentityMoves.size) // no duplicate destination operands
          val phiInstrs = new PhiCodeGenerator(nonIdentityMoves.toMap, tempRegNo1, tempRegNo2).getInstructions()

          instrList.insertAll(idx + numInsertedInstrs, phiInstrs)
          numInsertedInstrs += phiInstrs.length
        }
      }
    }
  }

  /** Saves and restores callee-saved registers that have actually been used in the function. */
  private def saveCalleeSaveRegs(usedRegs: Set[Int]) = {
    CalleeSaveRegisters.filter(r => usedRegs.contains(r)).foreach { reg =>
      val regOp = RegisterOperand(reg, 8)
      val arOp = activationRecordOperand(8)

      val saveInstr = Mov(regOp, arOp).withComment("Save callee-save register")
      val restoreInstr = Mov(arOp, regOp).withComment("Restore callee-save register")

      function.prologue.instructions.prepend(saveInstr)
      function.epilogue.instructions.append(restoreInstr)
    }
  }

  private def convertArOperands() = {
    val rsp = RegisterOperand(RSP, 8)
    activationRecordSize = CodeGenerator.align(activationRecordSize)

    for (b <- function.basicBlocks) {
      for (instr <- b.allInstructions) {
        for ((op, idx) <- instr.operands.zipWithIndex) {
          op match {
            case a: ActivationRecordOperand =>
              instr.operands(idx) = AddressOperand(
                base = Some(rsp),
                displacement = a.offset + activationRecordSize + instr.stackPointerDisplacement,
                sizeBytes = a.sizeBytes)
            case _ =>
          }
        }
      }
    }

    function.activationRecordSize = activationRecordSize
    if (activationRecordSize > 0) {
      val arSizeOp = ConstOperand(activationRecordSize, firm.Mode.getIs.getSizeBytes)
      function.prologue.instructions.prepend(Sub(arSizeOp, rsp).withComment("Build stack frame"))
      function.epilogue.instructions += Add(arSizeOp, rsp).withComment("Restore stack frame")
    }
  }

  def allocateRegs(): Unit = {
    buildLivenessIntervals()
    val mapping = linearScan()
    resolveAndDeconstructSSA(mapping)
    applyMapping(mapping)
    insertInstrs()
    saveCalleeSaveRegs(
      // used physical registers + registers mapped to liveness intervals (of virtual registers)
      (intervals.keys.filter(_ < 0) ++
      mapping.values.flatMap { case r: RegisterOperand => Some(r.regNr); case _ => None }).toSet)
    convertArOperands()
  }
}
