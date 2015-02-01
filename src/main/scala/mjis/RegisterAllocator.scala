package mjis

import java.io.BufferedWriter

import mjis.asm.OperandSpec._
import mjis.asm._
import mjis.asm.AMD64Registers._
import mjis.util.MapExtensions._
import mjis.RegisterAllocator._

import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._

object RegisterAllocator {
  def nextEven(i: Int) = i + i%2
  def nextOdd(i: Int) = i + (1 - i%2)
  def prevOdd(i: Int) = i - (1 - i%2)
}

class RegisterAllocator(input: AsmProgram) extends Phase[AsmProgram] {
  override def getResult(): AsmProgram = {
    for (f <- input.functions) new FunctionRegisterAllocator(f,
      Seq(RAX, RBX, RDI, RSI, RDX, RCX, R8, R9, R10, R11, R12, R13, R14, R15, RBP),
      AMD64Registers.CallerSaveRegisters).allocateRegs()
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
  val CallerSaveRegisters: Set[Int] = Set()) {
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

  lazy val tempArOperands: Seq[ActivationRecordOperand] =
    Seq.fill(PhiCodeGenerator.MaxTempRegisters)(activationRecordOperand(8))

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
      case a: AddressOperand => Seq[Option[Operand]](a.base, a.indexAndScale.map(_._1)) foreach {
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
    function.basicBlocks.map(b => (b.instructions.length + 1) * 2).scan(0)(_ + _)
  ).toMap

  private val blockStartPositions = blockStartPos.values.toSet

  private val blocksByStartPos = SortedMap[Int, AsmBasicBlock](blockStartPos.map(t => t._2 -> t._1).toSeq:_*)

  /** Returns the blocks that contain the given range of instruction positions, ordered by start position. */
  private def blocksInPosRange(startPos: Int, endPos: Int) = {
    // The block startPos lies in (if not already contained in the second part of the result) ...
    (blocksByStartPos.to(startPos).lastOption match {
      case Some((bStart, b)) if bStart < startPos && blockEndPos(b) >= startPos => Some(b)
      case _ => None
    }) ++
    // ... plus the blocks whose *start* position is in the specified range ...
    blocksByStartPos.range(startPos, endPos).values
  }

  private val blockEndPos =
    function.basicBlocks.map(b => b -> (blockStartPos(b) + (b.instructions.length + 1) * 2)).toMap

  private val blockEndPositions = blockEndPos.values.toSet

  private def instrsWithPos(b: AsmBasicBlock) =
    b.instructions.zip(Range(blockStartPos(b) + 2, blockEndPos(b), 2))

  private val loopDepth = mutable.Map[AsmBasicBlock, Int]().withDefaultValue(0)


  /** Additional move instructions with parallel move semantics. The keys must be
    * odd numbers or start positions of blocks. */
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
        blocksByStartPos.range(blockStartPos(b), blockEndPos(loopEnd) + 1).values.foreach(loopDepth(_) += 1)
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
    val unhandled = mutable.PriorityQueue[LivenessInterval]()(
      Ordering.by(i => (-i.start, i.regOp.regNr /* for determinism */)))

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

      def splitInterval(it: LivenessInterval, maxSplitPos: Int, appendToUnhandled: Boolean = true): LivenessInterval = {
        assert(maxSplitPos >= position)

        // Splitting may occur only at a block boundary (the resolve phase will insert the
        // appropriate move instructions) or at an odd position (because the move instructions
        // are inserted there).
        val splitPos =
          if (blockStartPositions.contains(maxSplitPos)) maxSplitPos
          else prevOdd(maxSplitPos)
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

        if (freeUntilPos.values.max <= nextEven(position)) {
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
            if (regHint != 0 && freeUntilPos(regHint) > nextEven(position)) regHint
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
            // TODO: only if next alive pos is before end of current?
            splitInterval(it, it.nextAlivePos(position).get)
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

  /** Moves the split positions of intervals so that splits happen at block boundaries and/or outside of loops. */
  private def optimizeSplitPositions(mapping: Map[LivenessInterval, Operand]): Unit = {
    for (it <- intervals.values.filter(_.regOp.regNr >= 0)) {
      for (Seq(it1, it2) <- it.andChildren.sliding(2) if it1.end == it2.start) {
        (mapping(it1), mapping(it2)) match {
          case (_: RegisterOperand, _: ActivationRecordOperand) =>
            // can move spill backwards without problems
            if (it1.usages.size() > 0) {
              // can spill *after* the last usage
              val it1LastUsePos = it1.usages.lastEntry().getKey
              val minSpillPos =
                if (blockStartPositions.contains(it1LastUsePos)) it1LastUsePos
                else nextOdd(it1LastUsePos)

              val loopDepths = blocksInPosRange(minSpillPos, it2.start).map(b => b -> loopDepth(b))
              val spillBlock = loopDepths.minBy(_._2)._1

              val splitPos = blockStartPos(spillBlock) max minSpillPos
              it1.moveSplitPos(it2, splitPos)
            }
          case _ =>
            // TODO: can also move Register -> Register if the successor register is free
        }
      }
    }
  }

  /** Inserts moves between adjacent split intervals and deconstructs the phi functions. */
  private def resolveAndDeconstructSSA(mapping: Map[LivenessInterval, Operand]): Unit = {
    // Insert moves at block boundaries (also deconstructs phi functions).
    for ((pred, succ) <- function.controlFlowEdges) {
      val insertionPos =
        if (succ.predecessors.flatten.length == 1)
          blockStartPos(succ)
        else {
          assert(pred.successors.size == 1, s"Critical edge from ${pred.nr} (successors " +
            pred.successors.map(_.nr).mkString(", ") + s") to ${succ.nr} (predecessors " +
            succ.predecessors.flatten.map(_.nr).mkString(", ") + ")")
          blockEndPos(pred) - 1
        }

      for (it <- intervals.values if it.regOp.regNr >= 0 && it.andChildren.exists(_.contains(blockStartPos(succ)))) {
        val moveFrom =
          if (it.andChildren.map(_.start).min == blockStartPos(succ)) {
            // interval is defined by a phi function
            succ.phis.find(_.dest.regNr == it.regOp.regNr) match {
              case Some(phi) => phi.srcs(succ.predecessors.indexOf(Some(pred))) match {
                case c: ConstOperand => c
                case r: RegisterOperand => mapping(interval(r).childAtExcl(blockEndPos(pred)).get)
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
        if (pred.end == succ.start && !blockStartPositions.contains(pred.end)) {
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
          indexAndScale = a.indexAndScale.map {
            case (index, scale) => (mapping(index).asInstanceOf[RegisterOperand], scale)
          }
        )
      case _ =>
    }
  }

  /** Replaces virtual registers by their assigned operand in all instructions. */
  private def applyMapping(mapping: Map[LivenessInterval, Operand]) = {
    def replaceRegOp(pos: Int)(regOp: RegisterOperand): Operand = {
      if (regOp.regNr < 0) regOp
      else mapping(interval(regOp).childAt(pos).get)
      // childAt(pos) is uniquely defined because intervals may not be split
      // at the position of an instruction.
    }

    for (b <- function.basicBlocks) {
      for ((instr, pos) <- instrsWithPos(b)) {
        replaceRegisterOperands(instr, replaceRegOp(pos))
      }
    }
  }

  /** Inserts the additional move instructions generated by the resolve phase. */
  private def insertInstrs(mapping: Map[LivenessInterval, Operand]) = {
    lazy val occupationMap = {
      val result = new OccupationMap(PhysicalRegisters)

      for (regNr <- PhysicalRegisters) {
        intervals.get(regNr) match {
          case Some(it) => result.addInterval(it, it.regOp.regNr)
          case None =>
        }
      }

      for ((it, operand) <- mapping) {
        operand match {
          case r: RegisterOperand => result.addInterval(it, r.regNr)
          case _ =>
        }
      }

      result
    }

    for (b <- function.basicBlocks) {
      var numInsertedInstrs = 0
      // Instructions for `blockStartPos(b)` are also inserted at index 0, but before those
      // for `blockStartPos(b) + 1`.
      for ((pos, idx) <- (blockStartPos(b), 0) +: (blockStartPos(b) + 1 until blockEndPos(b) + 1 by 2).zipWithIndex
        if insertedInstrs.contains(pos)) {

        val nonIdentityMoves = insertedInstrs(pos).filter(t => t._1 != t._2)
        assert(nonIdentityMoves.toMap.size == nonIdentityMoves.size) // no duplicate destination operands

        val permutation = nonIdentityMoves.toMap
        val phiGen = new PhiCodeGenerator(permutation)

        val instrs =
          if (phiGen.neededTempRegs == 0) phiGen.getInstructions()
          else {
            // Need some temporary registers to perform the Phi move.
            // Look for registers that are currently available.

            val regOccupationsWithoutPhi =
              // For insertion at block boundaries, ignore liveness intervals in other blocks that
              // happen to start/end at the block boundary.
              if (blockStartPositions.contains(pos)) occupationMap.nonEndingOccupationAt(pos)
              else if (blockEndPositions.contains(pos + 1)) occupationMap.nonStartingOccupationAt(pos + 1)
              else occupationMap.nonEndingOccupationAt(pos) // if a liveness interval ends here, it will be part of the permutation
            val occupiedRegNrsWithoutPhi = regOccupationsWithoutPhi.filter(_._2.isDefined).map(_._1)

            // The occupation map only returns the occupation at the current position,
            // which might not take into account all registers needed by the permutation
            // (the control flow edge might start at some other position, far away).
            val phiSourceRegs = permutation.values.filter(_.isInstanceOf[RegisterOperand]).asInstanceOf[Iterable[RegisterOperand]]
            val phiSourceRegNrs = phiSourceRegs.map(_.regNr).toSet
            val phiDestRegs = permutation.keys.filter(_.isInstanceOf[RegisterOperand]).asInstanceOf[Iterable[RegisterOperand]]
            val phiDestRegNrs = phiDestRegs.map(_.regNr).toSet

            val freeRegs = PhysicalRegisters.filter(regNr => !occupiedRegNrsWithoutPhi.contains(regNr)
              && !phiSourceRegNrs.contains(regNr) && !phiDestRegNrs.contains(regNr)).sorted
            phiGen.tempRegNrs = freeRegs.take(phiGen.neededTempRegs)

            if (freeRegs.size >= phiGen.neededTempRegs) phiGen.getInstructions()
            else {
              // Not enough temporary registers. We'll have to spill and reload some registers.
              val numRegsToSpill = phiGen.neededTempRegs - freeRegs.size
              var spillInstrs = mutable.Seq[Instruction]()
              var reloadInstrs = mutable.Seq[Instruction]()

              // Try to spill registers that aren't used in the permutation.
              val (nonPermutationRegNrs, permutationRegNrs) = occupiedRegNrsWithoutPhi.partition(
                regNr => !phiSourceRegNrs.contains(regNr) && !phiDestRegNrs.contains(regNr))

              for (regNr <- nonPermutationRegNrs.take(numRegsToSpill)) {
                val it = regOccupationsWithoutPhi(regNr).get
                val regOp = it.regOp.copy(regNr = regNr)

                // We can use the AR slot reserved for `it`.
                spillInstrs +:= Mov(regOp, activationRecordOperand(it.regOp)).withComment(" - spill for Phi code generation")
                reloadInstrs +:= Mov(activationRecordOperand(it.regOp), regOp).withComment(" - reload for Phi code generation")
                phiGen.tempRegNrs +:= regNr
              }

              // If that's still not enough, spill registers participating in the Phi generation,
              // but then we have to rewrite the Phi map to point to the corresponding AR operands
              // instead of the spilled registers.
              if (nonPermutationRegNrs.size < numRegsToSpill) {
                // The spilling could generate memory/memory moves, so play it safe and free the
                // maximum number of temp registers.
                val regNrsToSpill = permutationRegNrs.take(PhiCodeGenerator.MaxTempRegisters - phiGen.tempRegNrs.size).toSeq

                def arOperand(regNr: Int) = tempArOperands(regNrsToSpill.indexOf(regNr))
                def rewrite(op: Operand): Operand = op match {
                  case r: RegisterOperand if regNrsToSpill.contains(r.regNr) =>
                    arOperand(r.regNr).copy(sizeBytes = op.sizeBytes)
                  case _ => op
                }

                for (regNr <- regNrsToSpill) {
                  val arOp = arOperand(regNr)
                  phiSourceRegs.find(_.regNr == regNr) match {
                    case Some(regOp) => spillInstrs +:= Mov(regOp, arOp.copy(sizeBytes = regOp.sizeBytes)).
                      withComment(" - spill for Phi code generation")
                    case None =>
                  }
                  phiDestRegs.find(_.regNr == regNr) match {
                    case Some(regOp) => reloadInstrs +:= Mov(arOp.copy(sizeBytes = regOp.sizeBytes), regOp).
                      withComment(" - reload for Phi code generation")
                    case None =>
                  }
                }

                val rewrittenPermutation = permutation.map { case (dest, src) => rewrite(dest) -> rewrite(src) }
                val phiGen2 = new PhiCodeGenerator(rewrittenPermutation)
                phiGen2.tempRegNrs = phiGen.tempRegNrs ++ regNrsToSpill
                spillInstrs ++ phiGen2.getInstructions() ++ reloadInstrs
              } else {
                spillInstrs ++ phiGen.getInstructions() ++ reloadInstrs
              }
            }
          }

        b.instructions.insertAll(idx + numInsertedInstrs, instrs)
        numInsertedInstrs += instrs.length
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
      // insert before 'ret'
      function.epilogue.instructions.insert(function.epilogue.instructions.length - 1, restoreInstr)
    }
  }

  private def convertArOperands() = {
    val rsp = RegisterOperand(RSP, 8)
    activationRecordSize = CodeGenerator.align(activationRecordSize)

    for (b <- function.basicBlocks) {
      for (instr <- b.instructions) {
        for ((op, idx) <- instr.operands.zipWithIndex) {
          op match {
            case a: ActivationRecordOperand =>
              instr.operands(idx) = AddressOperand(
                base = Some(rsp),
                offset = a.offset + activationRecordSize + instr.stackPointerOffset,
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
      // insert before 'ret'
      function.epilogue.instructions.insert(function.epilogue.instructions.length - 1, Add(arSizeOp, rsp).withComment("Restore stack frame"))
    }
  }

  def allocateRegs(): Unit = {
    buildLivenessIntervals()
    val mapping = linearScan()
    optimizeSplitPositions(mapping)
    resolveAndDeconstructSSA(mapping)
    applyMapping(mapping)
    insertInstrs(mapping)
    saveCalleeSaveRegs(
      // used physical registers + registers mapped to liveness intervals (of virtual registers)
      (intervals.keys.filter(_ < 0) ++
      mapping.values.flatMap { case r: RegisterOperand => Some(r.regNr); case _ => None }).toSet)
    convertArOperands()
  }
}
