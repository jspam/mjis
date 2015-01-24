package mjis

import java.io.BufferedWriter

import firm.Mode
import mjis.asm.OperandSpec._
import mjis.asm._
import mjis.asm.AMD64Registers._
import mjis.util.MapExtensions._

import scala.collection.mutable

class RegisterAllocator(input: AsmProgram) extends Phase[AsmProgram] {
  override def getResult(): AsmProgram = {
    input.functions.foreach(new FunctionRegisterAllocator(_).allocateRegs())
    input
  }

  override def dumpResult(writer: BufferedWriter): Unit = {}

  override def findings = List[Finding]()
}

class FunctionRegisterAllocator(function: AsmFunction) {
  private case class LivenessInterval(reg: RegisterOperand, start: Int, end: Int) extends Ordered[LivenessInterval] {
    override def compare(that: LivenessInterval): Int = this.start.compare(that.start)
    def intersects(that: LivenessInterval): Boolean = !(that.end <= this.start || this.end <= that.start)
  }

  // activationRecordSize takes into account only the values written to the AR by the function itself,
  // but not the parameters and return address.
  var activationRecordSize: Int = 0
  val activationRecord = mutable.HashMap[Int, Int]()

  private def activationRecordOperand(sizeBytes: Int): ActivationRecordOperand = {
    activationRecordSize += sizeBytes
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

  private class LivenessIntervalMap {
    // R12-R15 are reserved for spills/reloads at the moment
    private val internalRegisters = Seq(RAX, RBX, RCX, RDX, RDI, RSI, RBP, R8, R9, R10, R11 /*, R12, R13, R14, R15*/)

    private val livenessIntervalsByRegister = mutable.Map[Int, mutable.SortedSet[LivenessInterval]]().
      withPersistentDefault(_ => mutable.SortedSet[LivenessInterval]())

    val regUsageCount = mutable.Map[Int, Int]().withPersistentDefault(_ => 0)

    def addInterval(interval: LivenessInterval): Unit =
      addInterval(interval, interval.reg.regNr)

    def addInterval(interval: LivenessInterval, regNr: Int): Unit =
      livenessIntervalsByRegister(regNr) += interval

    def removeInterval(interval: LivenessInterval): Unit =
      removeInterval(interval, interval.reg.regNr)

    def removeInterval(interval: LivenessInterval, regNr: Int): Unit =
      livenessIntervalsByRegister(regNr) -= interval

    def getIntervalForPos(regNr: Int, instrPos: Int): Option[LivenessInterval] =
      livenessIntervalsByRegister(regNr).
        find(interval => interval.start <= instrPos && interval.end >= instrPos)

    def getIntervalsForPos(instrPos: Int): Map[Int, LivenessInterval] =
      internalRegisters.flatMap(reg => getIntervalForPos(reg, instrPos) match {
        case Some(interval) => Some(reg -> interval)
        case None => None
      }).toMap

    def allIntervals(): Set[LivenessInterval] = livenessIntervalsByRegister.values.foldLeft[Set[LivenessInterval]](Set())(_ ++ _)

    def isAlive(regNr: Int): Boolean = livenessIntervalsByRegister(regNr).nonEmpty

    def searchFreeRegister(interval: LivenessInterval): Option[Int] = {
      val freeRegisters = internalRegisters.filter(livenessIntervalsByRegister(_).forall(!_.intersects(interval)))
      // Prefer registers whose liveness ends at the start of "interval" -- this generates
      // Mov instructions with the same source and target register
      if (freeRegisters.isEmpty) None
      else Some(freeRegisters.find(livenessIntervalsByRegister(_).exists(_.end == interval.start)).getOrElse(freeRegisters.head))
    }
  }

  def allocateRegs() = {
    // Insert Mov instructions for Phi functions
    for ((pred, succ) <- function.controlFlowEdges) {
      val parallelMoves: Map[Operand, Operand] = succ.phis.map { phi =>
        phi.dest -> phi.srcs(succ.predecessors.indexOf(Some(pred)))
      }.toMap
      val phiInstrs = new PhiCodeGenerator(parallelMoves, 0, 0).getInstructions()

      if (succ.predecessors.flatten.length == 1) {
        succ.instructions.insertAll(0, phiInstrs)
      } else {
        assert(pred.successors.size == 1, s"Critical edge from ${pred.nr} (successors " +
          pred.successors.map(_.nr).mkString(", ") + s") to ${succ.nr} (predecessors " +
          succ.predecessors.flatten.map(_.nr).mkString(", ") + ")")
        pred.instructions ++= phiInstrs
      }
    }

    // Prevent the AssemblerFileGenerator from outputting the Phis
    function.basicBlocks.foreach(_.phis.clear())

    // Save ActivationRecordOperands for later conversion to "real" operands
    val activationRecordOperands = mutable.ListBuffer[(Instruction, /* operand index */ Int)]()
    val intervals = computeLivenessIntervals()
    val intervalMapping = mutable.Map[LivenessInterval, Operand]()

    // smaller return value => more likely to be spilled
    def spillOrder(interval: LivenessInterval): Int = {
      intervals.regUsageCount(interval.reg.regNr)
    }

    def assignRealRegister(interval: LivenessInterval) = {
      intervals.searchFreeRegister(interval) match {
        case Some(freeReg) =>
          intervalMapping(interval) = interval.reg.copy(regNr = freeReg)
          intervals.addInterval(interval, freeReg)
        case None =>
          // Select live interval to spill among the current interval and the currently active
          // intervals for temporaries
          val (freeReg, intervalToSpill) = intervals.getIntervalsForPos(interval.start).toSeq.
            filter(_._2.reg.regNr >= 0).sortBy(t => spillOrder(t._2)).head

          // Compare with the current interval as well
          if (spillOrder(intervalToSpill) < spillOrder(interval)) {
            intervals.removeInterval(intervalToSpill, freeReg)
            intervals.addInterval(interval, freeReg)

            intervalMapping(intervalToSpill) = activationRecordOperand(intervalToSpill.reg)
            intervalMapping(interval) = interval.reg.copy(regNr = freeReg)
          } else {
            intervalMapping(interval) = activationRecordOperand(interval.reg)
          }
      }
    }

    intervals.allIntervals().filter(_.reg.regNr >= 0).toSeq.sorted.foreach(assignRealRegister)

    var instrPos = 0

    function.basicBlocks.foreach(block => Seq(block.instructions, block.controlFlowInstructions).foreach { instrList =>
      var i = 0
      while (i < instrList.length) {
        val instr = instrList(i)
        val instrsBefore = mutable.ArrayBuffer[Instruction]()
        val instrsAfter = mutable.ArrayBuffer[Instruction]()

        def saveCallerSaveRegs(exclude: Set[RegisterOperand]) = {
          val excludeRegNrs = exclude.map(_.regNr)
          val regsToSave = intervals.getIntervalsForPos(instrPos).
            filter { case (regNr, _) => !excludeRegNrs.contains(regNr) }.
            map { case (regNr, interval) => interval.reg.copy(regNr = regNr) }

          regsToSave.foreach { regOp =>
            instrsBefore += Mov(regOp, activationRecordOperand(regOp)).withComment("Save caller-save register")
          }

          regsToSave.foreach { regOp =>
            instrsAfter += Mov(activationRecordOperand(regOp), regOp).withComment("Restore caller-save register")
          }
        }

        instr match {
          case Call(_, registerParams) => saveCallerSaveRegs(registerParams.toSet)
          case CallWithReturn(_, _, registerParams) => saveCallerSaveRegs(registerParams.toSet ++ Seq(RegisterOperand(RAX, 8)))
          case _ =>
            for (((operand, idx), spec) <- instr.operands.zipWithIndex.zip(instr.operandSpecs)) {
              def hasMemoryReference = instr.operands.exists{
                case _: ActivationRecordOperand | _: AddressOperand => true
                case _ => false
              }

              // Returns a replacement operand for a register operand and inserts spill/reload instructions
              // if necessary. spillRegNr is used as a temporary registers for spilled/reloaded values.
              def replaceRegisterOperand(oldOp: RegisterOperand, spec: OperandSpec, spillRegNr: Int): Operand = {
                if (oldOp.regNr < 0)
                  oldOp
                else {
                  val newOp = intervalMapping(intervals.getIntervalForPos(oldOp.regNr, instrPos).get)
                  newOp match {
                    case a: ActivationRecordOperand if
                      !spec.contains(MEMORY) || hasMemoryReference ||
                        // HACK: Cannot do a movslq with a memory operand as target
                        (idx == 1 && instr.isInstanceOf[Mov] && !instr.asInstanceOf[Mov].src.isInstanceOf[ConstOperand] &&
                          instr.asInstanceOf[Mov].src.sizeBytes < instr.asInstanceOf[Mov].dest.sizeBytes) =>

                      val regOp = RegisterOperand(spillRegNr, a.sizeBytes)
                      if (spec.contains(READ)) instrsBefore += Mov(a, regOp).withComment(s"reload for $a")
                      if (spec.contains(WRITE)) instrsAfter += Mov(regOp, a).withComment(s"spill for $a")
                      regOp
                    case _ => newOp
                  }
                }
              }

              // In the worst case, we need four temporary register operands:
              // mov x(%REG1, %REG2), y(%REG3, %REG4) where REG1-4 have been spilled
              // R12-R15 are currently used for that purpose (hard-coded)
              operand match {
                case r: RegisterOperand =>
                  val newOp = replaceRegisterOperand(r, spec, if (idx == 0) R12 else R14)
                  instr.operands(idx) = newOp
                  instr.comment += s" - $operand => $newOp"
                case a : AddressOperand =>
                  var newOp = a
                  newOp.base match {
                    case Some(r: RegisterOperand) => newOp = newOp.copy(base = Some(replaceRegisterOperand(r, OperandSpec.READ, if (idx == 0) R12 else R14)))
                    case _ =>
                  }
                  newOp.offset match {
                    case Some(r: RegisterOperand) => newOp = newOp.copy(offset = Some(replaceRegisterOperand(r, OperandSpec.READ, if (idx == 0) R13 else R15)))
                    case _ =>
                  }
                  instr.operands(idx) = newOp
                  instr.comment += s" - $operand => $newOp"
                case _ =>
              }
            }
        }

        // Add all ActivationRecordOperands to the list of AR operands
        (instrsBefore ++ Seq(instr) ++ instrsAfter).foreach(instruction =>
          instruction.operands.zipWithIndex.foreach {
            case (_: ActivationRecordOperand, idx) => activationRecordOperands += ((instruction, idx))
            case _ =>
          }
        )

        instrsBefore.foreach(newInstr => instrList.insert(i, newInstr.withStackPointerDisplacement(instr.stackPointerDisplacement)))
        i += instrsBefore.length

        instrsAfter.foreach(newInstr => instrList.insert(i+1, newInstr.withStackPointerDisplacement(instr.stackPointerDisplacement)))
        i += instrsAfter.length

        i += 1
        instrPos += 1
      }
    })

    // Save callee-save registers
    CalleeSaveRegisters.filter(intervals.isAlive).foreach { reg =>
      val regOp = RegisterOperand(reg, 8)
      val arOp = activationRecordOperand(8)

      val saveInstr = Mov(regOp, arOp).withComment("Save callee-save register")
      activationRecordOperands += ((saveInstr, 1))
      val restoreInstr = Mov(arOp, regOp).withComment("Restore callee-save register")
      activationRecordOperands += ((restoreInstr, 0))

      function.prologue.instructions.prepend(saveInstr)
      function.epilogue.instructions.append(restoreInstr)
    }

    val rsp = RegisterOperand(RSP, 8)

    // Now that the AR size is known, convert ActivationRecordOperands
    for ((instr, idx) <- activationRecordOperands) {
      val arOperand = instr.operands(idx).asInstanceOf[ActivationRecordOperand]
      instr.operands(idx) = AddressOperand(
        base = Some(rsp),
        displacement = arOperand.offset + activationRecordSize + instr.stackPointerDisplacement,
        sizeBytes = arOperand.sizeBytes)
    }

    function.activationRecordSize = activationRecordSize
    if (activationRecordSize > 0) {
      val arSizeOp = ConstOperand(activationRecordSize, Mode.getIs.getSizeBytes)
      function.prologue.instructions.prepend(Sub(arSizeOp, rsp).withComment("Build stack frame"))
      function.epilogue.instructions += Add(arSizeOp, rsp).withComment("Restore stack frame")
    }
  }

  /* Returns the instrPos of the first instruction of each block. */
  private def computeBlockInstrPos(): Map[AsmBasicBlock, Int] = {
    val instrPositions = function.basicBlocks
      .map(block => block.instructions.length + block.controlFlowInstructions.length)
      .scan(0)(_ + _)
    function.basicBlocks.zip(instrPositions).toMap
  }

  private def computeLivenessIntervals(): LivenessIntervalMap = {
    val result = new LivenessIntervalMap()
    val currentIntervalForReg = mutable.Map[Int, LivenessInterval]()

    val blockInstrPos = computeBlockInstrPos()
    var instrPos = 0

    def handleJmp(targetBlock: AsmBasicBlock) = {
      val targetInstrPos = blockInstrPos(targetBlock)
      // Only back jumps will need to be handled specially.
      if (targetInstrPos < instrPos)
        // Extend currently active liveness intervals until the Jmp instruction;
        // ignore temporary registers
        for ((reg, interval) <- currentIntervalForReg) {
          if (reg > 0 && interval.end >= targetInstrPos)
            currentIntervalForReg(reg) = interval.copy(end = instrPos)
        }
    }

    def readRegister(reg: RegisterOperand) = {
      result.regUsageCount(reg.regNr) += 1
      currentIntervalForReg(reg.regNr) = LivenessInterval(reg,
        currentIntervalForReg.get(reg.regNr) match {
          case Some(interval) => interval.start
          case None =>
            if (ParamRegisters.contains(reg.regNr)) {
              // parameters passed in registers
              -1
            } else instrPos // can happen with unreachable code
        }, instrPos)
    }

    def writeRegister(reg: RegisterOperand) = {
      result.regUsageCount(reg.regNr) += 1
      currentIntervalForReg.get(reg.regNr) match {
        case Some(interval) =>
          if (reg.regNr <= 0) {
            // The temporary register (0) and internal registers are only alive within a basic block.
            // We can therefore easily assign multiple liveness intervals to them.
            result.addInterval(interval)
            currentIntervalForReg(reg.regNr) = LivenessInterval(reg, instrPos, instrPos)
          } else {
            // Other registers get one long interval at the moment.
            currentIntervalForReg(reg.regNr) = interval.copy(end = instrPos)
          }
        case None =>
          currentIntervalForReg(reg.regNr) = LivenessInterval(reg, instrPos, instrPos)
      }
    }

    function.basicBlocks.foreach(block => (block.instructions ++ block.controlFlowInstructions).foreach {
      case Jmp(targetBlockOp) =>
        handleJmp(targetBlockOp.basicBlock)
        instrPos += 1
      case JmpConditional(targetBlockOp, _, _) =>
        handleJmp(targetBlockOp.basicBlock)
        instrPos += 1
      case instr =>
        for ((operand, spec) <- instr.operands zip instr.operandSpecs) {
          operand match {
            case r : RegisterOperand if r.regNr != RSP =>
              /* if spec contains both READ and WRITE, we must continue the existing liveness interval
               * (because the register stays the same), so we just ignore the WRITE */
              if (spec.contains(READ)) readRegister(r)
              else if (spec.contains(WRITE)) writeRegister(r)
            case op @ AddressOperand(base, offset, _, _, _) =>
              Seq(base, offset).foreach {
                case Some(r: RegisterOperand) if r.regNr != RSP =>
                  readRegister(r)
                case _ =>
              }
            case _ =>
          }
        }

        instrPos += 1
    })

    currentIntervalForReg.values.foreach(result.addInterval)
    result
  }
}
