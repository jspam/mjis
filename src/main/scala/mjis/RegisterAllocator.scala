package mjis

import java.io.BufferedWriter

import firm.Mode
import mjis.asm.AMD64Registers._
import mjis.asm._

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
  var activationRecordSize: Int = 0
  val activationRecord = mutable.HashMap[Int, Int]()

  private def activationRecordOperand(reg: RegisterOperand): ActivationRecordOperand =
    activationRecord.get(reg.regNr) match {
      case Some(offset) => ActivationRecordOperand(offset, reg.sizeBytes)
      case None =>
        activationRecordSize += reg.sizeBytes
        val offset = -activationRecordSize
        activationRecord(reg.regNr) = offset
        ActivationRecordOperand(offset, reg.sizeBytes)
    }

  def intConstOp(i: Int): Operand = new ConstOperand(i, Mode.getIs.getSizeBytes)

  def allocateRegs() = {
    // Save ActivationRecordOperands for later conversion to "real" operands
    val activationRecordOperands = mutable.ListBuffer[(Instruction, /* operand index */ Int)]()

    function.allBlocks.foreach(block => Seq(block.instructions, block.controlFlowInstructions).foreach { instrList =>
      var i = 0
      while (i < instrList.length) {
        instrList(i) match {
          case _ : Forget | _ : Reserve =>
            instrList.remove(i)
          case instr =>
            var rbxUsed = false
            def getRegister(sizeBytes: Int): RegisterOperand =
              if (!rbxUsed) { rbxUsed = true; RegisterOperand(RBX, sizeBytes) }
              else RegisterOperand(RCX, sizeBytes)

            def hasMemoryReference: Boolean = instr.operands.exists(op =>
              op.isInstanceOf[RegisterOffsetOperand] || op.isInstanceOf[ActivationRecordOperand])
            def reload(oldReg: RegisterOperand, actualRegister: RegisterOperand): Unit = {
              val reloadInstr = Mov(activationRecordOperand(oldReg), actualRegister).
                withComment(s"Reload for internal register ${oldReg.regNr} ${instr.comment}").
                withStackPointerDisplacement(instr.stackPointerDisplacement)
              activationRecordOperands += ((reloadInstr, 0))
              instrList.insert(i, reloadInstr)
              i += 1
            }

            instr.operands.zipWithIndex.zip(instr.operandSpecs).foreach { case((operand, idx), spec) => operand match {
              case c: ConstOperand if !spec.contains(OperandSpec.CONST) =>
                val actualRegister = getRegister(c.sizeBytes)
                instrList.insert(i, Mov(c, actualRegister).withComment(s" - Load constant ${instr.comment}"))
                i += 1
                instr.operands(idx) = actualRegister
              case r@RegisterOffsetOperand(reg, _, _) if reg.regNr >= 0 =>
                val actualRegister = getRegister(reg.sizeBytes)
                reload(reg, actualRegister)
                instr.operands(idx) = r.copy(base = actualRegister)
                // no spill since the register's value itself won't be manipulated
              case r@RegisterOperand(oldRegNr, sizeBytes) if oldRegNr >= 0 =>
                val isRead = spec.contains(OperandSpec.READ)
                val isWrite = spec.contains(OperandSpec.WRITE)

                val canUseArOperand = spec.contains(OperandSpec.MEMORY) && !hasMemoryReference
                if (canUseArOperand) {
                  instr.operands(idx) = activationRecordOperand(r)
                  activationRecordOperands += ((instr, idx))
                  if (isRead) instr.comment += s" - operand $idx reloads internal register $oldRegNr"
                  if (isWrite) instr.comment += s" - operand $idx spills internal register $oldRegNr"
                } else {
                  val actualRegister = getRegister(sizeBytes)
                  instr.operands(idx) = actualRegister
                  if (isRead)
                    reload(r, actualRegister)
                  if (isWrite) {
                    val spillInstr = Mov(actualRegister, activationRecordOperand(r)).
                      withComment(s"Spill for internal register $oldRegNr ${instr.comment}").
                      withStackPointerDisplacement(instr.stackPointerDisplacement)
                    activationRecordOperands += ((spillInstr, 1))
                    instrList.insert(i + 1, spillInstr)
                    i += 1
                  }
                }
              case a@ActivationRecordOperand(_, _) =>
                activationRecordOperands += ((instr, idx))
              case _ =>
            }}

            i += 1
        }
      }
    })

    val rsp = RegisterOperand(RSP, 8)

    // Now that the AR size is known, convert ActivationRecordOperands
    activationRecordOperands.foreach { case (instr, idx) =>
      val arOperand = instr.operands(idx).asInstanceOf[ActivationRecordOperand]
      instr.operands(idx) = RegisterOffsetOperand(rsp,
        arOperand.offset + activationRecordSize + instr.stackPointerDisplacement, arOperand.sizeBytes)
    }

    function.activationRecordSize = activationRecordSize
    if (activationRecordSize > 0) {
      function.prologue.instructions.prepend(Sub(intConstOp(activationRecordSize), rsp).withComment("Build stack frame"))
      function.epilogue.instructions += Add(intConstOp(activationRecordSize), rsp).withComment("Restore stack frame")
    }
  }
}
