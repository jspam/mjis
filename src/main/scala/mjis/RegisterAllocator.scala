package mjis

import java.io.BufferedWriter

import firm.Mode
import firm.nodes.Node
import mjis.asm.AMD64Registers._
import mjis.asm._

import mjis.util.MapExtensions._
import mjis.opt.FirmExtensions._

import scala.collection.mutable
import scala.language.implicitConversions

class RegisterAllocator(input: AsmProgram) extends Phase[AsmProgram] {
  override def getResult: AsmProgram = {
    input.functions.foreach(new FunctionRegisterAllocator(_).allocateRegs())
    input
  }

  override def dumpResult(writer: BufferedWriter): Unit = {}

  override def findings = List[Finding]()
}

class FunctionRegisterAllocator(function: AsmFunction) {
  // activationRecordSize takes into account only the values written to the AR by the function itself,
  // but not the parameters and return address.
  var activationRecordSize: Int = 0
  val activationRecord = mutable.HashMap[Int, Int]().withPersistentDefault(regNr => {
    activationRecordSize += 8 // TODO: align(register(regNr).sizeBytes) -- when instructions use matching registers
    -activationRecordSize
  })

  private def activationRecordOperand(regNo: Int) = ActivationRecordOperand(activationRecord(regNo), 8 /* TODO */)

  def intConstOp(i: Int): Operand = new ConstOperand(i, Mode.getIs.getSizeBytes)
  implicit def regOp(regNr: Int): RegisterOperand = new RegisterOperand(regNr, Registers(regNr).sizeBytes)

  def allocateRegs() = {
    // Save ActivationRecordOperands for later conversion to "real" operands
    val activationRecordOperands = mutable.ListBuffer[(Instruction, /* operand index */ Int)]()

    function.allBlocks.foreach(block => Seq(block.instructions, block.controlFlowInstructions).foreach { instrList =>
      var i = 0
      while (i < instrList.length) {
        instrList(i) match {
          case _ : Forget =>
            instrList.remove(i)
          case instr =>
            var rbpUsed = false
            var hasMemoryReference = false
            def getRegister: RegisterOperand = if (!rbpUsed) { rbpUsed = true; RBP } else RBX

            instr.operands.zipWithIndex.zip(instr.operandSpecs).foreach { case((operand, idx), spec) => operand match {
              case c: ConstOperand if !spec.contains(OperandSpec.CONST) =>
                val actualRegister = getRegister
                instrList.insert(i, Movq(c, actualRegister).withComment(s" - Load constant ${instr.comment}"))
                i += 1
                instr.operands(idx) = actualRegister
              case r@RegisterOffsetOperand(regNo, _, _) =>
                // TODO: At the moment only RSP relative addressing is used
                assert(regNo < 0)
                hasMemoryReference = true
              case r@RegisterOperand(oldRegNo, sizeBytes) if oldRegNo >= 0 =>
                val isRead = spec.contains(OperandSpec.READ)
                val isWrite = spec.contains(OperandSpec.WRITE)

                val canUseArOperand = spec.contains(OperandSpec.MEMORY) && !hasMemoryReference
                if (canUseArOperand) {
                  instr.operands(idx) = activationRecordOperand(oldRegNo)
                  activationRecordOperands += ((instr, idx))
                  hasMemoryReference = true
                  if (isRead) instr.comment += s" - operand $idx reloads internal register $oldRegNo"
                  if (isWrite) instr.comment += s" - operand $idx spills internal register $oldRegNo"
                } else {
                  val actualRegister = getRegister
                  instr.operands(idx) = actualRegister
                  if (isRead) {
                    val reloadInstr = Movq(activationRecordOperand(r.regNo), actualRegister).
                      withComment(s"Reload for internal register $oldRegNo ${instr.comment}")
                    activationRecordOperands += ((reloadInstr, 0))
                    instrList.insert(i, reloadInstr)
                    i += 1
                  }
                  if (isWrite) {
                    val spillInstr = Movq(actualRegister, activationRecordOperand(r.regNo)).
                      withComment(s"Spill for internal register $oldRegNo ${instr.comment}")
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

    // Now that the AR size is known, convert ActivationRecordOperands
    activationRecordOperands.foreach { case (instr, idx) =>
      val arOperand = instr.operands(idx).asInstanceOf[ActivationRecordOperand]
      instr.operands(idx) = RegisterOffsetOperand(RSP,
        arOperand.offset + activationRecordSize + instr.stackPointerDisplacement, arOperand.sizeBytes)
    }

    function.activationRecordSize = activationRecordSize
    if (activationRecordSize > 0) {
      function.prologue.instructions.prepend(Subq(intConstOp(activationRecordSize), RSP).withComment("Build stack frame"))
      function.epilogue.instructions += Addq(intConstOp(activationRecordSize), RSP).withComment("Restore stack frame")
    }
  }
}
