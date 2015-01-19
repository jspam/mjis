package mjis

import java.io._

import firm.{Program, Backend, Util}
import mjis.asm.AMD64Registers._
import mjis.asm._

import System.{lineSeparator => n}

import scala.io.Source
import scala.collection.JavaConversions._

object AssemblerFileGenerator {
  val AsmFileName = "a.s"
}

abstract class AssemblerFileGenerator extends Phase[Unit] {
  def writeCode(): Writer

  override def getResult() = {
    val fw = writeCode()

    // concatenate our implementation of System_out_println to the assembly code
    val stdlib = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("System_out_println_64.s"))
    stdlib.foreach(fw.write(_))
    fw.flush()
  }

  val findings = List()

  def dumpResult(writer: BufferedWriter) = {
    Source.fromFile(AssemblerFileGenerator.AsmFileName).foreach(writer.write(_))
  }
}

class MjisAssemblerFileGenerator(input: AsmProgram) extends AssemblerFileGenerator {
  private def instrToString(instr: Instruction): String = {
    def opToString(op: Operand): String = op match {
      case r: RegisterOperand if Registers.contains(r.regNr) => "%" + Registers(r.regNr).subregs(r.sizeBytes)
      case r: RegisterOperand => s"%REG${r.regNr}{${r.sizeBytes}}"
      case r: AddressOperand =>
        val params = Seq[Option[String]](
          r.base.map(opToString),
          r.offset.map(opToString),
          if (r.scale != 1) Some(r.scale.toString) else None
        ).flatten
        val displacement = if (r.displacement != 0) r.displacement.toString else ""
        s"$displacement(${params.mkString(",")})"
      case l: LabelOperand => l.name
      case c: ConstOperand => s"$$${c.value}"
      case a: ActivationRecordOperand => s"${a.offset}(%rbp){${a.sizeBytes}}"
    }

    val operandsToPrint = instr.operands.zip(instr.operandSpecs).
      filter { case (_, spec) => !spec.contains(OperandSpec.IMPLICIT) }.
      map(_._1)
    val operandsResult = if (operandsToPrint.isEmpty) "" else " " + operandsToPrint.map(opToString).mkString(", ")
    val instrAndOperands = instr.opcode + instr.suffix + operandsResult
    if (instr.comment.nonEmpty)
      // Align comments
      instrAndOperands + Seq.fill((30 - instrAndOperands.length) max 0)(" ").mkString("") + " # " + instr.comment
    else
      instrAndOperands
  }

  override def writeCode() = {
    val fw = new BufferedWriter(new FileWriter(AssemblerFileGenerator.AsmFileName, /* append */ false))
    fw.write(generateCode())
    fw
  }

  def generateCode(): String = {
    val result = new StringBuilder
    def emit(s: String, indent: Boolean = true) = result.append(if (indent && s.nonEmpty) s"\t$s$n" else s"$s$n")

    emit(".text")
    emit(".p2align 4,,15")

    input.functions.foreach(function => {
      emit("")
      emit(s"${function.name}:", indent = false)

      def emitBlock(block: AsmBasicBlock): Unit = {
        if (block.nr >= 0) {
          emit(s".L${block.nr}: # Block ${block.nr}", indent = false)
        } else {
          emit("# Unnamed block", indent = false)
        }
        block.instructions.foreach(instr => emit(instrToString(instr)))
        block.controlFlowInstructions.foreach(instr => emit(instrToString(instr)))
      }

      emitBlock(function.prologue)
      function.basicBlocks.foreach(emitBlock)
      emitBlock(function.epilogue)
    })

    result.toString()
  }
}

class FirmAssemblerFileGenerator(a: Unit) extends AssemblerFileGenerator {
  def writeCode(): Writer = {
    Program.getGraphs.foreach(_.check())
    Util.lowerSels()
    Backend.createAssembler(AssemblerFileGenerator.AsmFileName, "<input>")
    new BufferedWriter(new FileWriter(AssemblerFileGenerator.AsmFileName, /* append */ true))
  }
}
