package mjis

import java.io._

import firm.bindings.binding_irnode.ir_opcode
import firm._
import firm.nodes._
import mjis.asm._
import mjis.CodeGenerator._
import mjis.opt.FirmExtractors.ProjExtr
import mjis.util.MapExtensions._
import mjis.asm.AMD64Registers._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.collection.JavaConversions._
import scala.language.implicitConversions

import System.{lineSeparator => n}

object CodeGenerator {
  def align(x: Int, alignment: Int = 4) = if (x % alignment == 0) x else x + (alignment - x % alignment)
}

class CodeGenerator(a: Unit) extends Phase[String] {
  val useFirmBackend = false
  def findings = List()
  def dumpResult(a: BufferedWriter) = {
    Source.fromFile("a.s").foreach(a.write(_))
  }

  val variableAssigned = Set[ir_opcode] (
    ir_opcode.iro_Add, ir_opcode.iro_And, ir_opcode.iro_Div, ir_opcode.iro_Minus, ir_opcode.iro_Mod, ir_opcode.iro_Mul,
    ir_opcode.iro_Mulh, ir_opcode.iro_Not, ir_opcode.iro_Or, ir_opcode.iro_Shl, ir_opcode.iro_Shr, ir_opcode.iro_Shrs,
    ir_opcode.iro_Sub, ir_opcode.iro_Load, ir_opcode.iro_Member, ir_opcode.iro_Sel, ir_opcode.iro_Call, ir_opcode.iro_Conv
  )

  def instrToString(instr: Instruction, activationRecordSize: Int): String = {
    val operandsResult = if (instr.operands.isEmpty) "" else " " + instr.operands.map {
      case r: RegisterOperand => "%" + (if (Registers.contains(r.regNo)) Registers(r.regNo).name else "REG" + r.regNo)
      case r: RegisterOffsetOperand =>
        // Convert rbp relative addressing to rsp relative addressing
        // now that the AR size is known
        if (r.regNr == RBP) {
          r.regNr = RSP
          r.offset += activationRecordSize + instr.stackPointerDisplacement
        }
        s"${r.offset}(%${Registers(r.regNr).name})"
      case l: LabelOperand => l.name
      case c: ConstOperand => s"$$${c.value}"
    }.mkString(", ")
    val instrAndOperands = instr.opcode + operandsResult
    if (instr.comment.nonEmpty)
      // Align comments
      instrAndOperands + Seq.fill((30 - instrAndOperands.length) max 0)(" ").mkString("") + " # " + instr.comment
    else
      instrAndOperands
  }

  def getResult(): String = {
    Program.getGraphs.foreach(_.check())

    val asm = "a.s"
    var fw: BufferedWriter = null

    val result = new StringBuilder
    def emit(s: String, indent: Boolean = true) = result.append(if (indent && s.nonEmpty) s"\t$s$n" else s"$s$n")

    if (useFirmBackend) {
      Util.lowerSels()
      Backend.createAssembler(asm, "<input>")
      fw = new BufferedWriter(new FileWriter(asm, /* append */ true))
    } else {
      emit(".text")
      emit(".p2align 4,,15")

      Program.getGraphs.foreach(g => {
        val generator = new MethodCodeGenerator(g)
        val blocks = generator.getResult()

        val name = g.getEntity.getLdName
        emit("")
        emit(s"$name:", indent = false)

        generator.prologue.foreach(instr => emit(instrToString(instr, generator.activationRecordSize)))

        emit(s".L${g.getStartBlock.getNr}: # Start Block ${g.getStartBlock.getNr}", indent = false)
        blocks(g.getStartBlock).foreach(instr => emit(instrToString(instr, generator.activationRecordSize)))
        blocks.foreach {
          case (block, blockInstrs) => if (block != null && block != g.getStartBlock && block != g.getEndBlock) {
            emit(s".L${block.getNr}: # Basic Block ${block.getNr}", indent = false)
            blockInstrs.foreach(instr => emit(instrToString(instr, generator.activationRecordSize)))
          }
        }
        emit(s".L${g.getEndBlock.getNr}: # End Block ${g.getEndBlock.getNr}", indent = false)
        blocks(g.getEndBlock).foreach(instr => emit(instrToString(instr, generator.activationRecordSize)))

        generator.epilogue.foreach(instr => emit(instrToString(instr, generator.activationRecordSize)))
      })

      fw = new BufferedWriter(new FileWriter(asm, /* append */ false))
      fw.write(result.toString())
    }

    // concatenate our implementation of System_out_println to the assembly code
    val stdlib = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("System_out_println_64.s"))
    stdlib.foreach(fw.write(_))
    fw.flush()
    val gcc = Runtime.getRuntime.exec(s"gcc -m64 $asm")
    val stderr = new BufferedReader(new InputStreamReader(gcc.getErrorStream))
    gcc.waitFor()
    val stream = Stream.continually(stderr.readLine()).takeWhile(_ != null)
    if (gcc.exitValue() != 0 || stream.nonEmpty)
      System.err.println(s"GCC returned exit status ${gcc.exitValue}\n${stream.mkString("\n")}")
    result.toString()
  }

  def intConstOp(i: Int): Operand = new ConstOperand(i, Mode.getIs.getSizeBytes)
  implicit def regOp(regNr: Int): RegisterOperand = new RegisterOperand(regNr, Registers(regNr).sizeBytes)

  class MethodCodeGenerator(g: Graph) {
    // activationRecordSize takes into account only the values written to the AR by the function itself,
    // but not the parameters and return address.
    var activationRecordSize: Int = 0
    val activationRecord = mutable.HashMap[Node, Int]().withPersistentDefault(node => {
      activationRecordSize += 8 // TODO: align(node.getMode.getSizeBytes) -- when instructions use matching registers
      -activationRecordSize
    })

    val prologue = ListBuffer[Instruction]()
    val epilogue = ListBuffer[Instruction]()

    def getResult(): mutable.Map[Block, ListBuffer[Instruction]] = {
      val linearizedNodes = mutable.HashMap[Block, ListBuffer[Node]]().withPersistentDefault(_ => new ListBuffer[Node]())
      val result = mutable.HashMap[Block, ListBuffer[Instruction]]().withPersistentDefault(_ => new ListBuffer[Instruction]())
      g.walkTopological(new NodeVisitor.Default {
        override def defaultVisit(n: Node): Unit = linearizedNodes(n.getBlock.asInstanceOf[Block]) += n
      })

      linearizedNodes.foreach {
        case (block, nodes) => nodes.foreach(n => {
          if (variableAssigned(n.getOpCode) || n.getOpCode == ir_opcode.iro_Return) {
            result(block) ++= createValue(n, RAX)
            if (n.getOpCode != ir_opcode.iro_Return) {
              result(block) += Movq(RAX, activationRecordOperand(n)).withComment(s"Spill for $n")
            }
          }
        })
      }

      if (activationRecordSize > 0) {
        prologue += Subq(intConstOp(activationRecordSize), RSP).withComment("Build stack frame")
        epilogue += Addq(intConstOp(activationRecordSize), RSP).withComment("Restore stack frame")
      }

      epilogue += Ret()

      result
    }

    private val paramSizes: Seq[Int] = {
      val methodType = g.getEntity.getType.asInstanceOf[MethodType]
      0.until(methodType.getNParams).map(i => {
        val paramType = methodType.getParamType(i)
        8 // paramType.getSizeBytes TODO: when operations chose registers accordingly
      })
    }
    // Parameter offsets relative to RBP (= RSP upon entry into the function)
    private val paramOffsets: Seq[Int] =
      if (paramSizes.isEmpty) Seq()
      else paramSizes.tail.scanLeft(align(paramSizes(0)))(_ + align(_))

    private def createValue(node: Node, destRegNo: Int): Seq[Instruction] = {
      def loadValueComment: Instruction => Instruction = instr => { instr.comment += s" - load argument of $node"; instr }
      val result = mutable.ListBuffer[Instruction]()

      node match {
        case n : firm.nodes.Const =>
          result += Movq(ConstOperand(n.getTarval.asInt(), n.getMode.getSizeBytes), destRegNo).withComment(node.toString)

        case n : firm.nodes.Add =>
          result ++= getValue(n.getLeft, destRegNo) map loadValueComment
          val otherReg = if (destRegNo == RAX) RBX else RAX
          result ++= getValue(n.getRight, otherReg) map loadValueComment
          result += Addq(otherReg, destRegNo).withComment(node.toString)

        case n : firm.nodes.Call  =>
          var stackPointerDisplacement = 0
          def addStackPointerDisplacement(i: Instruction): Instruction =
            { i.stackPointerDisplacement = stackPointerDisplacement
              i.comment += s" - stackPointerDisplacement = $stackPointerDisplacement"
              i }
          for (i <- n.getPredCount - 1 until 1 by -1) {
            result ++= getValue(n.getPred(i), RAX) map loadValueComment map addStackPointerDisplacement
            result += addStackPointerDisplacement(Pushq(RAX).withComment(s"Push argument $i of $node"))
            stackPointerDisplacement += 8 // TODO
          }
          val methodName = n.getPtr.asInstanceOf[Address].getEntity.getLdName
          result += mjis.asm.Call(LabelOperand(methodName)).withComment(node.toString)
          result += Addq(intConstOp(stackPointerDisplacement), RSP).withComment(s"Restore stack pointer for $node")

        case n : firm.nodes.Mul =>
          result ++= getValue(n.getLeft, RAX) map loadValueComment
          result ++= getValue(n.getRight, RBX) map loadValueComment
          result += Mulq(RBX).withComment(node.toString)
          result += Movq(RAX, destRegNo).withComment(node.toString)

        case n : firm.nodes.Return =>
          if (n.getPredCount == 2) { // 2 because the Mem predecessor always exists
            val returnValue = getValue(n.getPred(1), RAX)
            assert(returnValue.length >= 1)
            result ++= returnValue map loadValueComment
          }
          result += mjis.asm.Jmp(new LabelOperand(g.getEndBlock.getNr)).withComment(node.toString)

        case n : firm.nodes.Proj =>
          if (node.getPredCount > 0 && node.getPred(0).getOpCode == ir_opcode.iro_Proj) {
            new Proj(node.getPred(0).ptr).getNum match {
              // Arguments
              case Start.pnTArgs =>
                result += Movq(
                  new RegisterOffsetOperand(RBP, paramOffsets(n.getNum), paramSizes(n.getNum)),
                  destRegNo
                ).withComment(s"Load parameter ${n.getNum}")

              case _ =>
            }
          }

        case _ : firm.nodes.End =>
        case _ : firm.nodes.Start =>
        case _ : firm.nodes.Block =>
        case _ : firm.nodes.Address =>
      }

      result
    }

    private def activationRecordOperand(node: Node): RegisterOffsetOperand = {
      RegisterOffsetOperand(RBP, activationRecord(node), node.getMode.getSizeBytes)
    }

    private def getValue(node: Node, destRegNo: Int): Seq[Instruction] = {
      // Call results are stored in the AR under the Call node, but accessed via Proj nodes
      val arNode = node match {
        case _: Proj => node.getPred(0) match {
          case ProjExtr(call: firm.nodes.Call, firm.nodes.Call.pnTResult) => call
          case _ => node
        }
        case _ => node
      }

      if (activationRecord.contains(arNode)) {
        Seq(Movq(activationRecordOperand(arNode), destRegNo).withComment(s"Reload for $arNode"))
      } else {
        createValue(arNode, destRegNo)
      }
    }
  }

}

class FirmCodeGenerator(a: Unit) extends CodeGenerator(a) {
  override val useFirmBackend = true
}
