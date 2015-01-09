package mjis

import java.io._

import firm._
import firm.nodes._
import mjis.asm._
import mjis.CodeGenerator._
import mjis.opt.FirmExtractors._
import mjis.opt.FirmExtensions._
import mjis.opt.NodeCollector
import mjis.util.MapExtensions._
import mjis.asm.AMD64Registers._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._
import scala.language.implicitConversions

object CodeGenerator {
  def align(x: Int, alignment: Int = 4) = if (x % alignment == 0) x else x + (alignment - x % alignment)
}

class CodeGenerator(a: Unit) extends Phase[AsmProgram] {
  def findings = List()
  def dumpResult(a: BufferedWriter) = {
    a.write(new MjisAssemblerFileGenerator(result).generateCode())
  }
  val resultProgram = new AsmProgram()

  override def getResult(): AsmProgram = {
    Program.getGraphs.foreach(g => {
      resultProgram.functions += new MethodCodeGenerator(g).getResult()
    })
    resultProgram
  }

  def intConstOp(i: Int): Operand = new ConstOperand(i, Mode.getIs.getSizeBytes)
  def regOp(node: Node): RegisterOperand = new RegisterOperand(node.idx, node.getMode.getSizeBytes)
  implicit def regOp(regNr: Int): RegisterOperand = new RegisterOperand(regNr, Registers(regNr).sizeBytes)

  class MethodCodeGenerator(g: Graph) {
    def regParamsIndexes = 0.until(g.getEntity.getType.asInstanceOf[MethodType].getNParams min ParamRegisters.length)
    def appendComment(s: String): Instruction => Instruction = { instr => instr.comment += s; instr }
    val basicBlocks = mutable.ListMap[Block, AsmBasicBlock]().
      withPersistentDefault(b => new AsmBasicBlock(if (b == null) -1 else b.getNr))
    val function = new AsmFunction(g.getEntity.getLdName)

    def getResult(): AsmFunction = {
      val backEdgesWereEnabled = BackEdges.enabled(g)
      BackEdges.enable(g)

      for (n <- NodeCollector.fromWalk(g.walkTopological)) {
        val basicBlock = basicBlocks(n.getBlock.asInstanceOf[Block])
        basicBlock.instructions ++= createValue(n) map appendComment(" - " + n.toString)
        basicBlock.controlFlowInstructions ++= createControlFlow(n) map appendComment(" - " + n.toString)
      }
      if (!backEdgesWereEnabled) BackEdges.disable(g)

      basicBlocks.values.foreach(generateInstructionsForPhi)

      function.epilogue.controlFlowInstructions += Ret()
      function.basicBlocks = (Seq(basicBlocks(g.getStartBlock)) ++
        basicBlocks.keys.filter(b => b != g.getStartBlock && b != g.getEndBlock).map(basicBlocks) ++
        Seq(basicBlocks(g.getEndBlock))).toList
      function
    }

    /** Creates Mov instructions for the contents of block.phi, which is destroyed during this operation. */
    private def generateInstructionsForPhi(block: AsmBasicBlock) = {
      // entries: r3 -> List(x, r1, r2) if (x -> r1), (r1 -> r2), (r2 -> r3) are in the permutation
      //   (which is represented as { r1 -> x, r2 -> r1, r3 -> r2 } in the Phi map)
      val permutations = mutable.ListMap[Int, List[Operand]]() // ListMap for determinism
      def regOp(regNr: Int) = RegisterOperand(regNr, 8 /* TODO */)

      @tailrec
      def getPermutation(destRegNr: Int, acc: List[Operand]): List[Operand] = {
        permutations.get(destRegNr) match {
          case Some(p) =>
            permutations.remove(destRegNr)
            p ++ acc
          case None => block.phi.get(destRegNr) match {
            case Some(src@RegisterOperand(regNr, _)) =>
              block.phi.remove(destRegNr)
              getPermutation(regNr, src :: acc)
            case Some(src) =>
              block.phi.remove(destRegNr)
              src :: acc
            case _ =>
              acc
          }
        }
      }

      while (block.phi.nonEmpty) {
        val dest = block.phi.keys.head
        permutations(dest) = getPermutation(dest, Nil)
      }

      permutations.foreach { case (dest, l) =>
        val destRegOp = regOp(dest)
        l.head match {
          case RegisterOperand(`dest`, _) => /* cyclic permutation */
            val comment = "cyclic permutation " + l.mkString(" -> ") + s" -> $destRegOp"
            val tempRegister = regOp(0)

            (tempRegister :: l.reverse).sliding(2).foreach {
              case Seq(destOp, srcOp) => block.instructions += Movq(srcOp, destOp).withComment(comment)
            }
            block.instructions += Movq(tempRegister, destRegOp).withComment(comment)
            block.instructions += Forget(tempRegister).withComment(comment)

          case _ =>
            val comment = "permutation " + l.mkString(" -> ") + s" -> $destRegOp"
            (destRegOp :: l.reverse).sliding(2).foreach {
              case Seq(destOp, srcOp) => block.instructions += Movq(srcOp, destOp).withComment(comment)
            }
        }
      }
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
      if (paramSizes.length <= ParamRegisters.length) Seq()
      else {
        val stackParams = paramSizes.drop(ParamRegisters.length)
        stackParams.tail.scanLeft(align(stackParams(0)))(_ + align(_))
      }

    private def createControlFlow(node: Node): Seq[Instruction] = {
      def successorBlock(node: Node) = BackEdges.getOuts(node).iterator().next().node.asInstanceOf[Block]
      def successorBlockOperand(node: Node) = new LabelOperand(successorBlock(node).getNr)

      node match {
        case _ : firm.nodes.Jmp =>
          basicBlocks(node.block).successors += basicBlocks(successorBlock(node))
          Seq(mjis.asm.Jmp(successorBlockOperand(node)))

        case ReturnExtr(retvalOption) =>
          basicBlocks(node.block).successors += basicBlocks(successorBlock(node))
          (retvalOption match {
            case Some(retval) => Seq(Movq(getOperand(retval), RAX))
            case None => Seq()
          }) ++ Seq(mjis.asm.Jmp(successorBlockOperand(node)))

        case CondExtr(cmp: Cmp) =>
          val result = mutable.ListBuffer[Instruction]()

          // Const operand must be on the left
          val (leftOp, rightOp, relation) = getOperand(cmp.getLeft) match {
            case left : ConstOperand => (getOperand(cmp.getRight), left, cmp.getRelation.inversed())
            case left : Operand => (left, getOperand(cmp.getRight), cmp.getRelation)
          }
          result += Cmpq(rightOp, leftOp).withComment(s"Evaluate $cmp (actual relation: $relation)")

          val successors = BackEdges.getOuts(node).map(_.node.asInstanceOf[Proj]).toList
          val projTrue = successors.find(_.getNum == Cond.pnTrue)
          val projFalse = successors.find(_.getNum == Cond.pnFalse)
          assert (projTrue.isDefined && projFalse.isDefined)

          basicBlocks(node.block).successors += basicBlocks(successorBlock(projTrue.get))
          result += JmpConditional(successorBlockOperand(projTrue.get), relation, negate = false).
            withComment(projTrue.get.toString)

          basicBlocks(node.block).successors += basicBlocks(successorBlock(projFalse.get))
          result += mjis.asm.Jmp(successorBlockOperand(projFalse.get)).
            withComment(projFalse.get.toString)

          result

        case _ => Seq()
      }
    }

    private def createValue(node: Node): Seq[Instruction] = {
      node match {
        case n : firm.nodes.Add => Seq(Movq(getOperand(n.getLeft), regOp(n)), Addq(getOperand(n.getRight), regOp(n)))

        case n : firm.nodes.Mul => Seq(Movq(getOperand(n.getLeft), RAX), Mulq(getOperand(n.getRight)),
          Movq(RAX, regOp(n)), Forget(RAX))

        case n@ShlExtr(x, ConstExtr(shift)) => Seq(Movq(getOperand(x), regOp(n)), Shlq(ConstOperand(shift, -1), regOp(n)))

        case n : firm.nodes.Load => Seq(Movq(RegisterOffsetOperand(n.getPtr.idx, 0, n.getMode.getSizeBytes), regOp(n)))
        case n : firm.nodes.Store => Seq(Movq(getOperand(n.getValue), RegisterOffsetOperand(n.getPtr.idx, 0, n.getMode.getSizeBytes)))

        case n : firm.nodes.Call =>
          val resultInstrs = ListBuffer[Instruction]()
          var stackPointerDisplacement = 0
          def addStackPointerDisplacement(i: Instruction): Instruction = {
            i.stackPointerDisplacement = stackPointerDisplacement
            i.comment += s" - stackPointerDisplacement = $stackPointerDisplacement"
            i
          }

          // Pass first parameters in registers. Skip first 2 preds (memory and method address)
          for (i <- 0 until (n.getPredCount - 2).min(ParamRegisters.length)) {
            resultInstrs += Movq(getOperand(n.getPred(i+2)), ParamRegisters(i)).withComment(s"Load register argument $i")
          }
          // Push rest of parameters onto the stack in reverse order
          for (i <- n.getPredCount - 1 until ParamRegisters.length by -1) {
            resultInstrs += addStackPointerDisplacement(Pushq(n.getPred(i).getNr).withComment(s"Push stack argument $i"))
            stackPointerDisplacement += 8 // TODO
          }

          val methodEntity = n.getPtr.asInstanceOf[Address].getEntity
          val methodName = methodEntity.getLdName
          resultInstrs += mjis.asm.Call(LabelOperand(methodName))
          if (stackPointerDisplacement > 0)
            resultInstrs += Addq(intConstOp(stackPointerDisplacement), RSP).withComment(s"Restore stack pointer")

          if (methodEntity.getType.asInstanceOf[MethodType].getNRess > 0) {
            resultInstrs ++= Seq(Movq(RAX, regOp(n)), Forget(RAX))
          }

          resultInstrs

        case n : firm.nodes.Phi =>
          if (n.getMode != Mode.getM && n.getMode != Mode.getX) {
            n.getPreds.zipWithIndex.foreach { case (pred, idx) =>
              val predBB = basicBlocks(n.getBlock.getPred(idx).block)
              predBB.phi(n.idx) = getOperand(pred)
            }
          }
          Seq()

        case n @ ProjExtr(ProjExtr(_, Start.pnTArgs), argNum) =>
          if (argNum < ParamRegisters.length) {
            function.prologue.instructions += Movq(ParamRegisters(argNum), regOp(n))
          }
          Seq()

        case _ : firm.nodes.Block | _ : firm.nodes.Start | _ : firm.nodes.End | _ : firm.nodes.Proj |
             _ : firm.nodes.Address | _ : firm.nodes.Const | _ : firm.nodes.Return | _ : firm.nodes.Jmp |
             _ : firm.nodes.Cmp | _ : firm.nodes.Cond | _ : firm.nodes.Conv | _ : firm.nodes.Bad => Seq()
      }
    }

    private def getOperand(node: Node): Operand = getCanonicalNode(node) match {
      case n : firm.nodes.Const => ConstOperand(n.getTarval.asInt(), n.getMode.getSizeBytes)
      case n @ ProjExtr(ProjExtr(_, Start.pnTArgs), argNum) =>
        if (argNum < ParamRegisters.length) regOp(n) else ActivationRecordOperand(
          paramOffsets(argNum - ParamRegisters.length), paramSizes(argNum - ParamRegisters.length))
      case n => regOp(n)
    }

    private def getCanonicalNode(node: Node) = node match {
      case ProjExtr(ProjExtr(call: firm.nodes.Call, firm.nodes.Call.pnTResult), _) => call
      case ProjExtr(load: firm.nodes.Load, firm.nodes.Load.pnRes) => load
      case n: firm.nodes.Conv =>
        // TODO - nothing to do as long there's only one register size
        n.getOp
      case _ => node
    }
  }

}
