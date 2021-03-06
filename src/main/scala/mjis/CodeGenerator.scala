package mjis

import java.io._

import firm._
import firm.nodes.{Bad, Block, Node}
import mjis.asm._
import mjis.opt.FirmExtractors._
import mjis.opt.FirmExtensions._
import mjis.util.MapExtensions._
import mjis.asm.AMD64Registers._
import mjis.util.{Digraph, PowerOfTwo}
import mjis.CodeGenerator._

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.JavaConversions._

object CodeGenerator {
  def align(x: Int, alignment: Int = 8) = if (x % alignment == 0) x else x + (alignment - x % alignment)

  var uniqueLabelNr = 0
  def newTempLabelOp() = {
    val labelNr = uniqueLabelNr
    uniqueLabelNr += 1
    LabelOperand(s".T$labelNr")
  }

  val MIN_INT = -2147483648

  def getDivModCode(dividend: Operand, divisor: Operand): Seq[Instruction] = {
    val normalCaseLabel = newTempLabelOp()
    val exitLabel = newTempLabelOp()

    val result = ArrayBuffer[Instruction](Mov(dividend, RegisterOperand(RAX, 4)))

    // Special case MIN_INT / -1 = MIN_INT, remainder zero, see
    // http://hg.openjdk.java.net/jdk7/modules/hotspot/file/9646293b9637/src/cpu/x86/vm/assembler_x86.cpp#l5847
    dividend match {
      case ConstOperand(c, _) =>
        if (c != MIN_INT) result ++= Seq(
          // Result of cdq is statically known
          (if (c < 0) Mov(ConstOperand(0xFFFFFFFF, 4), RegisterOperand(RDX, 4))
            else Xor(RegisterOperand(RDX, 4), RegisterOperand(RDX, 4))),
          IDiv(divisor))
        else result ++= Seq(
          Xor(RegisterOperand(RDX, 4), RegisterOperand(RDX, 4)),
          Cmp(divisor, ConstOperand(-1, 4)),
          JmpConditional(exitLabel, Relation.Equal),
          Mov(ConstOperand(0xFFFFFFFF, 4), RegisterOperand(RDX, 4)),
          IDiv(divisor),
          Label(exitLabel))
      case _ => result ++= Seq(
        Cmp(RegisterOperand(RAX, 4), ConstOperand(MIN_INT, 4)),
        JmpConditional(normalCaseLabel, Relation.UnorderedLessGreater),
        Xor(RegisterOperand(RDX, 4), RegisterOperand(RDX, 4)),
        Cmp(divisor, ConstOperand(-1, 4)),
        JmpConditional(exitLabel, Relation.Equal),
        Label(normalCaseLabel),
        Cdq(4),
        IDiv(divisor),
        Label(exitLabel))
    }

    result
  }
}

class CodeGenerator(a: Unit) extends Phase[AsmProgram] {
  var onlyReachableFromMain: Boolean = true

  override def dumpResult(a: BufferedWriter) = {
    a.write(new MjisAssemblerFileGenerator(result, null).generateCode())
  }

  override def getResult(): AsmProgram = {
    val functions = Program.getGraphs.map(g => g -> new MethodCodeGenerator(g).getResult()).toMap
    val callEdges = functions.map {
      case (graph, fun) => fun -> CallGraph.calls(graph).flatMap(_.getCalledGraph).flatMap(functions.get)
    }.toMap

    val callGraph = new Digraph[AsmFunction](callEdges)
    val mainFunction = functions.values.find(_.name == "__main").get

    new AsmProgram(
      if (onlyReachableFromMain) callGraph.getTopologicalSorting(mainFunction) else functions.values.toSeq,
      new Digraph[AsmFunction](callEdges))
  }

  def intConstOp(i: Int): Operand = ConstOperand(i, Mode.getIs.getSizeBytes)

  class MethodCodeGenerator(g: Graph) {
    val basicBlocks = mutable.ListMap[Block, AsmBasicBlock]().
      withPersistentDefault(b => new AsmBasicBlock(if (b == null) -1 else b.getNr))
    val function = new AsmFunction(g.getEntity.getLdName)

    val usedParams = mutable.Map[Int, Node]()
    val toVisit = mutable.Queue[Node]()

    val inductionVarAdds = g.getInductionVariables.map(_.incrAdd).toSet[Node]

    def regOp(node: Node, nr: Int = 0): RegisterOperand = RegisterOperand(node.idx + nr * (1 + g.getLastIdx), node match {
      case n : nodes.Call =>
        val methodEntity = n.getPtr.asInstanceOf[nodes.Address].getEntity
        val methodType = methodEntity.getType.asInstanceOf[MethodType]
        if (methodType.getNRess > 0) methodType.getResType(0).getSizeBytes else -1
      case n : nodes.Load => n.getLoadMode.getSizeBytes
      case n : firm.nodes.Div => n.getResmode.getSizeBytes
      case n : firm.nodes.Mod => n.getResmode.getSizeBytes
      case _ => node.getMode.getSizeBytes
    })

    def getResult(): AsmFunction = {
      val backEdgesWereEnabled = BackEdges.enabled(g)
      BackEdges.enable(g)

      val instructions = mutable.Map[Node, Seq[Instruction]]()

      g.walkTopologicalWith { n =>
        val isBlockRoot = !n.isInstanceOf[nodes.Const] && BackEdges.getOuts(n).exists(e => e.node.block != null && e.node.block != n.block)
        if (isBlockRoot)
          instructions(n) = createValue(n)
      }

      while (toVisit.nonEmpty) {
        val n = toVisit.dequeue()
        if (!instructions.contains(n))
          instructions(n) = createValue(n)
      }

      val firmBlocks = linearizeBlocks().map(basicBlocks).toList

      firmBlocks.head.predecessors += Some(function.prologue)
      function.prologue.successors += firmBlocks.head
      function.epilogue = basicBlocks(g.getEndBlock)

      function.basicBlocks = function.prologue :: firmBlocks

      for ((firmBlock, asmBlock) <- basicBlocks) {
        asmBlock.predecessors ++= firmBlock.getPreds.map(b =>
          if (b.isInstanceOf[Bad] || !function.basicBlocks.contains(basicBlocks(b.block))) None
          else Some(basicBlocks(b.block)))
      }

      val nextBlock = function.basicBlocks.zip(function.basicBlocks.tail).toMap

      val yoloScheduling = mutable.HashMap[AsmBasicBlock, Seq[Instruction]]().withPersistentDefault(_ => Seq())

      g.walkTopologicalWith { n =>
        val basicBlock = basicBlocks(n.block)
        var instrs = instructions.getOrElse(n, Seq()) map (_.withComment(s" - $n"))

        // delay scheduling of single-use induction variable adds in order to force them into a single register
        if (inductionVarAdds(n) && BackEdges.getNOuts(n) == 1)
          yoloScheduling(basicBlock) ++= instrs
        else
          basicBlock.instructions ++= instrs
      }

      basicBlocks.values.foreach(b => b.instructions ++= yoloScheduling(b))

      g.walkTopologicalWith { n =>
        val basicBlock = basicBlocks(n.block)
        basicBlock.instructions ++= createControlFlow(n, nextBlock.get(basicBlock)) map (_.withComment(s" - $n"))
      }
      if (!backEdgesWereEnabled) BackEdges.disable(g)

      for ((argNum, n) <- usedParams.toSeq.sortBy(_._1))
        // save used param registers, load used stack params
        function.prologue.instructions += Mov(
          if (argNum < ParamRegisters.length) RegisterOperand(ParamRegisters(argNum), n.getMode.getSizeBytes)
          else ActivationRecordOperand(8 * (argNum - ParamRegisters.length + 1 /* return address */), n.getMode.getSizeBytes),
          regOp(n))

      function.epilogue.instructions +=
        (if (g.methodType.getNRess > 0) Ret(g.methodType.getResType(0).getSizeBytes) else Ret())
      function
    }

    private def linearizeBlocks(): Seq[Block] = {
      // keep blocks of a loop together by recursively using a topological ordering of the condensation graphs
      g.getBlockGraph.transposed.getSCCTree(g.getStartBlock).flatMap(_.nodes)
    }

    private def createControlFlow(node: Node, nextBlock: Option[AsmBasicBlock]): Seq[Instruction] = {
      def successorBlock(node: Node) = BackEdges.getOuts(node).iterator().next().node.asInstanceOf[Block]

      node match {
        case _: nodes.Jmp =>
          basicBlocks(node.block).successors += basicBlocks(successorBlock(node))
          Seq()

        case ReturnExtr(retvalOption) =>
          basicBlocks(node.block).successors += basicBlocks(successorBlock(node))
          retvalOption match {
            case Some(retval) => Seq(Mov(getOperand(retval), RegisterOperand(RAX, g.methodType.getResType(0).getSizeBytes)))
            case None => Seq()
          }

        case CondExtr(cmp: nodes.Cmp) =>
          val successors = BackEdges.getOuts(node).map(_.node.asInstanceOf[nodes.Proj]).toList
          val projTrue = successors.find(_.getNum == nodes.Cond.pnTrue)
          val projFalse = successors.find(_.getNum == nodes.Cond.pnFalse)
          assert(projTrue.isDefined && projFalse.isDefined)

          basicBlocks(node.block).successors += basicBlocks(successorBlock(projTrue.get))
          basicBlocks(node.block).successors += basicBlocks(successorBlock(projFalse.get))

          basicBlocks(node.block).relation = cmp.getRelation
          Seq(asm.Cmp(getOperand(cmp.getLeft), getOperand(cmp.getRight)).withComment(s"Evaluate $cmp"))

        case _ => Seq()
      }
    }

    private def createValue(n: Node): Seq[Instruction] = {
      def getAddressOperand(node: Node, sizeBytes: Int): AddressOperand = {
        def mk(base: Option[Node] = None, indexAndScale: Option[(Node, Int)] = None, offset: Int = 0) =
          AddressOperand(base = base.map(getRegisterOperand),
            indexAndScale = for ((i, s) <- indexAndScale) yield (getRegisterOperand(i), s),
            offset = offset,
            sizeBytes = sizeBytes
          )
        node match {
          case n: nodes.Member => mk(base = Some(n.getPtr), offset = n.getEntity.getOffset)
          case n@SelExtr(base, index) =>
            val elemSize = n.asInstanceOf[nodes.Sel].getType.asInstanceOf[firm.ArrayType].getElementType.getSizeBytes
            index match {
              case ConstExtr(c) =>
                try {
                  return mk(base = Some(base), offset = Math.multiplyExact(c, elemSize))
                } catch {
                  case _: ArithmeticException => // offset calculation overflows, we can't encode that as an offset
                }
              case _ =>
            }
            mk(base = Some(base), indexAndScale = Some((index, elemSize)))
          case base + ConstExtr(offset) => mk(base = Some(base), offset = offset)
          case
            GenAddExtr(base, offset) +
            GenMulExtr(
              Some(index),
              scale@(1 | 2 | 4 | 8)
            ) => mk(base = base, indexAndScale = Some((index, scale)), offset = offset)
          case base + index => mk(base = Some(base), indexAndScale = Some((index, 1)))
          case other => mk(base = Some(other))
        }
      }

      def divByConstant(dividend: Node, divisor: Int): Seq[Instruction] = {
        // See http://support.amd.com/TechDocs/25112.PDF pages 189 and following
        def log2(i: Long) = {
          assert(i > 0)
          def rec(i: Long, acc: Int): Int = i match {
            case 0 => acc
            case _ => rec(i >> 1, acc + 1)
          }
          rec(i >> 1, 0)
        }

        assert(dividend.getMode == Mode.getIs)
        val r = regOp(n)
        val eax = RegisterOperand(AMD64Registers.RAX, 4)
        val edx = RegisterOperand(AMD64Registers.RDX, 4)
        val (instrs: Seq[Instruction], resultReg) = Math.abs(divisor.toLong) match {
          case 0 => (Seq(
            Mov(getOperand(dividend), eax),
            Mov(ConstOperand(0, 4), r),
            IDiv(r)
          ), eax)
          case 1 => ??? // handled by Identities
          case 2 => (Seq(
            Mov(getOperand(dividend), r),
            Cmp(r, ConstOperand(1 << 31, 4)),
            new Instruction("sbb", (ConstOperand(-1, 4), OperandSpec.READ), (r, OperandSpec.READ | OperandSpec.WRITE)),
            Sar(ConstOperand(1, 4), r)
          ), r)
          case d@PowerOfTwo(pow) => (Seq(
            Mov(getOperand(dividend), eax),
            Cdq(4),
            And(ConstOperand((d - 1).toInt, 4), edx),
            Add(edx, eax),
            Sar(ConstOperand(pow, 4), eax)
          ), eax)
          case d =>
            /* Determine algorithm (a), multiplier (m), and shift factor (s) for 32-bit
            signed integer division. Based on: Granlund, T.; Montgomery, P.L.:
            "Division by Invariant Integers using Multiplication". SIGPLAN Notices,
            Vol. 29, June 1994, page 61.
            */
            var l = log2(d)
            val j = (1l << 31) % d
            val k = (1l << (32 + l)) / ((1l << 31) - j)
            var m_low = (1l << (32 + l)) / d
            var m_high = ((1l << (32 + l)) + k) / d
            while (((m_low >> 1) < (m_high >> 1)) && (l > 0)) {
              m_low >>= 1
              m_high >>= 1
              l -= 1
            }
            val m = m_high.toInt
            (Seq(
              Mov(ConstOperand(m, 4), eax),
              IMul(getOperand(dividend)),
              Mov(getOperand(dividend), eax)) ++
              (if ((m_high >> 31) != 0) Seq(Add(eax, edx)) else Seq()) ++ Seq(
              Sar(ConstOperand(l, 4), edx),
              Shr(ConstOperand(31, 4), eax),
              Add(eax, edx)
            ), edx)
        }
        instrs ++ (if (divisor < 0) Seq(Neg(resultReg)) else Seq()) :+ Mov(resultReg, r)
      }

      n match {
        case n: nodes.Const => Seq(Mov(ConstOperand(n.getTarval.asInt, n.getMode.getSizeBytes), regOp(n)))
        case n: nodes.Unknown => Seq(Mov(ConstOperand(0, n.getMode.getSizeBytes), regOp(n)))

        case n: nodes.Add => Seq(Lea(getAddressOperand(n, n.getMode.getSizeBytes), regOp(n)))

        case n: nodes.Sel =>
          // This only happens in the case of Loop Strength Reduction, which inserts Pointer Phis.
          Seq(Lea(getAddressOperand(n, n.getMode.getSizeBytes), regOp(n)))

        case n: nodes.Load =>
          toVisit += n.getMem
          Seq(Mov(getAddressOperand(n.getPtr, n.getLoadMode.getSizeBytes), regOp(n)))
        case n: nodes.Store =>
          toVisit += n.getMem
          Seq(Mov(getOperand(n.getValue), getAddressOperand(n.getPtr, n.getType.getSizeBytes)))

        case n: nodes.Mux =>
          val cmp = n.getSel.asInstanceOf[nodes.Cmp]
          val cmpLeft = getOperand(cmp.getLeft)
          val cmpRight = getOperand(cmp.getRight)
          val ifFalse = getOperand(n.getFalse)
          val ifTrue = getOperand(n.getTrue)

          // Conditional move does not take immediates, so try to make trueOperand a non-constant
          // if it happens to be equal to the left operand of the comparison.
          val (relation, trueOperand, falseOperand) = (cmp.getRelation, cmpRight, ifTrue, ifFalse) match {
            // y = if (left == const) const else b  ==>  y = if (left == const) left else b
            case (Relation.Equal, _: ConstOperand, `cmpRight`, _) => (cmp.getRelation, cmpLeft, ifFalse)
            // y = if (left != const) b else const  ==>  y = if (left == const) left else b
            case (Relation.UnorderedLessGreater, _: ConstOperand, _, `cmpRight`) => (cmp.getRelation.negated(), cmpLeft, ifTrue)
            case _ => (cmp.getRelation, ifTrue, ifFalse)
          }

          trueOperand match {
            case _: ConstOperand => Seq(
              Mov(trueOperand, regOp(n, 1)).withComment("Load cmov argument"),
              Mov(falseOperand, regOp(n)),
              asm.Cmp(cmpLeft, cmpRight).withComment(s"Evaluate $cmp"),
              MovConditional(regOp(n, 1), regOp(n), relation)
            )
            case _ => Seq(
              Mov(falseOperand, regOp(n)),
              asm.Cmp(cmpLeft, cmpRight).withComment(s"Evaluate $cmp"),
              MovConditional(trueOperand, regOp(n), relation)
            )
          }

        case n: nodes.And => Seq(Mov(getOperand(n.getLeft), regOp(n)), asm.And(getOperand(n.getRight), regOp(n)))
        case l - r => Seq(Mov(getOperand(l), regOp(n)), asm.Sub(getOperand(r), regOp(n)))
        case n: nodes.Minus => Seq(Mov(getOperand(n.getOp), regOp(n)), Neg(regOp(n)))
        case n: nodes.Not => Seq(Mov(getOperand(n.getOp), regOp(n)), Not(regOp(n)))

        case x * (c@ConstExtr(PowerOfTwo(shift))) =>
          Seq(Mov(getOperand(x), regOp(n)),
            Shl(ConstOperand(shift, c.getMode.getSizeBytes), regOp(n)))

        case l * r =>
          val tempRegister = RegisterOperand(RAX, n.getMode.getSizeBytes)
          // Normalization moves constants to the right of a Mul node,
          // but the Mul instruction cannot take a constant as operand.
          // Avoid having to allocate an extra register by swapping left and right.
          Seq(Mov(getOperand(r), tempRegister), asm.Mul(getOperand(l)),
            Mov(tempRegister, regOp(n)))

        case dividend / ConstExtr(divisor) => divByConstant(dividend, divisor)
        case dividend / divisor => Seq(DivMod(getOperand(dividend), getOperand(divisor)), Mov(RegisterOperand(RAX, 4), regOp(n)))
        case dividend % divisor => Seq(DivMod(getOperand(dividend), getOperand(divisor)), Mov(RegisterOperand(RDX, 4), regOp(n)))

        case n@CallExtr(address, params) =>
          toVisit += n.asInstanceOf[nodes.Call].getMem
          val resultInstrs = ListBuffer[Instruction]()

          val (regParams, stackParams) = params.splitAt(ParamRegisters.length)
          val regSrcOperands = regParams.map(getOperand)
          val paramRegisters = regSrcOperands.zip(ParamRegisters).map {
            case (srcOp, reg) => RegisterOperand(reg, srcOp.sizeBytes)
          }

          for ((srcOp, destOp) <- regSrcOperands zip paramRegisters) {
            resultInstrs += Mov(srcOp, destOp).withComment(s"Load register argument")
          }

          val stackPointerOffset = 8 * stackParams.length
          if (stackParams.nonEmpty) {
            resultInstrs += Sub(intConstOp(stackPointerOffset), RegisterOperand(RSP, 8)).withComment(s"Move stack pointer for parameters")
            // Push rest of parameters onto the stack in reverse order
            for ((param, i) <- stackParams.zipWithIndex.reverse) {
              val srcOp = getOperand(param)
              val tempOp = RegisterOperand(n.idx, srcOp.sizeBytes) // needed if srcOp is a memory operand
              resultInstrs += Mov(srcOp, tempOp).
                withComment(s"Reload stack argument").
                withStackPointerOffset(stackPointerOffset)
              resultInstrs += Mov(tempOp, AddressOperand(
                base = Some(RegisterOperand(RSP, 8)),
                offset = 8 * i,
                sizeBytes = srcOp.sizeBytes)).
                withComment(s"Push stack argument").
                withStackPointerOffset(stackPointerOffset)
            }
          }

          val methodName = address.getEntity.getLdName
          val methodType = address.getEntity.getType.asInstanceOf[MethodType]
          val returnsValue = methodType.getNRess > 0

          resultInstrs +=
            (if (returnsValue) mjis.asm.Call(LabelOperand(methodName), methodType.getResType(0).getSizeBytes, paramRegisters)
            else mjis.asm.Call(LabelOperand(methodName), paramRegisters))

          if (stackParams.nonEmpty)
            resultInstrs += Add(intConstOp(stackPointerOffset), RegisterOperand(RSP, 8)).withComment(s"Restore stack pointer")

          if (returnsValue) {
            resultInstrs += Mov(RegisterOperand(RAX, methodType.getResType(0).getSizeBytes), regOp(n))
          }

          resultInstrs

        case n: nodes.Phi =>
          // always evaluate all preds
          val preds = n.getPreds.map(getOperand).toList
          if (n.getMode != Mode.getM && n.getMode != Mode.getX)
            basicBlocks(n.block).phis += Phi(preds, regOp(n))
          Seq()

        case _: nodes.Block | _: nodes.Start | _: nodes.End | _: nodes.Proj |
             _: nodes.Address | _: nodes.Return | _: nodes.Jmp |
             _: nodes.Cmp | _: nodes.Cond | _: nodes.Bad |
             _: nodes.Member =>
          // evaluate all preds
          n.getPreds.foreach(getOperand)
          Seq()
      }
    }

    private def getOperand(node: Node): Operand = getCanonicalNode(node) match {
      case n : nodes.Const => ConstOperand(n.getTarval.asInt(), n.getMode.getSizeBytes)
      case _ => getRegisterOperand(node)
    }

    private def getRegisterOperand(node: Node): RegisterOperand = getCanonicalNode(node) match {
      case n @ ProjExtr(ProjExtr(_, nodes.Start.pnTArgs), argNum) =>
        usedParams(argNum) = n
        regOp(n)
      case n =>
        toVisit += n
        regOp(n)
    }

    private def getCanonicalNode(node: Node): Node = node match {
      case ProjExtr(ProjExtr(call: nodes.Call, nodes.Call.pnTResult), resultNo) =>
        assert(resultNo == 0)
        call
      case ProjExtr(load: nodes.Load, nodes.Load.pnRes) => load
      case ProjExtr(div: nodes.Div, nodes.Div.pnRes) => div
      case ProjExtr(mod: nodes.Mod, nodes.Mod.pnRes) => mod
      case _ => node
    }
  }

}
