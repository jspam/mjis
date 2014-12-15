package mjis

import firm._
import firm.nodes._
import java.io.BufferedWriter
import mjis.FirmExtractors._
import mjis.util.FirmDumpHelper
import scala.collection.JavaConversions._
import scala.collection.mutable

class Optimizer(input: Unit) extends Phase[Unit] {
  override val findings = List[Finding]()

  override def dumpResult(writer: BufferedWriter): Unit = {
    firm.Program.getGraphs.foreach(FirmDumpHelper.dumpGraph(_, "-Optimizer"))
  }

  override protected def getResult(): Unit = firm.Program.getGraphs.foreach(optimizeGraph)

  private def optimizeGraph(g: Graph): Unit = {
    BackEdges.enable(g)

    constantFolding(g)
    normalizeNodes(g)
    eliminateCommonSubexpressions(g)
    eliminateTrivialPhis(g)

    Util.lowerSels() // make address computation optimizable
    applyIdentities(g)

    BackEdges.disable(g)
  }

  private def constantFolding(g: Graph): Unit =
    new ConstantFoldingVisitor(g).foldAndReplace()

  private class ConstantFoldingVisitor(g: Graph) extends NodeVisitor.Default {

    /** Extends Firm's TargetValue class with some convenience functions **/
    private implicit class TargetValueWithLatticeOps(tval: TargetValue) {

      def sup(others: Iterable[TargetValue]): TargetValue =
        others.foldLeft(tval)((agg, next) => agg.sup(next))

      def sup(other: TargetValue): TargetValue = {
        if (tval == TargetValue.getBad || other == TargetValue.getBad)
          return TargetValue.getBad
        if (tval == TargetValue.getUnknown)
          return other
        if (other == TargetValue.getUnknown || sameConstantAs(other))
          return tval
        TargetValue.getBad
      }

      def sameConstantAs(other: TargetValue): Boolean = {
        tval.isConstant && other.isConstant && tval.asInt == other.asInt
      }

      def ===(other: TargetValue): Boolean =
        tval == other || (tval.isConstant && other.isConstant
          && tval.compare(other) == Relation.Equal)

    }

    // maps node to their values in the lattice
    var nodeToTarval: Map[Node, TargetValue] = Map().withDefaultValue(TargetValue.getUnknown)
    val workList = new mutable.Stack[Node]

    /**
     * Executes constant folding in the irg.
     */
    def foldAndReplace(): Unit = {
      val visitor = new NodeVisitor.Default {
        override def defaultVisit(node: Node): Unit = {
          workList.push(node)
        }
      }
      g.walkTopological(visitor)
      while (workList.nonEmpty) {
        val head = workList.pop()
        head.accept(this)
      }
      // Replace all nodes with const's if possible
      for ((node, targetval) <- nodeToTarval) {
        if (targetval.isConstant) {
          node match {
            case _: Div | _: Mod =>
              // the Div / Mod node itself is not exchanged, instead it's result Proj
              // will be replaced
              deleteDivOrMod(node)
            case _ : Proj => if (node.getMode != Mode.getX) GraphBase.exchange(node, g.newConst(targetval))
            case cond: Cond =>
              // all successors of cond nodes are proj true / false
              val pred = targetval == TargetValue.getBTrue
              val trueProj = BackEdges.getOuts(node).filter(_.node.asInstanceOf[Proj].getNum == Cond.pnTrue).head.node
              val falseProj = BackEdges.getOuts(node).filter(_.node.asInstanceOf[Proj].getNum == Cond.pnFalse).head.node
              val takenBranch = if (pred) trueProj else falseProj
              val discardedBranch = if (pred) falseProj else trueProj

              GraphBase.exchange(node, g.newBad(Mode.getX))
              GraphBase.exchange(takenBranch, g.newJmp(takenBranch.getBlock))
              GraphBase.exchange(discardedBranch, g.newBad(Mode.getX))

            case _ => GraphBase.exchange(node, g.newConst(targetval))
          }
        }
      }
    }

    /*
     * "Deletes" a div or mod node by redirecting the graph's memory flow
     */
    private def deleteDivOrMod(node: Node): Unit = {
      val succs = BackEdges.getOuts(node).toIndexedSeq.map(_.node)
      val divMem: Node = if (succs(0).getMode == Mode.getM) succs(0) else succs(1)
      // Update Mem node if necessary
      if (BackEdges.getNOuts(divMem) > 0) {
        val edge = BackEdges.getOuts(divMem).head
        edge.node.setPred(edge.pos, node.getPred(0))
      }
    }

    override def visit(node: Const): Unit = {
      updateWorkList(node, node.getTarval)
    }

    override def visit(node: Phi): Unit = {
      val merged = TargetValue.getUnknown.sup(node.getPreds.map(nodeToTarval(_)))
      updateWorkList(node, merged)
    }

    override def visit(node: Add): Unit =
      foldBinaryIntOperator((x, y) => x.add(y), node)

    override def visit(node: Sub): Unit =
      foldBinaryIntOperator((x, y) => x.sub(y, Mode.getIs), node)

    override def visit(node: Proj): Unit = {
      if (node.getPred.isInstanceOf[Start]
        || node.getPred.isInstanceOf[Call]
        || node.getPred.isInstanceOf[Load])
        updateWorkList(node, TargetValue.getBad)
      else if (node.getMode == Mode.getIs || node.getMode == Mode.getX)
        foldUnaryIntOperator(x => x, node)
    }

    override def visit(node: Div): Unit = nodeToTarval(node.getPred(1)) match {
      case TargetValueExtr(0) =>
        // 0 / x == 0 (ignoring 0/0)
        updateWorkList(node, new TargetValue(0, Mode.getIs))
      case _ =>
        foldBinaryIntOperator((x, y) => x.div(y), node, node.getPred(1), node.getPred(2))
    }

    override def visit(node: Mod): Unit = nodeToTarval(node.getPred(2)) match {
      case TargetValueExtr(1) =>
        // x % 1 == 0
        updateWorkList(node, new TargetValue(0, Mode.getIs))
      case _ => foldBinaryIntOperator((x, y) => x.mod(y), node, node.getPred(1), node.getPred(2))
    }

    override def visit(node: Mul): Unit = (nodeToTarval(node.getPred(0)), nodeToTarval(node.getPred(1))) match {
      case (TargetValueExtr(0), _) | (_, TargetValueExtr(0)) =>
        // x * 0 == 0 * x == 0
        updateWorkList(node, new TargetValue(0, Mode.getIs))
      case _ => foldBinaryIntOperator((x, y) => x.mul(y), node)
    }

    override def visit(node: Minus): Unit =
      foldUnaryIntOperator(x => x.neg, node)

    override def visit(node: Cmp): Unit = {
      node.getRelation match {
        case Relation.Equal => foldBinaryBooleanOperator((x, y) => x == y, node)
        case Relation.Greater => foldBinaryBooleanOperator((x, y) => x > y, node)
        case Relation.GreaterEqual => foldBinaryBooleanOperator((x, y) => x >= y, node)
        case Relation.Less => foldBinaryBooleanOperator((x, y) => x < y, node)
        case Relation.LessEqual => foldBinaryBooleanOperator((x, y) => x <= y, node)
        case Relation.UnorderedLessGreater => foldBinaryBooleanOperator((x, y) => x != y, node)
        case _ => ???
      }
    }

    override def visit(node: Cond): Unit = {
      val pred = nodeToTarval(node.getSelector)
      updateWorkList(node, pred)
    }

    private def foldUnaryIntOperator(op: TargetValue => TargetValue, node: Node): Unit = {
      val predVal = nodeToTarval(node.getPred(0))
      if (predVal == TargetValue.getBad)
        updateWorkList(node, TargetValue.getBad)
      else if (predVal.isConstant)
        updateWorkList(node, op(predVal))
    }

    private def foldBinaryIntOperator(op: (TargetValue, TargetValue) => TargetValue, node: Node): Unit =
      foldBinaryIntOperator(op, node, node.getPred(0), node.getPred(1))

    private def foldBinaryIntOperator(op: (TargetValue, TargetValue) => TargetValue, node: Node,
      leftNode: Node, rightNode: Node): Unit = {
      val left = nodeToTarval(leftNode)
      val right = nodeToTarval(rightNode)
      if (left != TargetValue.getUnknown
        && right != TargetValue.getUnknown) {
        if (left.isConstant && right.isConstant)
          updateWorkList(node, op(left, right))
        else
          updateWorkList(node, TargetValue.getBad)
      }
    }

    private def foldBinaryBooleanOperator(op: (Int, Int) => Boolean, cmp: Cmp): Unit = {
      foldBinaryIntOperator((left, right) =>
          if (op(left.asInt, right.asInt)) TargetValue.getBTrue else TargetValue.getBFalse,
        cmp, cmp.getLeft, cmp.getRight
      )
    }

    private def updateWorkList(node: Node, tarVal: TargetValue): Unit = {
      // Compare old and current value, only update worklist if something changed
      if (!(nodeToTarval(node) === tarVal)) {
        nodeToTarval += node -> tarVal
        workList pushAll BackEdges.getOuts(node).map(_.node)
      }
    }
  }

  private def eliminateTrivialPhis(g: Graph): Unit = {
    g.walk(new NodeVisitor.Default {
      override def visit(node: Phi): Unit = {
        // skip memory phis
        if (node.getMode == Mode.getM)
          return

        val preds = node.getPreds.toSet - node

        // trivial phi
        if (preds.size == 1) {
          val users = BackEdges.getOuts(node).toList
          GraphBase.exchange(node, preds.head)
          // recurse into all users which may have become trivial too
          users.foreach(_.node match {
            case phi: Phi => visit(phi)
            case _ =>
          })
        }
      }
    })
  }

  private def normalizeNodes(g: Graph): Unit = {
    g.walkTopological(new NodeVisitor.Default {
      override def defaultVisit(node: Node): Unit = node match {
        case AddExtr(_: Const, _) | MulExtr(_: Const, _) =>
          val pred0 = node.getPred(0)
          node.setPred(0, node.getPred(1))
          node.setPred(1, pred0)
        case SubExtr(x, ConstExtr(c)) =>
          // x - c == x + (-c)
          GraphBase.exchange(node, g.newAdd(node.getBlock, x, g.newConst(-c.toInt, node.getMode), node.getMode))
        case SubExtr(ConstExtr(0), x) =>
          // 0 - x = -x
          GraphBase.exchange(node, g.newMinus(node.getBlock, x, node.getMode))
        case _ =>
      }
    })
  }

  private def eliminateCommonSubexpressions(g: Graph): Unit = {
    // map from data uniquely identifying a subexpression to the representing node
    val m = mutable.Map[AnyRef, Node]()

    def getData: PartialFunction[Node, AnyRef] = {
      case c: Const => c.getTarval.asLong.underlying()
      case _: Add | _: Sub | _: Minus | _: Mul | _: Div | _: Mod | _: Sel | _: Conv | _: Cond | _: Phi => null // no data
      case proj: Proj => (proj.getPred, proj.getNum)
      case cmp: Cmp => cmp.getRelation
      case addr: Address => addr.getEntity
      case member: Member => member.getEntity
    }

    g.walkPostorder(new NodeVisitor.Default {
      override def defaultVisit(node: Node): Unit = getData.lift(node) match {
        case Some(nodeData) =>
          val data = (node.getOpCode, node.getBlock, node.getMode, node.getPreds.toList, nodeData)
          m.get(data) match {
            case Some(n2) => GraphBase.exchange(node, n2)
            case None => m += data -> node
          }
        case None =>
      }
    })
  }

  private def applyIdentities(g: Graph): Unit = {
    object PowerOfTwo {
      def unapply(x: Long): Option[Int] = {
        // everybody's favorite trick
        if (x > 0 && (x & (x-1)) == 0) {
          var ret = 0
          var x2 = x
          while (x2 != 1) {
            x2 >>= 1
            ret += 1
          }
          Some(ret)
        } else
          None
      }
    }

    def applyIdentity: PartialFunction[Node, Node] = {
      case AddExtr(x, ConstExtr(0)) => x
      case MulExtr(x, ConstExtr(1)) => x
      case DivExtr(x, ConstExtr(1)) => x
      case n@MulExtr(x, ConstExtr(PowerOfTwo(exp))) =>
        // The new node won't participate in CSE, but it shouldn't matter for a Const node.
        g.newShl(n.getBlock, x, g.newConst(exp, Mode.getIu), n.getMode)
    }

    g.walkTopological(new NodeVisitor.Default {
      override def defaultVisit(node: Node): Unit = applyIdentity.lift(node) match {
        case Some(newNode) => GraphBase.exchange(node, newNode)
        case None =>
      }
    })
  }
}
