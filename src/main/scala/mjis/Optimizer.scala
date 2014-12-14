package mjis

import firm._
import firm.nodes._
import java.io.BufferedWriter
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
    eliminateCommonSubexpressions(g)
    eliminateTrivialPhis(g)

    BackEdges.disable(g)
  }

  private def constantFolding(g: Graph): Unit =
    new ConstantFoldingVisitor(g).foldAndReplace

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
        return tval.isConstant && other.isConstant && tval.asInt == other.asInt
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
      while (!workList.isEmpty) {
        val head = workList.pop
        head.accept(this)
      }
      // Replace all nodes with const's if possible
      for ((node, targetval) <- nodeToTarval) {
        if (targetval.isConstant) {
          if (node.isInstanceOf[Div] || node.isInstanceOf[Mod]) {
            // the Div / Mod node itself is not exchanged, instead it's result Proj
            // will be replaced
            deleteDivOrMod(node)
          } else
            GraphBase.exchange(node, g.newConst(targetval))
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
      else if (node.getMode == Mode.getIs)
        foldUnaryIntOperator(x => x, node)
    }

    override def visit(node: Div): Unit =
      foldBinaryIntOperator((x, y) => x.div(y), node, node.getPred(1), node.getPred(2))

    override def visit(node: Mod): Unit =
      foldBinaryIntOperator((x, y) => x.mod(y), node, node.getPred(1), node.getPred(2))

    override def visit(node: Mul): Unit =
      foldBinaryIntOperator((x, y) => x.mul(y), node)

    override def visit(node: Minus): Unit =
      foldUnaryIntOperator(x => x.neg, node)

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

}
