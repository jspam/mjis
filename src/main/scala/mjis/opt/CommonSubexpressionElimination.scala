package mjis.opt

import firm._
import firm.nodes._
import scala.collection.mutable
import scala.collection.JavaConversions._

object CommonSubexpressionElimination extends NodeBasedOptimization {

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

  override def beforeOptimize(g: Graph) = m.clear()

  override def _optimize(g: Graph, node: Node) = getData.lift(node) match {
    case Some(nodeData) =>
      val data = (node.getOpCode, node.getBlock, node.getMode, node.getPreds.toList, nodeData)
      m.get(data) match {
        case Some(`node`) =>
        case Some(n2) => exchange(node, n2)
        case None => m(data) = node
      }
    case None =>
  }

}
