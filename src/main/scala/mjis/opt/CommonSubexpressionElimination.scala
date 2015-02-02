package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtractors.ProjExtr
import mjis.opt.FirmExtensions._
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
      val preds = node match {
        case _: Div | _: Mod => node.getPreds.drop(1).toList // we don't care about the memory edge
        case _ => node.getPreds.toList
      }
      val data = (node.getOpCode, node.getBlock, node.getMode, preds, nodeData)
      m.get(data) match {
        case Some(`node`) =>
        case Some(n2) => node match {
          case _: Div | _: Mod =>
            // order by reachability via memory edges, else we could create an in-block cycle
            def n2Reachable(n: Node): Boolean = n == n2 || (n.getPredCount > 0 && n2Reachable(n.getPred(0)))
            val (first, second) = if (n2Reachable(node)) (n2, node) else (node, n2)
            for (proj@ProjExtr(_, Div.pnRes) <- second.successors) setPred(proj, 0, first)
            killMemoryNode(first)
          case _ => exchange(node, n2)
        }
        case None => m(data) = node
      }
    case None =>
  }

}
