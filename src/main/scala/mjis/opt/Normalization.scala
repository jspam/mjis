package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtractors._

object Normalization extends NodeBasedOptimization {

  override def _optimize(g: Graph, node: Node): Unit = node match {
    case (_: Const) + _ | (_: Const) * _ =>
      val pred0 = node.getPred(0)
      setPred(node, 0, node.getPred(1))
      setPred(node, 1, pred0)
    case x - ConstExtr(c) =>
      // x - c == x + (-c)
      exchange(node, g.newAdd(node.getBlock, x, g.newConst(-c, node.getMode), node.getMode))
    case ConstExtr(0) - x =>
      // 0 - x = -x
      exchange(node, g.newMinus(node.getBlock, x, node.getMode))
    case _ =>
  }

}
