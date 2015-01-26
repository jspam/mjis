package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtractors._

object Normalization extends Optimization {

  override def _optimize(g: Graph): Unit = {
    g.walkTopological(new NodeVisitor.Default {
      override def defaultVisit(node: Node): Unit = node match {
        case AddExtr(_: Const, _) | MulExtr(_: Const, _) =>
          val pred0 = node.getPred(0)
          node.setPred(0, node.getPred(1))
          node.setPred(1, pred0)
          changed = true
        case SubExtr(x, ConstExtr(c)) =>
          // x - c == x + (-c)
          exchange(node, g.newAdd(node.getBlock, x, g.newConst(-c, node.getMode), node.getMode))
        case SubExtr(ConstExtr(0), x) =>
          // 0 - x = -x
          exchange(node, g.newMinus(node.getBlock, x, node.getMode))
        case _ =>
      }
    })
  }

}
