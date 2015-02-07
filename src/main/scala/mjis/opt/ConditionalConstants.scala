package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtensions._
import mjis.opt.FirmExtractors._

object ConditionalConstants extends Optimization(needsBackEdges = true) {

  override def _optimize(g: Graph) = {
    val dominators = g.getDominators
    g.walkWith {
      case cond@CondExtr(CmpExtr(rel@(Relation.Equal | Relation.UnorderedLessGreater), x, c: Const)) =>
        val succBlock = cond.succProj(if (rel == Relation.Equal) Cond.pnTrue else Cond.pnFalse).get.successors.head.asInstanceOf[Block]
        // make sure succBlock isn't shared with other Conds
        if (succBlock.getPredCount == 1)
          replaceUses(x, c, user => dominators(user.block).contains(succBlock))
      case _ =>
    }
  }

}
