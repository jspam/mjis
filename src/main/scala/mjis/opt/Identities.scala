package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtractors._
import mjis.opt.FirmExtensions._
import mjis.util.PowerOfTwo

object Identities extends NodeBasedOptimization() {

  override def _optimize(g: Graph, node: Node) = node match {
    case n@AddExtr(x, ConstExtr(0)) =>
      if (n.getMode != x.getMode)
        exchange(n, g.newConv(n.getBlock, x, n.getMode))
      else
        exchange(n, x)
    case n@MulExtr(x, ConstExtr(1)) => exchange(n, x)
    case n@MulExtr(x, ConstExtr(PowerOfTwo(exp))) =>
      exchange(n, g.newShl(n.getBlock, x, g.newConst(exp, Mode.getIu), n.getMode))
    // (x+c1)*c2 == x*c2+c1*c2
    case n@MulExtr(AddExtr(x, c1: Const), c2: Const) =>
      exchange(n, g.newAdd(n.getBlock, g.newMul(n.getBlock, x, c2, n.getMode), g.newConst(c1.getTarval mul c2.getTarval), n.getMode))
    case n@ProjExtr(div@DivExtr(x, ConstExtr(1)), Div.pnRes) =>
      killMemoryNode(div)
      exchange(n, x)
    case n@ProjExtr(div@DivExtr(x, ConstExtr(-1)), Div.pnRes) =>
      killMemoryNode(div)
      exchange(n, g.newMinus(n.block, x, x.getMode))
    // x % 2^k ==/!= 0
    case CmpExtr(Relation.Equal | Relation.UnorderedLessGreater, proj@ProjExtr(mod@ModExtr(x, ConstExtr(modulo@PowerOfTwo(_))), Mod.pnRes), ConstExtr(0)) =>
      killMemoryNode(mod)
      // |x % 2^k| = x & (modulo-1)
      exchange(proj, g.newAnd(proj.getBlock, x, g.newConst(modulo - 1, x.getMode), x.getMode))
    case _ =>
  }

}
