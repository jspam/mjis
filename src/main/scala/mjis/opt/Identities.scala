package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtractors._
import mjis.opt.FirmExtensions._

object Identities extends Optimization {

  private object PowerOfTwo {
    def unapply(x: Int): Option[Int] = {
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

  def optimize(g: Graph): Unit = {
    BackEdges.enable(g)

    g.walkTopological(new NodeVisitor.Default {
      override def defaultVisit(node: Node): Unit = node match {
        case n@AddExtr(x, ConstExtr(0)) => GraphBase.exchange(n, x)
        case n@MulExtr(x, ConstExtr(1)) => GraphBase.exchange(n, x)
        case n@MulExtr(x, ConstExtr(PowerOfTwo(exp))) =>
          GraphBase.exchange(n, g.newShl(n.getBlock, x, g.newConst(exp, Mode.getIu), n.getMode))
        case n@ProjExtr(div@DivExtr(x, ConstExtr(1)), Div.pnRes) =>
          g.deleteDivOrMod(div)
          GraphBase.exchange(n, x)
        // x % 2^k ==/!= 0
        case CmpExtr(Relation.Equal | Relation.UnorderedLessGreater, proj@ProjExtr(mod@ModExtr(x, ConstExtr(modulo@PowerOfTwo(_))), Mod.pnRes), ConstExtr(0)) =>
          g.deleteDivOrMod(mod)
          // |x % 2^k| = x & (modulo-1)
          GraphBase.exchange(proj, g.newAnd(proj.getBlock, x, g.newConst(modulo-1, x.getMode), x.getMode))
        case _ =>
      }
    })

    BackEdges.disable(g)
  }

}
