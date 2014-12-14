package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtractors._

object Identities extends Optimization {

  private object PowerOfTwo {
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

  def optimize(g: Graph): Unit = {
    BackEdges.enable(g)

    def applyIdentity: PartialFunction[Node, Node] = {
      case AddExtr(x, ConstExtr(0)) => x
      case MulExtr(x, ConstExtr(1)) => x
      case ProjExtr(div@DivExtr(x, ConstExtr(1)), Div.pnRes) =>
        Optimization.deleteDivOrMod(div)
        x
      case n@MulExtr(x, ConstExtr(PowerOfTwo(exp))) =>
        // The new node won't participate in CSE, but it shouldn't matter for a Const node.
        g.newShl(n.getBlock, x, g.newConst(exp, Mode.getIu), n.getMode)
    }

    g.walkTopological(new NodeVisitor.Default {
      override def defaultVisit(node: Node): Unit = applyIdentity.lift(node) match {
        case Some(newNode) => GraphBase.exchange(node, newNode)
        case _ =>
      }
    })

    BackEdges.disable(g)
  }

}
