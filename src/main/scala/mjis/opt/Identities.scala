package mjis.opt

import firm._
import firm.bindings.binding_ircons.op_pin_state
import firm.nodes._
import mjis.opt.FirmExtractors._
import mjis.opt.FirmExtensions._
import mjis.util.PowerOfTwo

object Identities extends NodeBasedOptimization() {

  override def _optimize(g: Graph, node: Node) = node match {
    // x + 0 == x
    case n@AddExtr(x, ConstExtr(0)) =>
      if (n.getMode != x.getMode)
        exchange(n, g.newConv(n.getBlock, x, n.getMode))
      else
        exchange(n, x)
    // (((x+1)+1)+1)... == x+(1+1+1...)
    case n@AddExtr(n2@AddExtr(x, y: Const), z: Const) if n2.successors.size == 1 =>
      exchange(n, g.newAdd(n.getBlock, x, g.newConst(y.getTarval add z.getTarval), n.getMode))
    // x-x == 0
    case n@SubExtr(x, y) if x == y =>
      exchange(n, g.newConst(0, n.getMode))
    // ((x*1)*2)*3... == x*(1*2*3...)
    case n@MulExtr(MulExtr(x, y: Const), z: Const) =>
      exchange(n, g.newMul(n.getBlock, x, g.newConst(y.getTarval mul z.getTarval), n.getMode))
    // -(x-y) == (y-x)
    case n@MinusExtr(SubExtr(x, y)) =>
      exchange(n, g.newSub(n.getBlock, y, x, n.getMode))
    // -(x+c) == ((-c)-x)
    case n@MinusExtr(AddExtr(x, d@ConstExtr(c))) =>
      exchange(n, g.newSub(n.getBlock, g.newConst(-c, d.getMode), x, n.getMode))
    // -(-x) == x (while FirmConstructor may not create these, previous identity applications may)
    case n@MinusExtr(MinusExtr(x)) => exchange(n, x)
    // c-(x+d) == (c-d)-x
    case n@SubExtr(c: Const, AddExtr(x, d: Const)) =>
      exchange(n, g.newSub(n.getBlock, g.newConst(c.getTarval.sub(d.getTarval, n.getMode)), x, n.getMode))
    // x-(-y) == x+y
    case n@SubExtr(x, MinusExtr(y)) =>
      exchange(n, g.newAdd(n.getBlock, x, y, n.getMode))
    // x+(-y) == x-y
    case n@AddExtr(x, MinusExtr(y)) =>
      exchange(n, g.newSub(n.getBlock, x, y, n.getMode))
    // (-x)+y == y-x
    case n@AddExtr(MinusExtr(x), y) =>
      exchange(n, g.newSub(n.getBlock, y, x, n.getMode))
    // y-(x+y) == -x
    case n@SubExtr(y, AddExtr(x, z)) if y == z =>
      exchange(n, g.newMinus(n.getBlock, x, n.getMode))
    // x-(x-y) == y
    case n@SubExtr(x, SubExtr(z, y)) if x == z => exchange(n, y)
    // y-(y+x) == -x
    case n@SubExtr(y, AddExtr(x, z)) if y == x =>
      exchange(n, g.newMinus(n.getBlock, z, n.getMode))
    // (x+y)-y == x
    case n@SubExtr(AddExtr(x, y), z) if y == z =>
      exchange(n, x)
    // (y+x)-y == x
    case n@SubExtr(AddExtr(y, x), z) if y == z =>
      exchange(n, x)
    // (x-y)+y == y
    case n@AddExtr(SubExtr(x, y), z) if y == z => exchange(n, y)
    // (x-y)-x == -y
    case n@SubExtr(SubExtr(x, y), z) if x == z =>
      exchange(n, g.newMinus(n.getBlock, y, n.getMode))
    // x*(-1) == -x
    case n@MulExtr(x, ConstExtr(-1)) =>
      exchange(n, g.newMinus(n.getBlock, x, n.getMode))
    // x*1 == x
    case n@MulExtr(x, ConstExtr(1)) => exchange(n, x)
    // (x+c1)*c2 == x*c2+c1*c2
    case n@MulExtr(AddExtr(x, c1: Const), c2: Const) =>
      exchange(n, g.newAdd(n.getBlock, g.newMul(n.getBlock, x, c2, n.getMode), g.newConst(c1.getTarval mul c2.getTarval), n.getMode))
    // x / 1 == x
    case n@ProjExtr(div@DivExtr(x, ConstExtr(1)), Div.pnRes) =>
      killMemoryNode(div)
      exchange(n, x)
    // x / -1 == -x
    case n@ProjExtr(div@DivExtr(x, ConstExtr(-1)), Div.pnRes) =>
      killMemoryNode(div)
      exchange(n, g.newMinus(n.block, x, x.getMode))
    // x % 0 == x / 0 (allows us to handle only division by zero in the code generator)
    case n@ProjExtr(mod@ModExtr(x, y@ConstExtr(0)), Mod.pnRes) =>
      val div = g.newDiv(n.block, mod.asInstanceOf[Mod].getMem, x, y, n.getMode, op_pin_state.op_pin_state_pinned)
      exchange(n, g.newProj(div, n.getMode, Div.pnRes))
    // x % y == x - x / y * y (which is only useful if 'x / y' can be computed by fast division
    case n@ProjExtr(mod@ModExtr(x, y@ConstExtr(_)), Mod.pnRes) =>
      val div = g.newDiv(n.block, mod.asInstanceOf[Mod].getMem, x, y, n.getMode, op_pin_state.op_pin_state_pinned)
      exchange(n,
        g.newSub(n.block,
          x,
          g.newMul(n.block,
            g.newProj(div, n.getMode, Div.pnRes),
            y, n.getMode), n.getMode))
    case CmpExtr(rel, c: Const, x) =>
      exchange(node, g.newCmp(node.block, x, c, rel.inversed))
    // x % 2^k ==/!= 0
    case CmpExtr(Relation.Equal | Relation.UnorderedLessGreater, proj@ProjExtr(mod@ModExtr(x, ConstExtr(modulo@PowerOfTwo(_))), Mod.pnRes), ConstExtr(0)) =>
      killMemoryNode(mod)
      // |x % 2^k| = x & (modulo-1)
      exchange(proj, g.newAnd(proj.getBlock, x, g.newConst(modulo - 1, x.getMode), x.getMode))
    case mux@MuxExtr(c, _, _) if c.isInstanceOf[Const] =>
      val const = c.asInstanceOf[Const]
      exchange(mux, if (const.getTarval == Mode.getb().getOne) mux.getPred(2) else mux.getPred(1))
    case _ =>
  }

}
