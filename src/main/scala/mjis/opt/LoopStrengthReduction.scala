package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtensions._

/**
 * Rewrites
 *
 * i = start;
 * while (...) {
 *   ... a[i] ...
 *   i += incr;
 * }
 *
 * to
 *
 * i = start;
 * ptr = a + start * sizeof(a[0]);
 * while (...) {
 *   ... *ptr ...
 *   i += incr;
 *   ptr += incr * sizeof(a[0]);
 * }
 */
object LoopStrengthReduction extends Optimization {

  override def optimize(g: Graph): Unit = {
    BackEdges.enable(g)

    val inductionVars = g.getInductionVariables.map(v => v.value -> v).toMap[Node, InductionVariable]
    val dominators = g.getDominators

    g.walk(new NodeVisitor.Default {
      override def visit(sel: Sel): Unit = inductionVars.get(sel.getIndex) match {
        case Some(v) =>
          val condBlock = v.value.block
          val loopStartBlock = condBlock.getPred(0).block
          // the base address must be constant when entering the loop
          if (dominators(loopStartBlock).contains(sel.getPtr.block)) {
            val elementBytes = sel.getType.asInstanceOf[ArrayType].getElementType.getSizeBytes
            val baseAddress = g.newAdd(loopStartBlock, sel.getPtr,
              g.newConst(v.start.getTarval.asInt() * elementBytes, Mode.getIs),
              Mode.getP
            )
            val ptr = g.newPhi(condBlock, Array(baseAddress, g.newDummy(Mode.getP)), Mode.getP)
            val ptrIncrAdd = g.newAdd(v.incrAdd.block, ptr,
              g.newConst(v.incr.getTarval.asInt() * elementBytes, Mode.getIs),
              Mode.getP
            )
            ptr.setPred(1, ptrIncrAdd)
            GraphBase.exchange(sel, ptr)
          }
        case None =>
      }
    })

    BackEdges.disable(g)
  }

}
