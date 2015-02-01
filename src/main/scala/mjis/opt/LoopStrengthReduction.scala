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
object LoopStrengthReduction extends DeferredOptimization(needsBackEdges = true) {

  override def __optimize(g: Graph): Unit = {
    val inductionVars = g.getInductionVariables.map(v => v.value -> v).toMap[Node, InductionVariable]
    val dominators = g.getDominators

    g.walk(new NodeVisitor.Default {
      override def visit(sel: Sel): Unit = inductionVars.get(sel.getIndex) match {
        // only optimize if either the base or index register can be reused
        case Some(v) if BackEdges.getNOuts(sel.getPtr) == 1 || BackEdges.getNOuts(v.value) == 1 =>
          val condBlock = v.value.block
          val loopStartBlock = condBlock.getPred(0).block
          // the base address must be constant when entering the loop
          if (dominators(loopStartBlock).contains(sel.getPtr.block)) {
            val elementBytes = sel.getType.asInstanceOf[ArrayType].getElementType.getSizeBytes
            val baseAddress = g.newSel(loopStartBlock, sel.getPtr, v.start, sel.getType)
            val ptr = g.newPhi(condBlock, Array(baseAddress, g.newDummy(Mode.getP)), Mode.getP)
            val ptrIncrAdd = g.newAdd(v.incrAdd.block, ptr,
              g.newConst(v.incr.getTarval.asInt() * elementBytes, Mode.getIs),
              Mode.getP
            )
            setPred(ptr, 1, ptrIncrAdd)
            exchange(sel, ptr)
          }
        case _ =>
      }
    })
  }

}
