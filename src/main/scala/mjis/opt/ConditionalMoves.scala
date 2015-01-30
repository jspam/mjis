package mjis.opt

import firm.nodes.Phi
import firm.{Graph, Mode}
import mjis.opt.FirmExtensions._
import mjis.opt.FirmExtractors.{DiamondExtr, HardToPredictExtr}

import scala.collection.JavaConversions._

object ConditionalMoves extends Optimization(needsBackEdges = true) {

  /* Heuristics for easy-to-calculate operands: Not more than one node of the operand's dependency chain
   * may be in the conditional predecessor block. */
  private def operandsAreEasyToCalculate(phi: Phi) = phi.getPreds.zipWithIndex.forall { case (n, idx) =>
    val predBlock = phi.block.predBlock(idx)
    n.getBlock != predBlock || n.getPreds.forall(_.getBlock != predBlock)
  }

  private val MaxPhis = 1

  override protected def _optimize(g: Graph): Unit = {
    g.walkBlocksWith {
      case bottom@DiamondExtr(top, ifFalse, ifTrue, cmp@HardToPredictExtr()) =>
        val phis = bottom.nodes.collect({ case phi: Phi => phi })
        if (phis.size == MaxPhis &&
            phis.forall(phi => phi.getMode != Mode.getM && phi.getMode != Mode.getX && phi.getMode != Mode.getBu) &&
            phis.forall(operandsAreEasyToCalculate)) {

          // Merge top, ifFalse, ifTrue, and bottom
          for (block <- Seq(bottom, ifFalse, ifTrue)) {
            for (node <- block.nodes) {
              node.setBlock(top)
            }
          }
          phis.foreach { phi => exchange(phi, g.newMux(top, cmp, phi.getPred(0), phi.getPred(1), phi.getMode)) }
        }
      case _ =>
    }
  }
}
