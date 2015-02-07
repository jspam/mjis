package mjis.opt

import firm.{Mode, Graph}
import firm.nodes._
import mjis.opt.FirmExtensions._
import mjis.util.{SCCLeaf, SCCLoop, SCCTreeNode}
import scala.collection.JavaConversions._

object LoopInvariantCodeMotion extends DeferredOptimization(needsBackEdges = true) {

  def __optimize(g: Graph): Unit = {
    val dominators = g.getDominators
    val blockGraph = g.getBlockGraph

    def rec(s: SCCTreeNode[Block]): Unit = s match {
      case loop: SCCLoop[Block] =>
        val preHeader = blockGraph.edges(loop.dominator).find(!loop.nodes.contains(_)).get
        // don't look at nodes in nested loops
        val nodes = loop.tree.collect({ case SCCLeaf(n) => n}).flatMap(_.nodes)
        for (node <- nodes if node.getMode != Mode.getX && node.getMode != Mode.getb) node match {
          case _: Phi =>
          // don't hoist potential FPEs
          case _: Div =>
          case _ =>
            if (node.getPreds.forall(pred => dominators(preHeader)(pred.block))) {
              setBlock(node, preHeader)
            }
        }
        loop.tree.foreach(rec)
      case _ =>
    }
    blockGraph.transposed.getSCCTree(g.getStartBlock).foreach(rec)
  }

}
