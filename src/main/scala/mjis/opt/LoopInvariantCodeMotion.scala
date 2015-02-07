package mjis.opt

import firm.{Mode, Graph}
import firm.nodes._
import mjis.opt.FirmExtensions._
import mjis.util.{SCCLoop, SCCTreeNode}
import scala.collection.JavaConversions._
import scala.collection.mutable

object LoopInvariantCodeMotion extends Optimization(needsBackEdges = true) {

  def _optimize(g: Graph): Unit = {
    val dominators = g.getDominators
    val blockGraph = g.getBlockGraph
    val hoisted = mutable.Set[Node]()

    def rec(s: SCCTreeNode[Block]): Unit = s match {
      case loop: SCCLoop[Block] =>
        val preHeader = blockGraph.edges(loop.dominator).find(!loop.nodes.contains(_)).get
        // remember that SCCTreeNode.nodes are actually Blocks here
        val nodes = loop.nodes.flatMap(_.successors)
        for (node <- nodes if node.getMode != Mode.getX &&
            // don't move a node twice
            !hoisted(node) &&
            // Cond nodes are essentially control flow nodes, but have mode T
            !node.isInstanceOf[Cond] &&
            !node.isInstanceOf[Phi] &&
            node.getPreds.forall(pred => dominators(preHeader)(pred.block))) {
          hoisted += node
          setBlock(node, preHeader)
        }
        loop.tree.foreach(rec)
      case _ =>
    }
    blockGraph.transposed.getSCCTree(g.getStartBlock).foreach(rec)
  }

}
