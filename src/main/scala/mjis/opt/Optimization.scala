package mjis.opt

import firm._
import firm.nodes.Node
import scala.collection.JavaConversions._

object Optimization {

  /*
   * "Deletes" a div or mod node by redirecting the graph's memory flow
   */
  def deleteDivOrMod(node: Node): Unit = {
    val succs = BackEdges.getOuts(node).toIndexedSeq.map(_.node)
    val divMem: Node = if (succs(0).getMode == Mode.getM) succs(0) else succs(1)
    // Update Mem node if necessary
    if (BackEdges.getNOuts(divMem) > 0) {
      val edge = BackEdges.getOuts(divMem).head
      edge.node.setPred(edge.pos, node.getPred(0))
    }
  }

}

trait Optimization {
  def optimize(): Unit = {
    Program.getGraphs.foreach(optimize)
  }

  def optimize(g: Graph): Unit
}
