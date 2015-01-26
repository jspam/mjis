package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtractors._
import scala.collection.JavaConversions._

abstract class Optimization(needsBackEdges: Boolean = false) {
  def optimize(): Boolean = {
    // always optimize all graphs
    Program.getGraphs.toSeq.map(optimize).exists(b => b)
  }

  var changed = false

  final def optimize(g: Graph): Boolean = {
    changed = false
    if (needsBackEdges)
      BackEdges.enable(g)
    try
      _optimize(g)
    finally
      if (needsBackEdges)
        BackEdges.disable(g)
    changed
  }

  protected def exchange(oldNode: Node, newNode: Node) = {
    changed = true
    GraphBase.exchange(oldNode, newNode)
  }

  protected def _optimize(g: Graph): Unit
}
