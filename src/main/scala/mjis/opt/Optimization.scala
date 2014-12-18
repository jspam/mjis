package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtractors._
import scala.collection.JavaConversions._

object Optimization {

  /*
   * "Deletes" a div or mod node by redirecting the graph's memory flow
   */
  def deleteDivOrMod(node: Node): Unit = {
    for (proj@ProjExtr(_, Div.pnM /* == Mod.pnM */) <- BackEdges.getOuts(node).map(_.node))
      GraphBase.exchange(proj, node.getPred(0))
  }

}

trait Optimization {
  def optimize(): Unit = {
    Program.getGraphs.foreach(optimize)
  }

  def optimize(g: Graph): Unit
}
