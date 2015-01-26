package mjis.opt

import firm._
import firm.nodes._
import scala.collection.JavaConversions._

object TrivialPhiElimination extends Optimization(needsBackEdges = true) {

  override def _optimize(g: Graph): Unit = {
    g.walk(new NodeVisitor.Default {
      override def visit(node: Phi): Unit = {
        // skip memory phis
        if (node.getMode == Mode.getM)
          return

        val preds = node.getPreds.toSet - node

        // trivial phi
        if (preds.size == 1) {
          val users = BackEdges.getOuts(node).toList
          exchange(node, preds.head)
          // recurse into all users which may have become trivial too
          users.foreach(_.node match {
            case phi: Phi => visit(phi)
            case _ =>
          })
        }
      }
    })
  }

}
