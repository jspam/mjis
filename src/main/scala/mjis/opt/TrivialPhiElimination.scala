package mjis.opt

import firm._
import firm.nodes._
import scala.collection.JavaConversions._

object TrivialPhiElimination extends Optimization {

  override def optimize(g: Graph): Unit = {
    BackEdges.enable(g)

    g.walk(new NodeVisitor.Default {
      override def visit(node: Phi): Unit = {
        // skip memory phis
        if (node.getMode == Mode.getM)
          return

        val preds = node.getPreds.toSet - node

        // trivial phi
        if (preds.size == 1) {
          val users = BackEdges.getOuts(node).toList
          GraphBase.exchange(node, preds.head)
          // recurse into all users which may have become trivial too
          users.foreach(_.node match {
            case phi: Phi => visit(phi)
            case _ =>
          })
        }
      }
    })

    BackEdges.disable(g)
  }

}
