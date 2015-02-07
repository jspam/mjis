package mjis.opt

import firm._
import firm.nodes._
import scala.collection.JavaConversions._

object TrivialPhiElimination extends NodeBasedOptimization() {

  override def _optimize(g: Graph, node: Node): Unit = node match {
    // skip memory phis
    case _: Phi =>

      val dependencies = node.getPreds.filter(!_.isInstanceOf[Bad]).toSet - node

      if (dependencies.size == 1)
        if (node.getMode == Mode.getM)
          for (e <- BackEdges.getOuts(node)) e.node match {
            case _: End | _: Anchor =>
            case n => if (n != node) n.setPred(e.pos, dependencies.head)
          }
        else
          exchange(node, dependencies.head)
    case _ =>
  }

}
