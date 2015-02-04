package mjis.opt

import firm._
import firm.nodes._
import scala.collection.JavaConversions._

object TrivialPhiElimination extends NodeBasedOptimization() {

  override def _optimize(g: Graph, node: Node): Unit = node match {
    // skip memory phis
    case _: Phi if node.getMode != Mode.getM =>

      val dependencies = node.getPreds.filter(!_.isInstanceOf[Bad]).toSet - node

      if (dependencies.size == 1)
        exchange(node, dependencies.head)
    case _ =>
  }

}
