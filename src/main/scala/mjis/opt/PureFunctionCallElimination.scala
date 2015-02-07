package mjis.opt

import firm.Graph
import firm.nodes.{Address, Call, Node}
import mjis.opt.FirmExtensions._

object PureFunctionCallElimination extends NodeBasedOptimization() {

  private def isPure(c: Call) = c.getPtr.asInstanceOf[Address].getEntity.getName == "calloc"

  override protected def _optimize(g: Graph, node: Node): Unit = node match {
    case c: Call if isPure(c) && c.successors.size == 1 /* only memory edge */ => killMemoryNode(c)
    case _ =>
  }

}
