package mjis.opt

import firm._
import firm.nodes.Node
import scala.collection.mutable
import scala.collection.JavaConversions._

object DataFlowAnalysis {
  def iterate[A](g: Graph, bot: A, transfer: (Node, List[A]) => A): Map[Node, A] = {
    val m = mutable.Map[Node, A]().withDefaultValue(bot)

    val worklist = NodeCollector.fromWalk(g.walkTopological).to[mutable.Queue]

    while (worklist.nonEmpty) {
      val node = worklist.dequeue()
      val value = transfer(node, node.getPreds.map(m).toList)

      if (value != m(node)) {
        m(node) = value
        BackEdges.getOuts(node).foreach(e => worklist.enqueue(e.node))
      }
    }

    m.toMap
  }
}
