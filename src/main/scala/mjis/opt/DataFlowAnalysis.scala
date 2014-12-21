package mjis.opt

import firm._
import firm.nodes._
import scala.collection.mutable
import scala.collection.JavaConversions._
import mjis.opt.FirmExtensions._

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

    m.toMap.withDefaultValue(bot)
  }

  def iterateBlocks[A](g: Graph, bot: A, transfer: (Block, List[A]) => A): Map[Block, A] = {
    val m = mutable.Map[Block, A]().withDefaultValue(bot)

    val worklist = NodeCollector.fromBlockWalk(g.walkBlocks).to[mutable.Queue]
    val backEdges = g.getBlockBackEdges

    while (worklist.nonEmpty) {
      val block = worklist.dequeue()
      val value = transfer(block, block.getPreds.map(n => m(n.getBlock.asInstanceOf[Block])).toList)

      if (value != m(block)) {
        m(block) = value
        backEdges(block).foreach(worklist.enqueue(_))
      }
    }

    m.toMap.withDefaultValue(bot)
  }
}
