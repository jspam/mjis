package mjis.opt

import firm.nodes._
import firm.{BlockWalker, Graph}
import FirmExtensions._

import scala.collection.mutable.ArrayBuffer

object NodeCollector {
  def fromWalk(walker: NodeVisitor => Unit): Seq[Node] = {
    val nodes = ArrayBuffer[Node]()

    walker(new NodeVisitor.Default {
      override def defaultVisit(node: Node): Unit = nodes += node
    })
    nodes
  }

  def fromBlockWalk(walker: BlockWalker => Unit): Seq[Block] = {
    val blocks = ArrayBuffer[Block]()

    walker(new BlockWalker {
      override def visitBlock(block: Block): Unit = blocks += block
    })
    blocks
  }

  def getBlocksInReverseBackEdgesPostOrder(g: Graph): Seq[Block] = {
    val postOrder = ArrayBuffer[Block]()
    val edges = g.getBlockBackEdges

    def visit(block: Block): Unit = {
      if (!block.blockVisited) {
        block.markBlockVisited()
        edges(block).foreach(visit)
        postOrder += block
      }
    }

    g.incBlockVisited()
    visit(g.getStartBlock)
    postOrder.reverse
  }
}
