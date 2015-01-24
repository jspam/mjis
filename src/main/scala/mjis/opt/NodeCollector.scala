package mjis.opt

import firm.nodes._
import firm.{BlockWalker, Graph}
import FirmExtensions._

import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

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
    val dominators = g.getDominators
    val postOrder = ArrayBuffer[Block]()
    val edges = g.getBlockBackEdges

    def visit(block: Block): Unit = {
      if (!block.blockVisited) {
        block.markBlockVisited()

        // Make sure the last block of a loop in the linear order is always the one with a back jump
        // to the header. To ensure this, successors that lead into nested loops are visited last (so
        // they appear first in the reversed post order).
        val backEdgeDominators = block.getPreds.flatMap(_.block match {
          case b: Block if dominators(b).contains(block) => Some(dominators(b))
          case _ => None
        }).foldLeft(Set[Block]())(_ union _)
        val (loopSuccessors, nonLoopSuccessors) = edges(block).partition(backEdgeDominators.contains)
        nonLoopSuccessors.foreach(visit)
        loopSuccessors.foreach(visit)
        postOrder += block
      }
    }

    g.incBlockVisited()

    postOrder += g.getEndBlock
    g.getEndBlock.markBlockVisited()

    visit(g.getStartBlock)
    postOrder.reverse
  }
}
