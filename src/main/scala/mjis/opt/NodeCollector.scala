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
}
