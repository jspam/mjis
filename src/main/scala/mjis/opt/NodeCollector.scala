package mjis.opt

import firm.nodes._

import scala.collection.mutable.ArrayBuffer

object NodeCollector {
  def fromWalk(walker: NodeVisitor => Unit): Seq[Node] = {
    val collector = new NodeCollector()
    walker(collector)
    collector.nodes
  }
}

class NodeCollector extends NodeVisitor.Default {
  val nodes = ArrayBuffer[Node]()

  override def defaultVisit(node: Node): Unit = nodes += node
}
