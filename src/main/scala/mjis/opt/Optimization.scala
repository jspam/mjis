package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtractors._
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

abstract class Optimization(needsBackEdges: Boolean = false) {

  def optimize(): Boolean = {
    // always optimize all graphs
    Program.getGraphs.map(optimize).toList.exists(b => b)
  }

  var changed = false

  final def optimize(g: Graph): Boolean = {
    changed = false
    if (needsBackEdges)
      BackEdges.enable(g)
    _optimize(g)
    if (needsBackEdges)
      BackEdges.disable(g)
    changed
  }

  protected def _optimize(g: Graph): Unit

  protected def exchange(oldNode: Node, newNode: Node): Unit = {
    GraphBase.exchange(oldNode, newNode)
    changed = true
  }
  protected def setPred(node: Node, idx: Int, pred: Node): Unit = {
    node.setPred(idx, pred)
    changed = true
  }

  /**
   * "Deletes" a memory node by redirecting the graph's memory flow
   */
  protected def killMemoryNode(node: Node): Unit = node match {
    case _: Div | _: Mod | _: Store | _: Load =>
      for (proj@ProjExtr(_, Div.pnM /* == Mod.pnM == ... */) <- BackEdges.getOuts(node).map(_.node))
        exchange(proj, node.getPred(0))
  }
}

abstract class DeferredOptimization(needsBackEdges: Boolean = false) extends Optimization(needsBackEdges) {

  private val exchanges = mutable.Map[Node, Node]()
  private val setPreds = ArrayBuffer[(Node, Int, Node)]()

  private def getNewNode(oldNode: Node): Node = exchanges.get(oldNode).map(getNewNode).getOrElse(oldNode)

  protected final override def _optimize(g: Graph): Unit = {
    exchanges.clear()
    setPreds.clear()

    __optimize(g)

    for ((node, idx, pred) <- setPreds) super.setPred(getNewNode(node), idx, getNewNode(pred))
    for (oldNode <- exchanges.keys) super.exchange(oldNode, getNewNode(oldNode))
  }

  // I feel slightly silly, but full decorators would feel slightly sillier
  protected def __optimize(g: Graph): Unit

  protected override def exchange(oldNode: Node, newNode: Node): Unit = {
    assert(oldNode != newNode)
    exchanges(oldNode) = newNode
  }
  protected override def setPred(node: Node, idx: Int, pred: Node): Unit = setPreds += ((node, idx, pred))
}

abstract class NodeBasedOptimization() extends Optimization(needsBackEdges = true) {

  private val queue = mutable.Queue[Node]()

  protected final override def _optimize(g: Graph): Unit = {
    beforeOptimize(g)
    queue.clear()
    queue ++= NodeCollector.fromWalk(g.walk)
    while (queue.nonEmpty)
      _optimize(g, queue.dequeue())
  }

  protected def beforeOptimize(g: Graph): Unit = {}
  protected def _optimize(g: Graph, node: Node): Unit

  protected override def exchange(oldNode: Node, newNode: Node): Unit = {
    queue.dequeueAll(_ == oldNode)
    queue ++= BackEdges.getOuts(oldNode).map(_.node)
    super.exchange(oldNode, newNode)
  }
}
