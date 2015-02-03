package mjis.util

import scala.collection.Map
import scala.collection.mutable
import mjis.util.MapExtensions._

import scala.collection.mutable.ArrayBuffer

abstract class SCCTreeNode[V](val nodes: Seq[V])
case class SCCLeaf[V](node: V) extends SCCTreeNode[V](Seq(node))
case class SCCLoop[V](dominator: V, tree: Seq[SCCTreeNode[V]]) extends SCCTreeNode(tree.flatMap(_.nodes))

class Digraph[V](val edges: Map[V, Seq[V]]) {
 
  def getTopologicalSorting(start: V, visited: mutable.Set[V] = mutable.Set[V]()): Seq[V] = {
    val result = ArrayBuffer[V]()
    def walk(n: V): Unit = if (!visited(n)) {
      visited += n
      edges.getOrElse(n, Seq()).foreach(walk)
      result += n
    }
    walk(start)
    result.reverse
  }

  def transposed: Digraph[V] = {
    val reverse = mutable.ListMap[V, ArrayBuffer[V]]().withPersistentDefault(_ => ArrayBuffer[V]())
    for ((src, dests) <- edges) dests.foreach(reverse(_) += src)
    new Digraph(reverse)
  }

  def getSubgraph(nodes: scala.collection.Set[V]): Digraph[V] = new Digraph(edges.flatMap {
    case (src, dests) if nodes(src) => Some(src -> dests.filter(nodes))
    case _ => None
  })

  /** Kosaraju's algorithm. Returns SCCs and their respective dominating node (for reducible graphs)
    * in topological order, starting with the one containing `start`.
    */
  def findStronglyConnectedComponents(start: V): Seq[(V, Set[V])] = {
    val forwardVisited = mutable.Set[V]()
    val backwardVisited = mutable.Set[V]()
    val topo = getTopologicalSorting(start, forwardVisited)
    val transposed = this.getSubgraph(forwardVisited).transposed
    topo.flatMap { n =>
      transposed.getTopologicalSorting(n, backwardVisited) match {
        case Seq() => None
        case scc => Some((scc.head, scc.toSet))
      }
    }
  }

  def getSCCTree(start: V): Seq[SCCTreeNode[V]] = findStronglyConnectedComponents(start).map {
    case (_, scc) if scc.size == 1 => SCCLeaf[V](scc.head)
    case (dominator, scc) =>
      // break the cycle
      val tree = new Digraph(for ((src, dests) <- getSubgraph(scc).edges) yield src -> dests.filter(_ != dominator))
      SCCLoop[V](dominator, tree.getSCCTree(dominator))
  }

  def getCondensationGraph(components: Seq[SCCTreeNode[V]]): Digraph[SCCTreeNode[V]] = {
    val componentOfBlock = components.flatMap(c => c.nodes.map(_ -> c)).toMap
    def outEdges(c: SCCTreeNode[V]): Seq[SCCTreeNode[V]] =
      c.nodes.flatMap(n => edges(n).flatMap(componentOfBlock.get))
        .distinct
        .filter(_ != c)
    new Digraph(components.map(c => c -> outEdges(c)).toMap)
  }

}
