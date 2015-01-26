package mjis.util

import scala.collection.Map
import scala.collection.mutable
import mjis.util.MapExtensions._

import scala.collection.mutable.ArrayBuffer

object Digraph {
  def getTopologicalSorting[V](edges: Map[V, Seq[V]], start: V, nodes: Option[Set[V]] = None, visited: mutable.Set[V] = mutable.Set[V]()): Seq[V] = {
    val result = ArrayBuffer[V]()
    def walk(n: V): Unit = if (nodes.forall(_(n)) && !visited(n)) {
      visited += n
      edges.getOrElse(n, Seq()).foreach(walk)
      result += n
    }
    walk(start)
    result.reverse
  }

  def transpose[V](edges: Map[V, Seq[V]]): mutable.Map[V, ArrayBuffer[V]] = {
    val res = mutable.ListMap[V, ArrayBuffer[V]]().withPersistentDefault(_ => ArrayBuffer[V]())
    for ((src, dests) <- edges) dests.foreach(res(_) += src)
    res
  }

  /** Kosaraju's algorithm. Returns SCCs and their respective dominating node (for reducible graphs)
    * in topological order, starting with the one containing `start`.
    */
  def findStronglyConnectedComponents[V](edges: Map[V, Seq[V]], start: V, nodes: Option[Set[V]] = None): Seq[(V, Set[V])] = {
    val visited = mutable.Set[V]()
    val transposed = transpose(edges)
    getTopologicalSorting(edges, start, nodes).flatMap { n =>
      getTopologicalSorting(transposed, n, nodes, visited) match {
        case Seq() => None
        case scc => Some((scc.head, scc.toSet))
      }
    }
  }
}
