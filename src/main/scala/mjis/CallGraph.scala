package mjis

import firm._
import firm.nodes.NodeVisitor.Default
import firm.nodes._

import mjis.opt.FirmExtensions._
import mjis.util.MapExtensions._
import mjis.util.Digraph

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.collection.mutable.{ListBuffer, ArrayBuffer}

object CallGraph {
  private def main = Program.getGraphs.find(_.getEntity.getLdName == "__main").get

  // Optimizations may eliminate calls, so be careful not to trust the information gotten
  // from this for longer than one optimization phase
  def calls(graph: Graph) = {
    val calls = mutable.ListBuffer[Call]()
    graph.walkTopological(new Default {
      override def visit(call: Call) = { calls += call }
    })
    calls
  }

  def isRecursive(graph: Graph) = {
    calls(graph).flatMap(_.getCalledGraph).contains(graph)
  }

  // TODO maybe use Digraph for this?
  def graphsInTopologicalOrder() = {
    val callMap = mutable.HashMap[Graph, ListBuffer[Call]]().withPersistentDefault(calls)
    def getCalledGraphs(graph: Graph) = { callMap(graph).map(_.getCalledGraph).flatMap(x => x) }
    val visited = mutable.ArrayBuffer[Graph](main)
    val stack = mutable.Stack[Graph]()
    stack.pushAll(getCalledGraphs(main))
    while (stack.nonEmpty) {
      visited += stack.top
      stack.pushAll(getCalledGraphs(stack.pop()).filter(!visited.contains(_)))
    }
    visited.reverseIterator
  }

  def callerMap(): scala.collection.Map[Graph, ArrayBuffer[Call]] = {
    val result = mutable.Map[Graph, ArrayBuffer[Call]]().withPersistentDefault(_ => ArrayBuffer())
    Program.getGraphs.foreach(_.walk(new Default {
      override def visit(call: Call) = call.getCalledGraph match {
        case Some(calledGraph) => result(calledGraph) += call
        case None =>
      }
    }))
    result
  }
}
