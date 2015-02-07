package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtractors._
import mjis.util.Digraph
import scala.collection.JavaConversions._
import scala.collection.immutable.ListMap
import scala.collection.mutable.ArrayBuffer

/**
 * value = Phi(start, incrAdd)
 * ...
 * incrAdd = Add(value, incr)
 */
case class InductionVariable(value: Phi, start: Node, incr: Const, incrAdd: Add) {
  def incrVal = incr.getTarval.asInt
}

object FirmExtensions {

  implicit class GraphExt(g: Graph) {

    def getBlockGraph: Digraph[Block] = new Digraph(ListMap(
      NodeCollector.fromBlockWalk(g.walkBlocks)
      .map(b => b -> b.getPreds.map(_.block).filter(_ != null).toSeq): _*
    ))

    def getDominators: Map[Block, Set[Block]] = {
      DataFlowAnalysis.iterateBlocks[Set[Block]](g, NodeCollector.fromBlockWalk(g.walkBlocks).toSet,
        (node, dominators) => dominators match {
          case Nil => Set(node)
          case _ => dominators.reduce(_.intersect(_)) + node
        }
      )
    }

    def getInductionVariables: Seq[InductionVariable] = {
      val result = ArrayBuffer[InductionVariable]()
      g.walkWith { node => node match {
        case PhiExtr(start, incrAdd@AddExtr(`node`, incr: Const)) =>
          result += InductionVariable(node.asInstanceOf[Phi], start, incr, incrAdd.asInstanceOf[Add])
        case _ =>
      }}
      result
    }

    def methodType: MethodType = g.getEntity.getType.asInstanceOf[MethodType]

    def walkWith(walker: Node => Unit) = {
      g.walk(new NodeVisitor.Default {
        override def defaultVisit(node: Node): Unit = walker(node)
      })
    }

    def walkTopologicalWith(walker: Node => Unit) = {
      g.walkTopological(new NodeVisitor.Default {
        override def defaultVisit(node: Node): Unit = walker(node)
      })
    }

    def walkBlocksWith(walker: Block => Unit) = {
      g.walkBlocks(new BlockWalker {
        override def visitBlock(block: Block): Unit = walker(block)
      })
    }

    def nodeCount = {
      var numNodes = 0
      g.walkWith(_ => numNodes += 1)
      numNodes
    }
  }

  implicit class CallExt(call: Call) {
    def getCalledGraph: Option[Graph] = {
      val ent = call.getPtr.asInstanceOf[Address].getEntity
      Option(ent.getGraph)
    }
  }

  implicit class NodeExt(node: Node) {

    def block: Block = node match {
      case _: Bad => null
      case _ => node.getBlock match {
        case b: Block => b
        case _ => null
      }
    }

    def successors: Iterable[Node] = {
      BackEdges.getOuts(node).map(_.node)
    }

    def idx: Int = bindings.binding_irnode.get_irn_idx(node.ptr)

  }

  implicit class BlockExt(block: Block) {

    def nodes: Iterable[Node] = block.successors.filter(_.getBlock == block)

    def predBlock(n: Int = 0): Block = block.getPred(n).block

  }

}
