package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtractors._
import mjis.util.MapExtensions._
import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * value = Phi(start, incrAdd)
 * ...
 * incrAdd = Add(value, incr)
 */
case class InductionVariable(value: Phi, start: Const, incr: Const, incrAdd: Add)

object FirmExtensions {

  implicit class GraphExt(g: Graph) {

    /**
     * "Deletes" a div or mod node by redirecting the graph's memory flow
     */
    def deleteDivOrMod(node: Node): Unit = {
      for (proj@ProjExtr(_, Div.pnM /* == Mod.pnM */) <- BackEdges.getOuts(node).map(_.node))
        GraphBase.exchange(proj, node.getPred(0))
    }

    // TODO: Use ir_edgekind_t.EDGE_KIND_BLOCK if we're allowed to
    def getBlockBackEdges: Map[Block, Set[Block]] = {
      val m = mutable.Map[Block, Set[Block]]().withPersistentDefault(_ => Set.empty)
      g.walkBlocks(new BlockWalker {
        override def visitBlock(block: Block): Unit = {
          for (pred <- block.getPreds)
            m(pred.getBlock.asInstanceOf[Block]) += block
        }
      })
      m.toMap.withDefaultValue(Set.empty)
    }

    def getDominators: Map[Block, Set[Block]] = {
      DataFlowAnalysis.iterateBlocks[Set[Block]](g, NodeCollector.fromBlockWalk(g.walkBlocks).toSet,
        (node, dominators) => dominators match {
          case Nil => Set(node)
          case _ => dominators.reduce(_.intersect(_)) + node
        }
      )
    }

    def getInductionVariables: Seq[InductionVariable] = {
      // TODO: recognize loop-invariant start and incr nodes instead of just constant ones
      NodeCollector.fromWalk(g.walk).map(node => node match {
        case PhiExtr(start: Const, incrAdd@AddExtr(`node`, incr: Const)) =>
          Some(InductionVariable(node.asInstanceOf[Phi], start, incr, incrAdd.asInstanceOf[Add]))
        case _ => None
      }).flatten
    }

  }

  implicit class NodeExt(node: Node) {

    def block: Block = node.getBlock.asInstanceOf[Block]

    def idx: Int = bindings.binding_irnode.get_irn_idx(node.ptr)

  }

}
