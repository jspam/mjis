package mjis.opt

import firm._
import firm.nodes._

import mjis.opt.FirmExtensions._
import mjis.opt.FirmExtractors.{ConstExtr, CmpExtr}
import mjis.util.MapExtensions._
import mjis.util._

import scala.collection.mutable
import scala.collection.JavaConversions._

object LoopUnrolling extends Optimization(needsBackEdges = true) {

  /** We only want to unroll the innermost loops.
    * Those are those IVs which dominate no other SCC */
  def innermostLoops(sccTree: Seq[SCCTreeNode[Block]], IVBlocks: Set[Block]): Seq[SCCLoop[Block]] =
    sccTree.collect({
      case loop: SCCLoop[Block]
        if IVBlocks(loop.dominator) && !loop.tree.exists(_.isInstanceOf[SCCLoop[Block]]) =>
          Seq(loop)
      case loop: SCCLoop[Block] => innermostLoops(loop.tree, IVBlocks)
    }).flatten


  val maxLoopNodeNum = 30
  var maxUnrollCount = 16

  def shouldUnroll(loop: SCCLoop[Block]): Boolean = {
    loop.nodes.map(BackEdges.getNOuts(_)).sum < maxLoopNodeNum
  }

  private def evalRel(rel: Relation, x: Int, y: Int) = new TargetValue(x, Mode.getIs).compare(new TargetValue(y, Mode.getIs)).contains(rel)

  def getIterationCount(rel: Relation, start: Int, step: Int, end: Int): Long = {
    if (!evalRel(rel, start, end)) 0
    else if (step > 0 && (start < end || rel.contains(Relation.Less)) ||
             step < 0 && (start > end || rel.contains(Relation.Greater))) {
      val sgn = step / Math.abs(step)
      var res = (end.toLong - start.toLong) * sgn
      if (rel.contains(Relation.Equal))
        res += 1
      if (res < 0)
        res += 1l << 32
      res / Math.abs(step)
    } else ((if (step > 0) Int.MaxValue.toLong + 1 else Int.MinValue.toLong - 1) - start.toLong) / step
  }

  override def _optimize(g: Graph): Unit = {
    val inductionVars /* "IVs" */ = g.getInductionVariables
    // nothing to do. Should be true for most graphs
    if (inductionVars.isEmpty) return

    val ivByVal = inductionVars.map(iv => iv.value -> iv).toMap[Node, InductionVariable]
    val cfGraph = g.getBlockGraph.transposed
    val sccTree = cfGraph.getSCCTree(g.getStartBlock)

    // look for inner loops with comparisons of an IV and a constant
    for (loop <- innermostLoops(sccTree, inductionVars.map(_.value.block).toSet) if shouldUnroll(loop)) {
      loop.dominator.nodes.foreach {
        case cmp@CmpExtr(rel, value, ConstExtr(end)) => ivByVal.get(value) match {
          case Some(InductionVariable(_, ConstExtr(start), ConstExtr(step), _)) =>
            val iterCount = getIterationCount(rel, start, step, end)
            // Since the range is statically known, we can predict the exact exit value of the unrolled loop
            unroll(loop, iterCount, start + (iterCount / maxUnrollCount * maxUnrollCount).toInt * step, cmp.asInstanceOf[Cmp])
          case _ =>
        }
        case _ =>
      }
    }

    def unroll(loop: SCCLoop[Block], iterCount: Long, exitVal: Int, cmp: Cmp): Unit = {
      val body = loop.nodes.tail
      val backJmp = loop.dominator.getPred(1) // in-loop jump back to the header

      // TODO: To handle inter-dependent phis in the loop header, we'd have to raise the phi permutation
      // to the power of `maxUnrollCount`
      loop.dominator.nodes.foreach({
        case phi: Phi if phi.getPred(1).block == loop.dominator && phi.getMode != Mode.getM => return
        case _ =>
      })

      // when copyBlocks looks up a phi in the loop header, return its value in the original body instead
      val innerLookup = loop.dominator.nodes.collect({
        // trivial memory phis are okay, but shouldn't be in the map
        case phi: Phi if phi.getPred(1).block != loop.dominator => phi -> phi.getPred(1)
      }).toMap[Node, Node]
      // save users of loop header phis, which we'll have to redirect in the end
      val phiUsers = innerLookup.keys.map(phi => phi -> BackEdges.getOuts(phi).filter(e => !loop.nodes.contains(e.node.getBlock)).toList)

      /* duplicate the loop body inside the loop `maxUnrollCount - 1` times */

      cmp.setRelation(Relation.LessGreater)
      // Make cmp always-false if there's not enough iterations
      cmp.setRight(if (iterCount < maxUnrollCount) cmp.getLeft else g.newConst(exitVal, Mode.getIs))

      var innerCopies = Map[Node, Node]().withDefault(identity)
      for (_ <- 1.until(maxUnrollCount))
      // use the previous copy as a reference frame via mapping through innerCopies
        innerCopies = copyBlocks(g, body, innerLookup.mapValues(innerCopies), innerCopies(backJmp))

      loop.dominator.setPred(1, innerCopies(backJmp))
      for (phi <- innerLookup.keys) phi.setPred(1, innerCopies(phi.getPred(1)))

      val epilogueLength = iterCount % maxUnrollCount
      if (epilogueLength != 0) {
        /* append an epilogue of further `epilogueLength` copies */

        // for the first copy, the reference frame is the loop header itself
        var outerLookup = innerLookup.keys.map(phi => phi -> phi).toMap[Node, Node]
        val succ = cfGraph.edges(loop.dominator).find(!loop.nodes.contains(_)).get

        var copies = Map[Node, Node]()
        for (_ <- 1.to(epilogueLength.toInt)) {
          copies = copyBlocks(g, body, outerLookup, succ.getPred(0))
          outerLookup = outerLookup.keys.map(phi => phi -> copies(innerLookup(phi))).toMap
          succ.setPred(0, copies(backJmp))
        }

        // update phi users to the last unrolled value
        for ((phi, edges) <- phiUsers; e <- edges)
          e.node.setPred(e.pos, outerLookup(phi))
      }
    }

    /** Appends `blocks` to `pred`, using `lookup` for looking up referenced nodes outside of `blocks`.
      * Returns a map from original to copied nodes and blocks. */
    def copyBlocks(g: Graph, blocks: Seq[Block], lookup: Map[Node, Node], pred: Node): Map[Node, Node] = {
      val copies = mutable.HashMap[Node, Node]().withPersistentDefault({ n =>
        if (n.getBlock == null || blocks.contains(n.getBlock))
          n.getGraph.copyNode(n)
        else n
      })
      for (block <- blocks) {
        copies(block).asInstanceOf[Block].setPreds((if (block == blocks.head) Seq(pred) else block.getPreds.map(copies)).toArray)

        for (n <- block.nodes) {
          copies(n).setBlock(copies(block))
          for ((pred, idx) <- n.getPreds.zipWithIndex)
            copies(n).setPred(idx, lookup.getOrElse(pred, copies(pred)))
        }
      }
      copies.toMap
    }
  }
}
