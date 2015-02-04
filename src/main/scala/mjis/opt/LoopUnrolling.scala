package mjis.opt

import firm._
import firm.nodes._

import mjis.opt.FirmExtensions._
import mjis.opt.FirmExtractors.ConstExtr
import mjis.util.MapExtensions._
import mjis.util._

import scala.collection.mutable
import scala.collection.JavaConversions._

object LoopUnrolling extends Optimization(needsBackEdges = true) {

  val maxUnrolledLoopNodeNum = 200

  /** We only want to unroll the innermost loops.
    * Those are those IVs which dominate no other SCC */
  def innermostLoops(sccTree: Seq[SCCTreeNode[Block]], IVBlocks: Set[Block]): Seq[SCCLoop[Block]] =
    sccTree.collect({
      case loop: SCCLoop[Block]
        if IVBlocks(loop.dominator) && !loop.tree.exists(_.isInstanceOf[SCCLoop[Block]]) =>
          Seq(loop)
      case loop: SCCLoop[Block] => innermostLoops(loop.tree, IVBlocks)
    }).flatten

  def shouldUnroll(loop: SCCLoop[Block], cfGraph: Digraph[Block]): Boolean = {
    // TODO: To handle inter-dependent phis in the loop header, we'd have to raise the phi permutation
    // to the power of `maxUnrollCount`
    loop.dominator.nodes.foreach({
      case phi: Phi if phi.getPred(1).block == loop.dominator && phi.getMode != Mode.getM => return false
      case _ =>
    })

    // loop may have at most one exit (through the header)
    loop.nodes.tail.forall(cfGraph.edges(_).forall(succ => loop.nodes.contains(succ)))
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

    val dominators = g.getDominators
    val ivByVal = inductionVars.map(iv => iv.value -> iv).toMap[Node, InductionVariable]
    val cfGraph = g.getBlockGraph.transposed
    val sccTree = cfGraph.getSCCTree(g.getStartBlock)

    // look for inner loops with comparisons of an IV and a constant
    for (loop <- innermostLoops(sccTree, inductionVars.map(_.value.block).toSet) if shouldUnroll(loop, cfGraph)) {
      loop.dominator.nodes.foreach {
        case cmp: Cmp => {
          if (ivByVal.contains(cmp.getRight))
            exchange(cmp, g.newCmp(cmp.block, cmp.getRight, cmp.getLeft, cmp.getRelation.inversed))

          (ivByVal.get(cmp.getLeft), cmp.getRight) match {
            case (Some(iv@InductionVariable(_, ConstExtr(start), _, _)), ConstExtr(end)) =>
              val iterCount = getIterationCount(cmp.getRelation, start, iv.incrVal, end)
              // Since the range is statically known, we can predict the exact exit value of the unrolled loop
              unrollStatic(new Unroller(loop), start, iterCount, iv, cmp.asInstanceOf[Cmp])
            case (Some(iv), end) =>
              // `end` has to strictly dominate the loop
              if (dominators(loop.dominator).contains(end.block) && loop.dominator != end.block) {
                val rel = if (iv.incrVal > 0) cmp.getRelation else cmp.getRelation.inversed()
                // `rel` should 'point' in the opposite direction of `iv.incr`
                if (rel == Relation.Less || rel == Relation.LessEqual)
                  unrollDynamic(new Unroller(loop), iv, cmp.asInstanceOf[Cmp])
              }
            case _ =>
          }
        }
        case _ =>
      }
    }

    def getUnrollCount(loop: SCCLoop[Block]): Int = {
      var cnt = 1
      val blockAndNodeCount = loop.nodes.tail.size + loop.nodes.tail.map(_.nodes.size).sum
      while (cnt * blockAndNodeCount * 2 <= maxUnrolledLoopNodeNum) cnt *= 2
      cnt
    }

    def unrollStatic(unroller: Unroller, start: Int, iterCount: Long, iv: InductionVariable, cmp: Cmp): Unit = {
      val unrollCount = getUnrollCount(unroller.loop)
      if (unrollCount < 2) return
      println(s"${g.getEntity.getLdName}: static unrolling by $unrollCount")

      val exitVal = start + (iterCount / unrollCount * unrollCount).toInt * iv.incrVal
      cmp.setRelation(Relation.LessGreater)
      // Make cmp always-false if there's not enough iterations
      cmp.setRight(if (iterCount < unrollCount) cmp.getLeft else g.newConst(exitVal, Mode.getIs))

      if (iterCount >= unrollCount)
        unroller.unrollInLoop(unrollCount)

      val epilogueLength = iterCount % unrollCount
      if (epilogueLength != 0)
        unroller.unrollOutside(epilogueLength.toInt)
      unroller.finish()
    }

    def overflows(x: Long): Boolean = x < Int.MinValue || x > Int.MaxValue

    /**
     * Unrolls
     *   while (i < end) $body
     * to something like
     *   while (i < end - 3) 4*$body
     *   if (i < end - 1) 2*$body
     *   if (i < end) $body
     */
    def unrollDynamic(unroller: Unroller, iv: InductionVariable, cmp: Cmp): Unit = {
      // high unrollCounts would create long epilogues
      var unrollCount = getUnrollCount(unroller.loop) min 4
      if (unrollCount < 2) return
      println(s"${g.getEntity.getLdName}: dynamic unrolling by $unrollCount")

      // pre-check of form `start < end`
      var needsPreCheck = false
      val dominator = unroller.loop.dominator

      val end = cmp.getRight

      def delta = -iv.incrVal * (unrollCount-1)
      def getCurrentEnd(block: Node) = g.newAdd(block,
        end,
        g.newConst(delta, end.getMode),
        end.getMode)

      // statically check for possibility of overflows in `getCurrentEnd`
      end match {
        case ConstExtr(endVal) if !overflows(endVal.toLong + delta) =>
        case _ =>
          iv.start match {
            case ConstExtr(start) if !overflows(start + delta) =>
              needsPreCheck = true
            // now we can be sure that `rel(start, end)`, so `end + delta` shouldn't overflow, either
            case _ => return
          }
      }
      cmp.setRight(getCurrentEnd(cmp.block))

      unroller.unrollInLoop(unrollCount)

      // epilogue
      while (unrollCount > 1) {
        unrollCount /= 2
        val preBlock = g.newBlock(unroller.succ.getPreds.toArray)
        val cond = g.newCond(preBlock,
          g.newCmp(preBlock,
            unroller.outerLookup(iv.value),
            getCurrentEnd(preBlock),
            cmp.getRelation))

        unroller.succ.setPred(0, g.newProj(cond, Mode.getX, Cond.pnTrue))
        // save the lookup before the true branch and use it in the merging phi creation
        val lookup = unroller.outerLookup
        unroller.unrollOutside(unrollCount)
        unroller.mkPhis(g.newProj(cond, Mode.getX, Cond.pnFalse), lookup)
      }

      if (needsPreCheck) {
        // pre-check context is the values before loop entry, i.e. the first pred
        val block = g.newBlock(Array(dominator.getPred(0)))
        val cond = g.newCond(block, g.newCmp(block, iv.start, end, cmp.getRelation))
        dominator.setPred(0, g.newProj(cond, Mode.getX, Cond.pnTrue))
        unroller.mkPhis(g.newProj(cond, Mode.getX, Cond.pnFalse), _.getPred(0))
      }

      unroller.finish()
    }

    class Unroller(val loop: SCCLoop[Block]) {
      val body = loop.nodes.tail
      val backJmp = loop.dominator.getPred(1) // in-loop jump back to the header

      // when copyBlocks looks up a phi in the loop header, return its value in the original body instead
      val innerLookup = loop.dominator.nodes.collect({
        // trivial memory phis are okay, but shouldn't be in the map
        case phi: Phi if phi.getPred(1).block != loop.dominator => phi -> phi.getPred(1)
      }).toMap[Node, Node]
      // save users of loop header phis, which we'll have to redirect in the end
      val phiUsers = innerLookup.keys.map(phi => phi -> BackEdges.getOuts(phi).filter(e => !loop.nodes.contains(e.node.getBlock)).toList)
      var innerCopies = Map[Node, Node]().withDefault(identity)

      // for the first copy, the reference frame is the loop header itself
      var outerLookup = innerLookup.keys.map(phi => phi -> phi).toMap[Node, Node]
      var outerCopies = Map[Node, Node]()

      // the loop's original successor node
      val succ = cfGraph.edges(loop.dominator).find(!loop.nodes.contains(_)).get

      def unrollInLoop(count: Int): Unit = {
        for (_ <- 1 until count) // minus the original body
        // use the previous copy as a reference frame via mapping through innerCopies
          innerCopies = copyBlocks(g, body, innerLookup.mapValues(innerCopies), innerCopies(backJmp))

        loop.dominator.setPred(1, innerCopies(backJmp))
        for (phi <- innerLookup.keys) phi.setPred(1, innerCopies(phi.getPred(1)))
      }

      /** Inserts further unrollings between the loop and succ */
      def unrollOutside(count: Int): Unit = {
        for (_ <- 1 to count) {
          outerCopies = copyBlocks(g, body, outerLookup, succ.getPred(0))
          outerLookup = outerLookup.keys.map(phi => phi -> outerCopies(innerLookup(phi))).toMap
          succ.setPred(0, outerCopies(backJmp))
        }
      }

      /** Merges control flow of `firstPred` and the current epilogue */
      def mkPhis(firstPred: Node, phiArgsFirstPred: Node => Node): Unit = {
        val postBlock = g.newBlock(Array(firstPred, succ.getPred(0)))
        outerLookup = outerLookup.map {
          case (phi, target) => phi -> g.newPhi(postBlock, Array(phiArgsFirstPred(phi), target), phi.getMode)
        }
        succ.setPred(0, g.newJmp(postBlock))
      }

      /** Updates phi users to the last unrolled value */
      def finish(): Unit = {
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
