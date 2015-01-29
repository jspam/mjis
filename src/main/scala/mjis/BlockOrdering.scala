package mjis

import mjis.asm._
import mjis.util.{SCCLoop, SCCLeaf, SCCTreeNode, Digraph}

class BlockOrdering(program: AsmProgram) extends Phase[AsmProgram] {

  private def optimizeBlockOrder(function: AsmFunction) = {
    val g = new Digraph(function.basicBlocks.map(b => b -> b.successors).toMap)

    def rec(scc: SCCTreeNode[AsmBasicBlock]): Seq[AsmBasicBlock] = scc match {
      case SCCLeaf(block) => Seq(block)
      case l@SCCLoop(_, tree) =>
        val layoutedComponents = tree.map(rec)
        val inSCC = scc.nodes.toSet
        val componentOfNode = tree.flatMap(c => c.nodes.map(_ -> c)).toMap
        // try to put loop conditions last
        val orderedComponents = tree.find {
          case SCCLeaf(block) => block.successors.size > 1 && block.successors.exists(!inSCC(_))
          case _ => false
        } match {
          case Some(cond) =>
            val loopSucc = cond.nodes(0).successors.find(inSCC).get
            // use tree rooted at `loopSucc` and append `cond`
            g.getCondensationGraph(tree).getTopologicalSorting(componentOfNode(loopSucc)).filter(_ != cond) :+ cond
          case None => tree
        }
        orderedComponents.map(rec).flatten
    }

    function.basicBlocks = g.getSCCTree(function.prologue).map(rec).flatten.toList
  }

  override def getResult(): AsmProgram = {
    for (function <- program.functions) {
      optimizeBlockOrder(function)
      for (Seq(block, next) <- function.basicBlocks.sliding(2)) {
        def jmp(dest: AsmBasicBlock) = dest match {
          case `next` => Seq()
          case _ => Seq(asm.Jmp(BasicBlockOperand(dest)))
        }
        block.instructions ++= (block.successors.size match {
          case 2 =>
            // Prefer fallthrough
            if (block.successors(0) == next)
              Seq(JmpConditional(BasicBlockOperand(block.successors(1)), block.relation.negated))
            else {
              Seq(JmpConditional(BasicBlockOperand(block.successors(0)), block.relation)) ++ jmp(block.successors(1))
            }
          case 1 => jmp(block.successors(0))
          case 0 => Seq()
          case _ => ???
        })
      }
    }
    program
  }

}
