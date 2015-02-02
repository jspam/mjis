package mjis.opt

import firm.{MethodType, BackEdges, Graph}
import firm.nodes.{Proj, Call}
import mjis.CallGraph
import mjis.opt.FirmExtensions._

object UnusedParameterElimination extends Optimization {
  override def optimize(): Boolean = {
    val callers = CallGraph.callerMap()
    CallGraph.graphsInTopologicalOrder.map(
      g => if (g.methodType.getNParams > 0) _optimize(g, callers(g)) else false
    ).toList.contains(true)
  }

  protected def _optimize(g: Graph, callers: Seq[Call]): Boolean = {
    BackEdges.enable(g)
    // TODO: only count p.successors that aren't exclusively used as parameter to a call in the same SCC
    val usedArgProjs = g.getArgs.successors.filter(p => p.isInstanceOf[Proj] && p.successors.size >= 1).
      asInstanceOf[Seq[Proj]].sortBy(_.getNum)
    BackEdges.disable(g)

    if (usedArgProjs.size < g.methodType.getNParams) {
      val usedArgNums = usedArgProjs.map(_.getNum)

      val newMethodType = new MethodType(
        usedArgNums.map(g.methodType.getParamType).toArray,
        (0 until g.methodType.getNRess).map(g.methodType.getResType).toArray
      )
      g.getEntity.setType(newMethodType)

      for ((proj, idx) <- usedArgProjs.zipWithIndex) {
        proj.setNum(idx)
      }

      for (call <- callers) {
        exchange(call, g.newCall(call.getBlock, call.getMem, call.getPtr,
          usedArgNums.map(n => call.getPred(n + 2)).toArray, newMethodType))
      }
      true
    } else false
  }

  override protected def _optimize(g: Graph): Unit = ???
}
