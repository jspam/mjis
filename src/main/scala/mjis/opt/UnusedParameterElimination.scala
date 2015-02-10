package mjis.opt

import firm.{MethodType, BackEdges, Graph}
import firm.nodes.{Address, Proj, Call}
import mjis.CallGraph
import mjis.opt.FirmExtensions._

import scala.collection.mutable.ArrayBuffer

object UnusedParameterElimination extends Optimization {
  override def optimize(): Boolean = {
    val callers = CallGraph.callerMap()
    CallGraph.graphsInTopologicalOrder.map(
      g => if (g.methodType.getNParams > 0) _optimize(g, callers(g)) else false
    ).toList.contains(true)
  }

  protected def _optimize(g: Graph, callers: ArrayBuffer[Call]): Boolean = {
    BackEdges.enable(g)
    // TODO: only count p.successors that aren't exclusively used as parameter to a call in the same SCC
    val usedArgProjs = g.getArgs.successors.collect({
      case p: Proj if p.successors.count({
        // Only count successors that aren't used as an argument to a recursive call
        case c: Call if c.getPtr.asInstanceOf[Address].getEntity == g.getEntity && c.getPred(p.getNum + 2) == p => false
        case _ => true
      }) >= 1 => p
    }).toSeq.sortBy(_.getNum)
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

      for ((call, idx) <- callers.zipWithIndex) {
        val newCall = g.newCall(call.getBlock, call.getMem, call.getPtr,
          usedArgNums.map(n => call.getPred(n + 2)).toArray, newMethodType).asInstanceOf[Call]
        exchange(call, newCall)
        callers(idx) = newCall
      }
      true
    } else false
  }

  override protected def _optimize(g: Graph): Unit = ???
}
