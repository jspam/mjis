package mjis

import firm._
import java.io.BufferedWriter
import firm.nodes.Bad
import mjis.util.FirmDumpHelper
import mjis.opt._
import mjis.opt.FirmExtensions._
import scala.collection.JavaConversions._

class Optimizer(input: Unit, config: Config = Config()) extends Phase[Unit] {

  override val findings = List[Finding]()

  override def dumpResult(writer: BufferedWriter): Unit = {
    Program.getGraphs.foreach(FirmDumpHelper.dumpGraph(_, "Optimizer"))
  }

  def removeCriticalEdges(g: Graph): Unit = {
    val cfGraph = g.getBlockGraph.transposed
    g.walkBlocksWith { block =>
      if (block.getPreds.count(!_.isInstanceOf[Bad]) > 1)
        for ((pred, idx) <- block.getPreds.zipWithIndex if !pred.isInstanceOf[Bad])
          if (cfGraph.edges(pred.block).size > 1) {
            val newBlock = g.newBlock(Array(pred))
            block.setPred(idx, g.newJmp(newBlock))
          }
    }
  }

  var generalOptimizations = List(
    Identities, ConstantFolding, Normalization, CommonSubexpressionElimination, TrivialPhiElimination,
    LoopStrengthReduction, RedundantLoadElimination,
    PureFunctionCallElimination, LoopInvariantCodeMotion) ++
    (if (!config.useFirmBackend) Seq(ConditionalMoves) else Seq())

  def exec(optimizations: Seq[Optimization]): Unit = {
    val graphs: Iterator[Graph] = if (config.optimizeUnreachableGraphs) Program.getGraphs.iterator()
      else CallGraph.graphsInTopologicalOrder()
    for (g <- graphs)
      while (optimizations.map(opt => {
        val time = System.nanoTime()
        val changed = opt.optimize(g)
        if (config.printTimings)
          println(s"\t${opt.getClass.getSimpleName}:\t${(System.nanoTime() - time) / 1000000} ms\t" + (if (changed) "changed!" else ""))
        changed
      }).exists(identity)) {}
  }

  override protected def getResult(): Unit = {
    exec(generalOptimizations)
    Program.getGraphs.foreach(g => {
      bindings.binding_irgopt.remove_bads(g.ptr)
      bindings.binding_irgopt.remove_unreachable_code(g.ptr)
    })
    UnusedParameterElimination.optimize()
    if (config.inlining)
      Inlining.optimize()
    exec(generalOptimizations)
    LoopUnrolling.optimize()
    exec(generalOptimizations :+ ConditionalConstants)

    CallGraph.graphsInTopologicalOrder().foreach(g => {
      removeCriticalEdges(g)
      bindings.binding_irgopt.remove_bads(g.ptr)
      bindings.binding_irgopt.remove_unreachable_code(g.ptr)
    })

    if (config.firmDump)
      dumpResult(null)
  }

}
