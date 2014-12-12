package mjis

import firm._
import firm.nodes._
import java.io.BufferedWriter
import mjis.util.FirmDumpHelper
import scala.collection.JavaConversions._

class Optimizer(input: Unit) extends Phase[Unit] {
  override val findings = List[Finding]()

  override def dumpResult(writer: BufferedWriter): Unit = {
    firm.Program.getGraphs.foreach(FirmDumpHelper.dumpGraph(_, "-Optimizer"))
  }

  override protected def getResult(): Unit = firm.Program.getGraphs.foreach(optimizeGraph)

  private def optimizeGraph(g: Graph): Unit = {
    BackEdges.enable(g)

    g.walk(new NodeVisitor.Default {
      override def visit(node: Phi): Unit = {
        // skip memory phis
        if (node.getMode == Mode.getM)
          return

        val preds = node.getPreds.toSet - node

        // trivial phi
        if (preds.size == 1) {
          val users = BackEdges.getOuts(node).toList
          GraphBase.exchange(node, preds.head)
          // recurse into all users which may have become trivial too
          users.foreach(_.node match {
            case phi: Phi => visit(phi)
            case _ =>
          })
        }
      }
    })

    BackEdges.disable(g)
  }
}
