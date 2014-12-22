package mjis

import firm.Util
import java.io.BufferedWriter
import mjis.util.FirmDumpHelper
import mjis.opt._
import scala.collection.JavaConversions._

class Optimizer(input: Unit) extends Phase[Unit] {

  override val findings = List[Finding]()

  override def dumpResult(writer: BufferedWriter): Unit = {
    firm.Program.getGraphs.foreach(FirmDumpHelper.dumpGraph(_, "-Optimizer"))
  }

  override protected def getResult(): Unit = {
    ConstantFolding.optimize()
    Normalization.optimize()
    CommonSubexpressionElimination.optimize()
    TrivialPhiElimination.optimize()
    LoopStrengthReduction.optimize()

    Util.lowerSels()
    Identities.optimize()
    CommonSubexpressionElimination.optimize()
  }

}
