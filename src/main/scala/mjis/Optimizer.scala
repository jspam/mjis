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

  val highLevelOptimizations = List(LoopStrengthReduction)
  val generalOptimizations = List(
    ConstantFolding, Normalization, CommonSubexpressionElimination, TrivialPhiElimination, Identities)

  def exec(optimizations: List[Optimization]): Unit = {
    // always run all optimizations
    while (optimizations.map(_.optimize()).exists(b => b)) {}
  }

  override protected def getResult(): Unit = {
    exec(generalOptimizations ++ highLevelOptimizations)
    Util.lowerSels()
    exec(generalOptimizations)
  }

}
