package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtractors._
import scala.collection.JavaConversions._

trait Optimization {
  def optimize(): Unit = {
    Program.getGraphs.foreach(optimize)
  }

  def optimize(g: Graph): Unit
}
