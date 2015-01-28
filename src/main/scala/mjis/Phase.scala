package mjis

import java.io.BufferedWriter

/** Describes the result of a single compiler phase.
  *
  * If O is an iterator, findings will only be reported up to the current position.
  */
trait Phase[+O] {
  def name: String = getClass.getSimpleName.toLowerCase
  protected def getResult(): O
  def dumpResult(writer: BufferedWriter): Unit = {}
  lazy val result: O = getResult()
  def forceResult(): Unit = result
  def findings: List[Finding] = List()
}

trait AnalysisPhase[O] extends Phase[O] {
  def success: Boolean = findings.forall(_.severity != Severity.ERROR)
}
