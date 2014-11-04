package mjis

/** Describes the result of a single compiler phase.
  *
  * If O is an iterator, findings will only be reported up to the current position.
  */
trait Phase[O] {
  protected def getResult(): O
  def dumpResult(): Iterator[String]
  lazy val result = getResult()
  def findings: List[Finding]
}

trait AnalysisPhase[O] extends Phase[O] {
  def success: Boolean = findings.forall(_.severity != Severity.ERROR)
}
