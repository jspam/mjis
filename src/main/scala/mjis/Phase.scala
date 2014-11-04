package mjis

/** Describes the result of a single compiler phase.
  *
  * If ResultType is an iterator, findings will only be reported up to the current position.
  */
trait Phase[ResultType] {
  protected def getResult(): ResultType
  def dumpResult(): Iterator[String]
  lazy val result = getResult()
  def findings: List[Finding]
}

trait AnalysisPhase[ResultType] extends Phase[ResultType] {
  def success: Boolean = findings.forall(_.severity != Severity.ERROR)
}
