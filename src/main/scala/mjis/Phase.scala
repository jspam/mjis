package mjis

trait Phase[ResultType] {
  protected def getResult(): ResultType
  def dumpResult(): String
  lazy val result = getResult()
  protected def getFindings(): List[Finding]
  lazy val findings = getFindings()
}

trait AnalysisPhase[ResultType] extends Phase[ResultType] {
  lazy val success = findings.forall(_.severity != Severity.ERROR)
}
