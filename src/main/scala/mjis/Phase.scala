package mjis

trait Phase[ResultType] {
  def getResult(): ResultType
  lazy val result = getResult()
  var findings : Stream[Finding] = Stream()
}

trait AnalysisPhase {
  def successCallback(): Boolean
  lazy val success = successCallback()
}
