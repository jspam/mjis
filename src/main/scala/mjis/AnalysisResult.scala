package mjis

abstract class AnalysisResult[ResultType](successCallback: () => Boolean) {
  val findings: Stream[Finding] = Stream()
  val result: ResultType
  lazy val success = successCallback()
}
