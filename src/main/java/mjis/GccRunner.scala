package mjis

import java.io.{InputStreamReader, BufferedReader, BufferedWriter}

import scala.collection.mutable.ListBuffer

class GccRunner(a: Unit, config: Config) extends Phase[Unit] {
  override protected def getResult(): Unit = {
    val gcc = Runtime.getRuntime.exec(s"gcc -o ${config.outFile} -m64 ${config.asmOutFile}")
    val stderr = new BufferedReader(new InputStreamReader(gcc.getErrorStream))
    gcc.waitFor()
    val stream = Stream.continually(stderr.readLine()).takeWhile(_ != null)
    if (gcc.exitValue() != 0 || stream.nonEmpty) {
      _findings += new Finding() {
        override def pos: Position = Position.NoPosition
        override def msg: String = s"GCC returned exit status ${gcc.exitValue}\n${stream.mkString("\n")}"
        override def severity: Severity = Severity.ERROR
      }
    }
  }

  val _findings = ListBuffer[Finding]()

  override def findings: List[Finding] = _findings.toList

  override def dumpResult(writer: BufferedWriter): Unit = {}
}
