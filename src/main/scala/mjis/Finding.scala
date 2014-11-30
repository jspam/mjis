package mjis

import java.io.Writer

trait Finding {
  def pos: Position
  def severity: Severity
  def msg: String

  override def toString = s"$pos $severity: $msg"
}

object Finding {
  def printAll(findings: Seq[Finding], sourceLines: Iterable[String], writer: Writer): Unit = {
    var curLine = 1
    var curSourceLines = sourceLines
    findings sortBy (_.pos) foreach (f => {
      writer.write(f.toString)
      writer.write(System.lineSeparator)
      if (f.pos.line >= 1) {
        curSourceLines = curSourceLines.drop(f.pos.line - curLine)
        curLine = f.pos.line
        curSourceLines.headOption match {
          case Some(lineContents) =>
            writer.write(f.pos.longString(lineContents))
            writer.write(System.lineSeparator)
          case _ =>
        }
      }
    })
  }
}
