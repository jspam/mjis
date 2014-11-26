package mjis

import java.nio.file.{Path, Files}

trait Finding {
  def pos: Position
  def severity: Severity
  def msg: String

  override def toString = s"$pos $severity: $msg${System.lineSeparator}"

  def toString(file: Option[Path]) = {
    val lineContent = file.map(f => Files.lines(f).skip(pos.column).findFirst.get)
    s"$pos $severity: $msg${System.lineSeparator}${pos.longString(lineContent)}"
  }
}
