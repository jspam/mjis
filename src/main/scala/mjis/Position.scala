package mjis

import System.{ lineSeparator => n }
import mjis.util.LineReader

object Position {
  def apply(line: Int, column: Int, nameMe: String = "") = new Position((line.toLong << 32) + column)
}
class Position(val __value: Long) extends AnyVal {
  def line: Int = (__value >> 32).toInt
  def column: Int = __value.toInt
  override def toString: String = s"$line:$column"
  def longString(lineContents: String): String = longString(Some(lineContents))
  def longString(lineContents: Option[String]): String =
    lineContents.map(
      lineContents => s"${lineContents.takeWhile(c => c != '\r' && c != '\n' && c != LineReader.eof)}$n${lineContents.take(column - 1).map(c => if (c.isWhitespace) c else ' ').mkString}^").getOrElse("")
}
