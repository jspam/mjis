package mjis

import System.{ lineSeparator => n }
import mjis.util.LineReader

object Position {
  def apply(line: Int, column: Int, nameMe: String = "") = new Position((line.toLong << 32) + column)
  val NoPosition = Position(-1, -1, "NO POSITION")
  val SyntheticPosition = Position(-1, -1, "Synthetic Position for builtins ")
}
class Position(val __value: Long) extends AnyVal with Ordered[Position] {
  def line: Int = (__value >> 32).toInt
  def column: Int = __value.toInt
  override def compare(that: Position) = __value.compare(that.__value)
  override def toString: String = s"$line:$column"
  def longString(lineContents: String): String =
      s"${lineContents.takeWhile(c => c != '\r' && c != '\n' && c != LineReader.eof)}$n${lineContents.take(column - 1).map(c => if (c.isWhitespace) c else ' ').mkString}^"
}
