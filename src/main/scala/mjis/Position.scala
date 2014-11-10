package mjis

class Position(val line: Int, val column: Int, val lineContents: String) {
  override def toString: String = s"$line:$column"
  def longString = lineContents.takeWhile(c => c != '\r' && c != '\n' && c != '\u001a') + System.lineSeparator() +
    lineContents.take(column-1).map(c => if (c.isWhitespace) c else ' ').mkString + "^"
}
