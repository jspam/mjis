package mjis.util

import mjis.Position

/** Mutable version of scala.util.parsing.input.StreamReader.
  * The current input line is buffered for direct access.
  */
object LineReader {
  final val eof = '\u001a' // The SUB control character. Any char outside the spec will do.
}

class LineReader(input: java.io.Reader) {
  import LineReader._
  private var line = 1
  var offset = 0
  var lastLine = false
  var source = readLine()

  def pos = Position(line, offset + 1)
  def currentChar = source(offset)
  def atEnd = lastLine && offset >= source.length - 1 // at EOF or beyond

  private def readLine(): String = {
    val sb = new StringBuffer()
    var next: Int = 0

    do {
      next = input.read()
      if (next == -1) {
        lastLine = true
        sb.append(eof)
      } else
        sb.append(next.toChar)
    } while (next != -1 && next != '\n')

    sb.toString
  }

  def consume(): Char = {
    val c = currentChar
    offset += 1
    if (offset >= source.length) {
      line += 1
      offset = 0
      source = readLine()
    }
    c
  }
}
