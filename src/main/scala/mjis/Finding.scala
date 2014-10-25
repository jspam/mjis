package mjis

abstract class Finding(val line: Int, val char: Int) {
  override def toString = s"Line $line, char $char"
}
