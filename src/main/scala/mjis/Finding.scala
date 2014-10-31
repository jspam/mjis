package mjis

trait Finding {
  def line: Int
  def char: Int
  def severity: Severity
  def msg: String

  override def toString = s"Line $line, position $char: $msg"
}
