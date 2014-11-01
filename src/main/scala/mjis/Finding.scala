package mjis

trait Finding {
  def pos: Position
  def severity: Severity
  def msg: String

  override def toString = s"$pos $severity: $msg${System.lineSeparator}${pos.longString}"
}
