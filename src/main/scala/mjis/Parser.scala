package mjis

import scala.collection.immutable.Stream
import scala.collection.mutable.MutableList

object Parser {
  case class UnexpectedEOFError(pos: Position) extends Finding {
    def msg = "unexpected end of file"
    def severity = Severity.ERROR
  }

  case class UnexpectedTokenError(pos: Position, token: Token) extends Finding {
    def msg = s"unexpected token: $token"
    def severity = Severity.ERROR
  }
}

class Parser(tokens: BufferedIterator[Token]) extends AnalysisPhase[Any] {
  private val _findings = MutableList[Finding]()
  override def findings: List[Finding] = _findings.toList

  protected override def getResult() = {
    tokens.foreach(t => ()) // just exhaust the output for now
    ???
  }
  override def dumpResult() = ???

  def parseProgram(): Any = ???

  private def parseClassDeclaration() = ???
  private def parseClassMemberDeclaration() = ???
  // etc.
}
