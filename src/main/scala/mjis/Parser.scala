package mjis

import scala.collection.immutable.Stream
import scala.collection.mutable.MutableList

object Parser {
  case class UnexpectedEOFError(line: Int, char: Int) extends Finding {
    def msg = "unexpected end of file"
    def severity = Severity.ERROR
  }

  case class UnexpectedTokenError(line: Int, char: Int, token: Token) extends Finding {
    def msg = s"unexpected token: $token"
    def severity = Severity.ERROR
  }
}

class Parser(private val tokens: Stream[Token]) extends AnalysisPhase[Any] {
  private val _findings = MutableList[Finding]()
  protected override def getFindings(): List[Finding] = _findings.toList

  protected override def getResult() = ???
  override def dumpResult() = ???

  def parseProgram(): Any = ???

  private def parseClassDeclaration() = ???
  private def parseClassMemberDeclaration() = ???
  // etc.
}
