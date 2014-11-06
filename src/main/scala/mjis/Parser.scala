package mjis

import scala.collection.mutable.MutableList

import mjis.TokenData._

object Parser {
  case class UnexpectedEOFError(pos: Position) extends Finding {
    def msg = "unexpected end of file"
    def severity = Severity.ERROR
  }

  case class UnexpectedTokenError(token: Token) extends Finding {
    def msg = s"unexpected token: ${token.data}"
    def severity = Severity.ERROR
    override def pos: Position = token.pos
  }
}

class Parser(tokens: BufferedIterator[Token]) extends AnalysisPhase[Any] {
  private val _findings = MutableList[Finding]()
  private def atEOF = !tokens.hasNext
  private def currentToken: Option[Token] = if (!atEOF) Some(tokens.head) else None
  private var posAfterCurrentToken: Position = new Position(0, 1, "")
  override def findings: List[Finding] = _findings.toList

  protected override def getResult() = {
    parseProgram()
  }
  override def dumpResult() = ???

  private def consume(t: Token) = {
    posAfterCurrentToken = new Position(t.pos.line, t.pos.column + t.data.literal.length(), t.pos.lineContents)
    tokens.next()
  }

  private def expect[ReturnType](pred: TokenData => Option[ReturnType]): Option[ReturnType] = {
    currentToken match {
      case None =>
        _findings += new Parser.UnexpectedEOFError(posAfterCurrentToken)
        None
      case Some(t) =>
        val retval = pred(t.data)
        if (!retval.isDefined) {
          _findings += new Parser.UnexpectedTokenError(t)
        }
        consume(t)
        retval
    }
  }

  private def expectIdentifier(): Option[String] = {
    expect[String] {
      case Identifier(s) => Some(s)
      case _ => None
    }
  }

  private def expectSymbol(symbol: TokenData) = {
    expect[Unit](t => if (t == symbol) Some(Unit) else None)
  }

  private def parseProgram(): Any = {
    while (!atEOF) {
      parseClassDeclaration()
    }
  }

  private def parseClassDeclaration() = {
    expectSymbol(Class)
    expectIdentifier()
    expectSymbol(CurlyBraceOpen)
    // ... parseClassMemberDeclaration
    expectSymbol(CurlyBraceClosed)
  }
  private def parseClassMemberDeclaration() = ???
  // etc.
}
