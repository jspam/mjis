package mjis

import scala.collection.mutable.MutableList
import mjis.TokenData._

object Parser {

  case class UnexpectedTokenError(token: Token) extends Finding {
    def msg = s"unexpected token: $token"
    def severity = Severity.ERROR
    override def pos: Position = token.pos
  }
  
  case class InvalidMainMethodError(token: Token, msg: String) extends Finding {
    def severity = Severity.ERROR
    override def pos: Position = token.pos
  }
}

class Parser(tokens: BufferedIterator[Token]) extends AnalysisPhase[Any] {
  
  private case class UnexpectedEOFException() extends Exception
  
  private val _findings = MutableList[Finding]()
  private def currentToken: Token = tokens.head
  override def findings: List[Finding] = _findings.toList

  protected override def getResult() = {
    parseProgram()
  }
  override def dumpResult() = ???

  private def consume() = {
    tokens.next()
  }

  private def expect[ReturnType](pred: TokenData => Option[ReturnType]): Option[ReturnType] = {
    if (currentToken.data == EOF)
      throw new UnexpectedEOFException()

    val retval = pred(currentToken.data)
    if (!retval.isDefined) {
      _findings += new Parser.UnexpectedTokenError(currentToken)
    }
    if (currentToken.data != EOF) consume()
    retval
    
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
    try {
      while (currentToken.data != EOF) {
        parseClassDeclaration()
      }
    }
    catch {
      case UnexpectedEOFException() => _findings += new Parser.UnexpectedTokenError(currentToken)
    }
  }

  private def parseClassDeclaration() = {
    expectSymbol(Class)
    expectIdentifier()
    expectSymbol(CurlyBraceOpen)
    while (currentToken.data != CurlyBraceClosed) parseClassMemberDeclaration()
    expectSymbol(CurlyBraceClosed)
  }
  
  private def parseClassMemberDeclaration() = {
    expectSymbol(Public)
    if (currentToken.data == Static) {
      // found main method
      consume()
      expectSymbol(VoidType)
      expectIdentifier()
      expectSymbol(ParenOpen)
      if (expectIdentifier() != Some("String")) {
        _findings += new Parser.InvalidMainMethodError(currentToken, "main must have a single parameter of type String[]")
      }
      expectSymbol(SquareBracketOpen)
      expectSymbol(SquareBracketClosed)
      expectIdentifier()
      expectSymbol(ParenClosed)
      parseBlock()
    } else {
      parseType()
      expectIdentifier()
      currentToken.data match {
        case Semicolon =>
          // found field declaration
          consume()
        case ParenOpen =>
          // found method
          consume()
          if (currentToken.data != ParenClosed) parseParameters()
          expectSymbol(ParenClosed)
          parseBlock()
        case _ => _findings += new Parser.UnexpectedTokenError(currentToken)
      }
    }
  }

  // TODO(max): implement
  private def parseBlock() = {
    expectSymbol(CurlyBraceOpen)
    expectSymbol(CurlyBraceClosed)
  }

  private def parseType() = {
    currentToken.data match {
      case IntType => consume()
      case BooleanType => consume()
      case VoidType => consume()
      case Identifier(_) => consume()
      case _ => _findings += new Parser.UnexpectedTokenError(currentToken)
    }
    while (currentToken.data == SquareBracketOpen) {
      consume()
      expectSymbol(SquareBracketClosed)
    }
  }

  private def parseParameters() = {
    var more: Boolean = true
    do {
      parseType()
      expectIdentifier()
      if (currentToken.data == Comma)
        consume()
      else
        more = false
    } while(more)
  }
  
}
