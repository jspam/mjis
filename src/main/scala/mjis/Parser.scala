package mjis

import scala.collection.mutable.MutableList
import mjis.TokenData._

object Parser {

  case class UnexpectedTokenError(token: Token, expected: String) extends Finding {
    def msg = s"expected $expected, found '${token.data}'"
    def severity = Severity.ERROR
    override def pos: Position = token.pos
  }
  
  case class InvalidMainMethodError(token: Token, msg: String) extends Finding {
    def severity = Severity.ERROR
    override def pos: Position = token.pos
  }
}

class Parser(tokens: BufferedIterator[Token]) extends AnalysisPhase[Any] {
  import Parser._

  private case class UnexpectedTokenException(error: UnexpectedTokenError) extends Exception

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

  private def unexpectedToken(expected: String) = {
    throw new UnexpectedTokenException(new UnexpectedTokenError(currentToken, expected))
  }

  private def expect[A](pred: TokenData => Option[A], expected: String): A = pred(currentToken.data) match {
    case None => unexpectedToken(expected)
    case Some(x) =>
      if (currentToken.data != EOF) consume()
      x
  }

  private def expectIdentifier(): String = {
    expect[String]({
      case Identifier(s) => Some(s)
      case _ => None
    }, "identifier")
  }

  private def expectSymbol(symbol: TokenData) = {
    expect[Unit](t => if (t == symbol) Some(Unit) else None, symbol.literal)
  }

  private def parseProgram(): Any = {
    try {
      while (currentToken.data != EOF) {
        parseClassDeclaration()
      }
    }
    catch {
      case UnexpectedTokenException(error) => _findings += error
    }
  }

  private def parseClassDeclaration() = {
    expectSymbol(Class)
    expectIdentifier()
    expectSymbol(CurlyBraceOpen)
    while (currentToken.data != CurlyBraceClosed) parseClassMember()
    expectSymbol(CurlyBraceClosed)
  }
  
  private def parseClassMember() = {
    expectSymbol(Public)
    if (currentToken.data == Static) {
      // found main method
      consume()
      expectSymbol(VoidType)
      expectIdentifier()
      expectSymbol(ParenOpen)
      if (expectIdentifier() != "String") {
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
        case _ => unexpectedToken("'(' or ';'")
      }
    }
  }

  private def parseBasicType() = {
    currentToken.data match {
      case IntType => consume()
      case BooleanType => consume()
      case VoidType => consume()
      case Identifier(_) => consume()
      case _ => unexpectedToken("type name")
    }
  }

  private def parseNewArrayExpressionPostfix(): Unit = {
    // first dimension
    expectSymbol(SquareBracketOpen)
    parseExpression()
    expectSymbol(SquareBracketClosed)

    // other dimensions
    while (currentToken.data == SquareBracketOpen) {
      consume()
      expectSymbol(SquareBracketClosed)
    }
  }

  private def parseParenthesizedArguments(): Unit = {
    expectSymbol(ParenOpen)
    if (currentToken.data != ParenClosed) {
      parseExpression()
      while (currentToken.data != ParenClosed) {
        expectSymbol(Comma)
        parseExpression()
      }
    }
    consume()
  }

  private def parsePrimaryExpression() = {
    currentToken.data match {
      case Null => consume()
      case False => consume()
      case True => consume()
      case IntegerLiteral(_) => consume()
      case This => consume()
      case Identifier(_) =>
        // Identifier or method call
        consume()
        if (currentToken.data == ParenOpen)
          parseParenthesizedArguments()
      case ParenOpen =>
        consume()
        parseExpression()
        expectSymbol(ParenClosed)
      case New =>
        consume()
        currentToken.data match {
          case Identifier(_) =>
            consume()
            currentToken.data match {
              case ParenOpen =>
                // NewObjectExpression
                consume()
                expectSymbol(ParenClosed)
              case SquareBracketOpen =>
                // NewArrayExpression
                parseNewArrayExpressionPostfix()
              case _ => unexpectedToken("'(' or '['")
            }
          case _ =>
            consume()
            parseNewArrayExpressionPostfix()
        }
      case _ => unexpectedToken("primary expression")
    }
  }

  private def parseArrayAccess() = {
    expectSymbol(SquareBracketOpen)
    parseExpression()
    expectSymbol(SquareBracketClosed)
  }

  private def parsePostfixExpression(): Unit = {
    parsePrimaryExpression()
    parsePostfixOp()
  }

  private def parsePostfixOp(): Unit = {
    currentToken.data match {
      case SquareBracketOpen =>
        parseArrayAccess()
        parsePostfixOp()
      case Dot =>
        // field access or method invocation
        consume()
        expectIdentifier()
        if (currentToken.data == ParenOpen)
          parseParenthesizedArguments()
        parsePostfixOp()
      case _ =>
    }
  }

  private def parseUnaryExpression() = {
    while (currentToken.data == Not || currentToken.data == Minus) {
      consume()
    }
    parsePostfixExpression()
  }

  /**
   * Parses the right-hand side of a binary expression. The recursion is guaranteed to be finite
   * as the precedence level will increase with every recursive call.
   */
  private def parseBinaryExpressionRhs(curPrecedence: Integer = 1): Unit = {
    while(true) {
      var curTokenRightAssoc = false
      var curTokenPrecedence = 0
      currentToken.data match {
        case Assign =>
          curTokenPrecedence = 7
          curTokenRightAssoc = true
        case LogicalOr => curTokenPrecedence = 6
        case LogicalAnd => curTokenPrecedence = 5
        case Equals | Unequal => curTokenPrecedence = 4
        case Smaller | SmallerEquals | Greater | GreaterEquals => curTokenPrecedence = 3
        case Plus | Minus => curTokenPrecedence = 2
        case Mult | Divide | Modulo => curTokenPrecedence = 1
        case _ => return
      }

      val curPrecedenceWithAssoc: Integer = if (curTokenRightAssoc) curPrecedence else curPrecedence + 1

      if (curPrecedenceWithAssoc < curPrecedence) {
        // The token will be consumed and handled by a higher call to parseBinaryExpressionRhs
        return
      } else {
        consume()
        parseBinaryExpression(curPrecedenceWithAssoc)
      }
    }
  }

  private def parseBinaryExpression(curPrecedenceLevel: Integer = 1) = {
    parseUnaryExpression() // left-hand side
    parseBinaryExpressionRhs(curPrecedenceLevel)
  }

  private def parseExpression(): Unit = {
    parseBinaryExpression()
  }

  private def parseExpressionStatement() = {
    parseExpression()
    expectSymbol(Semicolon)
  }

  /**
   * Parses a statement // TODO: and adds it to the given block.
   */
  private def parseStatement(): Unit = {
    currentToken.data match {
      case CurlyBraceOpen =>
        parseBlock()
      case Semicolon =>
        // EmptyStatement
        consume()
        // TODO
      case If =>
        // IfStatement
        consume()
        // TODO
      case While =>
        // WhileStatement
        consume()
        // TODO
      case Return =>
        // ReturnStatement
        consume()
        // TODO
      case _ =>
        // ExpressionStatement or error
        parseExpressionStatement()
    }
  }

  private def parseBlock(): Unit = {
    expectSymbol(CurlyBraceOpen)
    while (currentToken.data != CurlyBraceClosed)
        parseStatement()
    consume()
  }

  private def parseType() = {
    parseBasicType()
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
