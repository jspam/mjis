package mjis

import scala.util.control.TailCalls._
import scala.collection.mutable.ListBuffer
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

class Parser(tokens: LookaheadIterator[Token]) extends AnalysisPhase[Any] {
  import Parser._

  private case class UnexpectedTokenException(error: UnexpectedTokenError) extends Exception

  private val _findings = ListBuffer.empty[Finding]
  private def currentToken: Token = tokens.head
  private def atEOF = currentToken.data == EOF
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
      if (!atEOF) consume()
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
      while (!atEOF) {
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
      parseBlock().result
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
          parseBlock().result
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

  // mutual recursion: Block -> BlockStatement -> Statement -> Block
  // to break the recursion, each recursive call from parseBlock() must be in a tailcall() in tail position

  private def parseBlock(): TailRec[Any] = {
    expectSymbol(CurlyBraceOpen)
    def remainder(): TailRec[Any] = {
      if (currentToken.data == CurlyBraceClosed) {
        expectSymbol(CurlyBraceClosed)
        done(null)
      } else {
        tailcall(parseBlockStatement()).flatMap(_ => remainder())
      }
    }
    remainder()
  }

  private def parseBlockStatement(): TailRec[Any] = {
    // the grammar doesn't allow variable declarations in conditional structures, hence the special production
    currentToken.data match {
      case IntType | BooleanType | VoidType =>
        done(parseLocalVariableDeclarationStatement())
      case Identifier(_) =>
        // tricky case! To my best knowledge the only SLL(3)-production in the grammar
        // this might be either a local variable declaration or an expression statement.
        tokens.peek(1).data match {
          case Identifier(_) =>
            // found: CustomType myvar   --> local var decl
            done(parseLocalVariableDeclarationStatement())
          case SquareBracketOpen =>
            if (tokens.peek(2).data == SquareBracketClosed)
            // found: CustomType[]  --> local var decl
              done(parseLocalVariableDeclarationStatement())
            else
            // found: myarray[   --> statement
              parseStatement()
          case _ =>
            parseStatement()
        }
      case _ => parseStatement()
    }
  }

  private def parseLocalVariableDeclarationStatement(): Any = {
    parseType()
    expectIdentifier()
    if (currentToken.data == Assign) {
      consume()
      parseExpression().result
    }
    expectSymbol(Semicolon)
  }

  // mutual recursion: Statement -> If/WhileStatement -> Statement

  private def parseStatement(): TailRec[Any] = {
    currentToken.data match {
      case CurlyBraceOpen => parseBlock()
      case Semicolon =>
        consume()
        done(null)
      case If => tailcall(parseIfStatement())
      case While => tailcall(parseWhileStatement())
      case Return => done(parseReturnStatement())
      case _ =>
        // expression statement
        parseExpression().result
        expectSymbol(Semicolon)
        done(null)
    }
  }

  private def parseIfStatement(): TailRec[Any] = {
    expectSymbol(If)
    expectSymbol(ParenOpen)
    parseExpression().result
    expectSymbol(ParenClosed)
    parseStatement().flatMap(st => {
      // this implicitly solves the dangling else problem by assigning it to the innermost-still-parsable if
      if (currentToken.data == Else) {
        consume()
        parseStatement()
      } else
        done(null)
    })
  }

  private def parseWhileStatement(): TailRec[Any] = {
    expectSymbol(While)
    expectSymbol(ParenOpen)
    parseExpression().result
    expectSymbol(ParenClosed)
    parseStatement()
  }

  private def parseReturnStatement(): Any = {
    expectSymbol(Return)
    if (currentToken.data != Semicolon && !atEOF) parseExpression().result
    expectSymbol(Semicolon)
    null
  }

  // mutual recursion: Expression -> {pretty much everyting} -> Expression

  private def parseExpression(): TailRec[Any] = tailcall(parseBinaryExpression())

  private def parseNewArrayExpressionSuffix(): TailRec[Any] = {
    // first dimension
    expectSymbol(SquareBracketOpen)
    parseExpression().flatMap(e => {
      expectSymbol(SquareBracketClosed)

      // other dimensions (we have to take care not to consume a [ that might belong to an array access)
      while (currentToken.data == SquareBracketOpen && tokens.peek(1).data == SquareBracketClosed) {
        consume()
        consume()
      }
      done(null)
    })
  }

  private def parseParenthesizedArguments(): TailRec[Any] = {
    expectSymbol(ParenOpen)
    def remainder(expectComma: Boolean): TailRec[Any] = {
      if (currentToken.data != ParenClosed) {
        if (expectComma) expectSymbol(Comma)
        parseExpression().flatMap(e => remainder(expectComma = true))
      } else {
        consume()
        done(null)
      }
    }
    remainder(expectComma = false)
  }

  private def parsePrimaryExpression(): TailRec[Any] = {
    currentToken.data match {
      case Null =>
        consume()
        done(null)
      case False =>
        consume()
        done(null)
      case True =>
        consume()
        done(null)
      case IntegerLiteral(_) =>
        consume()
        done(null)
      case This =>
        consume()
        done(null)
      case Identifier(_) =>
        // Identifier or method call
        consume()
        if (currentToken.data == ParenOpen)
          parseParenthesizedArguments()
        else
          done(null)
      case ParenOpen =>
        consume()
        parseExpression().map(e => {
          expectSymbol(ParenClosed)
          null
        })
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
                done(null)
              case SquareBracketOpen =>
                // NewArrayExpression
                parseNewArrayExpressionSuffix()
              case _ => unexpectedToken("'(' or '['")
            }
          case _ =>
            consume()
            parseNewArrayExpressionSuffix()
        }
      case _ => unexpectedToken("primary expression")
    }
  }

  private def parseArrayAccess(): TailRec[Any] = {
    expectSymbol(SquareBracketOpen)
    parseExpression().map(e => {
      expectSymbol(SquareBracketClosed)
      null
    })
  }

  private def parsePostfixExpression(): TailRec[Any] = {
    for {
      e <- parsePrimaryExpression()
      post <- parsePostfixOp()
    } yield done(null)
  }

  private def parsePostfixOp(): TailRec[Any] = {
    currentToken.data match {
      case SquareBracketOpen =>
        parseArrayAccess().flatMap(e => {
          parsePostfixOp()
        })
      case Dot =>
        // field access or method invocation
        consume()
        expectIdentifier()
        if (currentToken.data == ParenOpen)
          parseParenthesizedArguments().flatMap(_ => parsePostfixOp())
        else parsePostfixOp()
      case _ =>
        done(null)
    }
  }

  private def parseUnaryExpression(): TailRec[Any] = {
    while (currentToken.data == Not || currentToken.data == Minus) {
      consume()
    }
    parsePostfixExpression()
  }

  /**
   * Parses the right-hand side of a binary expression. The recursion is guaranteed to be finite
   * as the precedence level will increase with every recursive call.
   */
  private def parseBinaryExpressionRhs(curPrecedence: Integer = 1): TailRec[Any] = {
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
      case _ => return done(null)
    }

    val curPrecedenceWithAssoc: Integer = if (curTokenRightAssoc) curPrecedence else curPrecedence + 1

    if (curPrecedenceWithAssoc < curPrecedence) {
      // The token will be consumed and handled by a higher call to parseBinaryExpressionRhs
      done(null)
    } else {
      consume()
      tailcall(parseBinaryExpression(curPrecedenceWithAssoc))
    }
  }

  private def parseBinaryExpression(curPrecedenceLevel: Integer = 1): TailRec[Any] = {
    parseUnaryExpression(). // left-hand side
      flatMap(_ => parseBinaryExpressionRhs(curPrecedenceLevel))
  }
}
