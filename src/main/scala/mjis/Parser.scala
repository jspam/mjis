package mjis

import scala.collection.mutable.{MutableList, Stack}
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

  // Temporary until we have a real AST. When we do, certain AST nodes are put on the parser stack.
  private abstract class TempASTNode
  private class Block() extends TempASTNode
  private abstract class Expression extends TempASTNode
  private class RightAssocBinaryExpression() extends Expression
  private class ParenthesizedExpression() extends Expression
  private class ArrayAccessExpression() extends Expression
  private class ArrayCreateExpression() extends Expression
  private class MethodCall() extends Expression
  
  private case class UnexpectedEOFException() extends Exception

  private def expressionClosingToken(t: TokenData) = t == ParenClosed || t == SquareBracketClosed || t == Comma

  private val _findings = MutableList[Finding]()
  private def currentToken: Token = tokens.head
  override def findings: List[Finding] = _findings.toList

  // We avoid stack overflows by allocating our own stack of "unfinished" expressions on the heap ;)
  //
  // Potential stack overflow candidates are those expressions/statements that can be nested. Those are:
  //
  // Blocks:           Block -> { | Statements | }
  // Array accesses:   Expression -> UnaryExpression [ | Expression | ]([Expression])*
  // Parentheses:      Expression -> ( | Expression | )
  // Method calls:     Expression -> IDENT ( | Expressions | )
  // Array creations:  Expression -> new IDENT [ | Expressions | ]([])*
  // Binary operators: Expression -> UnaryExpression OP | Expression     for a right-associative binary operator OP (= or .)
  private val blockStack = Stack[Block]()
  private val expressionStack = Stack[Expression]()

  protected override def getResult() = {
    parseProgram()
  }
  override def dumpResult() = ???

  private def consume() = {
    tokens.next()
  }

  private def unexpectedToken() = {
    _findings += new Parser.UnexpectedTokenError(currentToken)
  }

  private def expect[ReturnType](pred: TokenData => Option[ReturnType]): Option[ReturnType] = {
    if (currentToken.data == EOF)
      throw new UnexpectedEOFException()

    val retval = pred(currentToken.data)
    if (!retval.isDefined) unexpectedToken()
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
      case UnexpectedEOFException() => unexpectedToken()
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
        case _ => unexpectedToken()
      }
    }
  }

  private def parseBasicType() = {
    currentToken.data match {
      case IntType => consume()
      case BooleanType => consume()
      case VoidType => consume()
      case Identifier(_) => consume()
      case _ => unexpectedToken()
    }
  }

  private def openNewArrayExpressionPostfix() = expressionStack.push(new ArrayCreateExpression)
  private def closeNewArrayExpressionPostfix() = {
    expressionStack.pop()
    while (currentToken.data == SquareBracketOpen) {
      consume()
      expectSymbol(SquareBracketClosed)
    }
  }

  private def openMethodCallExpression() = expressionStack.push(new MethodCall)
  private def continueMethodCallExpression() = {} /* leave the current MethodCall on the stack */
  private def closeMethodCallExpression() = expressionStack.pop()

  private def openParenthesizedExpression() = expressionStack.push(new ParenthesizedExpression)
  private def closeParenthesizedExpression() = expressionStack.pop()

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
        if (currentToken.data == ParenOpen) {
          consume()
          openMethodCallExpression()
        }
      case ParenOpen =>
        consume()
        openParenthesizedExpression()
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
                consume()
                openNewArrayExpressionPostfix()
            }
          case _ =>
            // New ArrayExpression
            parseBasicType()
            expectSymbol(SquareBracketOpen)
            openNewArrayExpressionPostfix()
        }
      case _ => unexpectedToken()
    }
  }

  private def openArrayAccessExpression() = expressionStack.push(new ArrayAccessExpression)
  private def closeArrayAccessExpression() = {
    expressionStack.pop()
    if (currentToken.data == SquareBracketOpen) {
      consume()
      expressionStack.push(new ArrayAccessExpression) /* lhs = the current ArrayAccessExpression */
    }
  }

  private def parseArrayAccessExpression() = {
    currentToken.data match {
      case SquareBracketOpen =>
        consume()
        openArrayAccessExpression()
      case _ => unexpectedToken()
    }
  }

  private def parsePostfixExpression() = {
    parsePrimaryExpression()
    // Note that Dot is handled in parseBinaryExpressionRhs
    if (currentToken.data == SquareBracketOpen) parseArrayAccessExpression()
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
          curTokenPrecedence = 8
          curTokenRightAssoc = true
        case LogicalOr => curTokenPrecedence = 7
        case LogicalAnd => curTokenPrecedence = 6
        case Equals | Unequal => curTokenPrecedence = 5
        case Smaller | SmallerEquals | Greater | GreaterEquals => curTokenPrecedence = 4
        case Plus | Minus => curTokenPrecedence = 3
        case Mult | Divide | Modulo => curTokenPrecedence = 2
        case Dot => curTokenPrecedence = 1
        case _ =>
          while (expressionStack.nonEmpty && expressionStack.head.isInstanceOf[RightAssocBinaryExpression]) expressionStack.pop()
          return
      }

      val curPrecedenceWithAssoc: Integer = if (curTokenRightAssoc) curPrecedence else curPrecedence + 1

      if (curPrecedenceWithAssoc < curPrecedence) {
        // The token will be consumed and handled by a higher call to parseBinaryExpressionRhs
        return
      } else if (curPrecedenceWithAssoc == curPrecedence) {
        consume()
        expressionStack.push(new RightAssocBinaryExpression())
        return
      } else { // curPrecedenceWithAssoc > curPrecedence
        consume()
        parseBinaryExpression(curPrecedenceWithAssoc)
      }
    }
  }

  private def parseBinaryExpression(curPrecedenceLevel: Integer = 1) = {
    parseUnaryExpression() // left-hand side
    parseBinaryExpressionRhs(curPrecedenceLevel)
  }

  private def closeExpression() = {
    if (expressionStack.isEmpty) {
      unexpectedToken()
    } else {
      currentToken.data match {
        case SquareBracketClosed =>
          if (expressionStack.head.isInstanceOf[ArrayAccessExpression]) { consume(); closeArrayAccessExpression() }
          else if (expressionStack.head.isInstanceOf[ArrayCreateExpression]) { consume(); closeNewArrayExpressionPostfix() }
          else unexpectedToken()
        case ParenClosed =>
          if (expressionStack.head.isInstanceOf[ParenthesizedExpression]) { consume(); closeParenthesizedExpression() }
          else if (expressionStack.head.isInstanceOf[MethodCall]) { consume(); closeMethodCallExpression() }
          else unexpectedToken()
        case Comma =>
          if (expressionStack.head.isInstanceOf[MethodCall]) { consume(); continueMethodCallExpression(); }
          else unexpectedToken()
      }
    }
  }

  private def parseExpression() = {
    do {
      parseBinaryExpression()

      // Handle the closing of an expression
      while (expressionClosingToken(currentToken.data)) {
        closeExpression()
      }

    } while (expressionStack.nonEmpty)
  }

  private def parseExpressionStatement() = {
    parseExpression()
    expectSymbol(Semicolon)
  }

  /**
   * Parses a statement // TODO: and adds it to the given block.
   */
  private def parseStatement(block: Block) = {
    currentToken.data match {
      case CurlyBraceOpen =>
        consume()
        blockStack.push(new Block())
        // return to parseBlock
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

  private def parseBlock() = {
    expectSymbol(CurlyBraceOpen)
    blockStack.push(new Block())
    while (blockStack.nonEmpty) {
      while (currentToken.data != EOF && currentToken.data != CurlyBraceClosed) {
        parseStatement(blockStack.top)
      }
      // currentToken == CurlyBraceClosed
      consume()
      blockStack.pop()
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
  
}
