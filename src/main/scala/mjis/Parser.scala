package mjis

import scala.util.control.TailCalls._
import scala.collection.mutable.ListBuffer
import mjis.TokenData.{ If => IfToken, While => WhileToken, New => NewToken, _ }
import mjis.ast._

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

class Parser(tokens: LookaheadIterator[Token]) extends AnalysisPhase[Option[Program]] {
  import Parser._

  private case class UnexpectedTokenException(error: UnexpectedTokenError) extends Exception

  private val _findings = ListBuffer.empty[Finding]
  private def currentToken: Token = tokens.head
  private def atEOF = currentToken.data == EOF
  override def findings: List[Finding] = _findings.toList

  protected override def getResult(): Option[Program] = parseProgram()

  override def dumpResult() = ???

  private def consume() = tokens.next()

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
      case _             => None
    }, "identifier")
  }

  private def expectSymbol(symbol: TokenData) = {
    expect[Unit](t => if (t == symbol) Some(Unit) else None, symbol.literal)
  }

  private def parseProgram(): Option[Program] = {
    val classes = ListBuffer.empty[ClassDecl]
    try {
      while (!atEOF) {
        classes += parseClassDeclaration()
      }
      Some(Program(classes.toList))

    } catch {
      case UnexpectedTokenException(error) =>
        _findings += error
        None
    }
  }

  private def parseClassDeclaration(): ClassDecl = {
    expectSymbol(Class)
    val ident = expectIdentifier()
    expectSymbol(CurlyBraceOpen)
    val methods: ListBuffer[MethodDecl] = ListBuffer.empty
    val fields: ListBuffer[FieldDecl] = ListBuffer.empty
    while (currentToken.data != CurlyBraceClosed) {
      val member = parseClassMember()
      member match {
        case method: MethodDecl => methods += method
        case field: FieldDecl   => fields += field
      }
    }
    expectSymbol(CurlyBraceClosed)
    ClassDecl(ident, methods.toList, fields.toList)
  }

  private def parseClassMember(): MemberDecl = {
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
      val ident = expectIdentifier()
      expectSymbol(ParenClosed)
      val block = parseBlock().result
      MethodDecl(ident, Nil, TypeBasic("Void"), block)
    } else {
      val typ = parseType()
      val ident = expectIdentifier()
      currentToken.data match {
        case Semicolon =>
          // found field declaration
          consume()
          FieldDecl(ident, typ)
        case ParenOpen =>
          // found method
          consume()
          var params: List[Parameter] = Nil
          if (currentToken.data != ParenClosed) params = parseParameters()
          expectSymbol(ParenClosed)
          val body = parseBlock().result
          MethodDecl(ident, params, typ, body)
        case _ => unexpectedToken("'(' or ';'")
      }
    }
  }

  private def parseBasicType(): TypeBasic = {
    currentToken.data match {
      case IntType           =>
        consume(); TypeBasic("Int")
      case BooleanType       =>
        consume(); TypeBasic("Boolean")
      case VoidType          =>
        consume(); TypeBasic("Void")
      case Identifier(ident) =>
        consume(); TypeBasic(ident)
      case _                 => unexpectedToken("type name")
    }
  }

  private def parseType(): TypeDef = {
    val basicTyp: TypeBasic = parseBasicType()
    var typ: TypeDef = basicTyp
    while (currentToken.data == SquareBracketOpen) {
      consume()
      typ = TypeConstructor("Array", basicTyp.name)
      expectSymbol(SquareBracketClosed)
    }
    typ
  }

  private def parseParameters(): List[Parameter] = {
    var more: Boolean = true
    val params = ListBuffer.empty[Parameter]
    do {
      val typ = parseType()
      val ident = expectIdentifier()
      params += Parameter(ident, typ)
      if (currentToken.data == Comma)
        consume()
      else
        more = false
    } while (more)
    params.toList
  }

  // mutual recursion: Block -> BlockStatement -> Statement -> Block
  // to break the recursion, each recursive call from parseBlock() must be in a tailcall() in tail position

  private def parseBlock(): TailRec[Block] = {
    expectSymbol(CurlyBraceOpen)
    def remainder(): TailRec[Block] = {
      if (currentToken.data == CurlyBraceClosed) {
        consume()
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

  private def parseLocalVariableDeclarationStatement(): LocalVarDeclStatement = {
    val typ = parseType()
    val name = expectIdentifier()
    val body = if (currentToken.data == Assign) {
      consume()
      Some(parseExpression().result)
    } else None
    expectSymbol(Semicolon)
    LocalVarDeclStatement(name, typ, body)
  }

  // mutual recursion: Statement -> If/WhileStatement -> Statement

  private def parseStatement(): TailRec[Statement] = {
    currentToken.data match {
      case CurlyBraceOpen => parseBlock()
      case Semicolon =>
        consume()
        done(EmptyStatement)
      case IfToken    => tailcall(parseIfStatement())
      case WhileToken => tailcall(parseWhileStatement())
      case Return     => done(parseReturnStatement())
      case _ =>
        val expr = parseExpression().result
        expectSymbol(Semicolon)
        done(ExpressionStatement(expr))
    }
  }

  private def parseIfStatement(): TailRec[If] = {
    expectSymbol(IfToken)
    expectSymbol(ParenOpen)
    val cond = parseExpression().result
    expectSymbol(ParenClosed)
    parseStatement().flatMap(trueStat => {
      // this implicitly solves the dangling else problem by assigning it to the innermost-still-parsable if
      if (currentToken.data == Else) {
        consume()
        val elseStmt = parseStatement().result
        done(If(cond, trueStat, elseStmt))
      } else
        done(If(cond, trueStat, EmptyStatement))
    })
  }

  private def parseWhileStatement(): TailRec[While] = {
    expectSymbol(WhileToken)
    expectSymbol(ParenOpen)
    val cond: Expression = parseExpression().result
    expectSymbol(ParenClosed)
    parseStatement().flatMap(stmt => done(While(cond, stmt)))
  }

  private def parseReturnStatement(): Statement = {
    expectSymbol(Return)
    val expr =
      if (currentToken.data != Semicolon && !atEOF)
        Some(parseExpression().result)
      else
        None
    expectSymbol(Semicolon)
    ReturnStatement(expr)
  }

  // mutual recursion: Expression -> {pretty much everything} -> Expression

  private def parseExpression(): TailRec[Expression] = tailcall(parseBinaryExpression(minPrecedence = 1)).
    flatMap(tuple => done(tuple._1))

  private def parseNewArrayExpressionSuffix(): TailRec[Expression] = {
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

  private def parseParenthesizedArguments(): TailRec[ListBuffer[Expression]] = {
    expectSymbol(ParenOpen)
    def remainder(params: ListBuffer[Expression], expectComma: Boolean): TailRec[ListBuffer[Expression]] = {
      if (currentToken.data != ParenClosed) {
        if (expectComma) expectSymbol(Comma)
        parseExpression().flatMap(e => remainder(params += e, expectComma = true))
      } else {
        consume()
        done(params)
      }
    }
    remainder(ListBuffer.empty, expectComma = false)
  }

  private def parsePrimaryExpression(): TailRec[Expression] = {
    currentToken.data match {
      case Null =>
        consume()
        done(NullLiteral)
      case False =>
        consume()
        done(FalseLiteral)
      case True =>
        consume()
        done(TrueLiteral)
      case IntegerLiteral(int) =>
        consume()
        done(IntLiteral(int))
      case This =>
        consume()
        done(ThisLiteral)
      case Identifier(s) =>
        // Identifier or method call
        consume()
        if (currentToken.data == ParenOpen)
          parseParenthesizedArguments().map(params => Apply(s, ThisLiteral +: params))
        else
          done(Ident(s))
      case ParenOpen =>
        consume()
        parseExpression().map(e => {
          expectSymbol(ParenClosed)
          null
        })
      case NewToken =>
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
            parseBasicType()
            parseNewArrayExpressionSuffix()
        }
      case _ => unexpectedToken("primary expression")
    }
  }

  private def parseArrayAccess(e: Expression): TailRec[Apply] = {
    expectSymbol(SquareBracketOpen)
    parseExpression().map(param => {
      expectSymbol(SquareBracketClosed)
      Apply("apply", ListBuffer(e, param))
    })
  }

  private def parsePostfixExpression(): TailRec[Expression] = parsePrimaryExpression().flatMap(parsePostfixOp)

  private def parsePostfixOp(e: Expression): TailRec[Expression] = {
    currentToken.data match {
      case SquareBracketOpen =>
        parseArrayAccess(e).flatMap(parsePostfixOp)
      case Dot =>
        // field access or method invocation
        consume()
        val ident = expectIdentifier()
        if (currentToken.data == ParenOpen)
          parseParenthesizedArguments().flatMap(params => parsePostfixOp(Apply(ident, ListBuffer(e) ++= params)))
        else parsePostfixOp(Select(e, ident))
      case _ =>
        done(e)
    }
  }

  private def parseUnaryExpression(): TailRec[Expression] = {
    while (currentToken.data == Not || currentToken.data == Minus) {
      consume()
    }
    parsePostfixExpression()
  }

  private def parseBinaryExpressionRhs(lhs: Expression, minPrecedence: Int): TailRec[(Expression, Int)] = {
    var curTokenRightAssoc = false
    var curTokenPrecedence = 1
    currentToken.data match {
      case Assign =>
        curTokenPrecedence = 1
        curTokenRightAssoc = true
      case LogicalOr => curTokenPrecedence = 2
      case LogicalAnd => curTokenPrecedence = 3
      case Equals | Unequal => curTokenPrecedence = 4
      case Smaller | SmallerEquals | Greater | GreaterEquals => curTokenPrecedence = 5
      case Plus | Minus => curTokenPrecedence = 6
      case Mult | Divide | Modulo => curTokenPrecedence = 7
      case _ => return done((null, -1))
    }

    def remainder(lhs: Expression, precedence: Int): TailRec[(Expression, Int)] = {
      if (precedence < minPrecedence) {
        done((lhs, precedence))
      } else {
        val expr = new Apply(currentToken.data.literal, ListBuffer(lhs))
        consume()
        tailcall(parseBinaryExpression(curTokenPrecedence + (if (curTokenRightAssoc) 0 else 1))).
          flatMap {
            case (rhs, precedence) => {
              expr.arguments += rhs
              remainder(expr, precedence)
            }
          }
      }
    }
    remainder(lhs, curTokenPrecedence)
  }

  private def parseBinaryExpression(minPrecedence: Int): TailRec[(Expression, Int)] = {
    parseUnaryExpression(). // left-hand side
      flatMap(lhs => parseBinaryExpressionRhs(lhs, minPrecedence))
  }
}
