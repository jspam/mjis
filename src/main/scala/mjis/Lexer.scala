package mjis

import java.io.{StringReader, Reader}
import scala.collection.mutable.MutableList
import scala.collection.mutable.Map
import scala.collection.immutable.Stream.Empty

import mjis.TokenData._

/** String-keyed map that allows lookup by prefix */
class Trie[A](keyValues: Iterable[(String, A)]) {
  private class Node {
    var item: Option[A] = None
    val children = Map[Char, Node]() // could be optimized to a linear array

    def add(key: String, value: A): Unit = key.headOption match {
      case None => item = Some(value)
      case Some(c) => children.getOrElseUpdate(c, new Node).add(key.tail, value)
    }

    def tryLookupLongestPrefix(s: Stream[Char], prefixLen: Int): (Option[A], Int) = s match {
      case Empty => (item, prefixLen)
      case c #:: s => children.get(c) match {
        case None => (item, prefixLen)
        case Some(child) => child.tryLookupLongestPrefix(s, prefixLen + 1)
      }
    }
  }

  private val root = new Node
  keyValues.foreach(kv => root.add(kv._1, kv._2))

  def tryLookupLongestPrefix(s: Stream[Char]): (Option[A], Int) = root.tryLookupLongestPrefix(s, 0)
}

object Lexer {
  case class UnclosedCommentError(line: Int, char: Int) extends Finding {
    def msg = "unclosed comment"
    def severity = Severity.ERROR
  }
  case class UnknownTokenError(line: Int, char: Int, token: String) extends Finding {
    def msg = s"unknown token: $token"
    def severity = Severity.ERROR
  }
}

class Lexer(val inputReader: Reader) extends AnalysisPhase[Stream[Token]] {
  // abstraction over constant-length (quasi-)tokens
  private abstract class Symbol()
  private case class TokenSymbol(data: TokenData) extends Symbol
  private case object LineBreak extends Symbol
  private case object CommentStart extends Symbol
  private case object CommentEnd extends Symbol

  private val whitespace = Set[Char](' ', '\t')
  private val lineBreaks = Map[String, Symbol](
    "\r" -> LineBreak,
    "\r\n" -> LineBreak,
    "\n" -> LineBreak
  )
  private val constLenTokens = List[TokenData](Unequal, Not, ParenOpen, ParenClosed,
    Mult, Plus, Comma, Minus, Dot, Divide, Semicolon, SmallerEquals, Smaller, Equals,
    Assign, GreaterEquals, Greater, Modulo, LogicalAnd, SquareBracketOpen,
    SquareBracketClosed, CurlyBraceOpen, CurlyBraceClosed, LogicalOr)
  private val unusedConstLenTokens = List[String]("*=", "++", "+=", "-=", "--", "/=",
    ":", "<<=", "<<", ">>=", ">>>=", ">>>", ">>", "?", "%=", "&=", "&", "^=", "^", "~", "|", "|=")
  private val symbols = new Trie[Symbol](
    lineBreaks ++
    constLenTokens.map(t => (t.literal, TokenSymbol(t))) ++
    unusedConstLenTokens.map(t => (t, TokenSymbol(UnusedFeature(t)))) +
    ("/*" -> CommentStart)
  )
  private val commentSymbols = new Trie[Symbol](lineBreaks + ("*/" -> CommentEnd))

  private var input = Stream continually inputReader.read() takeWhile (_ != -1) map (_.toChar)
  private val _findings = MutableList[Finding]()
  private var line = 1
  private var offset = 1

  def this(input: String) = {
    this(new StringReader(input))
  }

  private def consume(charCount: Int): Unit = {
    offset += charCount
    input = input.drop(charCount)
  }

  private def mkTokenAndConsume(charCount: Int, data: TokenData): Token = {
    val t = new Token(data, line, offset)
    consume(charCount)
    t
  }

  private def lexInteger(): Token = {
    val num = input.takeWhile(_.isDigit)
    mkTokenAndConsume(num.length, TokenData.IntegerLiteral(num.mkString))
  }

  private def lexIdentifier(): Token = {
    val ident = input.takeWhile(c => c.isLetterOrDigit || c == '_')
    // TODO: recognize keywords
    mkTokenAndConsume(ident.length, TokenData.Identifier(ident.mkString))
  }

  /** parse remainder after "/ *"  while memorizing its original position */
  @annotation.tailrec
  private def lexCommentRemainder(startLine: Int, startOffset: Int): Unit = {
    if (input.isEmpty) {
      _findings += new Lexer.UnclosedCommentError(startLine, startOffset)
    } else {
      val (symbol, len) = commentSymbols.tryLookupLongestPrefix(input)
      symbol match {
        case None => // any other token
          consume(1)
          lexCommentRemainder(startLine, startOffset)
        case Some(symbol) =>
          consume(len)
          symbol match {
            case LineBreak =>
              line += 1
              offset = 1
              lexCommentRemainder(startLine, startOffset)
            case CommentEnd =>
          }
      }
    }
  }

  private def lexSymbol() : Option[Token] = {
    val (symbol, len) = symbols.tryLookupLongestPrefix(input)
    symbol match {
      case None =>
        // TODO: some heuristics for separating and skipping the unknown 'token'
        _findings += new Lexer.UnknownTokenError(line, offset, input.mkString)
        None
      case Some(symbol) =>
        symbol match {
          case TokenSymbol(data) => Some(mkTokenAndConsume(len, data))
          case LineBreak =>
            consume(len)
            line += 1
            offset = 1
            lexToken()
          case CommentStart =>
            val startOffset = offset
            consume(len)
            lexCommentRemainder(line, startOffset)
            lexToken()
        }
    }
  }

  @annotation.tailrec
  private def lexToken(): Option[Token] = input match {
    case Empty => None
    case c #:: _ =>
      if (c == '_' || c.isLetter)
        Some(lexIdentifier())
      else if (c == '0') // special case: always a single token
        Some(mkTokenAndConsume(1, TokenData.IntegerLiteral("0")))
      else if (c.isDigit)
        Some(lexInteger())
      else if (whitespace(c)) {
        consume(1)
        lexToken()
      } else
        lexSymbol()
  }

  protected override def getResult(): Stream[Token] = (Stream continually lexToken takeWhile (_.isDefined)).flatten

  protected override def getFindings(): List[Finding] = {
    result.foreach(_ => ()) // lex all the things
    _findings.toList
  }

  override def dumpResult(): String = {
    var dump = result.map(token => token.data match {
      case Identifier(literal) => s"identifier $literal"
      case IntegerLiteral(literal) => s"integer literal $literal"
      case _ => token.data.literal
    })

    // TODO: Only append EOF if the file was read completely
    dump :+= "EOF"

    if (!this.success) {
      dump :+= "error"
    }

    dump.mkString(System.lineSeparator()) + System.lineSeparator()
  }
}
