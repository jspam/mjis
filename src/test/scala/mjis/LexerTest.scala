package mjis

import mjis.TokenData._
import org.scalatest._

class LexerTest extends FlatSpec with Matchers with Inspectors {

  def checkContainsTokenData(tokens: Seq[Token], expected: Seq[TokenData]): Unit = {
    tokens should have length expected.length
    for ((token, expectedTokenData) <- tokens zip expected) {
      token.data should === (expectedTokenData)
    }
  }

  /* Tests start here */

  "The lexer" should "return an empty token list when parsing the empty string" in {
    val lexer = new Lexer("")
    lexer.success shouldBe true
    lexer.result shouldBe empty
    lexer.findings shouldBe empty
  }

  ignore should "separate tokens by whitespace or comments" in {
    val lexer = new Lexer("a a  a\ta\ra\na\r\na/*comment*/a")
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, Range(0, 8) map { _ => new Identifier("a") })
  }

  ignore should "separate tokens by operator symbols, even without whitespace" in {
    val lexer = new Lexer("{a(!a.x&&b!=c[d])+5,b;}")
    val expected = List(
      CurlyBraceOpen, new Identifier("a"), ParenOpen, Not, new Identifier("a"), Dot,
      new Identifier("x"), LogicalAnd, new Identifier("b"), Unequal, new Identifier("c"),
      SquareBracketOpen, new Identifier("d"), SquareBracketClosed, ParenClosed,
      Plus, new IntegerLiteral(5), Comma, new Identifier("b"), Semicolon,
      CurlyBraceClosed
    )
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, expected)
  }

  ignore should "set line/char of tokens correctly" in {
    val input = "a aa      a\t a\ra a\na a\r\na a\n\na a "
    val expected = List( // (line, char)
      (1, 1),
      (1, 3),
      (1, 11),
      (1, 14), // Tab is counted as one character
      (2, 1), // Single \r is counted as newline
      (2, 3),
      (3, 1), // Single \n is counted as newline
      (3, 3),
      (4, 1), // \r\n is counted as one newline
      (4, 3),
      (6, 1), // \n\n is two newlines
      (6, 3)
    )

    val lexer = new Lexer(input)
    lexer.success shouldBe true
    lexer.result should have length expected.length
    for ((token, lineAndChar) <- lexer.result zip expected) {
      token.data shouldBe an [Identifier]
      (token.line, token.char) should equal(lineAndChar)
    }
  }

  ignore should "recognize all MiniJava operator symbols" in {
    val lexer = new Lexer("!= ! ( ) * + , - . / ; <= < == = >= > % && [ ] { } ||")
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, List(Unequal, Not, ParenOpen, ParenClosed,
      Mult, Plus, Comma, Minus, Dot, Divide, Semicolon, SmallerEquals, Smaller, Equals,
      Assign, GreaterEquals, Greater, Modulo, LogicalAnd, SquareBracketOpen,
      SquareBracketClosed, CurlyBraceOpen, CurlyBraceClosed, LogicalOr))
  }

  ignore should "recognize all other Java operator symbols" in {
    val expected = List("*=", "++", "+=", "-=", "--", "/=", ":", "<<=", "<<", ">>=", ">>>=", ">>>",
      ">>", "?", "%=", "&=", "&", "^=", "^", "~", "|")
    val lexer = new Lexer(expected.mkString(" "))
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, expected map { s => new UnusedFeature(s) })
  }

  ignore should "recognize all MiniJava keywords" in {
    val lexer = new Lexer("boolean class else false if int new null public return "
      + "static this true void while")
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, List(BooleanType, Class, Else, False, If, IntType,
      New, Null, Public, Return, Static, This, True, VoidType, While))
  }

  ignore should "recognize all other Java keywords" in {
    val expected = List("abstract", "assert", "break", "byte", "case", "catch",
      "char", "const", "continue", "default", "double", "do", "enum", "extends", "finally",
      "final", "float", "for", "goto", "implements", "import", "instanceof", "interface",
      "long", "native", "package", "private", "protected", "short", "strictfp", "super", "switch",
      "synchronized", "throws", "throw", "transient", "try", "volatile")
    val lexer = new Lexer(expected.mkString(" "))
    checkContainsTokenData(lexer.result, expected map { s => new UnusedFeature(s) })
  }

  ignore should "recognize Integer literals" in {
    val expected = List(0, 1234567890, 12, 23, 34, 45, 56, 67, 78, 89, 90)
    val lexer = new Lexer(expected.mkString(" "))
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, expected map { i => IntegerLiteral(i) })
  }

  ignore should "recognize identifiers" in {
    val expected = List("ident", "ident42", "ident0815", "ident_", "_ident42", "_", "__", "_42",
      "superman", "if0", "_if", "if_true", "iftrue")
    val lexer = new Lexer(expected.mkString(" "))
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, expected map { s => Identifier(s) })
  }

  /* Failure cases */

  ignore should "disallow nested comments" in {
    val lexer = new Lexer("a /* here is a comment /* and this one is nested */ ok */")
    lexer.success shouldBe false
    // TODO: findings should contain an error
  }

  ignore should "disallow integer literals starting with a 0" in {
    val lexer = new Lexer("a 0815")
    lexer.success shouldBe false
    // TODO: findings should contain an error
  }

  ignore should "fail on an unterminated comment" in {
    val lexer = new Lexer("a /* this is an unterminated comment")
    lexer.success shouldBe false
    // TODO: findings should contain an error
  }

}
