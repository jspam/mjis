package mjis

import mjis.TokenData._
import org.scalatest._

class LexerTest extends FlatSpec with Matchers with Inspectors {

  "The lexer" should "return an empty token list when parsing the empty string" in {
    val lexer = new Lexer("")
    lexer.success shouldBe true
    lexer.result shouldBe empty
    lexer.findings shouldBe empty
  }

  ignore should "separate tokens by whitespace or comments" in {
    val lexer = new Lexer("a a  a\ta\ra\na\r\na/*comment*/a")
    lexer.success shouldBe true
    lexer.result should have length 8
    forAll (lexer.result) { t: Token => t.data shouldBe an [Identifier] }
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
    lexer.result should have length expected.length
    for ((token, expectedTokenData) <- lexer.result zip expected) {
      token.data should === (expectedTokenData)
    }
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
    lexer.result should have length 24
    lexer.result(0).data shouldBe an [Unequal.type]
    lexer.result(1).data shouldBe a [Not.type]
    lexer.result(2).data shouldBe a [ParenOpen.type]
    lexer.result(3).data shouldBe a [ParenClosed.type]
    lexer.result(4).data shouldBe a [Mult.type]
    lexer.result(5).data shouldBe a [Plus.type]
    lexer.result(6).data shouldBe a [Comma.type]
    lexer.result(7).data shouldBe a [Minus.type]
    lexer.result(8).data shouldBe a [Dot.type]
    lexer.result(9).data shouldBe a [Divide.type]
    lexer.result(10).data shouldBe a [Semicolon.type]
    lexer.result(11).data shouldBe a [SmallerEquals.type]
    lexer.result(12).data shouldBe a [Smaller.type]
    lexer.result(13).data shouldBe a [Equals.type]
    lexer.result(14).data shouldBe an [Assign.type]
    lexer.result(15).data shouldBe a [GreaterEquals.type]
    lexer.result(16).data shouldBe a [Greater.type]
    lexer.result(17).data shouldBe a [Modulo.type]
    lexer.result(18).data shouldBe a [LogicalAnd.type]
    lexer.result(19).data shouldBe a [SquareBracketOpen.type]
    lexer.result(20).data shouldBe a [SquareBracketClosed.type]
    lexer.result(21).data shouldBe a [CurlyBraceOpen.type]
    lexer.result(22).data shouldBe a [CurlyBraceClosed.type]
    lexer.result(23).data shouldBe a [LogicalOr.type]
  }

  ignore should "recognize all other Java operator symbols" in {
    val lexer = new Lexer("*= ++ += -= -- /= : <<= << >>= >>>= >>> >> ? %= &= & ^= ^ ~ |")
    lexer.success shouldBe true
    lexer.result should have length 21
    forAll (lexer.result) { t: Token => t.data shouldBe an [UnusedFeature] }
  }

  ignore should "recognize all MiniJava keywords" in {
    val lexer = new Lexer("boolean class else false if int new null public return "
      + "static this true void while")
    lexer.success shouldBe true
    lexer.result should have length 15
    lexer.result(0).data shouldBe a [BooleanType.type]
    lexer.result(1).data shouldBe a [Class.type]
    lexer.result(2).data shouldBe a [Else.type]
    lexer.result(3).data shouldBe a [False.type]
    lexer.result(4).data shouldBe a [If.type]
    lexer.result(5).data shouldBe a [IntType.type]
    lexer.result(6).data shouldBe a [New.type]
    lexer.result(7).data shouldBe a [Null.type]
    lexer.result(8).data shouldBe a [Public.type]
    lexer.result(9).data shouldBe a [Return.type]
    lexer.result(10).data shouldBe a [Static.type]
    lexer.result(11).data shouldBe a [This.type]
    lexer.result(12).data shouldBe a [True.type]
    lexer.result(13).data shouldBe a [VoidType.type]
    lexer.result(14).data shouldBe a [While.type]
  }

  ignore should "recognize all other Java keywords" in {
    val lexer = new Lexer("abstract assert break byte case catch char const continue "
      + "default double do enum extends finally final float for goto implements import instanceof "
      + "interface long native package private protected short strictfp super switch synchronized "
      + "throws throw transient try volatile ")
    lexer.success shouldBe true
    lexer.result should have length 38
    forAll (lexer.result) { t: Token => t.data shouldBe an [UnusedFeature] }
  }

  ignore should "recognize Integer literals" in {
    val expected = List(0, 1234567890, 12, 23, 34, 45, 56, 67, 78, 89, 90)
    val lexer = new Lexer(expected.mkString(" "))
    lexer.success shouldBe true
    lexer.result should have length expected.length
    for ((token, expectedValue) <- lexer.result zip expected ) {
      token.data shouldBe an [IntegerLiteral]
      token.data.asInstanceOf[IntegerLiteral].value should equal(expectedValue)
    }
  }

  ignore should "recognize identifiers" in {
    val expected = List("ident", "ident42", "ident0815", "ident_", "_ident42", "_", "__", "_42",
      "superman", "if0", "_if", "if_true", "iftrue")
    val lexer = new Lexer(expected.mkString(" "))
    lexer.success shouldBe true
    lexer.result should have length expected.length
    for ((token, expectedName) <- lexer.result zip expected) {
      token.data shouldBe an [Identifier]
      token.data.asInstanceOf[Identifier].value shouldBe expectedName
    }
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
