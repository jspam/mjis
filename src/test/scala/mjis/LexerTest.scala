package mjis

import mjis.TokenData._
import org.scalatest._

class LexerTest extends FlatSpec with Matchers with Inspectors {

  "The lexer" should "return an empty token list when parsing the empty string" in {
    val lexResult = new Lexer().process("")
    lexResult.success shouldBe true
    lexResult.result shouldBe empty
    lexResult.findings shouldBe empty
  }

  ignore should "separate tokens by whitespace or comments" in {
    val lexResult = new Lexer().process("a a  a\ta\ra\na\r\na/*comment*/a")
    lexResult.success shouldBe true
    lexResult.result should have length 8
    forAll (lexResult.result) { t: Token => t.data shouldBe an [Identifier] }
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

    val lexResult = new Lexer().process(input)
    lexResult.success shouldBe true
    lexResult.result should have length expected.length
    for ((token, lineAndChar) <- lexResult.result zip expected) {
      token.data shouldBe an [Identifier]
      (token.line, token.char) should equal(lineAndChar)
    }
  }

  ignore should "recognize all MiniJava operator symbols" in {
    val lexResult = new Lexer().process("!= ! ( ) * + , - . / ; <= < == = >= > % && [ ] { } ||")
    lexResult.success shouldBe true
    lexResult.result should have length 24
    lexResult.result(0).data shouldBe an [Unequal.type]
    lexResult.result(1).data shouldBe a [Not.type]
    lexResult.result(2).data shouldBe a [ParenOpen.type]
    lexResult.result(3).data shouldBe a [ParenClosed.type]
    lexResult.result(4).data shouldBe a [Mult.type]
    lexResult.result(5).data shouldBe a [Plus.type]
    lexResult.result(6).data shouldBe a [Comma.type]
    lexResult.result(7).data shouldBe a [Minus.type]
    lexResult.result(8).data shouldBe a [Dot.type]
    lexResult.result(9).data shouldBe a [Divide.type]
    lexResult.result(10).data shouldBe a [Semicolon.type]
    lexResult.result(11).data shouldBe a [SmallerEquals.type]
    lexResult.result(12).data shouldBe a [Smaller.type]
    lexResult.result(13).data shouldBe a [Equals.type]
    lexResult.result(14).data shouldBe an [Assign.type]
    lexResult.result(15).data shouldBe a [GreaterEquals.type]
    lexResult.result(16).data shouldBe a [Greater.type]
    lexResult.result(17).data shouldBe a [Modulo.type]
    lexResult.result(18).data shouldBe a [LogicalAnd.type]
    lexResult.result(19).data shouldBe a [SquareBracketOpen.type]
    lexResult.result(20).data shouldBe a [SquareBracketClosed.type]
    lexResult.result(21).data shouldBe a [CurlyBraceOpen.type]
    lexResult.result(22).data shouldBe a [CurlyBraceClosed.type]
    lexResult.result(23).data shouldBe a [LogicalOr.type]
  }

  ignore should "recognize all other Java operator symbols" in {
    val lexResult = new Lexer().process("*= ++ += -= -- /= : <<= << >>= >>>= >>> >> ? %= &= & ^= ^ ~ |")
    lexResult.success shouldBe true
    lexResult.result should have length 21
    forAll (lexResult.result) { t: Token => t.data shouldBe an [UnusedFeature] }
  }

  ignore should "recognize all MiniJava keywords" in {
    val lexResult = new Lexer().process("boolean class else false if int new null public return "
      + "static this true void while")
    lexResult.success shouldBe true
    lexResult.result should have length 15
    lexResult.result(0).data shouldBe a [BooleanType.type]
    lexResult.result(1).data shouldBe a [Class.type]
    lexResult.result(2).data shouldBe a [Else.type]
    lexResult.result(3).data shouldBe a [False.type]
    lexResult.result(4).data shouldBe a [If.type]
    lexResult.result(5).data shouldBe a [IntType.type]
    lexResult.result(6).data shouldBe a [New.type]
    lexResult.result(7).data shouldBe a [Null.type]
    lexResult.result(8).data shouldBe a [Public.type]
    lexResult.result(9).data shouldBe a [Return.type]
    lexResult.result(10).data shouldBe a [Static.type]
    lexResult.result(11).data shouldBe a [This.type]
    lexResult.result(12).data shouldBe a [True.type]
    lexResult.result(13).data shouldBe a [VoidType.type]
    lexResult.result(14).data shouldBe a [While.type]
  }

  ignore should "recognize all other Java keywords" in {
    val lexResult = new Lexer().process("abstract assert break byte case catch char const continue "
      + "default double do enum extends finally final float for goto implements import instanceof "
      + "interface long native package private protected short strictfp super switch synchronized "
      + "throws throw transient try volatile ")
    lexResult.success shouldBe true
    lexResult.result should have length 38
    forAll (lexResult.result) { t: Token => t.data shouldBe an [UnusedFeature] }
  }

  ignore should "recognize Integer literals" in {
    val expected = List(0, 1234567890, 12, 23, 34, 45, 56, 67, 78, 89, 90)
    val lexResult = new Lexer().process(expected.mkString(" "))
    lexResult.success shouldBe true
    lexResult.result should have length expected.length
    for ((token, expectedValue) <- lexResult.result zip expected ) {
      token.data shouldBe an [IntegerLiteral]
      token.data.asInstanceOf[IntegerLiteral].value should equal(expectedValue)
    }
  }

  ignore should "recognize identifiers" in {
    val expected = List("ident", "ident42", "ident0815", "ident_", "_ident42", "_", "__", "_42",
      "superman", "if0", "_if", "if_true", "iftrue")
    val lexResult = new Lexer().process(expected.mkString(" "))
    lexResult.success shouldBe true
    lexResult.result should have length expected.length
    for ((token, expectedName) <- lexResult.result zip expected) {
      token.data shouldBe an [Identifier]
      token.data.asInstanceOf[Identifier].value shouldBe expectedName
    }
  }

  /* Failure cases */

  ignore should "disallow nested comments" in {
    val lexResult = new Lexer().process("a /* here is a comment /* and this one is nested */ ok */")
    lexResult.success shouldBe false
    // TODO: findings should contain an error
  }

  ignore should "disallow integer literals starting with a 0" in {
    val lexResult = new Lexer().process("a 0815")
    lexResult.success shouldBe false
    // TODO: findings should contain an error
  }

  ignore should "fail on an unterminated comment" in {
    val lexResult = new Lexer().process("a /* this is an unterminated comment")
    lexResult.success shouldBe false
    // TODO: findings should contain an error
  }

}
