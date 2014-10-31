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

  it should "separate tokens by whitespace or comments" in {
    val lexer = new Lexer("a a  a\ta\ra\na\r\na/*comment*/a")
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, Range(0, 8) map { _ => new Identifier("a") })
  }

  it should "recognize other whitespace" in {
    val lexer = new Lexer("\r\n\t")
    lexer.success shouldBe true
    lexer.result shouldBe empty
  }

  it should "not recognize unspecified whitespace" in {
    val lexer = new Lexer("\f")
    lexer.success shouldBe false
    lexer.findings.head shouldBe Lexer.UnknownTokenError(1, 1, "\f")
  }

  it should "separate operators and keywords by comments" in {
    val lexer = new Lexer("&/*comment*/& whi/*comment*/le")
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, List(new UnusedFeature("&"), new UnusedFeature("&"),
      new Identifier("whi"), new Identifier("le")))
  }

  it should "separate tokens by operator symbols, even without whitespace" in {
    val lexer = new Lexer("{a(!a.x&&b!=c[d])+5,b;}")
    val expected = List(
      CurlyBraceOpen, new Identifier("a"), ParenOpen, Not, new Identifier("a"), Dot,
      new Identifier("x"), LogicalAnd, new Identifier("b"), Unequal, new Identifier("c"),
      SquareBracketOpen, new Identifier("d"), SquareBracketClosed, ParenClosed,
      Plus, new IntegerLiteral("5"), Comma, new Identifier("b"), Semicolon,
      CurlyBraceClosed
    )
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, expected)
  }

  it should "set line/char of tokens correctly" in {
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

  it should "recognize all MiniJava operator symbols" in {
    val lexer = new Lexer("!= ! ( ) * + , - . / ; <= < == = >= > % && [ ] { } ||")
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, List(Unequal, Not, ParenOpen, ParenClosed,
      Mult, Plus, Comma, Minus, Dot, Divide, Semicolon, SmallerEquals, Smaller, Equals,
      Assign, GreaterEquals, Greater, Modulo, LogicalAnd, SquareBracketOpen,
      SquareBracketClosed, CurlyBraceOpen, CurlyBraceClosed, LogicalOr))
  }

  it should "recognize all other Java operator symbols" in {
    val expected = List("*=", "++", "+=", "-=", "--", "/=", ":", "<<=", "<<", ">>=", ">>>=", ">>>",
      ">>", "?", "%=", "&=", "&", "^=", "^", "~", "|", "|=")
    val lexer = new Lexer(expected.mkString(" "))
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, expected map { s => new UnusedFeature(s) })
  }

  it should "recognize all MiniJava keywords" in {
    val lexer = new Lexer("boolean class else false if int new null public return "
      + "static this true void while")
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, List(BooleanType, Class, Else, False, If, IntType,
      New, Null, Public, Return, Static, This, True, VoidType, While))
  }

  it should "recognize all other Java keywords" in {
    val expected = List("abstract", "assert", "break", "byte", "case", "catch",
      "char", "const", "continue", "default", "double", "do", "enum", "extends", "finally",
      "final", "float", "for", "goto", "implements", "import", "instanceof", "interface",
      "long", "native", "package", "private", "protected", "short", "strictfp", "super", "switch",
      "synchronized", "throws", "throw", "transient", "try", "volatile")
    val lexer = new Lexer(expected.mkString(" "))
    checkContainsTokenData(lexer.result, expected map { s => new UnusedFeature(s) })
  }

  it should "recognize Integer literals" in {
    val expected = List(0, 1234567890, 12, 23, 34, 45, 56, 67, 78, 89, 90)
    val lexer = new Lexer(expected.mkString(" "))
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, expected map { i => IntegerLiteral(i.toString) })
  }

  it should "recognize identifiers" in {
    val expected = List("ident", "ident42", "ident0815", "ident_", "_ident42", "_", "__", "_42",
      "superman", "if0", "_if", "if_true", "iftrue")
    val lexer = new Lexer(expected.mkString(" "))
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, expected map { s => Identifier(s) })
  }

  it should "parse '/ *' and '* /' as tokens, not comment start/end" in {
    val lexer = new Lexer("a / * abc * / c")
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, List(new Identifier("a"), Divide, Mult,
      new Identifier("abc"), Mult, Divide, new Identifier("c")))
  }

  it should "ignore '*/' outside comments" in {
    val lexer = new Lexer("/* this is a /* comment */ a */ b")
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, List(new Identifier("a"), Mult, Divide, new Identifier("b")))
  }

  it should "ignore multiple '*' in comments" in {
    val lexer = new Lexer("/*** abc ***/")
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, List[TokenData]())
  }

  it should "ignore tokens and '/*' in comments" in {
    val lexer = new Lexer("/* while(true) { int i = 42; i++; } /* */")
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, List[TokenData]())
  }

  it should "parse EOF as identifier" in {
    val lexer = new Lexer("EOF")
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, List(new Identifier("EOF")))
  }

  it should "parse greedily" in {
    val testCases = List(
      ("<<<<<",  List(new UnusedFeature("<<"), new UnusedFeature("<<"), Smaller)),
      ("<<<=",   List(new UnusedFeature("<<"), SmallerEquals)),
      ("<<<<=", List(new UnusedFeature("<<"), new UnusedFeature("<<="))),
      ("<<<<<=", List(new UnusedFeature("<<"), new UnusedFeature("<<"), SmallerEquals)),
      (">>>>",   List(new UnusedFeature(">>>"), Greater)),
      (">>>>>",  List(new UnusedFeature(">>>"), new UnusedFeature(">>"))),
      (">>>>>>", List(new UnusedFeature(">>>"), new UnusedFeature(">>>"))),
      (">>>>=",  List(new UnusedFeature(">>>"), GreaterEquals)),
      ("^^^=",   List(new UnusedFeature("^"), new UnusedFeature("^"), new UnusedFeature("^="))),
      ("||=",    List(LogicalOr, Assign)),
      ("|||=",   List(LogicalOr, new UnusedFeature("|="))),
      ("||||=",  List(LogicalOr, LogicalOr, Assign)),
      ("+++=",   List(new UnusedFeature("++"), new UnusedFeature("+="))),
      ("++++=",  List(new UnusedFeature("++"), new UnusedFeature("++"), Assign)),
      ("---=",   List(new UnusedFeature("--"), new UnusedFeature("-="))),
      ("----=",  List(new UnusedFeature("--"), new UnusedFeature("--"), Assign)),
      ("=====",  List(Equals, Equals, Assign)),
      ("&&&&&",  List(LogicalAnd, LogicalAnd, new UnusedFeature("&"))),
      ("|||||",  List(LogicalOr, LogicalOr, new UnusedFeature("|"))),
      ("!!!=",   List(Not, Not, Unequal))
    )

    for (((input, expected), i) <- testCases.zipWithIndex) {
      val lexer = new Lexer(input)
      lexer.success shouldBe true
      withClue(s"Test case $i") { checkContainsTokenData(lexer.result, expected) }
    }
  }

  it should "parse 0 at the start of an integer literal as separate token" in {
    val lexer = new Lexer("a 00815")
    lexer.success shouldBe true
    checkContainsTokenData(lexer.result, List(new Identifier("a"), new IntegerLiteral("0"),
      new IntegerLiteral("0"), new IntegerLiteral("815")))
  }

  /* Failure cases */

  it should "fail on an unterminated comment" in {
    val lexer = new Lexer("a /* this is an unterminated comment")
    lexer.success shouldBe false
    lexer.findings.head shouldBe Lexer.UnclosedCommentError(1, 3)
  }

  /* Result dumping */

  it should "dump the result in the format specified on assignment sheet 2" in {
    // Example from the assignment sheet
    val lexer = new Lexer(
      """/*
        | * @author Beate Best
        | */
        |class classic {
        |  public int method(int arg) {
        |    int res = arg+42;
        |    res >>= 4;
        |    return res;
        |  }
        |}
        |""".stripMargin)
    lexer.dumpResult should equal (
      """class
        |identifier classic
        |{
        |public
        |int
        |identifier method
        |(
        |int
        |identifier arg
        |)
        |{
        |int
        |identifier res
        |=
        |identifier arg
        |+
        |integer literal 42
        |;
        |identifier res
        |>>=
        |integer literal 4
        |;
        |return
        |identifier res
        |;
        |}
        |}
        |EOF""".stripMargin)
  }

}
