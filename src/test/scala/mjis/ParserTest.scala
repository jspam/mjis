package mjis

import mjis.Parser.UnexpectedTokenError
import mjis.TokenData.Assign
import org.scalatest._
import ParserTestMatchers._

class ParserTest extends FlatSpec with Matchers with Inspectors {

  def parseProgram(program: String) = new Parser(new Lexer(program).result)
  def parseStatements(statements: String) = parseProgram(
    "class Test { public void test() {" + System.lineSeparator() + statements + System.lineSeparator() + "} }"
  )

  def repeat(str: String, count: Integer) = Seq.fill(count)(str).mkString("")

  /* Tests start here */

  "The parser" should "accept an empty program" in {
    parseProgram("") should succeedParsing()
  }

  it should "accept a program consisting of an empty class" in {
    parseProgram("class Test { }") should succeedParsing()
  }

  it should "accept an empty block" in {
    parseStatements("") should succeedParsing()
  }

  it should "accept a nested empty block" in {
    parseStatements("{}") should succeedParsing()
  }

  ignore should "accept many nested blocks" in {
    // {{{{...}}{{...}}}}
    parseStatements(repeat("{", 10000) + repeat("}", 5000)
      + repeat("{", 5000) + repeat("}", 10000)) should succeedParsing()
  }

  it should "accept assignments" in {
    parseStatements("a=a;\na=a=a;") should succeedParsing()
  }

  ignore should "accept long assignment chains" in {
    // a=a=a=...=a;
    parseStatements(repeat("a=", 10000) + "a;") should succeedParsing()
  }

  it should "accept method calls with parameters" in {
    parseStatements("a(b);\na(b, c);\na(b, c(d));") should succeedParsing()
  }

  ignore should "accept long method call chains" in {
    // a((((((...(null)...))))));
    parseStatements(repeat("a(", 10000) + "null"
      + Seq.fill(10000)(")") + ";") should succeedParsing()
  }

  ignore should "accept long parenthesis chains" in {
    // ((((...((a))...))));
    parseStatements(repeat("(", 10000) + "a" + repeat(")", 10000) + ";") should succeedParsing()
  }

  ignore should "accept long chains of alternating expressions and parentheses" in {
    // a+(a+(a+(a+(...a+(a+(a))...))));
    parseStatements(repeat("a+(", 10000) + "a" + repeat(")", 10000) + ";") should succeedParsing()
  }

  it should "accept primary expressions" in {
    parseStatements(
      """
        |null;
        |false;
        |true;
        |1337;
        |myVar;
        |myFunc();
        |this;
        |(null);
        |a[2][3][b];
        |new myType();
      """.stripMargin) should succeedParsing()
  }

  it should "accept unary expressions" in {
    parseStatements("-c;\n-(-c);\n!c;\n!!c;\n!-!-c;\n!(-(!(-(c))));'") should succeedParsing()
  }

  it should "accept long chains of unary expressions" in {
    // a+(a+(a+(a+(...a+(a+(a))...))));
    parseStatements(repeat("!-", 10000) + "a;") should succeedParsing()
  }

  it should "accept binary expressions" in {
    parseStatements("a+b;a-b;a*b;a/b;a%b;a&&b;a||b;a>b;a<b;a<=b;a>=b;a==b;a!=b;"
      ) should succeedParsing()
  }

  it should "accept nested binary expressions" in {
    parseStatements("a+b*c-d!=(e>f*g);'") should succeedParsing()
  }

  it should "accept array creations" in {
    parseStatements("new int[5]; new int[a][][]; new a[c-(d)][];")
  }

  it should "accept field accesses and method calls" in {
    parseStatements("a.b.c;a.b.c();a[b].c().c()[d];") should succeedParsing()
  }

  it should "reject arbitrary expressions in member accesses" in {
    val parser = parseStatements("a.(b+c);")
    parser shouldNot succeedParsing()
    parser.findings.head shouldBe an [Parser.UnexpectedTokenError]
  }

  ignore should "accept long chains of field accesses" in {
    parseStatements(repeat("a.", 10000) + "b;") should succeedParsing()
  }

  it should "reject a class declaration without class name" in {
    val parser = parseProgram("class { }")
    parser shouldNot succeedParsing()
    parser.findings.head shouldBe an [Parser.UnexpectedTokenError]
  }

  it should "reject a program with a premature EOF" in {
    val parser = parseProgram("class")
    parser shouldNot succeedParsing()
    parser.findings.head shouldBe an [Parser.UnexpectedTokenError]
    parser.findings.head.asInstanceOf[Parser.UnexpectedTokenError].token.data shouldBe TokenData.EOF
    parser.findings.head.pos.column shouldBe 6  // the char after "class"
  }

}
