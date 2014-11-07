package mjis

import org.scalatest._
import ParserTestMatchers._

class ParserTest extends FlatSpec with Matchers with Inspectors {

  val progStart = "class Test { public void test() {" + System.lineSeparator()
  val progEnd = System.lineSeparator() + "} }"

  def repeat(str: String, count: Integer) = Seq.fill(count)(str).mkString("")

  /* Tests start here */

  "The parser" should "accept an empty program" in {
    new Parser(new Lexer("").result) should succeedParsing()
  }

  it should "accept a program consisting of an empty class" in {
    new Parser(new Lexer("class Test { }").result) should succeedParsing()
  }

  it should "accept an empty block" in {
    new Parser(new Lexer(progStart + progEnd).result) should succeedParsing()
  }

  it should "accept a nested empty block" in {
    new Parser(new Lexer(progStart + "{}" + progEnd).result) should succeedParsing()
  }

  it should "accept many nested blocks" in {
    // {{{{...}}{{...}}}}
    new Parser(new Lexer(progStart + repeat("{", 10000) + repeat("}", 5000)
      + repeat("{", 5000) + repeat("}", 10000) + progEnd).result) should succeedParsing()
  }

  it should "accept assignments" in {
    new Parser(new Lexer(progStart + "a=a;\na=a=a;" + progEnd).result) should succeedParsing()
  }

  it should "accept long assignment chains" in {
    // a=a=a=...=a;
    new Parser(new Lexer(progStart + repeat("a=", 10000) + "a;" + progEnd).result) should succeedParsing()
  }

  it should "accept method calls with parameters" in {
    new Parser(new Lexer(progStart + "a(b);\na(b, c);\na(b, c(d));" + progEnd).result) should succeedParsing()
  }

  it should "accept long method call chains" in {
    // a((((((...(null)...))))));
    new Parser(new Lexer(progStart + repeat("a(", 10000) + "null"
      + Seq.fill(10000)(")") + ";" + progEnd).result) should succeedParsing()
  }

  it should "accept long parenthesis chains" in {
    // ((((...((a))...))));
    new Parser(new Lexer(progStart + repeat("(", 10000) + "a" + repeat(")", 10000) + ";" + progEnd).result
      ) should succeedParsing()
  }

  it should "accept long chains of alternating expressions and parentheses" in {
    // a+(a+(a+(a+(...a+(a+(a))...))));
    new Parser(new Lexer(progStart + repeat("a+(", 10000) + "a" + repeat(")", 10000) + ";" + progEnd).result
      ) should succeedParsing()
  }

  it should "accept primary expressions" in {
    new Parser(new Lexer(progStart +
      """
        |null;
        |false;
        |true;
        |1337;
        |myVar;
        |myFunc();
        |this;
        |(null);
        |a[2][3][4];
        |new myType();
        |new int[5][][];
      """.stripMargin + progEnd).result) should succeedParsing()
  }

  it should "accept unary expressions" in {
    new Parser(new Lexer(progStart + "-c;\n-(-c);\n!c;\n!!c;\n!-!-c;\n!(-(!(-(c))));'" + progEnd).result
      ) should succeedParsing()
  }

  it should "accept long chains of unary expressions" in {
    // a+(a+(a+(a+(...a+(a+(a))...))));
    new Parser(new Lexer(progStart + repeat("!-", 10000) + "a;" + progEnd).result) should succeedParsing()
  }

  it should "accept binary expressions" in {
    new Parser(new Lexer(progStart + "a+b;a-b;a*b;a/b;a%b;a&&b;a||b;a>b;a<b;a<=b;a>=b;a==b;a!=b;" + progEnd).result
      ) should succeedParsing()
  }

  it should "accept nested binary expressions" in {
    new Parser(new Lexer(progStart + "a+b*c-d!=(e>f*g);'" + progEnd).result) should succeedParsing()
  }

  it should "accept field accesses and method calls" in {
    new Parser(new Lexer(progStart + "a.b.c;a.b.c();" + progEnd).result) should succeedParsing()
  }

  it should "accept long chains of field accesses" in {
    new Parser(new Lexer(progStart + repeat("a.", 10000) + "b;" + progEnd).result) should succeedParsing()
  }

  it should "reject a class declaration without class name" in {
    val parser = new Parser(new Lexer("class { }").result)
    parser shouldNot succeedParsing()
    parser.findings.head shouldBe an [Parser.UnexpectedTokenError]
  }

  it should "reject a program with a premature EOF" in {
    val parser = new Parser(new Lexer("class").result)
    parser shouldNot succeedParsing()
    parser.findings.head shouldBe an [Parser.UnexpectedTokenError]
    parser.findings.head.asInstanceOf[Parser.UnexpectedTokenError].token.data shouldBe TokenData.EOF
    parser.findings.head.pos.column shouldBe 6  // the char after "class"
  }

}
