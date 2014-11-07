package mjis

import org.scalatest._

class ParserTest extends FlatSpec with Matchers with Inspectors {

  val progStart = "class Test { public void test() {"
  val progEnd = "} }"

  /* Tests start here */

  "The parser" should "accept an empty program" in {
    val parser = new Parser(new Lexer("").result)
    parser.result
    parser.success shouldBe true
  }

  it should "accept a program consisting of an empty class" in {
    val parser = new Parser(new Lexer("class Test { }").result)
    parser.result
    parser.success shouldBe true
  }

  it should "accept an empty block" in {
    val parser = new Parser(new Lexer(progStart + progEnd).result)
    parser.result
    parser.findings.foreach(System.out.println)
    parser.success shouldBe true
  }

  it should "accept a nested empty block" in {
    val parser = new Parser(new Lexer(progStart + "{}" + progEnd).result)
    parser.result
    parser.success shouldBe true
  }

  it should "accept many nested blocks" in {
    val parser = new Parser(new Lexer(progStart + Seq.fill(10000)("{").mkString("")
      + Seq.fill(5000)("}").mkString("") + Seq.fill(5000)("{").mkString("")
      + Seq.fill(10000)("}").mkString("") + progEnd).result)
    parser.result
    parser.success shouldBe true
  }

  it should "reject a class declaration without class name" in {
    val parser = new Parser(new Lexer("class { }").result)
    parser.result
    parser.success shouldBe false
    parser.findings.head shouldBe an [Parser.UnexpectedTokenError]
  }

  it should "reject a program with a premature EOF" in {
    val parser = new Parser(new Lexer("class").result)
    parser.result
    parser.success shouldBe false
    parser.findings.head shouldBe an [Parser.UnexpectedTokenError]
    parser.findings.head.asInstanceOf[Parser.UnexpectedTokenError].token.data shouldBe TokenData.EOF
    parser.findings.head.pos.column shouldBe 6  // the char after "class"
  }

}
