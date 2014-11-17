package mjis

import mjis.CompilerTestMatchers._
import mjis.Typer.InvalidTypeError
import mjis.ast._
import org.scalatest._

class TyperTest extends FlatSpec with Matchers with Inspectors {

  def compile(prog: String): Program = {
    val lexer = new Lexer(prog)
    val parser = new Parser(lexer.result)
    parser.result
    // The lexer result is only traversable once, so evaluate it only after the parser has finished.
    lexer.result
    lexer should succeedLexing()
    parser should succeedParsing()
    parser.result.orNull
  }

  def stmt(text: String): Statement = {
    compile(s"class Test { public void test() { $text } }").
      classes(0).methods(0).body.statements(0)
  }

  def typedStmt(text: String): Typed = stmt(text).asInstanceOf[Typed]

  def expr(expr: String): Expression = {
    stmt(expr + ";").asInstanceOf[ExpressionStatement].expr
  }

  "The typer" should "type literals correctly" in {
    expr("true") should succeedTypingWith(TypeBasic("boolean"))
    expr("false") should succeedTypingWith(TypeBasic("boolean"))
    expr("42") should succeedTypingWith(TypeBasic("int"))
    expr("null") should succeedTypingWith(TypeBasic("null"))
  }

  it should "type NewArrayExpressions correctly" in {
    expr("new int[1]") should succeedTypingWith(TypeArray(TypeBasic("int"), 1))
    expr("new boolean[2][][]") should succeedTypingWith(TypeArray(TypeBasic("boolean"), 3))
  }

  it should "check that the dimensions in NewArrayExpressions are Integers" in {
    expr("new int[true][]") should failTypingWith(InvalidTypeError(TypeBasic("int"), TypeBasic("boolean")))
    expr("new boolean[null][]") should failTypingWith(InvalidTypeError(TypeBasic("int"), TypeBasic("null")))
  }

  it should "disallow NewArrayExpressions with void as basic type" in {
    expr("new void[42]") shouldNot succeedTyping
  }

  it should "type return statements correctly" in {
    typedStmt("return;") should succeedTypingWith(TypeBasic("void"))
    typedStmt("return true;") should succeedTypingWith(TypeBasic("boolean"))
    typedStmt("return null;") should succeedTypingWith(TypeBasic("null"))
    typedStmt("return 42;") should succeedTypingWith(TypeBasic("int"))
  }

  it should "type empty statements correctly" in {
    stmt(";") should succeedTyping
  }

  it should "check whether the condition of an If statement is a Boolean expression" in {
    stmt("if (true);") should succeedTyping
    stmt("if (null);") should failTypingWith(InvalidTypeError(TypeBasic("boolean"), TypeBasic("null")))
    stmt("if (42);") should failTypingWith(InvalidTypeError(TypeBasic("boolean"), TypeBasic("int")))
  }

  it should "check whether the condition of a While statement is a Boolean expression" in {
    stmt("while (true);") should succeedTyping
    stmt("while (null);") should failTypingWith(InvalidTypeError(TypeBasic("boolean"), TypeBasic("null")))
    stmt("while (42);") should failTypingWith(InvalidTypeError(TypeBasic("boolean"), TypeBasic("int")))
  }

}
