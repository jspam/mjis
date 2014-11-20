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

  def method(text: String): MethodDecl = {
    compile(s"class Test { $text }").classes(0).methods(0)
  }

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

  it should "disallow void everywhere but in method declarations" in {
    stmt("void x;") shouldNot succeedTyping
    compile("class Test { public void field; }") shouldNot succeedTyping
    compile("class Test { public int method(void foo) {} }") shouldNot succeedTyping
    expr("new void[42]") shouldNot succeedTyping
  }

  ignore should "check for a correct return type" in {
    for ((retType, retTypeIdx) <- List("void", "int", "boolean", "int[]", "boolean[][]").zipWithIndex) {
      for ((retVal, retValIdx) <- List("", "42", "true", "new int[42]", "new boolean[42][]").zipWithIndex) {
        val prog = s"public $retType test() { return $retVal; }"
        withClue(prog) {
          if (retTypeIdx == retValIdx) method(prog) should succeedTyping
          else method(prog) shouldNot succeedTyping
        }
      }
    }
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
