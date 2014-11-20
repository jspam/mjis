package mjis

import mjis.CompilerTestMatchers._
import mjis.Typer.InvalidTypeError
import mjis.ast._
import mjis.Builtins._
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

  def stmts(text: String): Program = compile(s"class Test { public void test() { $text } }")

  def stmt(text: String): Statement = stmts(text).classes(0).methods(0).body.statements(0)

  def method(text: String): MethodDecl = {
    compile(s"class Test { $text }").classes(0).methods(0)
  }

  def expr(expr: String): Expression = {
    stmt(expr + ";").asInstanceOf[ExpressionStatement].expr
  }

  "The typer" should "type literals correctly" in {
    expr("true") should succeedTypingWith(BooleanType)
    expr("false") should succeedTypingWith(BooleanType)
    expr("42") should succeedTypingWith(IntType)
    expr("null") should succeedTypingWith(NullType)
  }

  it should "type NewArrayExpressions correctly" in {
    expr("new int[1]") should succeedTypingWith(TypeArray(IntType, 1))
    expr("new boolean[2][][]") should succeedTypingWith(TypeArray(BooleanType, 3))
  }

  it should "check that the dimensions in NewArrayExpressions are Integers" in {
    expr("new int[true][]") should failTypingWith(InvalidTypeError(IntType, BooleanType))
    expr("new boolean[null][]") should failTypingWith(InvalidTypeError(IntType, NullType))
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
    stmt("if (null);") should failTypingWith(InvalidTypeError(BooleanType, NullType))
    stmt("if (42);") should failTypingWith(InvalidTypeError(BooleanType, IntType))
  }

  it should "check whether the condition of a While statement is a Boolean expression" in {
    stmt("while (true);") should succeedTyping
    stmt("while (null);") should failTypingWith(InvalidTypeError(BooleanType, NullType))
    stmt("while (42);") should failTypingWith(InvalidTypeError(BooleanType, IntType))
  }

  it should "type check initializers of local variables" in {
    stmt("int foo = true;") should failTypingWith(InvalidTypeError(IntType, BooleanType))
    // stmt("int foo = true + 2;") should failTypingWith(InvalidTypeError(IntType, BooleanType)) // needs namer
  }

  it should "allow assigning 'null' to references only" in {
    stmts("int x = null;") should failTypingWith(InvalidTypeError(IntType, NullType))
    stmts("boolean x = null;") should failTypingWith(InvalidTypeError(BooleanType, NullType))
    stmts("Test x = null;") should succeedTyping
  }

}
