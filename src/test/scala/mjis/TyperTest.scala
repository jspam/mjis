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

  def method(text: String): Program = {
    compile(s"class Test { $text }")
  }

  "The typer" should "type literals correctly" in {
    stmts("true;") should succeedTyping
    stmts("false;") should succeedTyping
    stmts("42;") should succeedTyping
    stmts("null;") should succeedTyping
  }

  it should "type NewArrayExpressions correctly" in {
    stmts("new int[1];") should succeedTyping
    stmts("new boolean[2][][];") should succeedTyping
  }

  it should "check that the dimensions in NewArrayExpressions are Integers" in {
    stmts("new int[true][];") should failTypingWith(InvalidTypeError(IntType, BooleanType))
    stmts("new boolean[null][];") should failTypingWith(InvalidTypeError(IntType, NullType))
  }

  it should "disallow void everywhere but in method declarations" in {
    stmts("void x;") shouldNot succeedTyping
    compile("class Test { public void field; }") shouldNot succeedTyping
    compile("class Test { public int method(void foo) {} }") shouldNot succeedTyping
    stmts("new void[42];") shouldNot succeedTyping
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
    stmts(";") should succeedTyping
  }

  it should "check whether the condition of an If statement is a Boolean expression" in {
    stmts("if (true);") should succeedTyping
    stmts("if (null);") should failTypingWith(InvalidTypeError(BooleanType, NullType))
    stmts("if (42);") should failTypingWith(InvalidTypeError(BooleanType, IntType))
  }

  it should "check whether the condition of a While statement is a Boolean expression" in {
    stmts("while (true);") should succeedTyping
    stmts("while (null);") should failTypingWith(InvalidTypeError(BooleanType, NullType))
    stmts("while (42);") should failTypingWith(InvalidTypeError(BooleanType, IntType))
  }

  it should "type check initializers of local variables" in {
    stmts("int foo = true;") should failTypingWith(InvalidTypeError(IntType, BooleanType))
    // stmt("int foo = true + 2;") should failTypingWith(InvalidTypeError(IntType, BooleanType)) // needs namer
  }

  it should "allow assigning 'null' to references only" in {
    stmts("int x = null;") should failTypingWith(InvalidTypeError(IntType, NullType))
    stmts("boolean x = null;") should failTypingWith(InvalidTypeError(BooleanType, NullType))
    stmts("Test x = null;") should succeedTyping
  }

}
