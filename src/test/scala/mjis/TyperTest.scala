package mjis

import mjis.CompilerTestHelper._
import mjis.CompilerTestMatchers._
import mjis.Typer._
import mjis.Builtins._
import mjis.ast.TypeBasic
import org.scalatest._

class TyperTest extends FlatSpec with Matchers with Inspectors {
  
  "The typer" should "type literals correctly" in {
    fromStatements("true;") should succeedTyping
    fromStatements("false;") should succeedTyping
    fromStatements("42;") should succeedTyping
    fromStatements("null;") should succeedTyping
  }

  it should "type NewArrayExpressions correctly" in {
    fromStatements("new int[1];") should succeedTyping
    fromStatements("new boolean[2][][];") should succeedTyping
  }

  it should "check that the dimensions in NewArrayExpressions are Integers" in {
    fromStatements("new int[true][];") should failTypingWith(InvalidTypeError(IntType, BooleanType))
    fromStatements("new boolean[null][];") should failTypingWith(InvalidTypeError(IntType, NullType))
  }

  it should "disallow void everywhere but in method declarations" in {
    fromStatements("void x;") shouldNot succeedTyping
    "class Test { public void field; }" shouldNot succeedTyping
    "class Test { public int fromMethod(void foo) {} }" shouldNot succeedTyping
    fromStatements("new void[42];") shouldNot succeedTyping
  }

  it should "check for a correct return type" in {
    for ((retType, retTypeIdx) <- List("void", "int", "boolean", "int[]", "boolean[][]").zipWithIndex) {
      for ((retVal, retValIdx) <- List("", "42", "true", "new int[42]", "new boolean[42][]").zipWithIndex) {
        val prog = s"public $retType test() { return $retVal; }"
        withClue(prog) {
          if (retTypeIdx == retValIdx) fromMethod(prog) should succeedTyping
          else fromMethod(prog) shouldNot succeedTyping
        }
      }
    }
  }

  it should "type empty statements correctly" in {
    fromStatements(";") should succeedTyping
  }

  it should "check whether the condition of an If statement is a Boolean expression" in {
    fromStatements("if (true);") should succeedTyping
    fromStatements("if (null);") should failTypingWith(InvalidTypeError(BooleanType, NullType))
    fromStatements("if (42);") should failTypingWith(InvalidTypeError(BooleanType, IntType))
  }

  it should "check whether the condition of a While statement is a Boolean expression" in {
    fromStatements("while (true);") should succeedTyping
    fromStatements("while (null);") should failTypingWith(InvalidTypeError(BooleanType, NullType))
    fromStatements("while (42);") should failTypingWith(InvalidTypeError(BooleanType, IntType))
  }

  it should "type check initializers of local variables" in {
    fromStatements("int foo = true;") should failTypingWith(InvalidTypeError(IntType, BooleanType))
    // stmt("int foo = true + 2;") should failTypingWith(InvalidTypeError(IntType, BooleanType)) // needs namer
  }

  it should "allow assigning 'null' to references only" in {
    fromStatements("int x = null;") should failTypingWith(InvalidTypeError(IntType, NullType))
    fromStatements("boolean x = null;") should failTypingWith(InvalidTypeError(BooleanType, NullType))
    fromStatements("Test x = null;") should succeedTyping
  }

  it should "check that a non-void function reaches a return statement on every code path" in {
    fromMethod("public void foo() { } ") should succeedTyping
    fromMethod("public int foo() { { { { return 0; } } } 42; } ") should succeedTyping
    fromMethod("public int foo() { { if (true) return 0; else return 1; } 42; }") should succeedTyping
    fromMethod("public int foo() { { if (true) { return 0; } else { return 1; } } 42; }") should succeedTyping

    fromMethod("public int foo() { 42; }") should failTypingWith(MissingReturnStatementError())
    fromMethod("public int foo() { { while (true) return 0; } 42; }") should failTypingWith(MissingReturnStatementError())
    fromMethod("public int foo() { if (true) return 0; else; 42; }") should failTypingWith(MissingReturnStatementError())
    fromMethod("public int foo() { if (true) { if (false) return 0; else; } else return 1; 42; }") should
      failTypingWith(MissingReturnStatementError())
  }

  it should "type check the equality and inequality operator" in {
    for (op <- List("==", "!=")) withClue(op) {
      fromStatements(s"null $op null;") should succeedTyping
      fromStatements(s"new Test() $op null;") should succeedTyping
      fromStatements(s"null $op new Test();") should succeedTyping
      fromStatements(s"42 $op 42;") should succeedTyping
      fromStatements(s"true $op false;") should succeedTyping

      fromStatements(s"true $op null;") should failTypingWith(IncomparableTypesError(BooleanType, NullType))
      fromStatements(s"null $op true;") should failTypingWith(IncomparableTypesError(NullType, BooleanType))
      fromStatements(s"42 $op null;") should failTypingWith(IncomparableTypesError(IntType, NullType))
      fromStatements(s"null $op 42;") should failTypingWith(IncomparableTypesError(NullType, IntType))

      fromStatements(s"true $op new Test();") should failTypingWith(IncomparableTypesError(BooleanType, TypeBasic("Test")))
      fromStatements(s"new Test() $op true;") should failTypingWith(IncomparableTypesError(TypeBasic("Test"), BooleanType))
      fromStatements(s"42 $op new Test();") should failTypingWith(IncomparableTypesError(IntType, TypeBasic("Test")))
      fromStatements(s"new Test() $op 42;") should failTypingWith(IncomparableTypesError(TypeBasic("Test"), IntType))

      fromStatements(s"true $op 42;") should failTypingWith(IncomparableTypesError(BooleanType, IntType))
      fromStatements(s"42 $op true;") should failTypingWith(IncomparableTypesError(IntType, BooleanType))

      fromStatements(s"boolean b = 42 $op 42;") should succeedTyping
      fromStatements(s"int i = 42 $op 42;") should failTypingWith(InvalidTypeError(IntType, BooleanType))
    }
  }

}
