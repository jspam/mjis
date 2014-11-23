package mjis

import mjis.CompilerTestHelper._
import mjis.CompilerTestMatchers._
import mjis.Typer._
import mjis.Builtins._
import mjis.ast.{TypeBasic, TypeArray}
import org.scalatest._

class TyperTest extends FlatSpec with Matchers with Inspectors {
  
  "The typer" should "type literals correctly" in {
    fromStatements("true;") should succeedTyping
    fromStatements("false;") should succeedTyping
    fromStatements("42;") should succeedTyping
    fromStatements("null;") should succeedTyping
  }
  
  it should "typecheck NewArrayExpressions" in {
    fromStatements("new int[1];") should succeedTyping
    fromStatements("new boolean[2][][];") should succeedTyping

    fromStatements("int x = new int[2];") should
      failTypingWith(InvalidTypeError(IntType, TypeArray(IntType, 1)))
    fromStatements("int x = new boolean[2];") should
      failTypingWith(InvalidTypeError(IntType, TypeArray(BooleanType, 1)))

    fromStatements("int[][] x = new int[2];") should
      failTypingWith(InvalidTypeError(TypeArray(IntType, 2), TypeArray(IntType, 1)))
    fromStatements("int[][] x = new int[2][];") should succeedTyping
    fromStatements("int[][] x = new int[2][][];") should
      failTypingWith(InvalidTypeError(TypeArray(IntType, 2), TypeArray(IntType, 3)))

    fromStatements("int[][] x = new boolean[2];") should
      failTypingWith(InvalidTypeError(TypeArray(IntType, 2), TypeArray(BooleanType, 1)))
    fromStatements("int[][] x = new boolean[2][];") should
      failTypingWith(InvalidTypeError(TypeArray(IntType, 2), TypeArray(BooleanType, 2)))
    fromStatements("int[][] x = new boolean[2][][];") should
      failTypingWith(InvalidTypeError(TypeArray(IntType, 2), TypeArray(BooleanType, 3)))

    fromStatements("int[] x = new int[true + 42];") should failTypingWith(InvalidTypeError(IntType, BooleanType))
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

  it should "typecheck return statements" in {
    "class Test { public int test() { return true + 42; } }" should
      failTypingWith(InvalidTypeError(IntType, BooleanType))
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

  it should "typecheck Expression statements" in {
    fromStatements("true + 42;") should failTypingWith(InvalidTypeError(IntType, BooleanType))
  }

  it should "typecheck method calls" in {
    def testProg(stmt: String) = "class Test2 {}" +
      s"class Test { public void main() { $stmt } " +
      "public void noParams() {} " +
      "public void oneIntParam(int x) {} " +
      "public void twoIntParams(int x, int y) {} " +
      "public void twoMixedParams(int x, boolean y) {} " +
      "public void twoArrayParams(int[] x, boolean[] y) {} " +
      "public void twoObjectParams(Test x, Test2 y) {} }"

    testProg("noParams();") should succeedTyping
    testProg("noParams(42 + 16);") should failTypingWith(WrongNumberOfParametersError(0, 1))
    testProg("noParams(42 + 16, new Test(), true);") should failTypingWith(WrongNumberOfParametersError(0, 3))

    testProg("oneIntParam();") should failTypingWith(WrongNumberOfParametersError(1, 0))
    testProg("oneIntParam(42 + 16);") should succeedTyping
    testProg("oneIntParam(true);") should failTypingWith(InvalidTypeError(IntType, BooleanType))

    testProg("twoIntParams();") should failTypingWith(WrongNumberOfParametersError(2, 0))
    testProg("twoIntParams(42 + 16, true);") should failTypingWith(InvalidTypeError(IntType, BooleanType))

    testProg("twoMixedParams(42 + 16, true);") should succeedTyping
    testProg("twoMixedParams(true, 42 + 16);") should failTypingWith(InvalidTypeError(IntType, BooleanType))

    testProg("twoArrayParams(new int[42], new boolean[42]);") should succeedTyping
    testProg("twoArrayParams(new int[42][], new boolean[42]);") should
      failTypingWith(InvalidTypeError(TypeArray(IntType, 1), TypeArray(IntType, 2)))
    testProg("twoArrayParams(42, new boolean[42]);") should
      failTypingWith(InvalidTypeError(TypeArray(IntType, 1), IntType))

    testProg("twoObjectParams(new Test(), new Test2());") should succeedTyping
    testProg("twoObjectParams(null, null);") should succeedTyping
    testProg("twoObjectParams(new Test2(), new Test2());") should
      failTypingWith(InvalidTypeError(TypeBasic("Test"), TypeBasic("Test2")))

    // type errors in the argument expressions themselves
    testProg("twoIntParams(true + 42, 42);") should failTypingWith(InvalidTypeError(IntType, BooleanType))
    testProg("twoIntParams(42, true + 42);") should failTypingWith(InvalidTypeError(IntType, BooleanType))
  }

  it should "typecheck the condition of an If statement" in {
    fromStatements("if (true);") should succeedTyping
    fromStatements("if (null);") should failTypingWith(InvalidTypeError(BooleanType, NullType))
    fromStatements("if (42);") should failTypingWith(InvalidTypeError(BooleanType, IntType))

    fromStatements("if (true || 2);") should failTypingWith(InvalidTypeError(BooleanType, IntType))
  }

  it should "typecheck the condition of a While statement" in {
    fromStatements("while (true);") should succeedTyping
    fromStatements("while (null);") should failTypingWith(InvalidTypeError(BooleanType, NullType))
    fromStatements("while (42);") should failTypingWith(InvalidTypeError(BooleanType, IntType))

    fromStatements("while (true || 2);") should failTypingWith(InvalidTypeError(BooleanType, IntType))
  }

  it should "type check initializers of local variables" in {
    fromStatements("int foo = true;") should failTypingWith(InvalidTypeError(IntType, BooleanType))

    fromStatements("int foo = true + 2;") should failTypingWith(InvalidTypeError(IntType, BooleanType))
  }

  it should "allow assigning 'null' to references only" in {
    fromStatements("int x = null;") should failTypingWith(InvalidTypeError(IntType, NullType))
    fromStatements("boolean x = null;") should failTypingWith(InvalidTypeError(BooleanType, NullType))
    fromStatements("Test x = null;") should succeedTyping
  }

  it should "allow assignment to lvalues only" in {
    fromStatements("int i; i = 42;") should succeedTyping
    fromStatements("int[] i; i[3] = 42;") should succeedTyping
    "class Test { public int i; public void test() { i = 42; } }" should succeedTyping
    "class Test { public int i; public void test() { this.i = 42; } }" should succeedTyping
    "class Test { public void test(int i) { i = 42; } }" should succeedTyping
    
    fromStatements("int i; i + 3 = 42;") should failTypingWith(AssignmentToNonLValueError())
    fromStatements("new int[42] = 42;") should failTypingWith(AssignmentToNonLValueError())
    fromStatements("new Test() = 42;") should failTypingWith(AssignmentToNonLValueError())
    fromStatements("int i; (i = 42) = 42;") should failTypingWith(AssignmentToNonLValueError())
    "class Test { public int i() { i() = 42; } }" should failTypingWith(AssignmentToNonLValueError())
    "class Test { public int i() { this.i() = 42; } }" should failTypingWith(AssignmentToNonLValueError())
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

  it should "type check the assignment operator" in {
    fromStatements("Test test; Test test2; Test test3 = test2 = test;") should succeedTyping
    fromStatements("Test test; Test test2; Test test3 = test2 = test = null;") should succeedTyping

    "class Test2{} class Test{ public void test() { Test test; Test2 test2 = test = null; }}" should
      failTypingWith(InvalidTypeError(TypeBasic("Test2"), TypeBasic("Test")))
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

  it should "type check the array access operator" in {
    fromStatements("int x; int[][] xs = new int[1][]; x = xs;") should
      failTypingWith(InvalidTypeError(IntType, TypeArray(IntType, 2)))
    fromStatements("int x; int[][] xs = new int[1][]; x = xs[1];") should
      failTypingWith(InvalidTypeError(IntType, TypeArray(IntType, 1)))
    fromStatements("int x; int[][] xs = new int[1][]; x = xs[1][2];") should succeedTyping
    fromStatements("int x = new int[1][][1][2];") should succeedTyping
    fromStatements("int x; int[][] xs = new int[1][]; x = xs[1][2][3];") should
      failTypingWith(ArrayAccessOnNonArrayError(IntType))

    fromStatements("int[] x; x[42];") should succeedTyping
    fromStatements("int[] x; x[x[1]];") should succeedTyping
    fromStatements("int[] x; x[x];") should failTypingWith(InvalidTypeError(IntType, TypeArray(IntType, 1)))
    fromStatements("int[] x; x[true];") should failTypingWith(InvalidTypeError(IntType, BooleanType))

    fromStatements("Test test; int[] x; x[test];") should failTypingWith(InvalidTypeError(IntType, TypeBasic("Test")))

    fromStatements("int[] x; x[true + 42];") should failTypingWith(InvalidTypeError(IntType, BooleanType))
  }

}
