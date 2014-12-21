package mjis

import mjis.CompilerTestHelper._
import mjis.CompilerTestMatchers._
import mjis.Typer._
import mjis.Builtins._
import mjis.Position.NoPosition
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
      failTypingWith(InvalidTypeError(IntType, TypeArray(IntType, 1), Position(3,19)))
    fromStatements("int x = new boolean[2];") should
      failTypingWith(InvalidTypeError(IntType, TypeArray(BooleanType, 1), Position(3,23)))

    fromStatements("int[][] x = new int[2];") should
      failTypingWith(InvalidTypeError(TypeArray(IntType, 2), TypeArray(IntType, 1), Position(3,23)))
    fromStatements("int[][] x = new int[2][];") should succeedTyping
    fromStatements("int[][] x = new int[2][][];") should
      failTypingWith(InvalidTypeError(TypeArray(IntType, 2), TypeArray(IntType, 3), Position(3,27)))

    fromStatements("int[][] x = new boolean[2];") should
      failTypingWith(InvalidTypeError(TypeArray(IntType, 2), TypeArray(BooleanType, 1), Position(3,27)))
    fromStatements("int[][] x = new boolean[2][];") should
      failTypingWith(InvalidTypeError(TypeArray(IntType, 2), TypeArray(BooleanType, 2), Position(3,29)))
    fromStatements("int[][] x = new boolean[2][][];") should
      failTypingWith(InvalidTypeError(TypeArray(IntType, 2), TypeArray(BooleanType, 3), Position(3,31)))

    fromStatements("int[] x = new int[true + 42];") should failTypingWith(InvalidTypeError(IntType, BooleanType, NoPosition))
  }

  it should "check that the dimensions in NewArrayExpressions are Integers" in {
    fromStatements("new int[true][];") should failTypingWith(InvalidTypeError(IntType, BooleanType, Position(3,16)))
    fromStatements("new boolean[null][];") should failTypingWith(InvalidTypeError(IntType, NullType, Position(3,20)))
  }

  it should "disallow void everywhere but in method declarations" in {
    fromStatements("void x;") should failTypingWith(VoidUsageError(NoPosition))
    fromStatements("void[] x;") should failTypingWith(VoidUsageError(NoPosition))
    fromMembers("public void test() { void x = test(); }") should failTypingWith(VoidUsageError(NoPosition))
    fromMembers("public void[] test() { }") should failTypingWith(VoidUsageError(NoPosition))
    fromMembers("public void field;") should failTypingWith(VoidUsageError(NoPosition))
    fromMembers("public void[] field;") should failTypingWith(VoidUsageError(NoPosition))
    fromMembers("public int test(void foo) {}") should failTypingWith(VoidUsageError(NoPosition))
    fromMembers("public int test(void[] foo) {}") should failTypingWith(VoidUsageError(NoPosition))
    fromStatements("new void[42];") should failTypingWith(VoidUsageError(NoPosition))

    fromMembers("public void test() { int x = test(); }") should failTypingWith(InvalidTypeError(IntType, VoidType, Position(2,36)))
    fromMembers("public int x; public void test() { this.x = test(); }") should failTypingWith(InvalidTypeError(IntType, VoidType, Position(2,51)))
  }

  it should "typecheck return statements" in {
    fromMembers("public int test() { return true + 42; }") should
      failTypingWith(InvalidTypeError(IntType, BooleanType, NoPosition))
  }

  it should "check for a correct return type" in {
    for ((retType, retTypeIdx) <- List("void", "int", "boolean", "int[]", "boolean[][]").zipWithIndex) {
      for ((retVal, retValIdx) <- List("", "42", "true", "new int[42]", "new boolean[42][]", "returnsVoid()").zipWithIndex) {
        val prog = s"public void returnsVoid() {} public $retType test() { return $retVal; }"
        withClue(prog) {
          if (retTypeIdx == retValIdx) fromMembers(prog) should succeedTyping
          else fromMembers(prog) shouldNot succeedTyping
        }
      }
    }
  }

  it should "type empty statements correctly" in {
    fromStatements(";") should succeedTyping
  }

  it should "typecheck Expression statements" in {
    fromStatements("true + 42;") should failTypingWith(InvalidTypeError(IntType, BooleanType, NoPosition))
  }

  it should "typecheck method calls" in {
    def testProg(stmt: String) = "class Test2 {}" +
      "class Test { public static void main(String[] args) {} " +
      s"public void test() { $stmt } " +
      "public void noParams() {} " +
      "public void oneIntParam(int x) {} " +
      "public void twoIntParams(int x, int y) {} " +
      "public void twoMixedParams(int x, boolean y) {} " +
      "public void twoArrayParams(int[] x, boolean[] y) {} " +
      "public void twoObjectParams(Test x, Test2 y) {} }"

    testProg("noParams();") should succeedTyping
    testProg("noParams(42 + 16);") should failTypingWith(WrongNumberOfParametersError(0, 1, Position(1,108)))
    testProg("noParams(42 + 16, new Test(), true);") should failTypingWith(WrongNumberOfParametersError(0, 3, Position(1,126)))

    testProg("oneIntParam();") should failTypingWith(WrongNumberOfParametersError(1, 0, Position(1,104)))
    testProg("oneIntParam(42 + 16);") should succeedTyping
    testProg("oneIntParam(true);") should failTypingWith(InvalidTypeError(IntType, BooleanType, Position(1,167)))

    testProg("twoIntParams();") should failTypingWith(WrongNumberOfParametersError(2, 0, Position(1,105)))
    testProg("twoIntParams(42 + 16, true);") should failTypingWith(InvalidTypeError(IntType, BooleanType, Position(1,219)))

    testProg("twoMixedParams(42 + 16, true);") should succeedTyping
    testProg("twoMixedParams(true, 42 + 16);") should failTypingWith(InvalidTypeError(IntType, BooleanType, Position(1,258)))

    testProg("twoArrayParams(new int[42], new boolean[42]);") should succeedTyping
    testProg("twoArrayParams(new int[42][], new boolean[42]);") should
      failTypingWith(InvalidTypeError(TypeArray(IntType, 1), TypeArray(IntType, 2), Position(1,325)))
    testProg("twoArrayParams(42, new boolean[42]);") should
      failTypingWith(InvalidTypeError(TypeArray(IntType, 1), IntType, Position(1,314)))

    testProg("twoObjectParams(new Test(), new Test2());") should succeedTyping
    testProg("twoObjectParams(null, null);") should succeedTyping
    testProg("twoObjectParams(new Test2(), new Test2());") should
      failTypingWith(InvalidTypeError(TypeBasic("Test"), TypeBasic("Test2"), Position(1,372)))

    // type errors in the argument expressions themselves
    testProg("twoIntParams(true + 42, 42);") should failTypingWith(InvalidTypeError(IntType, BooleanType, NoPosition))
    testProg("twoIntParams(42, true + 42);") should failTypingWith(InvalidTypeError(IntType, BooleanType, NoPosition))
  }

  it should "typecheck the condition of an If statement" in {
    fromStatements("if (true);") should succeedTyping
    fromStatements("if (null);") should failTypingWith(InvalidTypeError(BooleanType, NullType, Position(4,1)))
    fromStatements("if (42);") should failTypingWith(InvalidTypeError(BooleanType, IntType, Position(4,1)))

    fromStatements("if (true || 2);") should failTypingWith(InvalidTypeError(BooleanType, IntType, NoPosition))
  }

  it should "typecheck the condition of a While statement" in {
    fromStatements("while (true);") should succeedTyping
    fromStatements("while (null);") should failTypingWith(InvalidTypeError(BooleanType, NullType, Position(4,1)))
    fromStatements("while (42);") should failTypingWith(InvalidTypeError(BooleanType, IntType, Position(4,1)))

    fromStatements("while (true || 2);") should failTypingWith(InvalidTypeError(BooleanType, IntType, NoPosition))
  }

  it should "typecheck nested statements" in {
    fromStatements("{true; true || 2;}") should failTypingWith(InvalidTypeError(BooleanType, IntType, NoPosition))
    fromStatements("if (true) true || 2;") should failTypingWith(InvalidTypeError(BooleanType, IntType, NoPosition))
    fromStatements("while (true) true || 2;") should failTypingWith(InvalidTypeError(BooleanType, IntType, NoPosition))
  }

  it should "type check initializers of local variables" in {
    fromStatements("int foo = true;") should failTypingWith(InvalidTypeError(IntType, BooleanType, Position(3,15)))

    fromStatements("int foo = true + 2;") should failTypingWith(InvalidTypeError(IntType, BooleanType, NoPosition))
  }

  it should "allow assigning 'null' to references only" in {
    fromStatements("int x = null;") should failTypingWith(InvalidTypeError(IntType, NullType, Position(3,13)))
    fromStatements("boolean x = null;") should failTypingWith(InvalidTypeError(BooleanType, NullType, Position(3,17)))
    fromStatements("Test x = null;") should succeedTyping
  }

  it should "allow assignment to lvalues only" in {
    fromStatements("int i; i = 42;") should succeedTyping
    fromStatements("int[] i; i[3] = 42;") should succeedTyping
    fromMembers("public int i; public void test() { i = 42; }") should succeedTyping
    fromMembers("public int i; public void test() { this.i = 42; }") should succeedTyping
    fromMembers("public void test(int i) { i = 42; }") should succeedTyping

    fromStatements("int i; i + 3 = 42;") should failTypingWith(AssignmentToNonLValueError(Position(3,18)))
    fromStatements("new int[42] = 42;") should failTypingWith(AssignmentToNonLValueError(Position(3,17)))
    fromStatements("new Test() = 42;") should failTypingWith(AssignmentToNonLValueError(Position(3,16)))
    fromStatements("int i; (i = 42) = 42;") should failTypingWith(AssignmentToNonLValueError(Position(3,21)))
    fromStatements("this = new Test();") should failTypingWith(AssignmentToNonLValueError(Position(3,18)))
    fromMembers("public int i() { i() = 42; }") should failTypingWith(AssignmentToNonLValueError(Position(2,26)))
    fromMembers("public int i() { this.i() = 42; }") should failTypingWith(AssignmentToNonLValueError(Position(2,31)))
  }

  it should "check that a non-void function reaches a return statement on every code path" in {
    fromMembers("public void foo() { } ") should succeedTyping
    fromMembers("public int foo() { { { { return 0; } } } 42; } ") should succeedTyping
    fromMembers("public int foo() { { if (true) return 0; else return 1; } 42; }") should succeedTyping
    fromMembers("public int foo() { { if (true) { return 0; } else { return 1; } } 42; }") should succeedTyping

    fromMembers("public int foo() { 42; }") should failTypingWith(MissingReturnStatementError(Position(3,1)))
    fromMembers("public int foo() { { while (true) return 0; } 42; }") should failTypingWith(MissingReturnStatementError(Position(3,1)))
    fromMembers("public int foo() { if (true) return 0; else; 42; }") should failTypingWith(MissingReturnStatementError(Position(3,1)))
    fromMembers("public int foo() { if (true) { if (false) return 0; else; } else return 1; 42; }") should
      failTypingWith(MissingReturnStatementError(Position(3,1)))
  }

  it should "type check the assignment operator" in {
    fromStatements("Test test; Test test2; Test test3 = test2 = test;") should succeedTyping
    fromStatements("Test test; Test test2; Test test3 = test2 = test = null;") should succeedTyping

    "class Test2{} class Test{ public static void main(String[] args) { Test test; Test2 test2 = test = null; }}" should
      failTypingWith(InvalidTypeError(TypeBasic("Test2"), TypeBasic("Test"), Position(1,104)))

    fromStatements("int x; x = 42;") should succeedTyping
    fromStatements("int x; x = true;") should failTypingWith(InvalidTypeError(IntType, BooleanType, Position(3,16)))

    fromStatements("int[][][][] x; x[0][1] = 2;") should failTypingWith(InvalidTypeError(TypeArray(IntType, 2), IntType, Position(3,27)))
    fromStatements("int[][][][] x; x[0][1] = new int[2];") should failTypingWith(InvalidTypeError(TypeArray(IntType, 2), TypeArray(IntType, 1), Position(3,36)))
    fromStatements("int[][][][] x; x[0][1] = new int[2][];") should succeedTyping
    fromStatements("int[][][][] x; x[0][1] = new int[2][][];") should failTypingWith(InvalidTypeError(TypeArray(IntType, 2), TypeArray(IntType, 3), Position(3,40)))
  }

  it should "type check the equality and inequality operator" in {
    for (op <- List("==", "!=")) withClue(op) {
      fromStatements(s"null $op null;") should succeedTyping
      fromStatements(s"new Test() $op null;") should succeedTyping
      fromStatements(s"null $op new Test();") should succeedTyping
      fromStatements(s"42 $op 42;") should succeedTyping
      fromStatements(s"true $op false;") should succeedTyping

      fromStatements(s"true $op null;") should failTypingWith(IncomparableTypesError(BooleanType, NullType, Position(3,13)))
      fromStatements(s"null $op true;") should failTypingWith(IncomparableTypesError(NullType, BooleanType, Position(3,13)))
      fromStatements(s"42 $op null;") should failTypingWith(IncomparableTypesError(IntType, NullType, Position(3,11)))
      fromStatements(s"null $op 42;") should failTypingWith(IncomparableTypesError(NullType, IntType, Position(3,11)))

      fromStatements(s"true $op new Test();") should failTypingWith(IncomparableTypesError(BooleanType, TypeBasic("Test"), Position(3,19)))
      fromStatements(s"new Test() $op true;") should failTypingWith(IncomparableTypesError(TypeBasic("Test"), BooleanType, Position(3,19)))
      fromStatements(s"42 $op new Test();") should failTypingWith(IncomparableTypesError(IntType, TypeBasic("Test"), Position(3,17)))
      fromStatements(s"new Test() $op 42;") should failTypingWith(IncomparableTypesError(TypeBasic("Test"), IntType, Position(3,17)))

      fromStatements(s"true $op 42;") should failTypingWith(IncomparableTypesError(BooleanType, IntType, Position(3,11)))
      fromStatements(s"42 $op true;") should failTypingWith(IncomparableTypesError(IntType, BooleanType, Position(3,11)))

      fromStatements(s"boolean b = 42 $op 42;") should succeedTyping
      fromStatements(s"int i = 42 $op 42;") should failTypingWith(InvalidTypeError(IntType, BooleanType, Position(3,17)))

      fromStatements(s"(1+2) $op 1;") should succeedTyping
      fromStatements(s"(1+null) $op 1;") should failTypingWith(InvalidTypeError(IntType, NullType, NoPosition))
    }
  }

  it should "type check the array access operator" in {
    fromStatements("int x; int[][] xs = new int[1][]; x = xs;") should
      failTypingWith(InvalidTypeError(IntType, TypeArray(IntType, 2), Position(3,41)))
    fromStatements("int x; int[][] xs = new int[1][]; x = xs[1];") should
      failTypingWith(InvalidTypeError(IntType, TypeArray(IntType, 1), Position(3,44)))
    fromStatements("int x; int[][] xs = new int[1][]; x = xs[1][2];") should succeedTyping
    fromStatements("int x = new int[1][][1][2];") should succeedTyping
    fromStatements("int x; int[][] xs = new int[1][]; x = xs[1][2][3];") should
      failTypingWith(ArrayAccessOnNonArrayError(IntType))
    fromStatements("Test xs; xs[1];") should
      failTypingWith(ArrayAccessOnNonArrayError(TypeBasic("Test")))

    fromStatements("int[] x; x[42];") should succeedTyping
    fromStatements("int[] x; x[x[1]];") should succeedTyping
    fromStatements("int[] x; x[x];") should failTypingWith(InvalidTypeError(IntType, TypeArray(IntType, 1), NoPosition))
    fromStatements("int[] x; x[true];") should failTypingWith(InvalidTypeError(IntType, BooleanType, NoPosition))

    fromStatements("Test test; int[] x; x[test];") should failTypingWith(InvalidTypeError(IntType, TypeBasic("Test"), NoPosition))

    fromStatements("int[] x; x[true + 42];") should failTypingWith(InvalidTypeError(IntType, BooleanType, NoPosition))
    fromStatements("int[][] x; x[true + 42][2];") should failTypingWith(InvalidTypeError(IntType, BooleanType, NoPosition))
  }

  it should "type check the other operators" in {
    val intOpsReturningBool = List("< ", "<=", "> ", ">=") // same length, so that error reporting does not need to care later
    val intOpsReturningInt = List("+", "-", "*", "/", "%")
    val intOps = intOpsReturningBool ++ intOpsReturningInt
    val boolOps = List("&&", "||")

    intOps.foreach(op => withClue(op) {
      fromStatements(s"42 $op 43;") should succeedTyping
      fromStatements(s"42 $op true;") should failTypingWith(InvalidTypeError(IntType, BooleanType, NoPosition))
      fromStatements(s"true $op 42;") should failTypingWith(InvalidTypeError(IntType, BooleanType, NoPosition))
      fromStatements(s"true $op false;") should failTypingWith(InvalidTypeError(IntType, BooleanType, NoPosition))
      fromStatements(s"new Test() $op 42;") should failTypingWith(InvalidTypeError(IntType, TypeBasic("Test"), NoPosition))
      fromStatements(s"42 $op new Test();") should failTypingWith(InvalidTypeError(IntType, TypeBasic("Test"), NoPosition))
      fromStatements(s"new int[2][][0] $op 42;") should failTypingWith(InvalidTypeError(IntType, TypeArray(IntType, 1), NoPosition))
      fromStatements(s"42 $op new int[2][][0];") should failTypingWith(InvalidTypeError(IntType, TypeArray(IntType, 1), NoPosition))
    })
    intOpsReturningInt.foreach(op => withClue(op) {
      fromStatements(s"int x = 42 $op 43;") should succeedTyping
      fromStatements(s"boolean x = 42 $op 43;") should failTypingWith(InvalidTypeError(BooleanType, IntType, Position(3,20)))
    })
    intOpsReturningBool.foreach(op => withClue(op) {
      fromStatements(s"int x = 42 $op 43;") should failTypingWith(InvalidTypeError(IntType, BooleanType, Position(3,17)))
      fromStatements(s"boolean x = 42 $op 43;") should succeedTyping
    })
    boolOps.foreach(op => withClue(op) {
      fromStatements(s"42 $op 43;") should failTypingWith(InvalidTypeError(BooleanType, IntType, NoPosition))
      fromStatements(s"42 $op true;") should failTypingWith(InvalidTypeError(BooleanType, IntType, NoPosition))
      fromStatements(s"true $op 42;") should failTypingWith(InvalidTypeError(BooleanType, IntType, NoPosition))
      fromStatements(s"true $op false;") should succeedTyping
      fromStatements(s"new Test() $op true;") should failTypingWith(InvalidTypeError(BooleanType, TypeBasic("Test"), NoPosition))
      fromStatements(s"true $op new Test();") should failTypingWith(InvalidTypeError(BooleanType, TypeBasic("Test"), NoPosition))
      fromStatements(s"new boolean[2][][0] $op true;") should failTypingWith(InvalidTypeError(BooleanType, TypeArray(BooleanType, 1), NoPosition))
      fromStatements(s"true $op new boolean[2][][0];") should failTypingWith(InvalidTypeError(BooleanType, TypeArray(BooleanType, 1), NoPosition))
    })

    fromStatements("-42;") should succeedTyping
    fromStatements("-true;") should failTypingWith(InvalidTypeError(IntType, BooleanType, NoPosition))
    fromStatements("-new Test();") should failTypingWith(InvalidTypeError(IntType, TypeBasic("Test"), NoPosition))
    fromStatements("-new int[42][];") should failTypingWith(InvalidTypeError(IntType, TypeArray(IntType, 2), NoPosition))

    fromStatements("!true;") should succeedTyping
    fromStatements("!42;") should failTypingWith(InvalidTypeError(BooleanType, IntType, NoPosition))
    fromStatements("!new Test();") should failTypingWith(InvalidTypeError(BooleanType, TypeBasic("Test"), NoPosition))
    fromStatements("!new boolean[42][];") should failTypingWith(InvalidTypeError(BooleanType, TypeArray(BooleanType, 2), NoPosition))
  }

  it should "typecheck field accesses" in {
    def testProg(stmts: String) = fromMembers(s"public Test t; public int x; public Test getTest() { return new Test(); } public void test() { $stmts }")
    testProg("this.t = new Test();") should succeedTyping
    testProg("this.t = this.t;") should succeedTyping
    testProg("this.x = 42;") should succeedTyping
    testProg("this.t.x = 42;") should succeedTyping
    testProg("new Test().x = 42;") should succeedTyping
    testProg("getTest().x = 42;") should succeedTyping
    testProg("this.t.getTest().x = 42;") should succeedTyping
    testProg("(new Test[42])[0].x = 42;") should succeedTyping
  }

  it should "typecheck the builtin System.out.println function" in {
    fromStatements("System.out.println();") should failTypingWith(WrongNumberOfParametersError(1, 0, Position(3,21)))
    fromStatements("System.out.println(-42);") should succeedTyping
    fromStatements("System.out.println(false);") should failTypingWith(InvalidTypeError(IntType, BooleanType, NoPosition))
    fromStatements("System.out.println(new int[2]);") should failTypingWith(InvalidTypeError(IntType, TypeArray(IntType, 1), NoPosition))
  }

  it should "disallow big Integer literals" in {
    var value = "13456712834576298365798347569834756234523864759486734857320854032543252342342344"
    fromStatements(s"$value;") should failTypingWith(IntLiteralOutOfRangeError(value, Position(3,81)))
    fromStatements(s"-$value;") should failTypingWith(IntLiteralOutOfRangeError(value, Position(3,82)))

    value = "2147483649"
    fromStatements(s"$value;") should failTypingWith(IntLiteralOutOfRangeError(value, Position(3,11)))
    fromStatements(s"-$value;") should failTypingWith(IntLiteralOutOfRangeError(value, Position(3,12)))

    value = "2147483648"
    fromStatements(s"$value;") shouldNot succeedTyping
    fromStatements(s"-$value;") should succeedTyping
    fromStatements(s"-($value);") shouldNot succeedTyping
    fromStatements(s"-(-$value);") should succeedTyping

    value = "2147483647"
    fromStatements(s"$value;") should succeedTyping
    fromStatements(s"-$value;") should succeedTyping
  }

  it should "properly count the number of declared vars in a main method" in {
    val cls = assertExecClass[Typer]("class Test { public static void main(String[] args) { int a = 3; } }")
    cls.methods(0).numVars shouldBe 2 // parameter + a
  }

  it should "count the this-parameter in methods" in {
    val mth = assertExecMethod[Typer]("public void foo() {}")
    mth.numVars shouldBe 1
  }

  it should "properly count the number of declared vars in a method" in {
    val mth = assertExecMethod[Typer]("public int foo(int a, boolean b) { boolean z; return a; }")
    mth.numVars shouldBe 4 // this, a, b, z
  }

  it should "properly count variables in inner blocks" in {
    val mth = assertExecMethod[Typer]("public void foo() { int a; { int b; } }")
    mth.numVars shouldBe 3
  }
}
