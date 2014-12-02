package mjis

import mjis.CompilerTestHelper._
import mjis.CompilerTestMatchers._
import mjis.Namer._
import mjis.ast._
import mjis.Builtins._
import mjis.Position.NoPosition
import org.scalatest._

class NamerTest extends FlatSpec with Matchers with Inspectors {
  def flattenStatements(stmts: Seq[Statement]): Seq[Statement] = stmts.flatMap {
    case b: Block => flattenStatements(b.statements)
    case s: Statement => List(s)
  }
  def getStatements(input: String) = flattenStatements(assertExecStatements[Namer](input))
  def getMethod(c: ClassDecl, name: String) = c.methods.find(decl => decl.name == name).orNull

  def getRefDecl(s: Statement): Decl = s.asInstanceOf[ExpressionStatement].expr.asInstanceOf[Ref[Decl]].decl

  "The namer" should "recognize local variable references" in {
    val statements = getStatements("int x; x;")
    getRefDecl(statements(1)) shouldBe statements(0)
  }

  it should "recognize type references" in {
    val cls = assertExecClass[Namer]("class Test { public static void main(String[] args) { int x; Test y; boolean b; } }")
    val types = cls.methods(0).body.statements.map(_.asInstanceOf[LocalVarDeclStatement].typ.asInstanceOf[TypeBasic].decl)
    types shouldBe List(Builtins.IntDecl, cls, Builtins.BooleanDecl)
  }

  it should "not recognize undefined types" in {
    fromStatements("unknown x;") should failNamingWith(DefNotFoundError("unknown", "type", Position(3,9)))
  }

  it should "disallow shadowing variable references" in {
    assertExecFailure[Namer](fromStatements("int x; { boolean x; x; }")).head shouldBe a[DuplicateDefinitionError]
    assertExecFailure[Namer](fromMembers("public void test(int x) { int x; }")).head shouldBe a[DuplicateDefinitionError]
    fromStatements("{ int x; } { int x; }") should succeedNaming
  }

  it should "recognize method references" in {
    val foo = assertExecMethod[Namer]("public void foo() { foo(); }")
    getRefDecl(foo.body.statements(0)) shouldBe foo
  }

  it should "ignore parameters of a static method" in {
    "class Test{ public static void main(String[] args) { } }" should succeedNaming
  }

  it should "disallow accessing the 'this' pointer in a static method" in {
    "class Test{ public static void main(String[] args) { this; } }" should failNamingWith(DefNotFoundError("this", "value", Position(1, 58)))
  }

  it should "recognize built-in methods" in {
    val statements = getStatements("1+1;1-1;1*1;1/1;1%1;1<=1;1<1;1==1;1!=1;1>1;1>=1;false||true;true&&false;-1;!true;")
    val expected = List(getMethod(IntDecl, "+"), getMethod(IntDecl, "-"), getMethod(IntDecl, "*"),
      getMethod(IntDecl, "/"), getMethod(IntDecl, "%"), getMethod(IntDecl, "<="),
      getMethod(IntDecl, "<"), EqualsDecl, UnequalDecl, getMethod(IntDecl, ">"),
      getMethod(IntDecl, ">="), getMethod(BooleanDecl, "||"), getMethod(BooleanDecl, "&&"),
      getMethod(ExtendedIntDecl, "- (unary)"), getMethod(BooleanDecl, "!"))
    for ((expected, idx) <- expected.zipWithIndex) getRefDecl(statements(idx)) shouldBe expected
  }

  it should "recognize the built-in System.out.println function" in {
    val statements = getStatements("System.out.println(42);")
    getRefDecl(statements(0)) shouldBe SystemOutPrintlnDecl
  }

  it should "recognize the built-in System.out.println function when a System class is defined" in {
    val program = assertExec[Namer]("class Test { public static void main(String[] args) { System.out.println(42); } } " +
      "class Out { public void println(int x) {} } class System { public Out out; }").result
    val statements = program.classes(0).methods(0).body.statements
    getRefDecl(statements(0)) shouldBe SystemOutPrintlnDecl
  }

  it should "recognize a user-defined System.out.println function" in {
    val program = assertExec[Namer]("class Test { public void test() { System.out.println(42); } " +
      "public _System System; public static void main(String[] args){} } " +
      "class _Out { public void println(int x) {} } class _System { public _Out out; }").result
    val statements = program.classes(0).methods(0).body.statements
    getRefDecl(statements(0)) shouldBe program.classes(1).methods(0)
  }

  it should "fail to recognize System.out.println when a System class member is defined" in {
    "class Test { " +
      "public static void main(String[] args) {} " +
      "public void test() { System.out.println(42); }" +
      "public _System System; } " +
      "class _System {}" should failNamingWith(DefNotFoundError("out", "field", Position(1,87)))
  }

  it should "fail to recognize System.out.println when a System variable is defined" in {
    fromStatements("int System; System.out.println(42);") should failNamingWith(DefNotFoundError("out", "field", Position(3,23)))
  }

  it should "fail to recognize the built-in System in other contexts than System.out.println()" in {
    fromStatements("System;") should failNamingWith(DefNotFoundError("System", "value", Position(3,7)))
    fromStatements("System == System;") should failNamingWith(DefNotFoundError("System", "value", Position(3,8)))
    fromStatements("new System();") should failNamingWith(DefNotFoundError("System", "type", Position(3,13)))
  }

  it should "disallow static methods not called main" in {
    "class Test { public static void mine(String[] foo) {} } " should failNamingWith(InvalidMainMethodNameError(Position(1,55)))
  }

  it should "disallow programs without a main method" in {
    "class Test { public void main() {} } " should failNamingWith(NoMainMethodError(Position(1,38)))
  }

  it should "disallow more than one main method" in {
    assertExecFailure[Namer]("class Test { public static void main(String[] args) {} " +
      "public static void main(String[] args) {} }").head shouldBe a [DuplicateDefinitionError]
    assertExecFailure[Namer]("class Test { public static void main(String[] args) {} } " +
      "class Test2 { public static void main(String[] args) {} }").head shouldBe a [DuplicateDefinitionError]
  }

  it should "disallow calling the main method" in {
    assertExecFailure[Namer](
      "class String{} class Test{ public static void main(String[] args) {} public void test() { main(new String[42]); } }").
      head shouldBe a [InaccessibleDeclError]
  }

  it should "disallow accessing methods/fields of the enclosing class in the main method" in {
    "class Test{ public void test() {} public static void main(String[] args) { this.test(); } }" should
      failNamingWith(DefNotFoundError("this", "value", Position(1,80)))
    "class Test{ public void test() {} public static void main(String[] args) { test(); } }" should
      failNamingWith(DefNotFoundError("this", "value", Position(1,82)))
    "class Test{ public void test(int x) {} public static void main(String[] args) { this.test(42); } }" should
      failNamingWith(DefNotFoundError("this", "value", Position(1,85)))
    "class Test{ public void test(int x) {} public static void main(String[] args) { test(42); } }" should
      failNamingWith(DefNotFoundError("this", "value", Position(1,89)))
    "class Test{ public int test; public static void main(String[] args) { int x = this.test; } }" should
      failNamingWith(DefNotFoundError("this", "value", Position(1,83)))
    "class Test{ public int test; public static void main(String[] args) { int x = test; } }" should
      failNamingWith(DefNotFoundError("test", "value", Position(1,83)))
    "class Test{ public int test; public static void main(String[] args) { this.test = 42; } }" should
      failNamingWith(DefNotFoundError("this", "value", Position(1,75)))
    "class Test{ public int test; public static void main(String[] args) { test = 42; } }" should
      failNamingWith(DefNotFoundError("test", "value", Position(1,76)))
    "class String{} class Test{ public static void main(String[] args) { main(new String[42]); } }" should
      failNamingWith(DefNotFoundError("this", "value", Position(1,89)))
  }

  it should "disallow accessing or shadowing the main method's parameter" in {
    assertExecFailure[Namer](
      "class String{} class Test{ public static void main(String[] arguments) { String[] s = arguments; } }").
      head shouldBe a [InaccessibleDeclError]
    assertExecFailure[Namer](
      "class String{} class Test{ public static void main(String[] arguments) { arguments = new String[1]; } }").
      head shouldBe a [InaccessibleDeclError]
    assertExecFailure[Namer](
      "class String{} class Test{ public int args; public static void main(String[] args) { System.out.println(args); } }").
      head shouldBe a [InaccessibleDeclError]
    assertExecFailure[Namer](
      "class Test{ public static void main(String[] args) { int args; } }").
      head shouldBe a [DuplicateDefinitionError]
  }

  it should "check field accesses" in {
    "class _A {}" + fromStatements("_A x = new _A(); x.isAwesome = true;") should failNamingWith(DefNotFoundError("isAwesome", "field", Position(3,30)))
    fromStatements("int x; x.isAwesome = true;") should failNamingWith(DefNotFoundError("isAwesome", "field", Position(3,20)))
    fromStatements("boolean x; x.isFalse = true;") should failNamingWith(DefNotFoundError("isFalse", "field", Position(3,22)))
    fromStatements("int[] x; x.length;")  should failNamingWith(DefNotFoundError("length", "field", Position(3,18)))
    fromStatements("null.foo;")  should failNamingWith(DefNotFoundError("foo", "field", Position(3,9)))
  }

  it should "check method accesses" in {
    "class _A {}" + fromStatements("_A x = new _A(); x.isAwesome();") should failNamingWith(DefNotFoundError("isAwesome", "method", Position(3,31)))
    fromStatements("int x; x.isAwesome();") should failNamingWith(DefNotFoundError("isAwesome", "method", Position(3,21)))
    fromStatements("boolean x; x.isFalse();") should failNamingWith(DefNotFoundError("isFalse", "method", Position(3,23)))
    fromStatements("int[] x; x.length();")  should failNamingWith(DefNotFoundError("length", "method", Position(3,20)))
    fromStatements("null.foo();")  should failNamingWith(DefNotFoundError("foo", "method", Position(3,11)))
  }

  it should "disallow null as type name" in {
    assertExecFailure[Namer](fromMembers("public null x;", true)).head shouldBe a [Parser.UnexpectedTokenError]
  }

  it should "disallow two classes, methods, fields, parameters or variables with the same name" in {
    assertExecFailure[Namer]("class Test{ public static void main(String[] args) {} } class Test{}").head shouldBe a [DuplicateDefinitionError]
    assertExecFailure[Namer](fromMembers("public int x; public boolean x;")).head shouldBe a [DuplicateDefinitionError]
    assertExecFailure[Namer](fromMembers("public int x(){} public int x(){}")).head shouldBe a [DuplicateDefinitionError]
    assertExecFailure[Namer](fromMembers("public int x(int y){} public int x(boolean y){}")).head shouldBe a [DuplicateDefinitionError]
    assertExecFailure[Namer](fromMembers("public int x(){} public boolean x(){}")).head shouldBe a [DuplicateDefinitionError]
    assertExecFailure[Namer](fromMembers("public void foo(int x, int y, int z, int x) { }")).head shouldBe a [DuplicateDefinitionError]
    assertExecFailure[Namer](fromStatements("{ { int x; int x; } }")).head shouldBe a [DuplicateDefinitionError]
    assertExecFailure[Namer](fromStatements("{ { int x; boolean x; } }")).head shouldBe a [DuplicateDefinitionError]
    assertExecFailure[Namer](fromStatements("{ { int x; int[] x; } }")).head shouldBe a [DuplicateDefinitionError]
  }

  it should "allow the same name in different types of scopes" in {
    fromMembers("public int x; public int x() { x; x(); }") should succeedNaming
    fromMembers("public int x; public boolean x() { x; x(); }") should succeedNaming
    "class foo { public static void main(String[] args) {} public int foo; public boolean foo() { foo foo; } }" should succeedNaming

    val program = assertExec[Namer](fromMembers("public int x; public int y() { boolean x; x = this.x; }")).result
    val methodBody = program.classes(0).methods(0).body
    val fieldDecl = program.classes(0).fields(0)
    val localVarDecl = methodBody.statements(0)
    val statement = methodBody.statements(1).asInstanceOf[ExpressionStatement].expr.asInstanceOf[Assignment]
    statement.lhs.asInstanceOf[Ref[Decl]].decl shouldBe localVarDecl
    statement.rhs.asInstanceOf[Ref[Decl]].decl shouldBe fieldDecl
  }

  it should "allow(!) accessing a local variable in its initializer" in {
    fromStatements("int x = x;") should succeedNaming
  }

  it should "disallow accessing fields/methods of Literals" in {
    fromStatements(s"int x = 2147483648.x;") should failNamingWith(DefNotFoundError("x", "field", Position(3,21)))
    fromStatements(s"int x = 2147483648.x();") should failNamingWith(DefNotFoundError("x", "method", Position(3,23)))

    fromStatements(s"int x = 42.x;") should failNamingWith(DefNotFoundError("x", "field", Position(3,13)))
    fromStatements(s"int x = 42.x();") should failNamingWith(DefNotFoundError("x", "method", Position(3,15)))

    fromStatements(s"int x = true.x;") should failNamingWith(DefNotFoundError("x", "field", Position(3,15)))
    fromStatements(s"int x = true.x();") should failNamingWith(DefNotFoundError("x", "method", Position(3,17)))
  }
}
