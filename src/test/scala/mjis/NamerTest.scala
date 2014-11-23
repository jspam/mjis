package mjis

import mjis.CompilerTestHelper._
import mjis.CompilerTestMatchers._
import mjis.Namer._
import mjis.ast._
import mjis.Builtins._
import org.scalatest._

class NamerTest extends FlatSpec with Matchers with Inspectors {
  def flattenStatements(stmts: Seq[Statement]): Seq[Statement] = stmts.flatMap {
    case b: Block => flattenStatements(b.statements)
    case s: Statement => List(s)
  }
  def getStatements(input: String) = flattenStatements(assertExecStatements[Namer](input))
  def getMethod(c: ClassDecl, name: String) = c.methods.find(decl => decl.name == name).orNull

  def getRefDecl(s: Statement): Decl = s.asInstanceOf[ExpressionStatement].expr.asInstanceOf[Ref[Decl]].decl.get

  "The namer" should "recognize local variable references" in {
    val statements = getStatements("int x; x;")
    getRefDecl(statements(1)) shouldBe statements(0)
  }

  it should "recognize shadowing variable references" in {
    val statements = getStatements("int x; { boolean x; x; }")
    getRefDecl(statements(2)) shouldBe statements(1)
  }

  it should "recognize method references" in {
    val foo = assertExecMethod[Namer]("public void foo() { foo(); }")
    getRefDecl(foo.body.statements(0)) shouldBe foo
  }

  it should "ignore parameters of a static method" in {
    "class Test{ public static void main(String[] args) { } }" should succeedNaming
  }

  it should "disallow accessing the 'this' pointer in a static method" in {
    "class Test{ public static void main(String[] args) { this; } }" should failNamingWith(DefNotFoundError("this", "value"))
  }

  it should "recognize built-in methods" in {
    val statements = getStatements("1+1;1-1;1*1;1/1;1%1;1<=1;1<1;1==1;1!=1;1>1;1>=1;false||true;true&&false;-1;!true;")
    val expected = List(getMethod(IntDecl, "+"), getMethod(IntDecl, "-"), getMethod(IntDecl, "*"),
      getMethod(IntDecl, "/"), getMethod(IntDecl, "%"), getMethod(IntDecl, "<="),
      getMethod(IntDecl, "<"), EqualsDecl, UnequalDecl, getMethod(IntDecl, ">"),
      getMethod(IntDecl, ">="), getMethod(BooleanDecl, "||"), getMethod(BooleanDecl, "&&"),
      getMethod(IntDecl, "- (unary)"), getMethod(BooleanDecl, "!"))
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
    "class Test { public void test() { System.out.println(42); } public _System System; } " +
      "class _System {}" should failNamingWith(DefNotFoundError("out", "field"))
  }

  it should "fail to recognize System.out.println when a System variable is defined" in {
    fromStatements("int System; System.out.println(42);") should failNamingWith(DefNotFoundError("out", "field"))
  }

  it should "fail to recognize the built-in System in other contexts than System.out.println()" in {
    fromStatements("System;") should failNamingWith(DefNotFoundError("System", "value"))
    fromStatements("System == System;") should failNamingWith(DefNotFoundError("System", "value"))
    fromStatements("new System();") should failNamingWith(DefNotFoundError("System", "type"))
  }

  it should "disallow static methods not called main" in {
    "class Test { public static void mine(String[] foo) {} } " should failNamingWith(InvalidMainMethodNameError())
  }

  it should "disallow programs without a main method" in {
    "class Test { public void main() {} } " should failNamingWith(NoMainMethodError())
  }

  it should "disallow more than one main method" in {
    assertExecFailure[Namer]("class Test { public static void main(String[] args) {} " +
      "public static void main(String[] args) {} }").head shouldBe a [DuplicateDefinitionError]
    assertExecFailure[Namer]("class Test { public static void main(String[] args) {} } " +
      "class Test2 { public static void main(String[] args) {} }").head shouldBe a [DuplicateDefinitionError]
  }

  it should "disallow calling the main method" in {
    "class String{} class Test{ public static void main(String[] args) { main(new String[42]); } }" should
      failNamingWith(DefNotFoundError("this", "value"))
  }

  it should "disallow accessing the main method's parameter" in {
    "class String{} class Test{ public static void main(String[] arguments) { arguments; } }" should
      failNamingWith(DefNotFoundError("arguments", "value"))
  }
}
