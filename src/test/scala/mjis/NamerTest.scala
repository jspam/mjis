package mjis

import mjis.CompilerTestMatchers._
import mjis.ast._
import mjis.Builtins._
import org.scalatest._

class NamerTest extends FlatSpec with Matchers with Inspectors {
  def parseProgram(program: String): Program = {
    val lexer = new Lexer(program)
    val parser = new Parser(lexer.result)
    // The lexer result is only traversable once, so evaluate it only after the parser has finished.
    parser.result
    lexer should succeedLexing()
    parser should succeedParsing()
    parser.result.get
  }
  def parseStatements(statements: String): Program = parseProgram(
    "class Test { public void test() {" + System.lineSeparator() + statements + System.lineSeparator() + "} }"
  )

  /** Gets the non-Block statements of the first method in the program in a linearized form */
  def getStatements(p: Program): Seq[Statement] = getStatements(p.classes(0).methods(0).body)
  def getStatements(s: Statement): Seq[Statement] = s match {
    case b: Block => b.statements.flatMap(getStatements)
    case s: Statement => List(s)
  }
  def getMethod(c: ClassDecl, name: String) = c.methods.find(decl => decl.name == name).orNull

  def getRefDecl(s: Statement): Decl = s.asInstanceOf[ExpressionStatement].expr.asInstanceOf[Ref[Decl]].decl.get

  "The namer" should "recognize local variable references" in {
    val program = parseStatements("int x; x;")
    program should succeedNaming
    val statements = getStatements(program)
    getRefDecl(statements(1)) shouldBe statements(0)
  }

  it should "recognize shadowing variable references" in {
    val program = parseStatements("int x; { boolean x; x; }")
    program should succeedNaming
    val statements = getStatements(program)
    getRefDecl(statements(2)) shouldBe statements(1)
  }

  it should "recognize method references" in {
    val program = parseStatements("test();")
    program should succeedNaming
    val statements = getStatements(program)
    getRefDecl(statements(0)) shouldBe program.classes(0).methods(0)
  }

  it should "disallow accessing the 'this' pointer in a static method" in {
    val program = parseProgram("class Test{ public static void foo(String[] fooArgs) { this; } }")
    program shouldNot succeedNaming
  }

  it should "recognize built-in methods" in {
    val program = parseStatements("1+1;1-1;1*1;1/1;1%1;1<=1;1<1;1==1;1!=1;1>1;1>=1;false||true;true&&false;-1;!true;")
    program should succeedNaming
    val statements = getStatements(program)
    val expected = List(getMethod(IntDecl, "+"), getMethod(IntDecl, "-"), getMethod(IntDecl, "*"),
      getMethod(IntDecl, "/"), getMethod(IntDecl, "%"), getMethod(IntDecl, "<="),
      getMethod(IntDecl, "<"), EqualsDecl, UnequalDecl, getMethod(IntDecl, ">"),
      getMethod(IntDecl, ">="), getMethod(BooleanDecl, "||"), getMethod(BooleanDecl, "&&"),
      getMethod(IntDecl, "- (unary)"), getMethod(BooleanDecl, "!"))
    for ((expected, idx) <- expected.zipWithIndex) getRefDecl(statements(idx)) shouldBe expected
  }

  it should "recognize the built-in System.out.println function" in {
    val program = parseStatements("System.out.println(42);")
    program should succeedNaming
    val statements = getStatements(program)
    getRefDecl(statements(0)) shouldBe SystemOutPrintlnDecl
  }

  it should "recognize the built-in System.out.println function when a System class is defined" in {
    val program = parseProgram("class Test { public void test() { System.out.println(42); } } " +
      "class Out { public void println(int x) {} } class System { public Out out; }")
    program should succeedNaming
    val statements = getStatements(program)
    getRefDecl(statements(0)) shouldBe SystemOutPrintlnDecl
  }

  it should "recognize a user-defined System.out.println function" in {
    val program = parseProgram("class Test { public void test() { System.out.println(42); } public _System System; } " +
      "class _Out { public void println(int x) {} } class _System { public _Out out; }")
    program should succeedNaming
    val statements = getStatements(program)
    getRefDecl(statements(0)) shouldBe program.classes(1).methods(0)
  }

  it should "fail to recognize System.out.println when a System class member is defined" in {
    val program = parseProgram(
      "class Test { public void test() { System.out.println(42); } public _System System; } " +
      "class _System {}")
    program shouldNot succeedNaming
  }

  it should "fail to recognize System.out.println when a System variable is defined" in {
    val program = parseStatements("int System; System.out.println(42);")
    program shouldNot succeedNaming
  }

  it should "fail to recognize the built-in System in other contexts than System.out.println()" in {
    val programs = List(
      parseStatements("System;"),
      parseStatements("System == System;"),
      parseStatements("new System();")
    )
    for ((program, idx) <- programs.zipWithIndex) {
      withClue(s"Program $idx") { program shouldNot succeedNaming }
    }
  }

}
