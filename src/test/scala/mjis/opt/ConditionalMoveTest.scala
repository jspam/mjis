package mjis.opt

import firm._
import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}
import mjis.CompilerTestMatchers._
import mjis.FirmGraphTestHelper

class ConditionalMoveTest extends FlatSpec with Matchers with BeforeAndAfter {
  before {
    Firm.init()
  }

  after {
    Firm.finish()
  }

  "Conditional Move conversion" should "convert if/then/else structures to conditional moves" in {
    val methodEntity = new Entity(Program.getGlobalType, "_4Test_after", new MethodType(
      Array[Type](new PrimitiveType(Mode.getP), new PrimitiveType(Mode.getIs)),
      Array[Type](new PrimitiveType(Mode.getIs))
    ))
    FirmGraphTestHelper.buildFirmGraph(methodEntity,
      """start = Start
        |mem = Proj M M, start
        |args = Proj T T_args, start
        |arg_j = Proj Is Arg 1, args
        |const0 = Const 0 Is
        |const1 = Const 1 Is
        |and = And Is, arg_j, const1
        |cmp = Cmp Equal, const0, and
        |# Mux, ifFalse, ifTrue
        |retval = Mux Is, cmp, const0, const1
        |return = Return, mem, retval
        |end = End, return""".stripMargin)

    """public int before(int j) {
      |  int x;
      |  if (j % 2 == 0) {
      |    x = 1;
      |  } else {
      |    x = 0;
      |  }
      |  return x;
      |}
    """.stripMargin should optimizeTo(ConditionalMoves, after = Seq(Identities), before = Seq(Identities))("") // see constructed graph
  }

  it should "convert comparisons against Booleans" in {
    val methodEntity = new Entity(Program.getGlobalType, "_4Test_after", new MethodType(
      Array[Type](new PrimitiveType(Mode.getP), new PrimitiveType(Mode.getBu)),
      Array[Type](new PrimitiveType(Mode.getIs))
    ))
    FirmGraphTestHelper.buildFirmGraph(methodEntity,
      """start = Start
        |mem = Proj M M, start
        |args = Proj T T_args, start
        |arg_b = Proj Bu Arg 1, args
        |const0 = Const 0 Is
        |const1 = Const 1 Is
        |const1b = Const 1 Bu
        |cmp = Cmp Equal, const1b, arg_b
        |# Mux, ifFalse, ifTrue
        |retval = Mux Is, cmp, const0, const1
        |return = Return, mem, retval
        |end = End, return""".stripMargin)

    """public int before(boolean b) {
      |  int x;
      |  if (b) {
      |    x = 1;
      |  } else {
      |    x = 0;
      |  }
      |  return x;
      |}
    """.stripMargin should optimizeTo(ConditionalMoves, after = Seq(Identities), before = Seq(Identities))("") // see constructed graph
  }

  it should "not convert if more than one value depends on the condition" in {
    def method(name: String) = s"""public int $name(int j) {
      |  int x; int y;
      |  if (j % 2 == 0) {
      |    x = 1;
      |    y = 2;
      |  } else {
      |    x = 0;
      |    y = 1;
      |  }
      |  return x + y;
      |}
    """.stripMargin

    method("before") should optimizeTo(ConditionalMoves, after = Seq(Identities), before = Seq(Identities))(method("after"))
  }

  it should "not convert if the comparison is likely easy to predict" in {
    def method(name: String) = s"""public int $name(int j) {
      |  int x;
      |  if (j == 0) {
      |    x = 1;
      |  } else {
      |    x = 0;
      |  }
      |  return x;
      |}
    """.stripMargin

    method("before") should optimizeTo(ConditionalMoves, after = Seq(Identities), before = Seq(Identities))(method("after"))
  }

  it should "not convert if the operands are expensive to calculate" in {
    def method(name: String) = s"""public int $name(int j, int k, int l) {
      |  int x;
      |  if (j % 2 == 0) {
      |    x = 1 + k + l;
      |  } else {
      |    x = 0;
      |  }
      |  return x;
      |}
    """.stripMargin

    method("before") should optimizeTo(ConditionalMoves, after = Seq(Identities), before = Seq(Identities))(method("after"))
  }

  it should "not convert if the Phi's mode is a Boolean" in {
    // cmov does not work on 1-byte registers. Maybe we can do that later with setcc instruction.
    def method(name: String) = s"""public boolean $name(int j) {
      |  boolean result;
      |  if (j % 2 == 0) {
      |    result = true;
      |  } else {
      |    result = false;
      |  }
      |  return result;
      |}
    """.stripMargin

    method("before") should optimizeTo(ConditionalMoves, after = Seq(Identities), before = Seq(Identities))(method("after"))
  }

}
