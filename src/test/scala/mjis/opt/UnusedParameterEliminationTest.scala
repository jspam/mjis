package mjis.opt

import firm._
import mjis.CompilerTestMatchers._
import mjis.CompilerTestHelper._
import mjis.{FirmGraphTestHelper, FirmConstructor}
import org.scalatest._
import scala.collection.JavaConversions._

class UnusedParameterEliminationTest extends FlatSpec with Matchers with BeforeAndAfter {

  before {
    Firm.init()
  }

  after {
    Firm.finish()
  }

  "Unused parameter elimination" should "eliminate unused parameters" in {
    assertExec[FirmConstructor](fromMembers("""
      |public int foo(int foo, int bar, int baz) { return bar; }
      |
      |public int bar(int j) {
      |  return foo(j, j, j);
      |}
      |
      |public static void main(String[] args) { new Test().bar(42); }
    """.stripMargin, mainMethod = false))

    UnusedParameterElimination.optimize()

    // Check that the argument numbers of foo have been rearranged
    val fooAfterEntity = new Entity(Program.getGlobalType, "_4Test_foo_after", new MethodType(
      Array[Type](new PrimitiveType(Mode.getIs)),
      Array[Type](new PrimitiveType(Mode.getIs))))

    val mFooAfter = FirmGraphTestHelper.buildFirmGraph(fooAfterEntity,
      """
        |start = Start
        |args = Proj T T_args, start
        |arg_bar = Proj Is Arg 0, args
        |mem_before_return = Proj M M, start
        |return = Return, mem_before_return, arg_bar
        |end = End, return
      """.stripMargin)

    firm.Program.getGraphs.find(_.getEntity.getName == "_4Test_foo").get should beIsomorphicTo(mFooAfter)

    // Check that the parameters of the call to foo have been rearranged
    val barAfterEntity = new Entity(Program.getGlobalType, "_4Test_bar_after", new MethodType(
      Array[Type](new PrimitiveType(Mode.getIs)),
      Array[Type](new PrimitiveType(Mode.getIs))))

    val mBarAfter = FirmGraphTestHelper.buildFirmGraph(barAfterEntity,
      """
        |start = Start
        |args = Proj T T_args, start
        |# The this pointer is unused, and thus j is argument 0
        |arg_j = Proj Is Arg 0, args
        |mem_before_call = Proj M M, start
        |addr_foo = Addr _4Test_foo
        |# only one argument, and no this pointer
        |call = Call _4Test_foo, mem_before_call, addr_foo, arg_j
        |res = Proj T T_result, call
        |retval = Proj Is Arg 0, res
        |mem_after_call = Proj M M, call
        |return = Return, mem_after_call, retval
        |end = End, return
      """.stripMargin)

    firm.Program.getGraphs.find(_.getEntity.getName == "_4Test_bar").get should beIsomorphicTo(mBarAfter)
  }

}
