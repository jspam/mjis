package mjis.opt

import firm._
import mjis.CompilerTestMatchers._
import mjis.FirmGraphTestHelper
import org.scalatest._

class IdentitiesTest extends FlatSpec with Matchers with BeforeAndAfter {
  before {
    Firm.init()
  }

  after {
    Firm.finish()
  }

  "Identities" should "apply arithmetic identities" in {
    """
      |public void before(int i) {
      |  System.out.println(i + 0);
      |  System.out.println(i * 1);
      |  System.out.println(i / 1);
      |}
    """.stripMargin should optimizeTo(Identities)(
      """
        |public void after(int i) {
        |  System.out.println(i);
        |  System.out.println(i);
        |  System.out.println(i);
        |}
      """.stripMargin)
  }

  it should "optimize Mux with Sel = constant true to the corresponding input value" in {
    val before =
      """start = Start
        |mem = Proj M M, start
        |const0 = Const 0 Is
        |const1 = Const 1 Is
        |const1b = Const true b
        |retval = Mux Is, const1b, const0, const1
        |return = Return, mem, retval
        |end = End, return""".stripMargin

    val after =
      """start = Start
        |mem = Proj M M, start
        |const1 = Const 1 Is
        |return = Return, mem, const1
        |end = End, return""".stripMargin

    val methodType = new MethodType(Array[Type](), Array[Type](new PrimitiveType(Mode.getIs)))
    val beforeMethodEntity = new Entity(Program.getGlobalType, "_4Test_before", methodType)
    FirmGraphTestHelper.buildFirmGraph(beforeMethodEntity, before)
    val afterMethodEntity = new Entity(Program.getGlobalType, "_4Test_after", methodType)
    FirmGraphTestHelper.buildFirmGraph(afterMethodEntity, after)

    "" should optimizeTo(Identities)("") // see constructed graphs
  }

  it should "optimize Mux with Sel = constant false to the corresponding input value" in {
    val before =
      """start = Start
        |mem = Proj M M, start
        |const0 = Const 0 Is
        |const1 = Const 1 Is
        |const0b = Const false b
        |retval = Mux Is, const0b, const0, const1
        |return = Return, mem, retval
        |end = End, return""".stripMargin

    val after =
      """start = Start
        |mem = Proj M M, start
        |const0 = Const 0 Is
        |return = Return, mem, const0
        |end = End, return""".stripMargin

    val methodType = new MethodType(Array[Type](), Array[Type](new PrimitiveType(Mode.getIs)))
    val beforeMethodEntity = new Entity(Program.getGlobalType, "_4Test_before", methodType)
    FirmGraphTestHelper.buildFirmGraph(beforeMethodEntity, before)
    val afterMethodEntity = new Entity(Program.getGlobalType, "_4Test_after", methodType)
    FirmGraphTestHelper.buildFirmGraph(afterMethodEntity, after)

    "" should optimizeTo(Identities)("") // see constructed graphs
  }
}
