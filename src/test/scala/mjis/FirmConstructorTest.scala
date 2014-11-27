package mjis

import firm._
import mjis.CompilerTestHelper._
import mjis.CompilerTestMatchers._
import org.scalatest._

class FirmConstructorTest extends FlatSpec with Matchers with BeforeAndAfter {

  var IntType: PrimitiveType = null

  def initFirm() = {
    Firm.init()
    IntType = new PrimitiveType(Mode.getIs)
  }

  def reinitFirm(): Unit = {
    Firm.finish()
    initFirm()
  }

  before { initFirm() }
  after { Firm.finish() }

  val emptyMethodFirm =
    """
      |start = Start
      |mem = Proj M M, start
      |return = Return, mem
      |end = End, return
    """.stripMargin

  val returnZeroMethodFirm =
    """
      |start = Start
      |mem = Proj M M, start
      |const0 = Const 0 Is
      |return = Return, mem, const0
      |end = End, return
    """.stripMargin

  def getEmptyMainMethodGraph = {
    val methodEntity = new Entity(Program.getGlobalType, "__expected_main", new MethodType(Array[Type](), Array[Type]()))
    FirmGraphTestHelper.buildFirmGraph(methodEntity, emptyMethodFirm)
  }

  "The FIRM graph constructor" should "transform the smallest possible program" in {
    fromMembers("") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph))
  }

  it should "create methods with the correct types" in {
    val m1MethodEntity = new Entity(Program.getGlobalType, "__expected_m1", new MethodType(Array[Type](), Array[Type]()))
    val m1 = FirmGraphTestHelper.buildFirmGraph(m1MethodEntity, emptyMethodFirm)
    fromMembers("public void m1() {}") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, m1))

    reinitFirm()
    val m2MethodEntity = new Entity(Program.getGlobalType, "__expected_m2", new MethodType(Array[Type](), Array[Type](IntType)))
    val m2 = FirmGraphTestHelper.buildFirmGraph(m2MethodEntity, returnZeroMethodFirm)
    fromMembers("public int m2() {return 0;}") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, m2))

    reinitFirm()
    val m3MethodEntity = new Entity(Program.getGlobalType, "__expected_m3", new MethodType(Array[Type](IntType), Array[Type](IntType)))
    val m3 = FirmGraphTestHelper.buildFirmGraph(m3MethodEntity, returnZeroMethodFirm)
    fromMembers("public int m3(int p1) {return 0;}") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, m3))

    reinitFirm()
    val m4MethodEntity = new Entity(Program.getGlobalType, "__expected_m4", new MethodType(Array[Type](), Array[Type](IntType)))
    val m4 = FirmGraphTestHelper.buildFirmGraph(m4MethodEntity, returnZeroMethodFirm)
    fromMembers("public boolean m4() {return false;}") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, m4))
  }

}
