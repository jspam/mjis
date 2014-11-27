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

  def methodEntity(name: String, returnType: Type, paramTypes: Seq[Type]) = {
    new Entity(Program.getGlobalType, name, new MethodType(paramTypes.toArray, returnType match {
      case null => Array[Type]()
      case t => Array[Type](t)
    }))
  }

  def getEmptyMainMethodGraph = {
    val mainMethodEntity = methodEntity("__expected_main", null, Seq())
    FirmGraphTestHelper.buildFirmGraph(mainMethodEntity, emptyMethodFirm)
  }

  "The FIRM graph constructor" should "transform the smallest possible program" in {
    fromMembers("") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph))
  }

  it should "create methods with the correct types" in {
    val m1MethodEntity = methodEntity("__expected_m1", null, Seq())
    val m1 = FirmGraphTestHelper.buildFirmGraph(m1MethodEntity, emptyMethodFirm)
    fromMembers("public void m1() {}") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, m1))

    reinitFirm()
    val m2MethodEntity = methodEntity("__expected_m2", IntType, Seq())
    val m2 = FirmGraphTestHelper.buildFirmGraph(m2MethodEntity, returnZeroMethodFirm)
    fromMembers("public int m2() {return 0;}") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, m2))

    reinitFirm()
    val m3MethodEntity = methodEntity("__expected_m3", IntType, Seq(IntType))
    val m3 = FirmGraphTestHelper.buildFirmGraph(m3MethodEntity, returnZeroMethodFirm)
    fromMembers("public int m3(int p1) {return 0;}") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, m3))

    reinitFirm()
    val m4MethodEntity = methodEntity("__expected_m4", IntType, Seq())
    val m4 = FirmGraphTestHelper.buildFirmGraph(m4MethodEntity, returnZeroMethodFirm)
    fromMembers("public boolean m4() {return false;}") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, m4))
  }

  it should "create FIRM graphs for Integer arithmetic expressions" in {
    def progTemplate(op: String) = s"""
        |start = Start
        |const1 = Const 1 Is
        |const2 = Const 2 Is
        |$op
        |return = Return, mem_before_return, retval
        |end = End, return
      """.stripMargin

    val mPlusMethodEntity = methodEntity("__expected_m_plus", IntType, Seq())
    val mPlus = FirmGraphTestHelper.buildFirmGraph(mPlusMethodEntity,
      progTemplate("retval = Add Is, const1, const2\nmem_before_return = Proj M M, start"))
    fromMembers("public int m_plus() { return 1 + 2; }") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, mPlus))

    reinitFirm()
    val mMinusMethodEntity = methodEntity("__expected_m_minus", IntType, Seq())
    val mMinus = FirmGraphTestHelper.buildFirmGraph(mMinusMethodEntity,
      progTemplate("retval = Sub Is, const1, const2\nmem_before_return = Proj M M, start"))
    fromMembers("public int m_minus() { return 1 - 2; }") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, mMinus))

    reinitFirm()
    val mMultMethodEntity = methodEntity("__expected_m_mult", IntType, Seq())
    val mMult = FirmGraphTestHelper.buildFirmGraph(mMultMethodEntity,
      progTemplate("retval = Mul Is, const1, const2\nmem_before_return = Proj M M, start"))
    fromMembers("public int m_mult() { return 1 * 2; }") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, mMult))

    reinitFirm()
    val mDivMethodEntity = methodEntity("__expected_m_div", IntType, Seq())
    val mDiv = FirmGraphTestHelper.buildFirmGraph(mDivMethodEntity,
      progTemplate(
        """mem = Proj M M, start
          |divmod = Div Is, mem, const1, const2
          |retval = Proj Is ResDiv, divmod
          |mem_before_return = Proj M M, divmod
        """.stripMargin))
    fromMembers("public int m_div() { return 1 / 2; }") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, mDiv))

    reinitFirm()
    val mModMethodEntity = methodEntity("__expected_m_mod", IntType, Seq())
    val mMod = FirmGraphTestHelper.buildFirmGraph(mModMethodEntity,
      progTemplate(
        """mem = Proj M M, start
          |divmod = Mod Is, mem, const1, const2
          |retval = Proj Is ResMod, divmod
          |mem_before_return = Proj M M, divmod
        """.stripMargin))
    fromMembers("public int m_mod() { return 1 % 2; }") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, mMod))
  }

}
