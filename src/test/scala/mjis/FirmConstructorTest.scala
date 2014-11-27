package mjis

import firm._
import mjis.CompilerTestHelper._
import mjis.CompilerTestMatchers._
import org.scalatest._

class FirmConstructorTest extends FlatSpec with Matchers with BeforeAndAfter {

  var IntType: PrimitiveType = null
  var BooleanType: PrimitiveType = null

  def initFirm() = {
    Firm.init()
    IntType = new PrimitiveType(Mode.getIs)
    BooleanType = new PrimitiveType(Mode.getb)
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

  def returnConstMethodFirm(const: String = "Const 0 Is") =
    s"""
      |start = Start
      |mem = Proj M M, start
      |const0 = $const
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
    val m2 = FirmGraphTestHelper.buildFirmGraph(m2MethodEntity, returnConstMethodFirm())
    fromMembers("public int m2() {return 0;}") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, m2))

    reinitFirm()
    val m3MethodEntity = methodEntity("__expected_m3", IntType, Seq(IntType))
    val m3 = FirmGraphTestHelper.buildFirmGraph(m3MethodEntity, returnConstMethodFirm())
    fromMembers("public int m3(int p1) {return 0;}") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, m3))

    reinitFirm()
    val m4MethodEntity = methodEntity("__expected_m4", BooleanType, Seq())
    val m4 = FirmGraphTestHelper.buildFirmGraph(m4MethodEntity, returnConstMethodFirm("Const false b"))
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

  it should "create FIRM graphs for Integer comparison expressions" in {
    def progTemplate(op: String) = s"""
        |start = Start
        |const1 = Const 1 Is
        |const2 = Const 2 Is
        |$op
        |mem_before_return = Proj M M, start
        |return = Return, mem_before_return, retval
        |end = End, return
      """.stripMargin

    val mEqualMethodEntity = methodEntity("__expected_m_equal", BooleanType, Seq())
    val mEqual = FirmGraphTestHelper.buildFirmGraph(mEqualMethodEntity, progTemplate("retval = Cmp Equal, const1, const2"))
    fromMembers("public boolean m_equal() { return 1 == 2; }") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, mEqual))

    reinitFirm()
    val mUnequalMethodEntity = methodEntity("__expected_m_unequal", BooleanType, Seq())
    val mUnequal = FirmGraphTestHelper.buildFirmGraph(mUnequalMethodEntity,
      progTemplate("eq = Cmp Equal, const1, const2\nretval = Not b, eq"))
    fromMembers("public boolean m_unequal() { return 1 != 2; }") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, mUnequal))

    reinitFirm()
    val mLessMethodEntity = methodEntity("__expected_m_less", BooleanType, Seq())
    val mLess = FirmGraphTestHelper.buildFirmGraph(mLessMethodEntity,
      progTemplate("retval = Cmp Less, const1, const2"))
    fromMembers("public boolean m_less() { return 1 < 2; }") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, mLess))

    reinitFirm()
    val mLessEqualMethodEntity = methodEntity("__expected_m_less_equal", BooleanType, Seq())
    val mLessEqual = FirmGraphTestHelper.buildFirmGraph(mLessEqualMethodEntity,
      progTemplate("retval = Cmp LessEqual, const1, const2"))
    fromMembers("public boolean m_less_equal() { return 1 <= 2; }") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, mLessEqual))

    reinitFirm()
    val mGreaterMethodEntity = methodEntity("__expected_m_greater", BooleanType, Seq())
    val mGreater = FirmGraphTestHelper.buildFirmGraph(mGreaterMethodEntity,
      progTemplate("retval = Cmp Greater, const1, const2"))
    fromMembers("public boolean m_greater() { return 1 > 2; }") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, mGreater))

    reinitFirm()
    val mGreaterEqualMethodEntity = methodEntity("__expected_m_greater_equal", BooleanType, Seq())
    val mGreaterEqual = FirmGraphTestHelper.buildFirmGraph(mGreaterEqualMethodEntity,
      progTemplate("retval = Cmp GreaterEqual, const1, const2"))
    fromMembers("public boolean m_greater_equal() { return 1 >= 2; }") should succeedFirmConstructingWith(List(getEmptyMainMethodGraph, mGreaterEqual))
  }

}
