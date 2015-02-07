package mjis.opt

import firm._
import mjis.CompilerTestMatchers._
import org.scalatest._

class PureFunctionCallEliminationTest extends FlatSpec with Matchers with BeforeAndAfter {
  before {
    Firm.init()
  }

  after {
    Firm.finish()
  }

  "Pure Function Call elimination" should "eliminate unused calls to calloc" in {
    "public void before() { new Test(); }" should optimizeTo(PureFunctionCallElimination)("public void after() { }")
  }

  it should "not eliminate calls to impure functions" in {
    """public void impure() { System.out.println(42); }
      |public void before() { impure(); }
      |""".stripMargin should optimizeTo(PureFunctionCallElimination)(
        """public void impure() { System.out.println(42); }
          |public void after() { impure(); }""".stripMargin)
  }
}
