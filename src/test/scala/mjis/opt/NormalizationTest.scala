package mjis.opt

import firm._
import mjis.CompilerTestMatchers._
import org.scalatest._

class NormalizationTest extends FlatSpec with Matchers with BeforeAndAfter {
  before {
    Firm.init()
  }

  after {
    Firm.finish()
  }

  it should "normalize nodes" in {
    // needs constant folding for '-2'
    """
      |public void before(int i) {
      |  System.out.println(2 + i);
      |  System.out.println(2 * i);
      |  System.out.println(i - 2);
      |}
    """.stripMargin should optimizeTo(Normalization, after=List(ConstantFolding))(
      """
        |public void after(int i) {
        |  System.out.println(i + 2);
        |  System.out.println(i * 2);
        |  System.out.println(i + (-2));
        |}
      """.stripMargin)
  }
}
