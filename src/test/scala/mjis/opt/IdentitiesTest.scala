package mjis.opt

import firm._
import mjis.CompilerTestMatchers._
import org.scalatest._

class IdentitiesTest extends FlatSpec with Matchers with BeforeAndAfter {
  before {
    Firm.init()
  }

  after {
    Firm.finish()
  }

  it should "apply arithmetic identities" in {
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
}
