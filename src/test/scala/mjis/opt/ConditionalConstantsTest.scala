package mjis.opt

import firm._
import mjis.CompilerTestMatchers._
import org.scalatest._

class ConditionalConstantsTest extends FlatSpec with Matchers with BeforeAndAfter {
  before {
    Firm.init()
  }

  after {
    Firm.finish()
  }

  "Conditional Constants" should "conditionally replace branch nodes" in {
    """
      |public void before(int i) {
      |  if (i == 1)
      |    System.out.println(i);
      |  else
      |    System.out.println(i);
      |}
    """.stripMargin should optimizeTo(ConditionalConstants)(
      """
        |public void foo(int i) {}
        |public void after(int i) {
        |  if (i == 1)
        |    System.out.println(1);
        |  else
        |    System.out.println(i);
        |}
      """.stripMargin)
  }

  it should "unconditionally replace after loops" in {
    """
      |public int before(int i) {
      |  while (i != 1)
      |    System.out.println(i);
      |  return i;
      |}
    """.stripMargin should optimizeTo(ConditionalConstants)(
      """
        |public int after(int i) {
        |  while (i != 1)
        |    System.out.println(i);
        |  return 1;
        |}
      """.stripMargin)
  }

  it should "make sure the use is actually dominated" in {
    """
      |public void before(int i) {
      |  if (i == 0 || i == 1)
      |    System.out.println(i);
      |  else
      |    System.out.println(i);
      |}
    """.stripMargin should notOptimize(ConditionalConstants)
  }

}
