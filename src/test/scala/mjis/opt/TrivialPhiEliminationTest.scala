package mjis.opt

import firm._
import org.scalatest._
import mjis.CompilerTestMatchers._

class TrivialPhiEliminationTest extends FlatSpec with Matchers with BeforeAndAfter {
  before {
    Firm.init()
  }

  after {
    Firm.finish()
  }

  "The optimizer" should "remove trivial phis" in {
    """
      |public int before(int j) {
      |  int i = 1;
      |  if (j > 2) { j=2; }
      |  return i;
      |}
    """.stripMargin should optimizeTo(TrivialPhiElimination)(
      """
        |public int after(int j) {
        |  if (j > 2) { j=2; }
        |  int i = 1;
        |  return i;
        |}
      """.stripMargin)
  }

  it should "recursively remove trivial phis" in {
    """
      |public int before(int j) {
      |  int i = 1;
      |  if (j > 2) if (j > 3) { j=2; }
      |  return i;
      |}
    """.stripMargin should optimizeTo(TrivialPhiElimination)(
      """
        |public int after(int j) {
        |  if (j > 2) if (j > 3) { j=2; }
        |  int i = 1;
        |  return i;
        |}
      """.stripMargin)
  }

}
