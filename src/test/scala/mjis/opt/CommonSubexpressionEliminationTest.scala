package mjis.opt

import firm._
import org.scalatest._
import mjis.CompilerTestMatchers._

class CommonSubexpressionEliminationTest extends FlatSpec with Matchers with BeforeAndAfter {
  before {
    Firm.init()
  }

  after {
    Firm.finish()
  }

  "Common Subexpression Elimination" should "eliminate common subexpressions" in {
    """
      |public int before(int i, int j) {
      |  int k = i + j;
      |  return k * (i + j);
      |}
    """.stripMargin should optimizeTo(CommonSubexpressionElimination)(
      """
        |public int after(int i, int j) {
        |  int k = i + j;
        |  return k * k;
        |}
      """.stripMargin)
  }

  it should "not eliminate common subexpressions across blocks" in {
    """
      |public int before(int i, int j) {
      |  int k = i + j;
      |  if (i == 0)
      |    return k * (i + j);
      |  else
      |    return k * (i + j);
      |}
    """.stripMargin should optimizeTo(CommonSubexpressionElimination)(
      """
        |public int after(int i, int j) {
        |  int k = i + j;
        |  if (i == 0)
        |    return k * (i + j);
        |  else
        |    return k * (i + j);
        |}
      """.stripMargin)
  }

  it should "not eliminate expressions with the same subexpressions, but of different type" in {
    """
      |public int before(int i, int j) {
      |  return (i + j) * (i - j);
      |}
    """.stripMargin should optimizeTo(CommonSubexpressionElimination)(
      """
        |public int after(int i, int j) {
        |  return (i + j) * (i - j);
        |}
      """.stripMargin)
  }
}
