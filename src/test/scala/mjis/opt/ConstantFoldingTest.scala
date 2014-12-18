package mjis.opt

import firm._
import org.scalatest._
import mjis.CompilerTestMatchers._

class ConstantFoldingTest extends FlatSpec with Matchers with BeforeAndAfter {
  before {
    Firm.init()
  }

  after {
    Firm.finish()
  }

  it should "apply constant folding for integer expressions" in {
    """
      |public void before() {
      |  System.out.println(2 + 3);
      |  System.out.println(5 - 3);
      |  System.out.println(3 * 4);
      |  System.out.println(12 / 5);
      |  System.out.println(13 % 5);
      |  System.out.println(3 * 4 + 5);
      |  System.out.println(4 % 3);
      |  System.out.println(-12 / 5 + 14 % (10 - 5));
      |}
      """.stripMargin should optimizeTo(ConstantFolding)(
      """
      |public void after() {
      |  System.out.println(5);
      |  System.out.println(2);
      |  System.out.println(12);
      |  System.out.println(2);
      |  System.out.println(3);
      |  System.out.println(17);
      |  System.out.println(1);
      |  System.out.println(2);
      |}
      """.stripMargin)
  }

  it should "analyse while loops correctly" in {
    """
      |public int before(boolean b) {
      |  int x = 1;
      |  while (b) {
      |    x = x % 5;
      |    x = x / x;
      |    x = 2 - x;
      |  }
      | return x;
      |}
      """.stripMargin should optimizeTo(ConstantFolding)(
      """
      |public int after(boolean b) {
      |  int x = 1;
      |  while (b) {
      |    ;
      |  }
      |  return 1;
      |}
      """.stripMargin)
  }

  it should "detect if a variable is assigned to the same value in different branches" in {
    """
     |public int before(boolean b) {
     |  int x = 2;
     |  if (b)
     |    x = 4 - x;
     |  else
     |    x = 2;
     |  return x;
     |}
     """.stripMargin should optimizeTo(ConstantFolding)(
     """
     |public int after(boolean b) {
     |  if (b)
     |    ;
     |  else
     |    ;
     |  return 2;
     |}
     """.stripMargin)
  }

  it should "not optimize div or mod through zero" in {
    """
     |public int before() {
     |  return 5 / 0 + 4 % 0;
     |}""".stripMargin should optimizeTo(ConstantFolding)(
      """
     |public int after() {
     |  return 5 / 0 + 4 % 0;
     |}
     """.stripMargin)
  }
}
