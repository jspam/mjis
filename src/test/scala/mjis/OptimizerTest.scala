package mjis

import firm._
import org.scalatest._
import mjis.CompilerTestMatchers._

class OptimizerTest extends FlatSpec with Matchers with BeforeAndAfter {
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
    """.stripMargin should optimizeTo(
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
    """.stripMargin should optimizeTo(
      """
        |public int after(int j) {
        |  if (j > 2) if (j > 3) { j=2; }
        |  int i = 1;
        |  return i;
        |}
      """.stripMargin)
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
      """.stripMargin should optimizeTo(
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
      """.stripMargin should optimizeTo(
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
     |  int x = 5;
     |  if (b)
     |    x = 10 / x;
     |  else
     |    x = 2;
     |  return x;
     |}
     """.stripMargin should optimizeTo(
      """
     |public int after(boolean b) {
     |  if (b)
     |    ;
     |  else
          ;
     |  return 2;
     |}
     """.stripMargin)
  }

  it should "not optimize div or mod through zero" in {
    """
     |public int before() {
     |  return 5 / 0 + 4 % 0;
     |}""".stripMargin should optimizeTo(
      """
     |public int after() {
     |  return 5 / 0 + 4 % 0;
     |}
     """.stripMargin)
  }

  it should "eliminate common subexpressions" in {
    """
      |public int before(int i, int j) {
      |  int k = i + j;
      |  return k * (i + j);
      |}
    """.stripMargin should optimizeTo(
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
    """.stripMargin should optimizeTo(
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
    """.stripMargin should optimizeTo(
      """
        |public int after(int i, int j) {
        |  return (i + j) * (i - j);
        |}
      """.stripMargin)
  }

  it should "apply arithmetic identities" in {
    """
      |public int before(int i) {
      |  return i + i * 0;
      |}
    """.stripMargin should optimizeTo(
      """
        |public int after(int i) {
        |  return i;
        |}
      """.stripMargin)
  }
}
