package mjis.opt

import firm._
import mjis.CompilerTestMatchers._
import org.scalatest._

class RedundantLoadEliminationTest extends FlatSpec with Matchers with BeforeAndAfter {
  before {
    Firm.init()
  }

  after {
    Firm.finish()
  }

  "Redundant Load elimination" should "eliminate redundant member loads-after-stores" in {
    """
      |public int i;
      |public int before(Test t, int i) {
      |  t.i = i;
      |  return t.i;
      |}
    """.stripMargin should optimizeTo(RedundantLoadElimination, after = Seq(CommonSubexpressionElimination))(
      """
        |public int i;
        |public int after(Test t, int i) {
        |  t.i = i;
        |  return i;
        |}
      """.stripMargin)
  }

  it should "eliminate redundant array loads-after-stores" in {
    """
      |public int before(int[] xs, int i, int j) {
      |  xs[i] = j;
      |  return xs[i];
      |}
    """.stripMargin should optimizeTo(RedundantLoadElimination, after = Seq(CommonSubexpressionElimination))(
      """
        |public int after(int[] xs, int i, int j) {
        |  xs[i] = j;
        |  return j;
        |}
      """.stripMargin)
  }

  it should "eliminate redundant loads-after-loads" in {
    """
      |public int i;
      |public int before(Test t) {
      |  return t.i + t.i;
      |}
    """.stripMargin should optimizeTo(RedundantLoadElimination, after = Seq(CommonSubexpressionElimination))(
      """
        |public int i;
        |public int after(Test t) {
        |  int i = t.i;
        |  return i + i;
        |}
      """.stripMargin)
  }

  it should "ignore obviously unaliased stores" in {
    """
      |public int i;
      |public int j;
      |public int before(Test t) {
      |  int[] xs = new int[5];
      |  int[] ys = new int[5];
      |  xs[0] = 1;
      |  t.i = 2;
      |  ys[0] = 3;
      |  t.j = 4;
      |  return xs[0] + t.i;
      |}
    """.stripMargin should optimizeTo(RedundantLoadElimination, after = Seq(CommonSubexpressionElimination))(
      """
        |public int i;
        |public int j;
        |public int after(Test t) {
        |  int[] xs = new int[5];
        |  int[] ys = new int[5];
        |  xs[0] = 1;
        |  t.i = 2;
        |  ys[0] = 3;
        |  t.j = 4;
        |  return 1 + 2;
        |}
      """.stripMargin)
  }

  it should "recognize potentially aliased stores" in {
    """
      |public int i;
      |public int before(int[] xs, int[] ys, Test t, Test tt) {
      |  xs[0] = 1;
      |  ys[0] = 2;
      |  tt.i = 3;
      |  return xs[0] + t.i;
      |}
    """.stripMargin should optimizeTo(RedundantLoadElimination, after = Seq(CommonSubexpressionElimination))(
      """
        |public int i;
        |public int after(int[] xs, int[] ys, Test t, Test tt) {
        |  xs[0] = 1;
        |  ys[0] = 2;
        |  tt.i = 3;
        |  return xs[0] + t.i;
        |}
      """.stripMargin)
  }

  it should "analyze through phis" in {
    """
      |public int i;
      |public int j;
      |public int before(Test t) {
      |  t.i = 1;
      |  while (1 != t.j) t.j = t.i + 1;
      |  return t.i;
      |}
    """.stripMargin should optimizeTo(RedundantLoadElimination, after = Seq(CommonSubexpressionElimination))(
        """
          |public int i;
          |public int j;
          |public int after(Test t) {
          |  t.i = 1;
          |  while (1 != t.j) t.j = 1 + 1;
          |  return 1;
          |}
        """.stripMargin)

    """
      |public int i;
      |public int j;
      |public int before(Test t) {
      |  t.i = 1;
      |  while (1 != t.j) t.i = t.j;
      |  return t.i;
      |}
    """.stripMargin should notOptimize(RedundantLoadElimination, after = Seq(CommonSubexpressionElimination))

    """
      |public int i;
      |public int j;
      |public int before(Test t) {
      |  t.i = 1;
      |  if (t.i != 1) t.j = 2; else t.j = 3;
      |  return t.i;
      |}
    """.stripMargin should optimizeTo(RedundantLoadElimination, after = Seq(CommonSubexpressionElimination))(
      """
        |public int i;
        |public int j;
        |public int after(Test t) {
        |  t.i = 1;
        |  if (t.i != 1) t.j = 2; else t.j = 3;
        |  return 1;
        |}
      """.stripMargin)

    """
      |public int i;
      |public int j;
      |public int before(Test t) {
      |  t.i = 1;
      |  if (t.i != 1) t.i = 2; else t.j = 3;
      |  return t.i;
      |}
    """.stripMargin should notOptimize(RedundantLoadElimination, after = Seq(CommonSubexpressionElimination))

    """
      |public int i;
      |public int j;
      |public int before(Test t) {
      |  t.i = 1;
      |  if (t.i != 1) t.j = 2; else t.i = 3;
      |  return t.i;
      |}
    """.stripMargin should notOptimize(RedundantLoadElimination, after = Seq(CommonSubexpressionElimination))
  }
}
