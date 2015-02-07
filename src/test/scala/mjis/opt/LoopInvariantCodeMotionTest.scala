package mjis.opt

import firm._
import mjis.CompilerTestMatchers._
import org.scalatest._

class LoopInvariantCodeMotionTest extends FlatSpec with Matchers with BeforeAndAfter {
  before {
    Firm.init()
  }

  after {
    Firm.finish()
  }

  "Loop Invariant Code Motion" should "do the thing" in {
    """
      |public boolean foo() { return true; }
      |public void bar(int i) {}
      |public void before(int i, int j) {
      |  while (foo()) {
      |    bar(i + j);
      |  }
      |}
    """.stripMargin should optimizeTo(LoopInvariantCodeMotion)(
      """
        |public boolean foo() { return true; }
        |public void bar(int i) {}
        |public void after(int i, int j) {
        |  int k = i + j;
        |  while (foo()) {
        |    bar(k);
        |  }
        |}
      """.stripMargin)
  }

  "Loop Invariant Code Motion" should "move through multiple loops" in {
    """
      |public boolean foo() { return true; }
      |public void bar(int i) {}
      |public void before(int i, int j) {
      |  while (foo()) {
      |    while (foo()) {
      |      bar(i + j);
      |    }
      |  }
      |}
    """.stripMargin should optimizeTo(LoopInvariantCodeMotion)(
      """
        |public boolean foo() { return true; }
        |public void bar(int i) {}
        |public void after(int i, int j) {
        |  int k = i + j;
        |  while (foo()) {
        |    while (foo()) {
        |      bar(k);
        |    }
        |  }
        |}
      """.stripMargin)
  }

  "Loop Invariant Code Motion" should "not move ininvariant code" in {
    """
      |public boolean foo() { return true; }
      |public void bar(int i) {}
      |public void before(int i, int j) {
      |  while (foo()) {
      |    bar(i + j);
      |    i = i + 1;
      |  }
      |}
    """.stripMargin should notOptimize(LoopInvariantCodeMotion)
  }
}
