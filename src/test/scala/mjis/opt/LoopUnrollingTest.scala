package mjis.opt

import firm._
import mjis.CompilerTestMatchers._
import org.scalatest._

class LoopUnrollingTest extends FlatSpec with Matchers with BeforeAndAfter {
  before {
    Firm.init()
  }

  after {
    Firm.finish()
  }

  "getIterationCount" should "do the thing" in {
    // simple intervals
    LoopUnrolling.getIterationCount(Relation.Less, 0, 1, -1) shouldBe 0
    LoopUnrolling.getIterationCount(Relation.Less, 0, 1, 0) shouldBe 0
    LoopUnrolling.getIterationCount(Relation.Less, 0, 1, 1) shouldBe 1
    LoopUnrolling.getIterationCount(Relation.Less, 0, 1, 10) shouldBe 10

    LoopUnrolling.getIterationCount(Relation.LessEqual, 0, 1, -1) shouldBe 0
    LoopUnrolling.getIterationCount(Relation.LessEqual, 0, 1, 0) shouldBe 1
    LoopUnrolling.getIterationCount(Relation.LessEqual, 0, 1, 10) shouldBe 11

    // reverse intervals
    LoopUnrolling.getIterationCount(Relation.Greater, 0, -1, 1) shouldBe 0
    LoopUnrolling.getIterationCount(Relation.Greater, 0, -1, 0) shouldBe 0
    LoopUnrolling.getIterationCount(Relation.Greater, 0, -1, -1) shouldBe 1

    // step
    LoopUnrolling.getIterationCount(Relation.Less, 0, 2, 10) shouldBe 5
    LoopUnrolling.getIterationCount(Relation.Less, 0, 2, 11) shouldBe 5
    LoopUnrolling.getIterationCount(Relation.LessEqual, 0, 2, 11) shouldBe 6
    LoopUnrolling.getIterationCount(Relation.Greater, 0, -2, -10) shouldBe 5
    LoopUnrolling.getIterationCount(Relation.Greater, 0, -2, -11) shouldBe 5
    LoopUnrolling.getIterationCount(Relation.GreaterEqual, 0, -2, -11) shouldBe 6

    // sign change
    LoopUnrolling.getIterationCount(Relation.Greater, 42, 1, 42) shouldBe 0
    LoopUnrolling.getIterationCount(Relation.Greater, 42, 1, 41) shouldBe Int.MaxValue - 42 + 1
    LoopUnrolling.getIterationCount(Relation.GreaterEqual, 42, 1, 41) shouldBe Int.MaxValue - 42 + 1
    LoopUnrolling.getIterationCount(Relation.Greater, 42, 1, Int.MinValue) shouldBe Int.MaxValue - 42 + 1
    LoopUnrolling.getIterationCount(Relation.Less, 42, -1, 42) shouldBe 0
    LoopUnrolling.getIterationCount(Relation.Less, 42, -1, 43) shouldBe 42 - Int.MinValue.toLong + 1
    LoopUnrolling.getIterationCount(Relation.LessEqual, 42, -1, 43) shouldBe 42 - Int.MinValue.toLong + 1
    LoopUnrolling.getIterationCount(Relation.Less, 42, -1, Int.MaxValue) shouldBe 42 - Int.MinValue.toLong + 1

    // complete overflow
    LoopUnrolling.getIterationCount(Relation.UnorderedLessGreater, 42, 1, 41) shouldBe (1l << 32) - 1 // all but '41'
    LoopUnrolling.getIterationCount(Relation.UnorderedLessGreater, 42, 1, 41) shouldBe (1l << 32) - 1 // all but '41'
    LoopUnrolling.getIterationCount(Relation.UnorderedLessGreater, 42, -1, 43) shouldBe (1l << 32) - 1
  }

  // doesn't work without block merging
  /* "Loop Unrolling" should "unroll static loops" in {
    LoopUnrolling.maxUnrollCount = 3
    """
      |public void before() {
      |  int i = 0;
      |  while (i <= 10) {
      |    System.out.println(i);
      |    i = i + 1;
      |  }
      |}
    """.stripMargin should optimizeTo(LoopUnrolling)(
      """
        |public void after() {
        |  int i = 0;
        |  int j = 0;
        |  while (j < 3) {
        |    System.out.println(i);
        |    i = i + 1;
        |    System.out.println(i);
        |    i = i + 1;
        |    System.out.println(i);
        |    i = i + 1;
        |    j = j + 3;
        |  }
        |  System.out.println(i);
        |  i = i + 1;
        |  System.out.println(i);
        |  i = i + 1;
        |}
      """.stripMargin)
  } */
}
