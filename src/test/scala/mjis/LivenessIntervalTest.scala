package mjis

import mjis.asm._
import org.scalatest._

class LivenessIntervalTest extends FlatSpec with Matchers {

  val DummyRegOp = RegisterOperand(0, 0)

  "A liveness range" should "know whether it intersects another liveness range" in {
    LivenessRange(0, 2).intersects(LivenessRange(1, 3)) shouldBe true
    LivenessRange(0, 2).intersects(LivenessRange(2, 2)) shouldBe false
    LivenessRange(0, 2).intersects(LivenessRange(2, 3)) shouldBe false
    LivenessRange(1, 2).intersects(LivenessRange(0, 2)) shouldBe true
    LivenessRange(1, 2).intersects(LivenessRange(0, 3)) shouldBe true
    LivenessRange(1, 2).intersects(LivenessRange(1, 3)) shouldBe true
  }

  it should "know whether it contains a position" in {
    LivenessRange(0, 2).contains(0) shouldBe true
    LivenessRange(0, 2).contains(1) shouldBe true
    LivenessRange(0, 2).contains(2) shouldBe false // open interval
    LivenessRange(0, 2).containsIncl(2) shouldBe true // open interval
    LivenessRange(0, 2).contains(3) shouldBe false

    LivenessRange(2, 2).contains(1) shouldBe false
    LivenessRange(2, 2).contains(2) shouldBe true // one-point interval!
    LivenessRange(2, 2).containsIncl(2) shouldBe true
    LivenessRange(2, 2).contains(3) shouldBe false
  }

  "A liveness interval" should "know whether it contains a position" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(3, 5)
    l1.contains(2) shouldBe false
    l1.contains(3) shouldBe true
    l1.contains(4) shouldBe true
    l1.contains(5) shouldBe false // open interval
    l1.containsIncl(5) shouldBe true
    l1.contains(6) shouldBe false
  }

  it should "contain its start position if it's a one-point interval" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(3, 3)
    l1.contains(2) shouldBe false
    l1.contains(3) shouldBe true
    l1.contains(4) shouldBe false
  }

  it should "return itself for childAt queries" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(3, 5)
    l1.addRange(7, 8)
    l1.childAt(2) shouldBe None
    l1.childAt(3) shouldBe Some(l1)
    l1.childAt(4) shouldBe Some(l1)
    l1.childAt(5) shouldBe Some(l1)
    l1.childAt(6) shouldBe None
    l1.childAt(7) shouldBe Some(l1)
    l1.childAt(8) shouldBe Some(l1)
    l1.childAt(9) shouldBe None

    val l2 = new LivenessInterval(DummyRegOp)
    l2.addRange(3, 3)
    l2.childAt(2) shouldBe None
    l2.childAt(3) shouldBe Some(l2)
    l2.childAt(4) shouldBe None
  }

  it should "know its start and end position" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(0, 2)
    l1.addRange(4, 6)

    l1.start shouldBe 0
    l1.end shouldBe 6
  }

  it should "compute the next intersection with another liveness interval" in {
    val l1 = new LivenessInterval(DummyRegOp)
    val l2 = new LivenessInterval(DummyRegOp)

    l1.addRange(0, 2)
    l1.addRange(4, 6)
    l2.addRange(3, 5)

    l1.nextIntersectionWith(l2, 0) shouldBe Some(4)
    l1.nextIntersectionWith(l2, 2) shouldBe Some(4)
    l1.nextIntersectionWith(l2, 3) shouldBe Some(4)
    l1.nextIntersectionWith(l2, 4) shouldBe Some(4)
    l1.nextIntersectionWith(l2, 5) shouldBe None // open interval
    l1.nextIntersectionWith(l2, 6) shouldBe None
    l1.nextIntersectionWith(l2, 7) shouldBe None
  }

  it should "compute the next intersection with a one-point interval" in {
    val l1 = new LivenessInterval(DummyRegOp)
    val l2 = new LivenessInterval(DummyRegOp)
    l1.addRange(0, 3)
    l2.addRange(2, 2)

    l1.nextIntersectionWith(l2, 0) shouldBe Some(2)
    l1.nextIntersectionWith(l2, 2) shouldBe Some(2)
    l1.nextIntersectionWith(l2, 3) shouldBe None
    l1.nextIntersectionWith(l2, 4) shouldBe None
  }

  it should "know when it's next alive" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(1, 3)
    l1.addRange(5, 6)

    l1.nextAlivePos(0) shouldBe Some(1)
    l1.nextAlivePos(1) shouldBe Some(1)
    l1.nextAlivePos(2) shouldBe Some(2)
    l1.nextAlivePos(3) shouldBe Some(5)
    l1.nextAlivePos(4) shouldBe Some(5)
    l1.nextAlivePos(5) shouldBe Some(5)
    l1.nextAlivePos(6) shouldBe None
    l1.nextAlivePos(7) shouldBe None
  }

  it should "be split at a position" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(0, 2)
    l1.addRange(4, 6)
    l1.addRange(8, 10)

    l1.usages += RegisterUsage(1, OperandSpec.NONE)
    l1.usages += RegisterUsage(4, OperandSpec.NONE)
    l1.usages += RegisterUsage(5, OperandSpec.NONE)
    l1.usages += RegisterUsage(6, OperandSpec.NONE)
    l1.usages += RegisterUsage(9, OperandSpec.NONE)

    val l1split = l1.splitAt(5)

    l1.ranges should contain theSameElementsInOrderAs Seq(LivenessRange(0, 2), LivenessRange(4, 5))
    l1split.ranges should contain theSameElementsInOrderAs Seq(LivenessRange(5, 6), LivenessRange(8, 10))

    l1.usages should contain only (RegisterUsage(1, OperandSpec.NONE), RegisterUsage(4, OperandSpec.NONE))
    l1split.usages should contain only (RegisterUsage(5, OperandSpec.NONE),
      RegisterUsage(6, OperandSpec.NONE), RegisterUsage(9, OperandSpec.NONE))

    l1.start shouldBe 0
    l1.end shouldBe 5

    l1split.start shouldBe 5
    l1split.end shouldBe 10

    l1.andChildren should contain only (l1, l1split)

    l1.childAt(-1) shouldBe None
    l1.childAt(0) shouldBe Some(l1)
    l1.childAt(4) shouldBe Some(l1)
    l1.childAt(5) shouldBe Some(l1split)
    l1.childAt(7) shouldBe None
    l1.childAt(10) shouldBe Some(l1split)
    l1.childAt(11) shouldBe None
  }

  it should "split if asked to split at its end position" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(4, 7)

    l1.splitAt(7) shouldNot be(l1)
  }

  it should "not split if asked to split at its start position" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(4, 7)

    l1.splitAt(4) shouldBe l1
  }

  it should "yield a single-point interval when split at the end" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(4, 7)

    val l1split = l1.splitAt(7)
    l1.ranges should contain only LivenessRange(4, 7)
    l1split.ranges should contain only LivenessRange(7, 7)

    l1.childAt(7) shouldBe Some(l1split)
  }

  it should "add a new one-point range when setFrom is called on an empty interval" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.setFrom(5)

    l1.ranges should contain only LivenessRange(5, 5)
  }

}
