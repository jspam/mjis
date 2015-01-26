package mjis

import mjis.asm.AMD64Registers._
import mjis.asm._
import org.scalatest._
import scala.collection.JavaConversions._

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

  it should "merge newly inserted ranges with existing ones" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(3, 5)
    l1.addRange(2, 4)
    l1.ranges.values should contain only LivenessRange(2, 5)

    val l2 = new LivenessInterval(DummyRegOp)
    l2.addRange(3, 5)
    l2.addRange(5, 7)
    l2.ranges.values should contain only LivenessRange(3, 7)

    val l3 = new LivenessInterval(DummyRegOp)
    l3.addRange(2, 3)
    l3.addRange(5, 7)
    l3.addRange(1, 8)
    l3.ranges.values should contain only LivenessRange(1, 8)
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

  it should "know whether it intersects a liveness range" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(3, 5)

    l1.intersects(LivenessRange(0, 3)) shouldBe false
    l1.intersects(LivenessRange(1, 4)) shouldBe true
    l1.intersects(LivenessRange(3, 4)) shouldBe true
    l1.intersects(LivenessRange(5, 7)) shouldBe false
    l1.intersects(LivenessRange(6, 7)) shouldBe false
  }

  it should "know whether it intersects another liveness interval" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(3, 5)
    l1.addRange(7, 9)

    val l2 = new LivenessInterval(DummyRegOp)
    l2.addRange(5, 7)

    l1.intersects(l2) shouldBe false
    l2.intersects(l1) shouldBe false

    val l3 = new LivenessInterval(DummyRegOp)
    l3.addRange(4, 6)
    l1.intersects(l3) shouldBe true
    l3.intersects(l1) shouldBe true
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

    l1.addUsage(1, OperandSpec.NONE)
    l1.addUsage(4, OperandSpec.NONE)
    l1.addUsage(5, OperandSpec.NONE)
    l1.addUsage(6, OperandSpec.NONE)
    l1.addUsage(9, OperandSpec.NONE)

    val l1split = l1.splitAt(5)

    l1.ranges.values.toSeq should contain theSameElementsInOrderAs Seq(LivenessRange(0, 2), LivenessRange(4, 5))
    l1split.ranges.values.toSeq should contain theSameElementsInOrderAs Seq(LivenessRange(5, 6), LivenessRange(8, 10))

    l1.usages.map(t => t._1 -> t._2) should contain only (1 -> OperandSpec.NONE, 4 -> OperandSpec.NONE)
    l1split.usages.map(t => t._1 -> t._2) should contain only (5 -> OperandSpec.NONE, 6 -> OperandSpec.NONE, 9 -> OperandSpec.NONE)

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

  it should "append all split children to one parent" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(1, 6)
    val l2 = l1.splitAt(4)
    val l3 = l1.splitAt(2)

    l1.splitParent shouldBe None
    l2.splitParent shouldBe Some(l1)
    l3.splitParent shouldBe Some(l1)

    l1.splitChildren.values.toSeq should contain inOrderOnly(l3, l2)
    l2.splitChildren shouldBe empty
    l3.splitChildren shouldBe empty
  }

  it should "split when asked to split at its end position" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(4, 7)

    l1.splitAt(7) shouldNot be(l1)
  }

  it should "not split when asked to split at its start position" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(4, 7)

    l1.splitAt(4) shouldBe l1
  }

  it should "not split when asked to split before its start position" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(4, 7)
    l1.splitAt(2) shouldBe l1
  }

  it should "not split when asked to split after its end position" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(4, 7)
    l1.splitAt(8) shouldBe l1
  }

  it should "yield a single-point interval when split at the end" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(4, 7)

    val l1split = l1.splitAt(7)
    l1.ranges.values should contain only LivenessRange(4, 7)
    l1split.ranges.values should contain only LivenessRange(7, 7)

    l1.childAt(7) shouldBe Some(l1split)
  }

  it should "extend the first existing range when setFrom is called" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(3, 5)
    l1.addRange(7, 9)
    l1.setFrom(1)

    l1.ranges.values.toSeq should contain inOrderOnly (LivenessRange(1, 5), LivenessRange(7, 9))
  }

  it should "add a new one-point range when setFrom is called on an empty interval" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.setFrom(5)

    l1.ranges.values.toSeq should contain only LivenessRange(5, 5)
  }

  it should "return its next usage" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(3, 5)

    l1.addUsage(3, OperandSpec.WRITE)
    l1.addUsage(5, OperandSpec.READ)

    l1.nextUsage(0).map(e => e.getKey -> e.getValue) shouldBe Some(3 -> OperandSpec.WRITE)
    l1.nextUsage(3).map(e => e.getKey -> e.getValue) shouldBe Some(3 -> OperandSpec.WRITE)
    l1.nextUsage(4).map(e => e.getKey -> e.getValue) shouldBe Some(5 -> OperandSpec.READ)
    l1.nextUsage(5).map(e => e.getKey -> e.getValue) shouldBe Some(5 -> OperandSpec.READ)
    l1.nextUsage(6).map(e => e.getKey -> e.getValue) shouldBe None
  }

  it should "return its next usage position" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(3, 5)
    l1.addUsage(3, OperandSpec.WRITE)
    l1.addUsage(5, OperandSpec.READ)

    l1.nextUsagePos(0) shouldBe 3
    l1.nextUsagePos(3) shouldBe 3
    l1.nextUsagePos(4) shouldBe 5
    l1.nextUsagePos(5) shouldBe 5
    l1.nextUsagePos(6) shouldBe Int.MaxValue
  }

  it should "move the split position between two adjacent intervals" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(0, 3)
    l1.addRange(5, 13)
    val l2 = l1.splitAt(8)

    // l1 = [0,3[, [5,8[
    // l2 = [8,13[

    l1.moveSplitPos(l2, 6)
    l1.ranges.values.toSeq should contain inOrderOnly(LivenessRange(0, 3), LivenessRange(5, 6))
    l2.ranges.values should contain only LivenessRange(6, 13)

    l1.childAt(3) shouldBe Some(l1)
    l1.childAt(5) shouldBe Some(l1)
    l1.childAt(6) shouldBe Some(l2)
    l1.childAt(8) shouldBe Some(l2)
    l1.childAt(13) shouldBe Some(l2)
    l1.childAt(14) shouldBe None
  }

  it should "print a nice string representation of itself" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(3, 5)
    l1.addRange(7, 7)

    l1.toString shouldBe "0{0}: [3,5[, [7,7["
  }

  it should "contain nothing if empty" in {
    val l1 = new LivenessInterval(DummyRegOp)
    l1.contains(0) shouldBe false
    l1.containsIncl(0) shouldBe false
  }

  "An occupation map" should "report occupations" in {
    val o = new OccupationMap(Seq(RAX))

    val l1 = new LivenessInterval(DummyRegOp)
    l1.addRange(3, 5)
    l1.addRange(7, 9)

    val l2 = new LivenessInterval(DummyRegOp)
    l2.addRange(5, 6)

    o.addInterval(l1, RAX)
    o.addInterval(l2, RAX)

    o.nonEndingOccupationAt(2) shouldBe Map(RAX -> None)
    o.nonStartingOccupationAt(2) shouldBe Map(RAX -> None)

    o.nonEndingOccupationAt(3) shouldBe Map(RAX -> Some(l1))
    o.nonStartingOccupationAt(3) shouldBe Map(RAX -> None)

    o.nonEndingOccupationAt(4) shouldBe Map(RAX -> Some(l1))
    o.nonStartingOccupationAt(4) shouldBe Map(RAX -> Some(l1))

    o.nonEndingOccupationAt(5) shouldBe Map(RAX -> Some(l2))
    o.nonStartingOccupationAt(5) shouldBe Map(RAX -> Some(l1))

    o.nonEndingOccupationAt(6) shouldBe Map(RAX -> None)
    o.nonStartingOccupationAt(6) shouldBe Map(RAX -> Some(l2))

    o.nonEndingOccupationAt(7) shouldBe Map(RAX -> Some(l1))
    o.nonStartingOccupationAt(7) shouldBe Map(RAX -> None)

    o.nonEndingOccupationAt(8) shouldBe Map(RAX -> Some(l1))
    o.nonStartingOccupationAt(8) shouldBe Map(RAX -> Some(l1))

    o.nonEndingOccupationAt(9) shouldBe Map(RAX -> None)
    o.nonStartingOccupationAt(9) shouldBe Map(RAX -> Some(l1))

    o.nonEndingOccupationAt(10) shouldBe Map(RAX -> None)
    o.nonStartingOccupationAt(10) shouldBe Map(RAX -> None)
  }

}
