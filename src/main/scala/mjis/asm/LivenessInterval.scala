package mjis.asm

import scala.collection.mutable
import AMD64Registers._

case class RegisterUsage(position: Int, spec: OperandSpec)

case class LivenessRange(start: Int, end: Int) extends Ordered[LivenessRange] {
  override def compare(that: LivenessRange): Int = this.start.compare(that.start)
  def intersects(that: LivenessRange): Boolean = !(that.end <= this.start || this.end <= that.start)
  /* pos \in [start, end[ */
  def contains(pos: Int): Boolean = this.start == pos || (this.start < pos && pos < this.end)
  /* pos \in [start, end] */
  def containsIncl(pos: Int): Boolean = this.start == pos || (this.start < pos && pos <= this.end)
  override def toString = s"[$start,$end["
}

/** A liveness interval represents the liveness of a (virtual or physical) register and
  * consists of one or more non-overlapping liveness ranges.
  *
  * If intervals are split, one interval will become the parent for all split children
  * (even for intervals that are split from one of its child intervals). */
class LivenessInterval(val regOp: RegisterOperand) {
  val ranges = mutable.SortedSet[LivenessRange]()
  val usages = mutable.Set[RegisterUsage]()

  val splitChildren = mutable.Set[LivenessInterval]()
  var splitParent: Option[LivenessInterval] = None

  def start = ranges.head.start
  def end = ranges.last.end

  def contains(position: Int) = ranges.exists(_.contains(position))
  def containsIncl(position: Int) = ranges.exists(_.containsIncl(position))
  def intersects(range: LivenessRange): Boolean = ranges.exists(_.intersects(range))
  def intersects(that: LivenessInterval): Boolean = ranges.exists(that.intersects)

  def nextAlivePos(fromPos: Int): Option[Int] =
    if (this.contains(fromPos)) Some(fromPos)
    else this.ranges.filter(_.start > fromPos).map(_.start).headOption

  @annotation.tailrec
  final def nextIntersectionWith(that: LivenessInterval, fromPos: Int): Option[Int] = {
    (this.nextAlivePos(fromPos), that.nextAlivePos(fromPos)) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(nextAlive1), Some(nextAlive2)) =>
        if (nextAlive1 == nextAlive2) Some(nextAlive1)
        else if (nextAlive1 < nextAlive2) this.nextIntersectionWith(that, nextAlive2)
        else this.nextIntersectionWith(that, nextAlive1)
    }
  }

  def nextUsage(fromPos: Int) = usages.filter(_.position >= fromPos).toSeq.sortBy(_.position).headOption
  def nextUsagePos(fromPos: Int) = nextUsage(fromPos).map(_.position).getOrElse(Int.MaxValue)

  def addRange(from: Int, to: Int) = {
    val newRange = LivenessRange(from, to)
    // adjacent ranges are merged into one
    val intersectingRanges = ranges.filter(r => r.intersects(newRange) || r.end == newRange.start || r.start == newRange.end)
    if (intersectingRanges.isEmpty) {
      ranges += newRange
    } else {
      ranges --= intersectingRanges
      ranges += LivenessRange(
        (from +: intersectingRanges.map(_.start).toSeq).min,
        (to +: intersectingRanges.map(_.end).toSeq).max
      )
    }
  }

  def setFrom(from: Int) = {
    ranges.headOption match {
      case Some(firstRange) =>
        ranges -= firstRange
        ranges += LivenessRange(from, firstRange.end)
      case None =>
        ranges += LivenessRange(from, from)
    }
  }

  def splitAt(pos: Int): LivenessInterval = {
    if (pos == this.start || pos > this.end) this
    else {
      val newInterval = new LivenessInterval(this.regOp)
      for (range <- ranges.toArray) {
        if (range.containsIncl(pos)) {
          ranges -= range
          ranges += LivenessRange(range.start, pos)
          newInterval.ranges += LivenessRange(pos, range.end)
        } else if (range.start > pos) {
          ranges -= range
          newInterval.ranges += range
        }
      }

      newInterval.usages ++= this.usages.filter(_.position >= pos)
      this.usages --= newInterval.usages

      val parent = this.splitParent.getOrElse(this)
      newInterval.splitParent = Some(parent)
      parent.splitChildren += newInterval

      newInterval
    }
  }

  def andChildren = Seq(this) ++ this.splitChildren

  def childAt(pos: Int) = {
    // "containsIncl" might be true for two intervals if they are adjacent, return the later one
    val matching = this.andChildren.filter(_.containsIncl(pos))
    assert (matching.size <= 2)
    matching.sortBy(-_.start).headOption
  }

  override def toString =
    (if (this.regOp.regNr < 0) Registers(this.regOp.regNr).subregs(this.regOp.sizeBytes) + "/" else "") +
    s"${this.regOp.regNr}{${this.regOp.sizeBytes}}: " + ranges.mkString(", ")
}
