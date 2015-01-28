package mjis.asm

import java.util

import scala.collection.JavaConversions._
import AMD64Registers._

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
class LivenessInterval(val regOp: RegisterOperand) extends Ordered[LivenessInterval] {
  override def compare(that: LivenessInterval): Int = this.start.compare(that.start)

  val ranges = new util.TreeMap[Int, LivenessRange]()
  val usages = new util.TreeMap[Int, OperandSpec]()

  val splitChildren = new util.TreeMap[Int, LivenessInterval]()
  var splitParent: Option[LivenessInterval] = None

  def start = ranges.firstEntry().getValue.start
  def end = ranges.lastEntry().getValue.end

  def contains(position: Int) = if (position < this.start || (position > this.start && position >= this.end)) false
    else ranges.headMap(position, true).lastEntry() match {
      case null => false
      case candidate => candidate.getValue.contains(position)
    }
  def containsIncl(position: Int) = if (position < this.start || (position > this.start && position > this.end)) false
    else ranges.headMap(position, true).lastEntry() match {
      case null => false
      case candidate => candidate.getValue.containsIncl(position)
    }
  def intersects(range: LivenessRange): Boolean = this.nextAlivePos(range.start) match {
    case Some(nextAlive) => nextAlive < range.end
    case None => false
  }
  def intersects(that: LivenessInterval): Boolean = this.nextIntersectionWith(that, this.start max that.start).isDefined

  def nextAlivePos(fromPos: Int): Option[Int] =
    if (this.contains(fromPos)) Some(fromPos)
    else Option(this.ranges.tailMap(fromPos, true).firstEntry()).map(_.getValue.start)

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

  def addUsage(pos: Int, spec: OperandSpec): Unit = this.usages.put(pos, spec)
  def nextUsage(fromPos: Int) = Option(usages.ceilingEntry(fromPos))
  def nextUsagePos(fromPos: Int) = Option(usages.ceilingKey(fromPos)).getOrElse(Int.MaxValue)

  def addRange(from: Int, to: Int) = {
    val newRange = LivenessRange(from, to)
    // adjacent ranges are merged into one
    val intersectingRanges =
      Option(ranges.floorEntry(from)).filter(_.getValue.containsIncl(from)).map(_.getValue) ++
        ranges.subMap(from, true, to, true).values()
    if (intersectingRanges.isEmpty) {
      ranges.put(from, newRange)
    } else {
      intersectingRanges.foreach(r => ranges.remove(r.start))
      ranges.put(from, LivenessRange(
        from min intersectingRanges.head.start,
        to max intersectingRanges.last.end
      ))
    }
  }

  def setFrom(from: Int) = {
    ranges.firstEntry() match {
      case null =>
        ranges.put(from, LivenessRange(from, from))
      case firstRange =>
        ranges.remove(firstRange.getValue.start)
        ranges.put(from, LivenessRange(from, firstRange.getValue.end))
    }
  }

  def splitAt(pos: Int): LivenessInterval = {
    if (pos <= this.start || pos > this.end) this
    else {
      val newInterval = new LivenessInterval(this.regOp)
      for (range <- ranges.values().toArray(Array[LivenessRange]())) {
        if (range.containsIncl(pos)) {
          ranges.replace(range.start, LivenessRange(range.start, pos))
          newInterval.ranges.put(pos, LivenessRange(pos, range.end))
        } else if (range.start > pos) {
          ranges.remove(range.start)
          newInterval.ranges.put(range.start, range)
        }
      }

      newInterval.usages.putAll(this.usages.tailMap(pos, true))
      newInterval.usages.keysIterator.foreach(this.usages.remove)

      val parent = this.splitParent.getOrElse(this)
      newInterval.splitParent = Some(parent)
      parent.splitChildren.put(newInterval.start, newInterval)

      newInterval
    }
  }

  def andChildren = Seq(this) ++ this.splitChildren.values().toSeq

  /** Returns the split child interval that contains the given position, but does not start there. */
  def childAtExcl(pos: Int): Option[LivenessInterval] = {
    this.splitChildren.headMap(pos, false).lastEntry() match {
      case null => if (this.containsIncl(pos)) Some(this) else None
      case cand => if (cand.getValue.containsIncl(pos)) Some(cand.getValue) else None
    }
  }

  def childAt(pos: Int): Option[LivenessInterval] = {
    this.splitChildren.floorEntry(pos) match {
      case null => if (this.containsIncl(pos)) Some(this) else None
      case cand => if (cand.getValue.containsIncl(pos)) Some(cand.getValue) else None
    }
  }

  override def toString =
    (if (this.regOp.regNr < 0) Registers(this.regOp.regNr).subregs(this.regOp.sizeBytes) + "/" else "") +
    s"${this.regOp.regNr}{${this.regOp.sizeBytes}}: " + ranges.values().mkString(", ")
}
