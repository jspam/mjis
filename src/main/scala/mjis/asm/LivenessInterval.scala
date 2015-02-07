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

  def contains(position: Int) =
    if (this.ranges.isEmpty || position < this.start || (position > this.start && position >= this.end)) false
    else ranges.headMap(position, true).lastEntry() match {
      case null => false
      case candidate => candidate.getValue.contains(position)
    }
  def containsIncl(position: Int) =
    if (this.ranges.isEmpty || position < this.start || (position > this.start && position > this.end)) false
    else ranges.headMap(position, true).lastEntry() match {
      case null => false
      case candidate => candidate.getValue.containsIncl(position)
    }
  def intersects(range: LivenessRange): Boolean = this.nextAlivePos(range.start) match {
    case Some(nextAlive) => nextAlive < range.end
    case None => false
  }
  def intersects(that: LivenessInterval): Boolean =
    if (this.ranges.isEmpty) false
    else this.nextIntersectionWith(that, this.start max that.start).isDefined

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
    // adjacent ranges are merged into the new range
    val intersectingRanges =
      Option(ranges.floorEntry(from)).filter(_.getValue.containsIncl(from)).map(_.getValue) ++
        ranges.subMap(from, true, to, true).values()
    val newRange =
      if (intersectingRanges.isEmpty) LivenessRange(from, to)
      else {
        intersectingRanges.foreach(r => ranges.remove(r.start))
        LivenessRange(from min intersectingRanges.head.start, to max intersectingRanges.last.end)
      }
    ranges.put(newRange.start, newRange)
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

  /** Moves the start and end positions of two adjacent intervals. */
  def moveSplitPos(succ: LivenessInterval, newSplitPos: Int): Unit = {
    assert(succ.start == this.end)
    assert(newSplitPos >= this.start)
    assert(newSplitPos <= succ.end)

    if (newSplitPos != this.end) {
      val oldEndRangeStart = this.ranges.lastKey
      this.ranges.replace(oldEndRangeStart, LivenessRange(oldEndRangeStart, newSplitPos))

      val oldStartRange = succ.ranges.firstEntry
      succ.ranges.remove(oldStartRange.getKey)
      succ.ranges.put(newSplitPos, LivenessRange(newSplitPos, oldStartRange.getValue.end))
      succ.splitParent.get.splitChildren.remove(oldStartRange.getValue.start)
      succ.splitParent.get.splitChildren.put(newSplitPos, succ)
    }
  }

  override def toString =
    (if (this.regOp.regNr < 0) Registers(this.regOp.regNr).subregs(this.regOp.sizeBytes) + "/" else "") +
    s"${this.regOp.regNr}{${this.regOp.sizeBytes}}: " + ranges.values().mkString(", ")
}

/** Data structure to retrieve information about the occupation of physical registers
  * that are used by multiple liveness intervals. */
class OccupationMap(val PhysicalRegisters: Seq[Int]) {
  private val occupationMap = PhysicalRegisters.map {
    regNr => regNr -> new java.util.TreeMap[Int, LivenessInterval]()
  }.toMap

  def addInterval(it: LivenessInterval, regNr: Int) =
    it.ranges.values.foreach { range => occupationMap(regNr).put(range.start, it) }

  /** For each physical register, returns (if it exists) the liveness interval that contains the specified
   * position and does not end there. */
  def nonEndingOccupationAt(pos: Int): Map[Int, Option[LivenessInterval]] =
    PhysicalRegisters.map { regNr => regNr ->
      Option(occupationMap(regNr).headMap(pos, /* inclusive */ true).lastEntry()).map(_.getValue).filter(_.contains(pos))
    }.toMap

  /** For each physical register, returns (if it exists) the liveness interval that contains the specified
    * position and does not start there. */
  def nonStartingOccupationAt(pos: Int): Map[Int, Option[LivenessInterval]] =
    PhysicalRegisters.map { regNr => regNr ->
      Option(occupationMap(regNr).headMap(pos, /* inclusive */ false).lastEntry()).map(_.getValue).filter(_.containsIncl(pos))
    }.toMap
}
