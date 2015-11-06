package dhg.ccg.parse.pcfg

import scala.reflect.ClassTag
import dhg.util._

/**
 *
 */
class FastKMaxPriorityQueue[A: ClassTag](k: Int) {
  assert(k > 0, f"k must be a positive number (was $k)")
  private[this] val q = new Array[A](k)
  private[this] val v = new Array[Double](k) // log values
  private[this] var curCount = 0
  def add(e: A, _p: LogDouble) = {
    val pl = _p.logValue
    if (curCount == 0) {
      q(0) = e
      v(0) = pl
      curCount += 1
    }
    else if (pl < v(curCount - 1)) { // new element is lower priority than the last element 
      if (curCount < k) { // queue is not full, empty space to put new element
        q(curCount) = e // insert to the end
        v(curCount) = pl
        curCount += 1
      }
      else {
        // do nothing
      }
    }
    else { // there are existing elements, and the new value is larger than the lowest existing value (ie, it needs to be inserted)
      if (curCount < k) { // empty space to move the last element
        q(curCount) = q(curCount - 1) // shift last element
        v(curCount) = v(curCount - 1)
      }
      else {
        // no empty space, so don't try to move the last element
      }

      var i = curCount - 1
      while (i > 0 && pl > v(i - 1)) {
        q(i) = q(i - 1)
        v(i) = v(i - 1)
        i -= 1
      }
      q(i) = e
      v(i) = pl

      if (curCount < k)
        curCount += 1
    }
    this
  }
  def iterator = new Iterator[(A, LogDouble)] { // (q zip v.take(curCount).map(new LogDouble(_))).iterator
    private[this] var i = 0
    def next(): (A, LogDouble) = {
      assert(hasNext)
      val r = (q(i), new LogDouble(v(i)))
      i += 1
      r
    }
    def hasNext: Boolean = i < curCount
  }
  def toVector = iterator.toVector // (q zip v.take(curCount).map(new LogDouble(_))).toVector
  override def toString = f"FastKMaxPriorityQueue(k)[${toVector.map { case (e, p) => f"($e,${p.toDouble})" }.mkString(", ")}]"
}

//class FastKMaxPriorityQueue1[A: ClassTag] {
//  private[this] var cur: Option[A] = None
//  private[this] var logPCur = LogDouble.zero
//  def add(e: A, p: LogDouble) = {
//    if (cur.isEmpty || logPCur < p) {
//      cur = Some(e)
//      logPCur = p
//    }
//    this
//  }
//  def iterator = cur.fold(Iterator.empty[(A, LogDouble)])(e => Iterator[(A, LogDouble)](e -> logPCur))
//  def toVector = iterator.toVector
//  override def toString = f"FastKMaxPriorityQueue(k)[${toVector.map { case (e, p) => f"($e,${p.toDouble})" }.mkString(", ")}]"
//}

object FastKMaxPriorityQueue {
  def empty[A: ClassTag](k: Int) = new FastKMaxPriorityQueue[A](k)
}
