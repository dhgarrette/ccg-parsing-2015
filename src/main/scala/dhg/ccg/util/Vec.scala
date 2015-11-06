package dhg.ccg.util

import scala.reflect.ClassTag
import scala.util.control.Breaks._
import java.util.Arrays
import scalaz._
import Scalaz._
import dhg.util._

trait Vec[A] extends (Int => A) {
  def apply(trueIndex: Int): A
  def get(trueIndex: Int): Option[A]
  final def getOrElse(trueIndex: Int, default: => A): A = get(trueIndex).getOrElse(default)
  def containsKey(trueIndex: Int): Boolean
  def update(trueIndex: Int, e: A): Unit
  def activeKeys: Array[Int]
  def activeValues: Array[A]
  def activePairs: Array[(Int, A)]
  def activeCount: Int
  def length: Int
  final def withDefaultValue(default: A): Vec[A] = new WithDefaultValueVec(this, default)
}

object Vec {
  def empty[@specialized(Int, Double) A: ClassTag](length: Int): Vec[A] = DenseVec.empty[A](length)
}

object SparseVec {
  val threshold = 80
  
  def empty[@specialized(Int, Double) A: ClassTag](length: Int): Vec[A] = new IndirectSparseVec[A](new Array(0), new Array(0), 0, length)

  def apply[@specialized(Int, Double) A: ClassTag](activeKeysSorted: Array[Int], length: Int): Vec[A] = {
    val activeCount = activeKeysSorted.length
    if (activeCount * 100 / length < threshold) {
      IndirectSparseVec(activeKeysSorted, length)
    }
    else {
      DirectSparseVec(activeKeysSorted, length)
    }
  }

  def apply[@specialized(Int, Double) A: ClassTag](activeKeysSorted: Array[Int], activeValues: Array[A], length: Int): Vec[A] = {
    val activeCount = activeKeysSorted.length
    if (activeCount * 100 / length < threshold) {
      IndirectSparseVec(activeKeysSorted, activeValues, activeCount, length)
    }
    else {
      DirectSparseVec(activeKeysSorted, activeValues, activeCount, length)
    }
  }

  def apply[@specialized(Int, Double) A: ClassTag](pairs: Iterable[(Int, A)], length: Int): Vec[A] = {
    val activeCount = pairs.size
    if (activeCount * 100 / length < threshold) {
      IndirectSparseVec(pairs, length)
    }
    else {
      DirectSparseVec(pairs, length)
    }
  }

  def apply[@specialized(Int, Double) A: ClassTag](activeKeysSorted: Array[Int], activeValues: Array[A], activeCount: Int, length: Int): Vec[A] = {
    if (activeCount * 100 / length < threshold) {
      IndirectSparseVec(activeKeysSorted, activeValues, activeCount, length)
    }
    else {
      DirectSparseVec(activeKeysSorted, activeValues, activeCount, length)
    }
  }
}

class IndirectSparseVec[@specialized(Int, Double) A: ClassTag](val activeKeysSorted: Array[Int], val activeValues: Array[A], val activeCount: Int, val length: Int) extends Vec[A] {
  require(activeKeysSorted.length == activeCount)
  require(activeValues.length == activeCount)

  def apply(trueIndex: Int): A = {
    val idx = Arrays.binarySearch(activeKeysSorted, 0, activeCount, trueIndex)
    activeValues(idx)
  }

  def get(trueIndex: Int): Option[A] = {
    val idx = Arrays.binarySearch(activeKeysSorted, 0, activeCount, trueIndex)
    (idx >= 0).option(activeValues(idx))
  }

  def containsKey(trueIndex: Int): Boolean = {
    val idx = Arrays.binarySearch(activeKeysSorted, 0, activeCount, trueIndex)
    idx >= 0
  }

  def update(trueIndex: Int, e: A): Unit = {
    val idx = Arrays.binarySearch(activeKeysSorted, 0, activeCount, trueIndex)
    activeValues(idx) = e
  }

  def activeKeys = activeKeysSorted

  def activePairs = {
    val r = new Array[(Int, A)](activeCount)
    var i = 0
    while (i < activeCount) {
      r(i) = (activeKeysSorted(i), activeValues(i))
      i += 1
    }
    r
  }

  //  def clone() = {
  //    val activeValuesCopy = new Array[A](activeCount)
  //    Array.copy(activeValues, 0, activeValuesCopy, 0, activeCount)
  //    new IndirectSparseVec(activeKeysSorted, activeValuesCopy, activeCount, length)
  //  }

  //override def toString() = f"IndirectSparseVec($length)(${(activeKeysSorted.take(length) zip activeValues.take(length)).mkString(", ")})"
  override def toString() = f"IndirectSparseVec(${activeKeysSorted.zipWithIndex.map { case (k, i) => f"($k, ${activeValues(i)})" }.mkString(", ")})"
}

object IndirectSparseVec {
  def empty[@specialized(Int, Double) A: ClassTag](length: Int) = new IndirectSparseVec[A](new Array(0), new Array(0), 0, length)

  def apply[@specialized(Int, Double) A: ClassTag](activeKeysSorted: Array[Int], length: Int): IndirectSparseVec[A] = {
    val activeCount = activeKeysSorted.length
    IndirectSparseVec(activeKeysSorted, new Array(activeCount), activeCount, length)
  }

  def apply[@specialized(Int, Double) A: ClassTag](activeKeysSorted: Array[Int], activeValues: Array[A], length: Int): IndirectSparseVec[A] = {
    val activeCount = activeKeysSorted.length
    IndirectSparseVec(activeKeysSorted, activeValues, activeCount, length)
  }

  def apply[@specialized(Int, Double) A: ClassTag](pairs: TraversableOnce[(Int, A)], length: Int): IndirectSparseVec[A] = {
    val (activeKeysSorted, activeValues) = pairs.toArray.sortBy(_._1).unzip
    val activeCount = activeKeysSorted.length
    IndirectSparseVec(activeKeysSorted, activeValues, activeCount, length)
  }

  def apply[@specialized(Int, Double) A: ClassTag](activeKeysSorted: Array[Int], activeValues: Array[A], activeCount: Int, length: Int): IndirectSparseVec[A] = {
    assert(activeValues.length == activeCount, "keys and activeValues arrays must be the same length")
    assert(isSortedUnique(activeKeysSorted, activeCount), "keys must be unique and array must be sorted")
    if (activeCount > 0) {
      assert(0 <= activeKeysSorted(0), "no key can be less than zero")
      assert(activeKeysSorted(activeCount - 1) < length, "no key may exceed the vector length")
    }
    new IndirectSparseVec(activeKeysSorted, activeValues, activeCount, length)
  }

  def isSortedUnique(a: Array[Int], len: Int): Boolean = {
    if (len == 0) return true
    var i = 1
    breakable {
      while (i < len) {
        if (a(i - 1) >= a(i)) break
        i += 1
      }
    }
    return i == len
  }
}

class DirectSparseVec[@specialized(Int, Double) A: ClassTag](val activeKeysSorted: Array[Int], val data: Array[A], val activeCount: Int, val length: Int) extends Vec[A] {
  require(activeKeysSorted.length == activeCount)
  require(data.length == length)

  def apply(trueIndex: Int): A = {
    data(trueIndex)
  }

  def get(trueIndex: Int): Option[A] = {
    val idx = Arrays.binarySearch(activeKeysSorted, 0, activeCount, trueIndex)
    (idx >= 0).option(data(trueIndex))
  }

  def containsKey(trueIndex: Int): Boolean = {
    val idx = Arrays.binarySearch(activeKeysSorted, 0, activeCount, trueIndex)
    idx >= 0
  }

  def update(trueIndex: Int, e: A): Unit = {
    data(trueIndex) = e
  }

  def activeKeys = activeKeysSorted
  def activeValues = {
    //println(f"oifgjidf    maybe... don't do this?")
    val r = new Array[A](activeCount)
    var i = 0
    while (i < activeCount) {
      r(i) = data(activeKeysSorted(i))
      i += 1
    }
    r
  }

  def activePairs = {
    val r = new Array[(Int, A)](activeCount)
    var i = 0
    while (i < activeCount) {
      val ai = activeKeysSorted(i)
      r(i) = (ai, data(ai))
      i += 1
    }
    r
  }

  //  def clone() = {
  //    val activeValuesCopy = new Array[A](activeCount)
  //    Array.copy(activeValues, 0, activeValuesCopy, 0, activeCount)
  //    new DirectSparseVec(activeKeysSorted, activeValuesCopy, activeCount, length)
  //  }

  //override def toString() = f"DirectSparseVec($length)(${(activeKeysSorted.take(length) zip activeValues.take(length)).mkString(", ")})"
  override def toString() = f"DirectSparseVec(${activeKeysSorted.zipWithIndex.map { case (k, i) => f"($k, ${activeValues(i)})" }.mkString(", ")})"
}

object DirectSparseVec {
  def empty[@specialized(Int, Double) A: ClassTag](length: Int) = new DirectSparseVec[A](new Array(0), new Array(length), 0, length)

  def apply[@specialized(Int, Double) A: ClassTag](activeKeysSorted: Array[Int], length: Int): DirectSparseVec[A] = {
    val activeCount = activeKeysSorted.length
    DirectSparseVec(activeKeysSorted, new Array(length), activeCount, length)
  }

  def apply[@specialized(Int, Double) A: ClassTag](activeKeysSorted: Array[Int], activeValues: Array[A], length: Int): DirectSparseVec[A] = {
    val activeCount = activeKeysSorted.length
    DirectSparseVec(activeKeysSorted, activeValues, activeCount, length)
  }

  def apply[@specialized(Int, Double) A: ClassTag](pairs: TraversableOnce[(Int, A)], length: Int): DirectSparseVec[A] = {
    val (activeKeysSorted, activeValues) = pairs.toArray.sortBy(_._1).unzip
    val activeCount = activeKeysSorted.length
    DirectSparseVec(activeKeysSorted, activeValues, activeCount, length)
  }

  def apply[@specialized(Int, Double) A: ClassTag](activeKeysSorted: Array[Int], activeValues: Array[A], activeCount: Int, length: Int): DirectSparseVec[A] = {
    assert(activeValues.length == activeCount, "keys and activeValues arrays must be the same length")
    assert(isSortedUnique(activeKeysSorted, activeCount), "keys must be unique and array must be sorted")
    if (activeCount > 0) {
      assert(0 <= activeKeysSorted(0), "no key can be less than zero")
      assert(activeKeysSorted(activeCount - 1) < length, "no key may exceed the vector length")
    }

    val data = new Array[A](length)
    var i = 0
    while (i < activeCount) {
      data(activeKeysSorted(i)) = activeValues(i)
      i += 1
    }
    new DirectSparseVec(activeKeysSorted, data, activeCount, length)
  }

  //  def apply[@specialized(Int, Double) A: ClassTag](activeKeysSorted: Array[Int], data: Array[A], activeCount: Int, length: Int): DirectSparseVec[A] = {
  //    assert(data.length == length, "data must be the same as length")
  //    assert(activeKeysSorted.length == activeCount, "keys arrays must be activeCount length")
  //    assert(isSortedUnique(activeKeysSorted, activeCount), "keys must be unique and array must be sorted")
  //    if (activeCount > 0) {
  //      assert(0 <= activeKeysSorted(0), "no key can be less than zero")
  //      assert(activeKeysSorted(activeCount - 1) < length, "no key may exceed the vector length")
  //    }
  //    new DirectSparseVec(activeKeysSorted, data, activeCount, length)
  //  }

  def isSortedUnique(a: Array[Int], len: Int): Boolean = {
    if (len == 0) return true
    var i = 1
    breakable {
      while (i < len) {
        if (a(i - 1) >= a(i)) break
        i += 1
      }
    }
    return i == len
  }
}

//

class DenseVec[@specialized(Int, Double) A](val values: Array[A], val length: Int) extends Vec[A] {
  def apply(trueIndex: Int): A = activeValues(trueIndex)
  def get(trueIndex: Int): Option[A] = (0 <= trueIndex && trueIndex < length).option(activeValues(trueIndex))
  def containsKey(trueIndex: Int): Boolean = (0 <= trueIndex && trueIndex < length)
  def update(trueIndex: Int, e: A): Unit = { activeValues(trueIndex) = e }
  def keys: Array[Int] = (0 to length).toArray
  def activeKeys = keys
  val activeValues = values
  def activePairs = {
    val r = new Array[(Int, A)](length)
    var i = 0
    while (i < length) {
      r(i) = (i, activeValues(i))
      i += 1
    }
    r
  }
  def activeCount: Int = length
}

object DenseVec {
  def empty[@specialized(Int, Double) A: ClassTag](length: Int) = new DenseVec[A](new Array(0), length)

  def apply[@specialized(Int, Double) A: ClassTag](values: Array[A]): DenseVec[A] = {
    new DenseVec(values, values.length)
  }

  def apply[@specialized(Int, Double) A: ClassTag](values: Array[A], length: Int): DenseVec[A] = {
    assert(values.length == length, "values vector is the wrong length")
    new DenseVec(values, length)
  }

  def apply[@specialized(Int, Double) A: ClassTag](length: Int): DenseVec[A] = {
    new DenseVec(new Array[A](length), length)
  }

  def apply[@specialized(Int, Double) A: ClassTag](pairs: TraversableOnce[(Int, A)], length: Int): DenseVec[A] = {
    val values = new Array[A](length)
    for ((k, v) <- pairs) {
      values(k) = v
    }
    new DenseVec(values, length)
  }
}

//

class OrderedIndirectSparseVec[@specialized(Int, Double) A](val activeKeys: Array[Int], val activeValues: Array[A], vec: Vec[A], val activeCount: Int, val length: Int) extends Vec[A] {
  def apply(trueIndex: Int): A = vec(trueIndex)
  def get(trueIndex: Int): Option[A] = vec.get(trueIndex)
  def containsKey(trueIndex: Int): Boolean = vec.containsKey(trueIndex)
  def update(trueIndex: Int, e: A): Unit = { ??? }
  def activePairs = activeKeys zip activeValues
}

object OrderedIndirectSparseVec {
  def empty[@specialized(Int, Double) A: ClassTag](length: Int) = new OrderedIndirectSparseVec[A](new Array(0), new Array(0), Vec.empty[A](length), 0, length)

  def apply[@specialized(Int, Double) A: ClassTag](keys: Array[Int], length: Int): OrderedIndirectSparseVec[A] = {
    val activeCount = keys.length
    new OrderedIndirectSparseVec(keys, new Array(activeCount), new IndirectSparseVec(keys.sorted, new Array(activeCount), activeCount, length), activeCount, length)
  }

  def apply[@specialized(Int, Double) A: ClassTag](keys: Array[Int], values: Array[A], length: Int): OrderedIndirectSparseVec[A] = {
    val activeCount = keys.length
    assert(values.length == activeCount, "keys and values arrays must be the same length")
    val (activeKeysSorted, valuesBySorted) = (keys zip values).sortBy(_._1).unzip
    new OrderedIndirectSparseVec(keys, values, new IndirectSparseVec(activeKeysSorted, valuesBySorted, activeCount, length), activeCount, length)
  }
}

//

class WithDefaultValueVec[@specialized(Int, Double) A](delegate: Vec[A], default: A) extends Vec[A] {
  def apply(trueIndex: Int): A = delegate.getOrElse(trueIndex, default)
  def get(trueIndex: Int): Option[A] = Some(apply(trueIndex))
  def containsKey(trueIndex: Int): Boolean = delegate.containsKey(trueIndex)
  def update(trueIndex: Int, e: A): Unit = { delegate(trueIndex) = e }
  def activeKeys: Array[Int] = delegate.activeKeys
  def activeValues: Array[A] = delegate.activeValues
  def activePairs: Array[(Int, A)] = delegate.activePairs
  def activeCount: Int = delegate.activeCount
  def length: Int = delegate.length
}
