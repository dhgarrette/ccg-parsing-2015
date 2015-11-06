package dhg.ccg.util

import scala.reflect.ClassTag

trait Indexer[A] extends Serializable with (A => Int) {
  final def apply(e: A): Int = index(e)
  final def get(e: A): Option[Int] = getIndex(e)
  def obj(i: Int): A
  def getObj(i: Int): Option[A]
  def index(e: A): Int
  def getIndex(e: A): Option[Int]
  def objects: Vector[A]
  def size: Int
  def indices = 0 until size
  def append(e: Iterable[A]): Indexer[A]
}

object Indexer {
  def apply[A: ClassTag](items: TraversableOnce[A]) = new SimpleIndexer(items.toVector.distinct)
}

class SimpleIndexer[A: ClassTag](val objects: Vector[A]) extends Indexer[A] {
  require(objects.size == objects.toSet.size)
  private[this] val objs: Array[A] = objects.toArray
  private[this] val _index: Map[A, Int] = objects.zipWithIndex.toMap
  def obj(i: Int): A = objs(i)
  def getObj(i: Int): Option[A] = if (0 <= i && i < objs.length) Some(objs(i)) else None
  def index(e: A): Int = _index(e)
  def getIndex(e: A): Option[Int] = _index.get(e)
  def size = objs.length

  def append(additionalObjects: Iterable[A]): SimpleIndexer[A] = {
    val objSet = objects.toSet
    val newObjs = (additionalObjects.toSet -- objSet).toVector
    new SimpleIndexer(objects ++ newObjs)
  }
}

object SimpleIndexer {
  def apply[A: ClassTag](objects: TraversableOnce[A]) = new SimpleIndexer(objects.toVector.distinct)
}
