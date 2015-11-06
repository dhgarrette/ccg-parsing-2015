package dhg.ccg.parse.pcfg

import scala.reflect.ClassTag
import dhg.util._

class Chart[A](grid: Array[Array[A]], val n: Int) extends Serializable {
  def apply(row: Int, col: Int) = grid(row)(col - row - 1)
  def root = this(0, n)
  def update(row: Int, col: Int, e: A): Unit = grid(row)(col - row - 1) = e
  def length = n

  /**
   * Iterate over all cells
   */
  def bottomUpNodes = {
    for {
      span <- 1 to n //        span size
      i <- 0 to (n - span) //  start of span
      j = i + span //               end of span
    } yield (i, j, this(i, j))
  }

  /**
   * Iterate over all cells
   */
  def topDownNodes = {
    for {
      span <- n downto 1 //    span size
      i <- 0 to (n - span) //  start of span
      j = i + span //               end of span
    } yield (i, j, this(i, j))
  }
}

object Chart {
  def empty[A: ClassTag](n: Int): Chart[A] = new Chart[A](Array.tabulate(n) { row => new Array[A](n - row) }, n)
  def fillVal[A: ClassTag](n: Int)(v: A): Chart[A] = new Chart[A](Array.tabulate(n) { row => Array.fill[A](n - row)(v) }, n)
  def fill[A: ClassTag](n: Int)(v: => A): Chart[A] = new Chart[A](Array.tabulate(n) { row => Array.fill[A](n - row)(v) }, n)
  def tabulate[A: ClassTag](n: Int)(f: (Int, Int) => A): Chart[A] = new Chart[A](Array.tabulate(n) { i => Array.tabulate[A](n - i) { j => f(i, j + i + 1) } }, n)
}
