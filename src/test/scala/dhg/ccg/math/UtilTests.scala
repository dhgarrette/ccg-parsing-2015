package dhg.ccg.math

import org.junit.Test
import dhg.util.TestUtil._
import dhg.util._
import math.{ log, exp, abs, pow }
import org.junit.Assert._
import Double.NaN
import scala.util.Random
import dhg.util.FastMathUtil._
import org.apache.commons.math3.random.{ MersenneTwister, RandomGenerator }
import dhg.ccg.test.TestUtil.DoubleIteratorRandomGenerator
import scalaz._
import Scalaz._

class UtilTests {

  import Util._

  @Test
  def test_Dir_Map {
    val n = 100000
    assertMapEquals(Map('A -> 0.5, 'B -> 0.3, 'C -> 0.2), Vector.fill(n)(Dir(Map('A -> 0.5, 'B -> 0.3, 'C -> 0.2))).reduce(_ |+| _).mapVals(_ / n), 1e-2)
    assertMapEquals(Map('A -> 0.05, 'B -> 0.43, 'C -> 0.52), Vector.fill(n)(Dir(Map('A -> 0.5, 'B -> 4.3, 'C -> 5.2))).reduce(_ |+| _).mapVals(_ / n), 1e-2)
    assertEquals(Map[Symbol, Double](), Dir(Map[Symbol, Double]()))
  }

  @Test
  def test_Dir_Vector {
    val n = 100000
    assertVectorEquals(Vector(0.5, 0.3, 0.2), Vector.fill(n)(Dir(Vector(0.5, 0.3, 0.2))).transpose.map(_.sum / n), 1e-2)
    assertVectorEquals(Vector(0.05, 0.43, 0.52), Vector.fill(n)(Dir(Vector(0.5, 4.3, 5.2))).transpose.map(_.sum / n), 1e-2)
    assertEquals(Vector[Double](), Dir(Vector[Double]()))
  }

  @Test
  def test_LogDir_Map {
    val n = 100000
    assertMapEquals(Map('A -> 0.5, 'B -> 0.3, 'C -> 0.2), Vector.fill(n)(LogDir(Map('A -> 0.5, 'B -> 0.3, 'C -> 0.2).mapVals(LogDouble(_)))).reduce(_ |+| _).mapVals(_.toDouble / n), 1e-2)
    assertMapEquals(Map('A -> 0.05, 'B -> 0.43, 'C -> 0.52), Vector.fill(n)(LogDir(Map('A -> 0.5, 'B -> 4.3, 'C -> 5.2).mapVals(LogDouble(_)))).reduce(_ |+| _).mapVals(_.toDouble / n), 1e-2)
    assertEquals(Map[Symbol, LogDouble](), LogDir(Map[Symbol, LogDouble]()))
  }

  @Test
  def test_LogDir_Vector {
    val n = 100000
    assertVectorEquals(Vector(0.5, 0.3, 0.2), Vector.fill(n)(LogDir(Vector(0.5, 0.3, 0.2).map(LogDouble(_)))).transpose.map(_.sum.toDouble / n), 1e-2)
    assertVectorEquals(Vector(0.05, 0.43, 0.52), Vector.fill(n)(LogDir(Vector(0.5, 4.3, 5.2).map(LogDouble(_)))).transpose.map(_.sum.toDouble / n), 1e-2)
    assertEquals(Vector[LogDouble](), LogDir(Vector[LogDouble]()))
  }

  @Test
  def test_Beta {
    val n = 100000
    assertEquals(0.5, Vector.fill(n)(Beta(1.0, 1.0)).sum.toDouble / n, 1e-2)
    assertEquals(0.5, Vector.fill(n)(Beta(0.5, 0.5)).sum.toDouble / n, 1e-2)
    assertEquals(0.1, Vector.fill(n)(Beta(0.5, 4.5)).sum.toDouble / n, 1e-2)
    assertEquals(0.9, Vector.fill(n)(Beta(4.5, 0.5)).sum.toDouble / n, 1e-2)
    assertEquals(0.6, Vector.fill(n)(Beta(3.0, 2.0)).sum.toDouble / n, 1e-2)
    assertEquals(0.4, Vector.fill(n)(Beta(2.0, 3.0)).sum.toDouble / n, 1e-2)
  }

  @Test
  def test_LogBeta {
    val n = 100000
    assertEquals(0.5, Vector.fill(n)(LogBeta(LogDouble(1.0), LogDouble(1.0))).sum.toDouble / n, 1e-2)
    assertEquals(0.5, Vector.fill(n)(LogBeta(LogDouble(0.5), LogDouble(0.5))).sum.toDouble / n, 1e-2)
    assertEquals(0.1, Vector.fill(n)(LogBeta(LogDouble(0.5), LogDouble(4.5))).sum.toDouble / n, 1e-2)
    assertEquals(0.9, Vector.fill(n)(LogBeta(LogDouble(4.5), LogDouble(0.5))).sum.toDouble / n, 1e-2)
    assertEquals(0.6, Vector.fill(n)(LogBeta(LogDouble(3.0), LogDouble(2.0))).sum.toDouble / n, 1e-2)
    assertEquals(0.4, Vector.fill(n)(LogBeta(LogDouble(2.0), LogDouble(3.0))).sum.toDouble / n, 1e-2)
  }

  def assertMapEquals[T](expected: Map[T, Double], actual: Map[T, Double], acc: Double) {
    def keystr(m: Map[T, Double]) = s"${m.keys.toVector.map(_.toString).sorted.mkString(", ")}"
    assertEquals("Wrong keys.", keystr(expected), keystr(actual))
    for ((k, ev) <- expected) assertEquals(ev, actual(k), acc)
  }

  def assertVectorEquals[T](expected: Vector[Double], actual: Vector[Double], acc: Double) {
    for (((e, a), i) <- (expected zipSafe actual).zipWithIndex) assertEquals(f"mismatch at $i", e, a, acc)
  }
}
