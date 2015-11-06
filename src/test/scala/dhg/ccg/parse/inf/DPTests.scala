package dhg.ccg.parse.inf

import dhg.util._
import org.junit.Test
import org.junit.Assert._
import dhg.ccg.cat._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.dep.DepTree
import dhg.util.viz.TreeViz
import dhg.util._
import dhg.gfl.Fudg
import dhg.ccg.prob._
import scalaz._
import Scalaz._

class DPTests {

  @Test
  def test {
    type A = String

    val prior = new SimpleLogProbabilityDistribution[A](Map(
      "A" -> LogDouble(0.5),
      "B" -> LogDouble(0.25),
      "C" -> LogDouble(0.125),
      "D" -> LogDouble(0.0625),
      "E" -> LogDouble(0.03125),
      "F" -> LogDouble(0.015625),
      "G" -> LogDouble(0.0078125),
      "H" -> LogDouble(0.00390625),
      "I" -> LogDouble(0.001953125),
      "J" -> LogDouble(0.0009765625)))
    val alpha = LogDouble(1.0)

    val dp = new MemoizingDP(Map(), prior, alpha)
    dp("B")
    dp("A")
    dp("B")
  }

  @Test
  def test_avg {
    type A = String

    val prior = new SimpleLogProbabilityDistribution[A](Map(
      "A" -> LogDouble(0.5),
      "B" -> LogDouble(0.25),
      "C" -> LogDouble(0.125),
      "D" -> LogDouble(0.0625),
      "E" -> LogDouble(0.03125),
      "F" -> LogDouble(0.015625),
      "G" -> LogDouble(0.0078125),
      "H" -> LogDouble(0.00390625),
      "I" -> LogDouble(0.001953125),
      "J" -> LogDouble(0.0009765625)))
    val alpha = LogDouble(1.0)

    val n = 1000000
    val m: Map[A, LogDouble] = Vector.fill(n) {
      val dp = new MemoizingDP(Map(), prior, alpha)
      Vector.fill(3)(Vector("A", "B", "C", "D", "E")).flatten.shuffle.mapTo(a => dp(a)).toMap
    }.reduce(_ |+| _).mapVals(_ / LogDouble(n))

    m.toVector.sorted foreach println

    assertEquals(0.5, m("A").toDouble, 1e-2)
    assertEquals(0.25, m("B").toDouble, 1e-2)
    assertEquals(0.125, m("C").toDouble, 1e-2)
    assertEquals(0.0625, m("D").toDouble, 1e-2)
    assertEquals(0.03125, m("E").toDouble, 1e-2)
  }

  @Test
  def test_observed_avg {
    type A = String

    val observedCounts: Map[A, LogDouble] = Map( // 
      "A" -> LogDouble(2.0),
      "B" -> LogDouble(3.0),
      "C" -> LogDouble(1.0))
    val prior = new SimpleLogProbabilityDistribution[A](Map(
      "A" -> LogDouble(0.5),
      "B" -> LogDouble(0.25),
      "C" -> LogDouble(0.125),
      "D" -> LogDouble(0.0625),
      "E" -> LogDouble(0.03125),
      "F" -> LogDouble(0.015625),
      "G" -> LogDouble(0.0078125),
      "H" -> LogDouble(0.00390625),
      "I" -> LogDouble(0.001953125),
      "J" -> LogDouble(0.0009765625)))
    val alpha = LogDouble(1.0)

    val n = 1000000
    val m: Map[A, LogDouble] = Vector.fill(n) {
      val dp = new MemoizingDP(observedCounts, prior, alpha)
      Vector.fill(3)(Vector("A", "B", "C", "D", "E")).flatten.shuffle.mapTo(a => dp(a)).toMap
    }.reduce(_ |+| _).mapVals(_ / LogDouble(n))

    m.toVector.sorted foreach println

    assertEquals(2.5 / 7, m("A").toDouble, 1e-2)
    assertEquals(3.25 / 7, m("B").toDouble, 1e-2)
    assertEquals(1.125 / 7, m("C").toDouble, 1e-2)
    assertEquals(0.0625 / 7, m("D").toDouble, 1e-2)
    assertEquals(0.03125 / 7, m("E").toDouble, 1e-2)
  }

}
