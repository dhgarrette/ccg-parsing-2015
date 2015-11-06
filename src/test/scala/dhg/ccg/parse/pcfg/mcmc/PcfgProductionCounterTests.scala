package dhg.ccg.parse.pcfg.mcmc

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.scg._
import dhg.ccg.tagdict.StartEndTags
import dhg.util._
import dhg.ccg.tagdict._
import dhg.ccg.test.TestUtil.DoubleIteratorRandomGenerator
import org.apache.commons.math3.random.MersenneTwister
import scala.collection.immutable.ListMap
import scalaz._
import Scalaz._

class PcfgProductionCounterTests {

  val A: Cat = cat"A"
  val B: Cat = cat"B"
  val C: Cat = cat"C"
  val D: Cat = cat"D"
  val E: Cat = cat"E"
  val F: Cat = cat"F"
  val G: Cat = cat"G"
  val H: Cat = cat"H"

  @Test
  def test_SimplePcfgProductionFinder {
    type Word = String

    /*
     *              A
     *            /   \  
     *         B         C
     *         |       /   \
     *         A      A     B
     *        / \    / \    |
     * <s>   B   C  B   A   A  <e>
     *       b1  c1 b1  a1  a2
     */

    val t =
      CcgBinode(A,
        CcgUnode(B,
          CcgBinode(A,
            CcgLeaf(B, "b1", "FAKEPOS"),
            CcgLeaf(C, "c1", "FAKEPOS"))),
        CcgBinode(C,
          CcgBinode(A,
            CcgLeaf(B, "b1", "FAKEPOS"),
            CcgLeaf(A, "a1", "FAKEPOS")),
          CcgUnode(B,
            CcgLeaf(A, "a2", "FAKEPOS"))))

    val pf = new SimplePcfgProductionCounter()

    assertEqualsDoubleMap(
      Map(A -> 1.0),
      pf.rootCounts(t), 1e-9)

    assertEqualsDoubleMapMap(
      Map(
        A -> Map(
          BinaryProd(B, C) -> 2.0,
          BinaryProd(B, A) -> 1.0),
        C -> Map(
          BinaryProd(A, B) -> 1.0)),
      pf.binyCounts(t), 1e-9)

    assertEqualsDoubleMapMap(
      Map(
        B -> Map(
          UnaryProd(A) -> 2.0)),
      pf.unryCounts(t), 1e-9)

    assertEqualsDoubleMapMap(
      Map(
        A -> Map(
          TermProd("a1") -> 1.0,
          TermProd("a2") -> 1.0),
        B -> Map(
          TermProd("b1") -> 2.0),
        C -> Map(
          TermProd("c1") -> 1.0)),
      pf.termCounts(t), 1e-9)

    assertEqualsDoubleMapMap(
      Map(
        A -> Map[Prod, Double](
          BinaryProd(B, C) -> 2.0,
          BinaryProd(B, A) -> 1.0,
          TermProd("a1") -> 1.0,
          TermProd("a2") -> 1.0,
          TermProd("a1") -> 1.0,
          TermProd("a2") -> 1.0),
        B -> Map[Prod, Double](
          UnaryProd(A) -> 2.0,
          TermProd("b1") -> 2.0),
        C -> Map[Prod, Double](
          BinaryProd(A, B) -> 1.0,
          TermProd("c1") -> 1.0)),
      pf.prodCounts(t), 1e-9)
  }

  def assertEqualsDoubleMap[T](e: Map[T, Double], a: Map[T, Double], r: Double) = {
    assertEquals(e.keySet, a.keySet)
    for ((k, ev) <- e) assertEquals(ev, a(k), r)
  }

  def assertEqualsDoubleMapMap[A, B](e: Map[A, Map[B, Double]], a: Map[A, Map[B, Double]], r: Double) = {
    assertEquals(e.keySet, a.keySet)
    for ((k1, evs) <- e) {
      assertEquals(evs.keySet, a(k1).keySet)
      for ((k2, e) <- evs) assertEquals(f"expected: ($k1)($k2)=$e; was ($k1)($k2)=${a(k1)(k2)} ", e, a(k1)(k2), r)
    }
  }

}
