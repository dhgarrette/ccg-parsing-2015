package dhg.ccg.parse.scg.mcmc

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.parse.scg._
import dhg.ccg.tagdict.StartEndTags
import dhg.util._
import dhg.ccg.tagdict._
import dhg.ccg.test.TestUtil.DoubleIteratorRandomGenerator
import org.apache.commons.math3.random.MersenneTwister
import scala.collection.immutable.ListMap
import scalaz._
import Scalaz._

class ScgProductionFinderTests {

  val A: Cat = cat"A"
  val B: Cat = cat"B"
  val C: Cat = cat"C"
  val D: Cat = cat"D"
  val E: Cat = cat"E"
  val F: Cat = cat"F"
  val G: Cat = cat"G"
  val H: Cat = cat"H"
  val STA: Cat = cat"<S>"
  val END: Cat = cat"<E>"

  @Test
  def test_SimpleScgProductionFinder {
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

    val se = SimpleStartEndTags(STA, END)

    val mockRootCounts = Map[Cat, Double](cat"some root count" -> 1.0)
    val mockBinyCounts = Map[Cat, Map[BinaryProd, Double]](cat"some binary count" -> Map())
    val mockUnryCounts = Map[Cat, Map[UnaryProd, Double]](cat"some unary count" -> Map())
    val mockTermCounts = Map[Cat, Map[TermProd, Double]](cat"some term count" -> Map())
    val mockProdCounts = Map[Cat, Map[Prod, Double]](cat"some prodcount" -> Map())

    val mockPcfgProductionCounter = new PcfgProductionCounter {
      def rootCounts(t: CcgTree): Map[Cat, Double] = mockRootCounts
      def binyCounts(t: CcgTree): Map[Cat, Map[BinaryProd, Double]] = mockBinyCounts
      def unryCounts(t: CcgTree): Map[Cat, Map[UnaryProd, Double]] = mockUnryCounts
      def termCounts(t: CcgTree): Map[Cat, Map[TermProd, Double]] = mockTermCounts
      def prodCounts(t: CcgTree): Map[Cat, Map[Prod, Double]] = mockProdCounts
    }
    val pf = new SimpleScgProductionFinder(mockPcfgProductionCounter)

    assertSame(mockRootCounts, pf.rootCounts(t))
    assertSame(mockBinyCounts, pf.binyCounts(t))
    assertSame(mockUnryCounts, pf.unryCounts(t))
    assertSame(mockTermCounts, pf.termCounts(t))
    assertSame(mockProdCounts, pf.prodCounts(t))

    assertEqualsDoubleMapMap(
      Map(
        A -> Map(
          STA -> 2.0,
          A -> 1.0,
          B -> 1.0,
          C -> 1.0),
        B -> Map(
          STA -> 2.0,
          A -> 1.0,
          C -> 1.0),
        C -> Map(
          B -> 1.0,
          C -> 1.0)),
      pf.lctxCounts(t)(se), 1e-9)

    assertEqualsDoubleMapMap(
      Map(
        A -> Map(
          END -> 2.0,
          A -> 2.0,
          B -> 1.0),
        B -> Map(
          END -> 1.0,
          A -> 1.0,
          B -> 1.0,
          C -> 1.0),
        C -> Map(
          END -> 1.0,
          B -> 1.0)),
      pf.rctxCounts(t)(se), 1e-9)
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
