package dhg.ccg.parse.scg.mcmc

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
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

class ScgGuideChartProdFinderTests {

  val S = cat"S".asInstanceOf[AtomCat]
  val NP = cat"NP".asInstanceOf[AtomCat]
  val N = cat"N".asInstanceOf[AtomCat]
  val PP = cat"PP".asInstanceOf[AtomCat]
  val STA = cat"<S>"
  val END = cat"<E>"

  val A = cat"A".asInstanceOf[AtomCat]
  val B = cat"B".asInstanceOf[AtomCat]
  val C = cat"C".asInstanceOf[AtomCat]
  val D = cat"D".asInstanceOf[AtomCat]
  val E = cat"E".asInstanceOf[AtomCat]
  val F = cat"F".asInstanceOf[AtomCat]
  val G = cat"G".asInstanceOf[AtomCat]
  val H = cat"H".asInstanceOf[AtomCat]

  @Test
  def test_SimpleScgGuideChartProdFinder {
    type Word = String

    val gc1 = CfgGuideChart("the dogs run".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(
        ListMap(),
        ListMap(
          (N / N) -> Set(TermGuideChartEntry(TermProd("the"))),
          (NP / N) -> Set(TermGuideChartEntry(TermProd("the")))),
        ListMap(
          N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N))),
          NP -> Set(
            BinaryGuideChartEntry(1, BinaryProd((NP / N), N)),
            UnaryGuideChartEntry(UnaryProd(N)))),
        ListMap(
          S -> Set(
            BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))),
            BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))),
          N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / S), S))),
          NP -> Set(UnaryGuideChartEntry(UnaryProd(N))))),
      Vector(
        ListMap(),
        ListMap(),
        ListMap(
          N -> Set(TermGuideChartEntry(TermProd("dogs"))),
          (S \ N) -> Set(TermGuideChartEntry(TermProd("dogs"))),
          NP -> Set(
            UnaryGuideChartEntry(UnaryProd(N)),
            UnaryGuideChartEntry(UnaryProd(NP / N)))),
        ListMap(
          S -> Set(
            BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))),
            BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(
        ListMap(),
        ListMap(),
        ListMap(),
        ListMap(
          (S \ N) -> Set(TermGuideChartEntry(TermProd("run"))),
          (S \ NP) -> Set(UnaryGuideChartEntry(UnaryProd((S \ N))))))))
    //  +--------------------+--------------------+--------------------+
    //  |                    | (0,1)              | (0,2)              |
    //  | (N/N) -> "the"     | N -> 1:[(N/N) N]   | S -> 2:[N (S\N)]   |
    //  | (NP/N) -> "the"    | NP -> 1:[(NP/N) N] |      2:[NP (S\NP)] |
    //  |                    |       N            | N -> 1:[(N/S) S]   |
    //  |                    |                    | NP -> N            |  
    //  +--------------------+--------------------+--------------------+
    //  |                    |                    | (1,2)              |
    //  |                    | N -> "dogs"        | S -> 2:[N (S\N)]   |
    //  |                    | NP -> N            |      2:[NP (S\NP)] |
    //  |                    |       NP/N         |                    |
    //  |                    | (S\N) -> "dogs"    |                    |
    //  +--------------------+--------------------+--------------------+
    //  |                    |                    |                    |
    //  |                    |                    | (S\N) -> "run"     |
    //  |                    |                    | (S\NP) -> (S\N)    |
    //  |                    |                    |                    |
    //  |                    |                    |                    |
    //  +--------------------+--------------------+--------------------+

    val se = SimpleStartEndTags[Cat](STA, END)

    val gcu = new SimpleScgGuideChartProdFinder(new SimplePcfgGuideChartProdFinder())

    assertEquals(Set(S, N, NP), gcu.roots(gc1))

    val binys1 = gcu.binys(gc1)
    assertEquals(Set(S, NP, N), binys1.keySet)
    assertEquals(Set(BinaryProd(N, S \ N), BinaryProd(NP, S \ NP)), binys1(S))
    assertEquals(Set(BinaryProd(NP / N, N)), binys1(NP))
    assertEquals(Set(BinaryProd(N / S, S), BinaryProd(N / N, N)), binys1(N))

    val unrys1 = gcu.unrys(gc1)
    assertEquals(Set(NP, S \ NP), unrys1.keySet)
    assertEquals(Set(UnaryProd(S \ N)), unrys1(S \ NP))
    assertEquals(Set(UnaryProd(N), UnaryProd(NP / N)), unrys1(NP))

    val terms1 = gcu.terms(gc1)
    assertEquals(Set(N / N, NP / N, N, S \ N), terms1.keySet)
    assertEquals(Set(TermProd("dogs")), terms1(N))
    assertEquals(Set(TermProd("the")), terms1(N / N))
    assertEquals(Set(TermProd("the")), terms1(NP / N))
    assertEquals(Set(TermProd("run"), TermProd("dogs")), terms1(S \ N))

    val prods1 = gcu.prods(gc1)
    assertEquals(
      binys1.asInstanceOf[Map[Cat, Set[Prod]]] |+|
        unrys1.asInstanceOf[Map[Cat, Set[Prod]]] |+|
        terms1.asInstanceOf[Map[Cat, Set[Prod]]], prods1)

    val lctxs1 = gcu.lctxs(gc1)(se)
    assertEquals(Set(S, N / N, NP / N, N, NP, S \ N, S \ NP), lctxs1.keySet)
    assertEquals(Set(N / N, NP / N, STA), lctxs1(S))
    assertEquals(Set(STA), lctxs1(N / N))
    assertEquals(Set(STA), lctxs1(NP / N))
    assertEquals(Set(STA, N / N, NP / N), lctxs1(N))
    assertEquals(Set(STA, N / N, NP / N), lctxs1(NP))
    assertEquals(Set(N / N, NP / N, N, NP, S \ N), lctxs1(S \ N))
    assertEquals(Set(N, NP, S \ N), lctxs1(S \ NP))

    val rctxs1 = gcu.rctxs(gc1)(se)
    assertEquals(Set(S, N / N, NP / N, N, NP, S \ N, S \ NP), rctxs1.keySet)
    assertEquals(Set(END), rctxs1(S))
    assertEquals(Set(N, NP, S \ N), rctxs1(N / N))
    assertEquals(Set(N, NP, S \ N), rctxs1(NP / N))
    assertEquals(Set(S \ N, S \ NP, END), rctxs1(N))
    assertEquals(Set(S \ N, S \ NP, END), rctxs1(NP))
    assertEquals(Set(S \ N, S \ NP, END), rctxs1(S \ N))
    assertEquals(Set(END), rctxs1(S \ NP))

  }

}
