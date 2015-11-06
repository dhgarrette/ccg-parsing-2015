package dhg.ccg.parse.scg.mcmc

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.scg._
import dhg.ccg.tagdict.StartEndTags
import dhg.util._
import dhg.ccg.tagdict._

class ScgAlphaPriorMakerTests {

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
  def test_TrainDataScgAlphaPriorMaker_makeAll {
    type Word = String

    val gc1 = CfgGuideChart(Vector.empty, Vector(Vector(Map[Cat, Set[GuideChartEntry]](cat"this" -> Set()))))
    val gc2 = CfgGuideChart(Vector.empty, Vector(Vector(Map[Cat, Set[GuideChartEntry]](cat"and" -> Set()))))
    val gc3 = CfgGuideChart(Vector.empty, Vector(Vector(Map[Cat, Set[GuideChartEntry]](cat"that" -> Set()))))
    val guideCharts = Vector(gc1, gc2, gc3)

    val mockGuideChartProdFinder = new ScgGuideChartProdFinder {
      def roots(gc: CfgGuideChart): Set[Cat] = gc match {
        case _ if gc == gc1 => Set(A, B)
        case _ if gc == gc2 => Set(B, E)
        case _ if gc == gc3 => Set(F)
      }
      def prods(gc: CfgGuideChart): Map[Cat, Set[Prod]] = ???
      def binys(gc: CfgGuideChart): Map[Cat, Set[BinaryProd]] = gc match {
        case _ if gc == gc1 => Map(
          A -> Set(BinaryProd(B, C)),
          C -> Set(BinaryProd(D, F)))
        case _ if gc == gc2 => Map(
          A -> Set(BinaryProd(B, C), BinaryProd(E, D)),
          C -> Set(BinaryProd(D, E), BinaryProd(E, D)),
          E -> Set(BinaryProd(B, C)))
        case _ if gc == gc3 => Map(
          A -> Set(BinaryProd(B, C), BinaryProd(E, D)),
          C -> Set(BinaryProd(D, E), BinaryProd(A, D)),
          E -> Set(BinaryProd(D, F)))
      }
      def unrys(gc: CfgGuideChart): Map[Cat, Set[UnaryProd]] = gc match {
        case _ if gc == gc1 => Map(
          A -> Set(UnaryProd(B)),
          C -> Set(UnaryProd(D)))
        case _ if gc == gc2 => Map(
          A -> Set(UnaryProd(B), UnaryProd(D)),
          C -> Set(UnaryProd(D), UnaryProd(E)),
          E -> Set(UnaryProd(B)))
        case _ if gc == gc3 => Map(
          A -> Set(UnaryProd(B), UnaryProd(E)),
          C -> Set(UnaryProd(D), UnaryProd(A)),
          E -> Set(UnaryProd(D)))
      }
      def terms(gc: CfgGuideChart): Map[Cat, Set[TermProd]] = gc match {
        case _ if gc == gc1 => Map(
          A -> Set(TermProd("a1")),
          C -> Set(TermProd("c1")),
          D -> Set(TermProd("d2")))
        case _ if gc == gc2 => Map(
          A -> Set(TermProd("a1"), TermProd("a2")),
          C -> Set(TermProd("c3")),
          D -> Set(TermProd("d2")))
        case _ if gc == gc3 => Map(
          A -> Set(TermProd("a1"), TermProd("a2")),
          C -> Set(TermProd("c1")))
      }
      def lctxs(gc: CfgGuideChart)(se: StartEndTags[Cat]): Map[Cat, Set[Cat]] = gc match {
        case _ if gc == gc1 => Map(
          A -> Set(A, STA),
          C -> Set(B, C),
          D -> Set(STA),
          F -> Set(E))
        case _ if gc == gc2 => Map(
          A -> Set(D),
          C -> Set(A))
        case _ if gc == gc3 => Map(
          A -> Set(D),
          C -> Set(A))
      }
      def rctxs(gc: CfgGuideChart)(se: StartEndTags[Cat]): Map[Cat, Set[Cat]] = gc match {
        case _ if gc == gc1 => Map(
          A -> Set(B, END),
          C -> Set(END, B),
          D -> Set(END),
          F -> Set(E))
        case _ if gc == gc2 => Map(
          A -> Set(B),
          C -> Set(D))
        case _ if gc == gc3 => Map(
          B -> Set(END, A),
          C -> Set(E, F))
      }
    }

    val s1: CcgTree = CcgLeaf(A, "something", "FAKEPOS")
    val s2: CcgTree = CcgLeaf(B, "fake", "FAKEPOS")
    val goldLabeledSentences = Vector[CcgTree](s1, s2)

    val mockProductionFinder = new ScgProductionFinder {
      def rootCounts(t: CcgTree): Map[Cat, Double] = t match {
        case _ if t == s1 => Map(A -> 11, B -> 12)
        case _ if t == s2 => Map(B -> 13, C -> 14, D -> 15)
      }
      def binyCounts(t: CcgTree): Map[Cat, Map[BinaryProd, Double]] = t match {
        case _ if t == s1 => Map(
          A -> Map(BinaryProd(B, C) -> 21),
          C -> Map(BinaryProd(D, E) -> 22),
          D -> Map(BinaryProd(B, C) -> 23))
        case _ if t == s2 => Map(
          A -> Map(BinaryProd(B, C) -> 24, BinaryProd(E, F) -> 26),
          C -> Map(BinaryProd(D, E) -> 25, BinaryProd(E, F) -> 27))
      }
      def unryCounts(t: CcgTree): Map[Cat, Map[UnaryProd, Double]] = t match {
        case _ if t == s1 => Map(
          A -> Map(UnaryProd(B) -> 61),
          C -> Map(UnaryProd(D) -> 62),
          D -> Map(UnaryProd(B) -> 63))
        case _ if t == s2 => Map(
          A -> Map(UnaryProd(B) -> 64, UnaryProd(E) -> 66),
          C -> Map(UnaryProd(D) -> 65, UnaryProd(E) -> 67))
      }
      def termCounts(t: CcgTree): Map[Cat, Map[TermProd, Double]] = t match {
        case _ if t == s1 => Map(
          A -> Map(TermProd("a1") -> 31),
          C -> Map(TermProd("c1") -> 32, TermProd("c2") -> 33))
        case _ if t == s2 => Map(
          A -> Map(TermProd("a1") -> 35, TermProd("a2") -> 36),
          C -> Map(TermProd("c1") -> 37, TermProd("c3") -> 38),
          E -> Map(TermProd("e1") -> 39))
      }
      def prodCounts(t: CcgTree): Map[Cat, Map[Prod, Double]] = ???
      def lctxCounts(t: CcgTree)(se: StartEndTags[Cat]): Map[Cat, Map[Cat, Double]] = t match {
        case _ if t == s1 => Map(
          A -> Map(A -> 31, STA -> 32),
          C -> Map(B -> 33, C -> 34),
          D -> Map(STA -> 35))
        case _ if t == s2 => Map(
          A -> Map(STA -> 36, B -> 37),
          C -> Map(STA -> 38),
          E -> Map(D -> 39))
      }
      def rctxCounts(t: CcgTree)(se: StartEndTags[Cat]): Map[Cat, Map[Cat, Double]] = t match {
        case _ if t == s1 => Map(
          A -> Map(C -> 41, END -> 42),
          C -> Map(D -> 43),
          D -> Map(END -> 44, C -> 45))
        case _ if t == s2 => Map(
          A -> Map(END -> 46),
          C -> Map(END -> 47, E -> 48),
          E -> Map(D -> 49))
      }
    }

    val priorRootDist = new LogProbabilityDistribution[Cat] {
      def apply(b: Cat): LogDouble = LogDouble(b match {
        case A => 0.91
        case B => 0.92
        case C => 0.93
        case D => 0.94
        case E => 0.95
        case F => 0.96
      })
      def sample(): Cat = ???
      def defaultProb: LogDouble = ???
    }
    val priorBinyDist = new ConditionalLogProbabilityDistribution[Cat, BinaryProd] {
      def apply(x: BinaryProd, given: Cat): LogDouble = LogDouble((given, x) match {
        case (A, BinaryProd(B, C)) => 0.11
        case (A, BinaryProd(E, D)) => 0.12
        case (A, BinaryProd(E, F)) => 0.13

        case (C, BinaryProd(A, D)) => 0.14
        case (C, BinaryProd(D, E)) => 0.15
        case (C, BinaryProd(D, F)) => 0.16
        case (C, BinaryProd(E, D)) => 0.17
        case (C, BinaryProd(E, F)) => 0.18

        case (D, BinaryProd(B, C)) => 0.19

        case (E, BinaryProd(D, F)) => 0.21
        case (E, BinaryProd(B, C)) => 0.22
      })
      def sample(given: Cat): BinaryProd = ???
    }
    val priorUnryDist = new ConditionalLogProbabilityDistribution[Cat, UnaryProd] {
      def apply(x: UnaryProd, given: Cat): LogDouble = LogDouble((given, x) match {
        case (A, UnaryProd(B)) => 0.23
        case (A, UnaryProd(D)) => 0.24
        case (A, UnaryProd(E)) => 0.25

        case (C, UnaryProd(A)) => 0.26
        case (C, UnaryProd(D)) => 0.27
        case (C, UnaryProd(E)) => 0.28

        case (D, UnaryProd(B)) => 0.42

        case (E, UnaryProd(B)) => 0.43
        case (E, UnaryProd(D)) => 0.44
      })
      def sample(given: Cat): UnaryProd = ???
    }
    val priorTermDist = new ConditionalLogProbabilityDistribution[Cat, TermProd] {
      def apply(x: TermProd, given: Cat): LogDouble = LogDouble((given, x) match {
        case (A, TermProd("a1")) => 0.31
        case (A, TermProd("a2")) => 0.32

        case (C, TermProd("c1")) => 0.33
        case (C, TermProd("c2")) => 0.34
        case (C, TermProd("c3")) => 0.35

        case (D, TermProd("d2")) => 0.36

        case (E, TermProd("e1")) => 0.37
      })
      def sample(given: Cat): TermProd = ???
    }
    val priorLctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
      def apply(left: Cat, given: Cat): LogDouble = LogDouble((given, left) match {
        case (A, STA) => 0.51
        case (A, A) => 0.52
        case (A, B) => 0.53
        case (A, D) => 0.54

        case (C, STA) => 0.55
        case (C, A) => 0.56
        case (C, B) => 0.57
        case (C, C) => 0.58

        case (D, STA) => 0.61

        case (E, D) => 0.63

        case (F, E) => 0.64
      })
      def sample(given: Cat): Cat = ???
    }
    val priorRctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
      def apply(right: Cat, given: Cat): LogDouble = LogDouble((given, right) match {
        case (A, B) => 0.71
        case (A, C) => 0.72
        case (A, END) => 0.73

        case (B, A) => 0.74
        case (B, END) => 0.75

        case (C, B) => 0.76
        case (C, D) => 0.77
        case (C, E) => 0.78
        case (C, F) => 0.79
        case (C, END) => 0.81

        case (D, C) => 0.82
        case (D, END) => 0.83

        case (E, D) => 0.84

        case (F, E) => 0.85
      })
      def sample(given: Cat): Cat = ???
    }

    val alphaRoot = 2.1
    val alphaBiny = 2.2
    val alphaUnry = 2.6
    val alphaTerm = 2.3
    val alphaLctx = 2.4
    val alphaRctx = 2.5

    val se = SimpleStartEndTags[Cat](STA, END)

    val apm = new TrainDataScgAlphaPriorMaker(mockProductionFinder, mockGuideChartProdFinder)
    val (alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts, alphaPriorLctxCounts, alphaPriorRctxCounts) =
      apm.makeAll(guideCharts, goldLabeledSentences,
        priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, priorLctxDist, priorRctxDist,
        alphaRoot, alphaBiny, alphaUnry, alphaTerm, alphaLctx, alphaRctx)(se)

    assertLogMap(Map[Cat, Double](
      A -> (2.1 * 0.91 + 11),
      B -> (2.1 * 0.92 + 12 + 13),
      C -> (2.1 * 0.93 + 14),
      D -> (2.1 * 0.94 + 15),
      E -> (2.1 * 0.95 + 0),
      F -> (2.1 * 0.96 + 0)),
      alphaPriorRootCounts, 1e-9)

    assertEquals(Set(A, C, D, E), alphaPriorBinyCounts.keySet)
    assertLogMap(Map[BinaryProd, Double](
      BinaryProd(B, C) -> (2.2 * 0.11 + 21 + 24),
      BinaryProd(E, D) -> (2.2 * 0.12 + 0),
      BinaryProd(E, F) -> (2.2 * 0.13 + 26)),
      alphaPriorBinyCounts(A), 1e-9)
    assertLogMap(Map[BinaryProd, Double](
      BinaryProd(A, D) -> (2.2 * 0.14 + 0),
      BinaryProd(D, E) -> (2.2 * 0.15 + 22 + 25),
      BinaryProd(D, F) -> (2.2 * 0.16 + 0),
      BinaryProd(E, D) -> (2.2 * 0.17 + 0),
      BinaryProd(E, F) -> (2.2 * 0.18 + 27)),
      alphaPriorBinyCounts(C), 1e-9)
    assertLogMap(Map[BinaryProd, Double](
      BinaryProd(B, C) -> (2.2 * 0.19 + 23)),
      alphaPriorBinyCounts(D), 1e-9)
    assertLogMap(Map[BinaryProd, Double](
      BinaryProd(D, F) -> (2.2 * 0.21 + 0),
      BinaryProd(B, C) -> (2.2 * 0.22 + 0)),
      alphaPriorBinyCounts(E), 1e-9)

    assertEquals(Set(A, C, D, E), alphaPriorUnryCounts.keySet)
    assertLogMap(Map[UnaryProd, Double](
      UnaryProd(B) -> (2.6 * 0.23 + 61 + 64),
      UnaryProd(D) -> (2.6 * 0.24 + 0),
      UnaryProd(E) -> (2.6 * 0.25 + 66)),
      alphaPriorUnryCounts(A), 1e-9)
    assertLogMap(Map[UnaryProd, Double](
      UnaryProd(A) -> (2.6 * 0.26 + 0),
      UnaryProd(D) -> (2.6 * 0.27 + 62 + 65),
      UnaryProd(E) -> (2.6 * 0.28 + 67)),
      alphaPriorUnryCounts(C), 1e-9)
    assertLogMap(Map[UnaryProd, Double](
      UnaryProd(B) -> (2.6 * 0.42 + 63)),
      alphaPriorUnryCounts(D), 1e-9)
    assertLogMap(Map[UnaryProd, Double](
      UnaryProd(B) -> (2.6 * 0.43 + 0),
      UnaryProd(D) -> (2.6 * 0.44 + 0)),
      alphaPriorUnryCounts(E), 1e-9)

    assertEquals(Set(A, C, D, E), alphaPriorTermCounts.keySet)
    assertLogMap(Map(
      TermProd("a1") -> (2.3 * 0.31 + 31 + 35),
      TermProd("a2") -> (2.3 * 0.32 + 36)),
      alphaPriorTermCounts(A), 1e-9)
    assertLogMap(Map(
      TermProd("c1") -> (2.3 * 0.33 + 32 + 37),
      TermProd("c2") -> (2.3 * 0.34 + 33),
      TermProd("c3") -> (2.3 * 0.35 + 38)),
      alphaPriorTermCounts(C), 1e-9)
    assertLogMap(Map(
      TermProd("d2") -> (2.3 * 0.36 + 0)),
      alphaPriorTermCounts(D), 1e-9)
    assertLogMap(Map(
      TermProd("e1") -> (2.3 * 0.37 + 39)),
      alphaPriorTermCounts(E), 1e-9)

    assertEquals(Set(A, C, D, E, F), alphaPriorLctxCounts.keySet)
    assertLogMap(Map[Cat, Double](
      STA -> (2.4 * 0.51 + 32 + 36),
      A -> (2.4 * 0.52 + 31),
      B -> (2.4 * 0.53 + 37),
      D -> (2.4 * 0.54 + 0)),
      alphaPriorLctxCounts(A), 1e-9)
    assertLogMap(Map[Cat, Double](
      STA -> (2.4 * 0.55 + 38),
      A -> (2.4 * 0.56 + 0),
      B -> (2.4 * 0.57 + 33),
      C -> (2.4 * 0.58 + 34)),
      alphaPriorLctxCounts(C), 1e-9)
    assertLogMap(Map[Cat, Double](
      STA -> (2.4 * 0.61 + 35)),
      alphaPriorLctxCounts(D), 1e-9)
    assertLogMap(Map[Cat, Double](
      D -> (2.4 * 0.63 + 39)),
      alphaPriorLctxCounts(E), 1e-9)
    assertLogMap(Map[Cat, Double](
      E -> (2.4 * 0.64 + 0)),
      alphaPriorLctxCounts(F), 1e-9)

    assertEquals(Set(A, B, C, D, E, F), alphaPriorRctxCounts.keySet)
    assertLogMap(Map[Cat, Double](
      B -> (2.5 * 0.71 + 0),
      C -> (2.5 * 0.72 + 41),
      END -> (2.5 * 0.73 + 42 + 46)),
      alphaPriorRctxCounts(A), 1e-9)
    assertLogMap(Map[Cat, Double](
      A -> (2.5 * 0.74 + 0),
      END -> (2.5 * 0.75 + 0)),
      alphaPriorRctxCounts(B), 1e-9)
    assertLogMap(Map[Cat, Double](
      B -> (2.5 * 0.76 + 0),
      D -> (2.5 * 0.77 + 43),
      E -> (2.5 * 0.78 + 48),
      F -> (2.5 * 0.79 + 0),
      END -> (2.5 * 0.81 + 47)),
      alphaPriorRctxCounts(C), 1e-9)
    assertLogMap(Map[Cat, Double](
      C -> (2.5 * 0.82 + 45),
      END -> (2.5 * 0.83 + 44)),
      alphaPriorRctxCounts(D), 1e-9)
    assertLogMap(Map[Cat, Double](
      D -> (2.5 * 0.84 + 49)),
      alphaPriorRctxCounts(E), 1e-9)
    assertLogMap(Map[Cat, Double](
      E -> (2.5 * 0.85 + 0)),
      alphaPriorRctxCounts(F), 1e-9)
  }

//  @Test
//  def test_TrainDataNormalizingScgAlphaPriorMaker_makeAll {
//    type Word = String
//
//    val gc1 = CfgGuideChart(Vector.empty, Vector(Vector(Map[Cat, Set[GuideChartEntry]](AtomCat("this") -> Set()))))
//    val gc2 = CfgGuideChart(Vector.empty, Vector(Vector(Map[Cat, Set[GuideChartEntry]](AtomCat("and") -> Set()))))
//    val gc3 = CfgGuideChart(Vector.empty, Vector(Vector(Map[Cat, Set[GuideChartEntry]](AtomCat("that") -> Set()))))
//    val guideCharts = Vector(gc1, gc2, gc3)
//
//    val mockGuideChartProdFinder = new ScgGuideChartProdFinder {
//      def roots(gc: CfgGuideChart): Set[Cat] = gc match {
//        case _ if gc == gc1 => Set(A, B)
//        case _ if gc == gc2 => Set(B, E)
//        case _ if gc == gc3 => Set(F)
//      }
//      def binys(gc: CfgGuideChart): Map[Cat, Set[BinaryProd]] = gc match {
//        case _ if gc == gc1 => Map(
//          A -> Set(BinaryProd(B, C)),
//          C -> Set(BinaryProd(D, F)))
//        case _ if gc == gc2 => Map(
//          A -> Set(BinaryProd(B, C), BinaryProd(E, D)),
//          C -> Set(BinaryProd(D, E), BinaryProd(E, D)),
//          E -> Set(BinaryProd(B, C)))
//        case _ if gc == gc3 => Map(
//          A -> Set(BinaryProd(B, C), BinaryProd(E, D)),
//          C -> Set(BinaryProd(D, E), BinaryProd(A, D)),
//          E -> Set(BinaryProd(D, F)))
//      }
//      def unrys(gc: CfgGuideChart): Map[Cat, Set[UnaryProd]] = gc match {
//        case _ if gc == gc1 => Map(
//          A -> Set(UnaryProd(B)),
//          C -> Set(UnaryProd(D)))
//        case _ if gc == gc2 => Map(
//          A -> Set(UnaryProd(B), UnaryProd(D)),
//          C -> Set(UnaryProd(D), UnaryProd(E)),
//          E -> Set(UnaryProd(B)))
//        case _ if gc == gc3 => Map(
//          A -> Set(UnaryProd(B), UnaryProd(E)),
//          C -> Set(UnaryProd(D), UnaryProd(A)),
//          E -> Set(UnaryProd(D)))
//      }
//      def terms(gc: CfgGuideChart): Map[Cat, Set[TermProd]] = gc match {
//        case _ if gc == gc1 => Map(
//          A -> Set(TermProd("a1")),
//          C -> Set(TermProd("c1")),
//          D -> Set(TermProd("d2")))
//        case _ if gc == gc2 => Map(
//          A -> Set(TermProd("a1"), TermProd("a2")),
//          C -> Set(TermProd("c3")),
//          D -> Set(TermProd("d2")))
//        case _ if gc == gc3 => Map(
//          A -> Set(TermProd("a1"), TermProd("a2")),
//          C -> Set(TermProd("c1")))
//      }
//      def lctxs(gc: CfgGuideChart)(se: StartEndTags[Cat]): Map[Cat, Set[Cat]] = gc match {
//        case _ if gc == gc1 => Map(
//          A -> Set(A, STA),
//          C -> Set(B, C),
//          D -> Set(STA),
//          F -> Set(E))
//        case _ if gc == gc2 => Map(
//          A -> Set(D),
//          C -> Set(A))
//        case _ if gc == gc3 => Map(
//          A -> Set(D),
//          C -> Set(A))
//      }
//      def rctxs(gc: CfgGuideChart)(se: StartEndTags[Cat]): Map[Cat, Set[Cat]] = gc match {
//        case _ if gc == gc1 => Map(
//          A -> Set(B, END),
//          C -> Set(END, B),
//          D -> Set(END),
//          F -> Set(E))
//        case _ if gc == gc2 => Map(
//          A -> Set(B),
//          C -> Set(D))
//        case _ if gc == gc3 => Map(
//          B -> Set(END, A),
//          C -> Set(E, F))
//      }
//    }
//
//    val s1: CcgTree = CcgLeaf(A, "something")
//    val s2: CcgTree = CcgLeaf(B, "fake")
//    val goldLabeledSentences = Vector[CcgTree](s1, s2)
//
//    val mockProductionFinder = new ScgProductionFinder {
//      def rootCounts(t: CcgTree): Map[Cat, Double] = t match {
//        case _ if t == s1 => Map(A -> 11, B -> 12)
//        case _ if t == s2 => Map(B -> 13, C -> 14, D -> 15)
//      }
//      def binyCounts(t: CcgTree): Map[Cat, Map[BinaryProd, Double]] = t match {
//        case _ if t == s1 => Map(
//          A -> Map(BinaryProd(B, C) -> 21),
//          C -> Map(BinaryProd(D, E) -> 22),
//          D -> Map(BinaryProd(B, C) -> 23))
//        case _ if t == s2 => Map(
//          A -> Map(BinaryProd(B, C) -> 24, BinaryProd(E, F) -> 26),
//          C -> Map(BinaryProd(D, E) -> 25, BinaryProd(E, F) -> 27))
//      }
//      def unryCounts(t: CcgTree): Map[Cat, Map[UnaryProd, Double]] = t match {
//        case _ if t == s1 => Map(
//          A -> Map(UnaryProd(B) -> 61),
//          C -> Map(UnaryProd(D) -> 62),
//          D -> Map(UnaryProd(B) -> 63))
//        case _ if t == s2 => Map(
//          A -> Map(UnaryProd(B) -> 64, UnaryProd(E) -> 66),
//          C -> Map(UnaryProd(D) -> 65, UnaryProd(E) -> 67))
//      }
//      def termCounts(t: CcgTree): Map[Cat, Map[TermProd, Double]] = t match {
//        case _ if t == s1 => Map(
//          A -> Map(TermProd("a1") -> 31),
//          C -> Map(TermProd("c1") -> 32, TermProd("c2") -> 33))
//        case _ if t == s2 => Map(
//          A -> Map(TermProd("a1") -> 35, TermProd("a2") -> 36),
//          C -> Map(TermProd("c1") -> 37, TermProd("c3") -> 38),
//          E -> Map(TermProd("e1") -> 39))
//      }
//      def prodCounts(t: CcgTree): Map[Cat, Map[Prod, Double]] = ???
//      def lctxCounts(t: CcgTree)(se: StartEndTags[Cat]): Map[Cat, Map[Cat, Double]] = t match {
//        case _ if t == s1 => Map(
//          A -> Map(A -> 31, STA -> 32),
//          C -> Map(B -> 33, C -> 34),
//          D -> Map(STA -> 35))
//        case _ if t == s2 => Map(
//          A -> Map(STA -> 36, B -> 37),
//          C -> Map(STA -> 38),
//          E -> Map(D -> 39))
//      }
//      def rctxCounts(t: CcgTree)(se: StartEndTags[Cat]): Map[Cat, Map[Cat, Double]] = t match {
//        case _ if t == s1 => Map(
//          A -> Map(C -> 41, END -> 42),
//          C -> Map(D -> 43),
//          D -> Map(END -> 44, C -> 45))
//        case _ if t == s2 => Map(
//          A -> Map(END -> 46),
//          C -> Map(END -> 47, E -> 48),
//          E -> Map(D -> 49))
//      }
//    }
//
//    val priorRootDist = new LogProbabilityDistribution[Cat] {
//      def apply(b: Cat): LogDouble = LogDouble(b match {
//        case A => 0.91
//        case B => 0.92
//        case C => 0.93
//        case D => 0.94
//        case E => 0.95
//        case F => 0.96
//        //        5.61
//      })
//      def sample(): Cat = ???
//      def defaultProb: LogDouble = ???
//    }
//    val priorBinyDist = new ConditionalLogProbabilityDistribution[Cat, BinaryProd] {
//      def apply(x: BinaryProd, given: Cat): LogDouble = LogDouble((given, x) match {
//        case (A, BinaryProd(B, C)) => 0.11
//        case (A, BinaryProd(E, D)) => 0.12
//        case (A, BinaryProd(E, F)) => 0.13
//        //                             0.36
//        case (C, BinaryProd(A, D)) => 0.14
//        case (C, BinaryProd(D, E)) => 0.15
//        case (C, BinaryProd(D, F)) => 0.16
//        case (C, BinaryProd(E, D)) => 0.17
//        case (C, BinaryProd(E, F)) => 0.18
//        //                             0.80
//        case (D, BinaryProd(B, C)) => 0.19
//        //                             0.19
//        case (E, BinaryProd(D, F)) => 0.21
//        case (E, BinaryProd(B, C)) => 0.22
//        //                             0.43
//      })
//      def sample(given: Cat): BinaryProd = ???
//    }
//    val priorUnryDist = new ConditionalLogProbabilityDistribution[Cat, UnaryProd] {
//      def apply(x: UnaryProd, given: Cat): LogDouble = LogDouble((given, x) match {
//        case (A, UnaryProd(B)) => 0.23
//        case (A, UnaryProd(D)) => 0.24
//        case (A, UnaryProd(E)) => 0.25
//        //                        0.72
//        case (C, UnaryProd(A)) => 0.26
//        case (C, UnaryProd(D)) => 0.27
//        case (C, UnaryProd(E)) => 0.28
//        //                        0.61
//        case (D, UnaryProd(B)) => 0.42
//        //                        0.42
//        case (E, UnaryProd(B)) => 0.43
//        case (E, UnaryProd(D)) => 0.44
//        //                        0.87
//      })
//      def sample(given: Cat): UnaryProd = ???
//    }
//    val priorTermDist = new ConditionalLogProbabilityDistribution[Cat, TermProd] {
//      def apply(x: TermProd, given: Cat): LogDouble = LogDouble((given, x) match {
//        case (A, TermProd("a1")) => 0.31
//        case (A, TermProd("a2")) => 0.32
//        //                          0.63
//        case (C, TermProd("c1")) => 0.33
//        case (C, TermProd("c2")) => 0.34
//        case (C, TermProd("c3")) => 0.35
//        //                          1.02
//        case (D, TermProd("d2")) => 0.36
//        //                          0.36
//        case (E, TermProd("e1")) => 0.37
//        //                          0.37
//      })
//      def sample(given: Cat): TermProd = ???
//    }
//    val priorLctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
//      def apply(left: Cat, given: Cat): LogDouble = LogDouble((given, left) match {
//        case (A, STA) => 0.51
//        case (A, A) => 0.52
//        case (A, B) => 0.53
//        case (A, D) => 0.54
//        //             2.10
//        case (C, STA) => 0.55
//        case (C, A) => 0.56
//        case (C, B) => 0.57
//        case (C, C) => 0.58
//        //             2.26
//        case (D, STA) => 0.61
//        //               0.61
//        case (E, D) => 0.63
//        //             0.63
//        case (F, E) => 0.64
//        //             0.64
//      })
//      def sample(given: Cat): Cat = ???
//    }
//    val priorRctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
//      def apply(right: Cat, given: Cat): LogDouble = LogDouble((given, right) match {
//        case (A, B) => 0.71
//        case (A, C) => 0.72
//        case (A, END) => 0.73
//        //               2.16
//        case (B, A) => 0.74
//        case (B, END) => 0.75
//        //               1.49
//        case (C, B) => 0.76
//        case (C, D) => 0.77
//        case (C, E) => 0.78
//        case (C, F) => 0.79
//        case (C, END) => 0.81
//        //               3.91
//        case (D, C) => 0.82
//        case (D, END) => 0.83
//        //               1.65
//        case (E, D) => 0.84
//        //             0.84
//        case (F, E) => 0.85
//        //             0.85
//      })
//      def sample(given: Cat): Cat = ???
//    }
//
//    val alphaRoot = 2.1
//    val alphaBiny = 2.2
//    val alphaUnry = 2.6
//    val alphaTerm = 2.3
//    val alphaLctx = 2.4
//    val alphaRctx = 2.5
//
//    val se = SimpleStartEndTags[Cat](STA, END)
//
//    val apm = new TrainDataNormalizingScgAlphaPriorMaker(mockProductionFinder, mockGuideChartProdFinder)
//    val (alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts, alphaPriorLctxCounts, alphaPriorRctxCounts) =
//      apm.makeAll(guideCharts, goldLabeledSentences,
//        priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, priorLctxDist, priorRctxDist,
//        alphaRoot, alphaBiny, alphaUnry, alphaTerm, alphaLctx, alphaRctx)(se)
//
//    assertLogMap(Map[Cat, Double](
//      A -> (2.1 * (0.91 / 5.61) + 11),
//      B -> (2.1 * (0.92 / 5.61) + 12 + 13),
//      C -> (2.1 * (0.93 / 5.61) + 14),
//      D -> (2.1 * (0.94 / 5.61) + 15),
//      E -> (2.1 * (0.95 / 5.61) + 0),
//      F -> (2.1 * (0.96 / 5.61) + 0)),
//      alphaPriorRootCounts, 1e-9)
//    assertEqualsLog(LogDouble(2.1 + 11 + 12 + 13 + 14 + 15), alphaPriorRootCounts.values.sum, 1e-9)
//
//    assertEquals(Set(A, C, D, E), alphaPriorBinyCounts.keySet)
//    assertLogMap(Map[BinaryProd, Double](
//      BinaryProd(B, C) -> (2.2 * (0.11 / 0.36) + 21 + 24),
//      BinaryProd(E, D) -> (2.2 * (0.12 / 0.36) + 0),
//      BinaryProd(E, F) -> (2.2 * (0.13 / 0.36) + 26)),
//      alphaPriorBinyCounts(A), 1e-9)
//    assertEqualsLog(LogDouble(2.2 + 21 + 24 + 26), alphaPriorBinyCounts(A).values.sum, 1e-9)
//    assertLogMap(Map[BinaryProd, Double](
//      BinaryProd(A, D) -> (2.2 * (0.14 / 0.80) + 0),
//      BinaryProd(D, E) -> (2.2 * (0.15 / 0.80) + 22 + 25),
//      BinaryProd(D, F) -> (2.2 * (0.16 / 0.80) + 0),
//      BinaryProd(E, D) -> (2.2 * (0.17 / 0.80) + 0),
//      BinaryProd(E, F) -> (2.2 * (0.18 / 0.80) + 27)),
//      alphaPriorBinyCounts(C), 1e-9)
//    assertEqualsLog(LogDouble(2.2 + 22 + 25 + 27), alphaPriorBinyCounts(C).values.sum, 1e-9)
//    assertLogMap(Map[BinaryProd, Double](
//      BinaryProd(B, C) -> (2.2 * (0.19 / 0.19) + 23)),
//      alphaPriorBinyCounts(D), 1e-9)
//    assertEqualsLog(LogDouble(2.2 + 23), alphaPriorBinyCounts(D).values.sum, 1e-9)
//    assertLogMap(Map[BinaryProd, Double](
//      BinaryProd(D, F) -> (2.2 * (0.21 / 0.43) + 0),
//      BinaryProd(B, C) -> (2.2 * (0.22 / 0.43) + 0)),
//      alphaPriorBinyCounts(E), 1e-9)
//    assertEqualsLog(LogDouble(2.2 + 0), alphaPriorBinyCounts(E).values.sum, 1e-9)
//
//    assertEquals(Set(A, C, D, E), alphaPriorUnryCounts.keySet)
//    assertLogMap(Map[UnaryProd, Double](
//      UnaryProd(B) -> (2.6 * (0.23 / 0.72) + 61 + 64),
//      UnaryProd(D) -> (2.6 * (0.24 / 0.72) + 0),
//      UnaryProd(E) -> (2.6 * (0.25 / 0.72) + 66)),
//      alphaPriorUnryCounts(A), 1e-9)
//    assertEqualsLog(LogDouble(2.6 + 61 + 64 + 66), alphaPriorUnryCounts(A).values.sum, 1e-9)
//    assertLogMap(Map[UnaryProd, Double](
//      UnaryProd(A) -> (2.6 * (0.26 / 0.81) + 0),
//      UnaryProd(D) -> (2.6 * (0.27 / 0.81) + 62 + 65),
//      UnaryProd(E) -> (2.6 * (0.28 / 0.81) + 67)),
//      alphaPriorUnryCounts(C), 1e-9)
//    assertEqualsLog(LogDouble(2.6 + 62 + 65 + 67), alphaPriorUnryCounts(C).values.sum, 1e-9)
//    assertLogMap(Map[UnaryProd, Double](
//      UnaryProd(B) -> (2.6 * (0.42 / 0.42) + 63)),
//      alphaPriorUnryCounts(D), 1e-9)
//    assertEqualsLog(LogDouble(2.6 + 63), alphaPriorUnryCounts(D).values.sum, 1e-9)
//    assertLogMap(Map[UnaryProd, Double](
//      UnaryProd(B) -> (2.6 * (0.43 / 0.87) + 0),
//      UnaryProd(D) -> (2.6 * (0.44 / 0.87) + 0)),
//      alphaPriorUnryCounts(E), 1e-9)
//    assertEqualsLog(LogDouble(2.6 + 0), alphaPriorUnryCounts(E).values.sum, 1e-9)
//
//    assertEquals(Set(A, C, D, E), alphaPriorTermCounts.keySet)
//    assertLogMap(Map(
//      TermProd("a1") -> (2.3 * (0.31 / 0.63) + 31 + 35),
//      TermProd("a2") -> (2.3 * (0.32 / 0.63) + 36)),
//      alphaPriorTermCounts(A), 1e-9)
//    assertEqualsLog(LogDouble(2.3 + 31 + 35 + 36), alphaPriorTermCounts(A).values.sum, 1e-9)
//    assertLogMap(Map(
//      TermProd("c1") -> (2.3 * (0.33 / 1.02) + 32 + 37),
//      TermProd("c2") -> (2.3 * (0.34 / 1.02) + 33),
//      TermProd("c3") -> (2.3 * (0.35 / 1.02) + 38)),
//      alphaPriorTermCounts(C), 1e-9)
//    assertEqualsLog(LogDouble(2.3 + 32 + 33 + 37 + 38), alphaPriorTermCounts(C).values.sum, 1e-9)
//    assertLogMap(Map(
//      TermProd("d2") -> (2.3 * (0.36 / 0.36) + 0)),
//      alphaPriorTermCounts(D), 1e-9)
//    assertEqualsLog(LogDouble(2.3 + 0), alphaPriorTermCounts(D).values.sum, 1e-9)
//    assertLogMap(Map(
//      TermProd("e1") -> (2.3 * (0.37 / 0.37) + 39)),
//      alphaPriorTermCounts(E), 1e-9)
//    assertEqualsLog(LogDouble(2.3 + 39), alphaPriorTermCounts(E).values.sum, 1e-9)
//
//    assertEquals(Set(A, C, D, E, F), alphaPriorLctxCounts.keySet)
//    assertLogMap(Map[Cat, Double](
//      STA -> (2.4 * (0.51 / 2.10) + 32 + 36),
//      A -> (2.4 * (0.52 / 2.10) + 31),
//      B -> (2.4 * (0.53 / 2.10) + 37),
//      D -> (2.4 * (0.54 / 2.10) + 0)),
//      alphaPriorLctxCounts(A), 1e-9)
//    assertEqualsLog(LogDouble(2.4 + 31 + 32 + 36 + 37), alphaPriorLctxCounts(A).values.sum, 1e-9)
//    assertLogMap(Map[Cat, Double](
//      STA -> (2.4 * (0.55 / 2.26) + 38),
//      A -> (2.4 * (0.56 / 2.26) + 0),
//      B -> (2.4 * (0.57 / 2.26) + 33),
//      C -> (2.4 * (0.58 / 2.26) + 34)),
//      alphaPriorLctxCounts(C), 1e-9)
//    assertEqualsLog(LogDouble(2.4 + 33 + 34 + 38), alphaPriorLctxCounts(C).values.sum, 1e-9)
//    assertLogMap(Map[Cat, Double](
//      STA -> (2.4 * (0.61 / 0.61) + 35)),
//      alphaPriorLctxCounts(D), 1e-9)
//    assertEqualsLog(LogDouble(2.4 + 35), alphaPriorLctxCounts(D).values.sum, 1e-9)
//    assertLogMap(Map[Cat, Double](
//      D -> (2.4 * (0.63 / 0.63) + 39)),
//      alphaPriorLctxCounts(E), 1e-9)
//    assertEqualsLog(LogDouble(2.4 + 39), alphaPriorLctxCounts(E).values.sum, 1e-9)
//    assertLogMap(Map[Cat, Double](
//      E -> (2.4 * (0.64 / 0.64) + 0)),
//      alphaPriorLctxCounts(F), 1e-9)
//    assertEqualsLog(LogDouble(2.4 + 0), alphaPriorLctxCounts(F).values.sum, 1e-9)
//
//    assertEquals(Set(A, B, C, D, E, F), alphaPriorRctxCounts.keySet)
//    assertLogMap(Map[Cat, Double](
//      B -> (2.5 * (0.71 / 2.16) + 0),
//      C -> (2.5 * (0.72 / 2.16) + 41),
//      END -> (2.5 * (0.73 / 2.16) + 42 + 46)),
//      alphaPriorRctxCounts(A), 1e-9)
//    assertEqualsLog(LogDouble(2.5 + 41 + 42 + 46), alphaPriorRctxCounts(A).values.sum, 1e-9)
//    assertLogMap(Map[Cat, Double](
//      A -> (2.5 * (0.74 / 1.49) + 0),
//      END -> (2.5 * (0.75 / 1.49) + 0)),
//      alphaPriorRctxCounts(B), 1e-9)
//    assertEqualsLog(LogDouble(2.5 + 0), alphaPriorRctxCounts(B).values.sum, 1e-9)
//    assertLogMap(Map[Cat, Double](
//      B -> (2.5 * (0.76 / 3.91) + 0),
//      D -> (2.5 * (0.77 / 3.91) + 43),
//      E -> (2.5 * (0.78 / 3.91) + 48),
//      F -> (2.5 * (0.79 / 3.91) + 0),
//      END -> (2.5 * (0.81 / 3.91) + 47)),
//      alphaPriorRctxCounts(C), 1e-9)
//    assertEqualsLog(LogDouble(2.5 + 43 + 47 + 48), alphaPriorRctxCounts(C).values.sum, 1e-9)
//    assertLogMap(Map[Cat, Double](
//      C -> (2.5 * (0.82 / 1.65) + 45),
//      END -> (2.5 * (0.83 / 1.65) + 44)),
//      alphaPriorRctxCounts(D), 1e-9)
//    assertEqualsLog(LogDouble(2.5 + 44 + 45), alphaPriorRctxCounts(D).values.sum, 1e-9)
//    assertLogMap(Map[Cat, Double](
//      D -> (2.5 * (0.84 / 0.84) + 49)),
//      alphaPriorRctxCounts(E), 1e-9)
//    assertEqualsLog(LogDouble(2.5 + 49), alphaPriorRctxCounts(E).values.sum, 1e-9)
//    assertLogMap(Map[Cat, Double](
//      E -> (2.5 * (0.85 / 0.85) + 0)),
//      alphaPriorRctxCounts(F), 1e-9)
//    assertEqualsLog(LogDouble(2.5 + 0), alphaPriorRctxCounts(F).values.sum, 1e-9)
//  }

  def assertEqualsLog(a: LogDouble, b: LogDouble, d: Double) {
    assertEquals(a.toDouble, b.toDouble, d)
  }

  def assertLogMap[A](e: Map[A, Double], a: Map[A, LogDouble], d: Double) {
    assertEquals(e.keySet, a.keySet)
    for ((k, ev) <- e) {
      assertEquals(f"k=$k", ev, a(k).toDouble, d)
    }
  }

  def assertLogMapMap[A, B](e: Map[A, Map[B, Double]], a: Map[A, Map[B, LogDouble]], d: Double) {
    assertEquals(e.keySet, a.keySet)
    for ((k1, ev1) <- e) {
      assertEquals(e.keySet, a(k1).keySet)
      for ((k2, ev2) <- ev1) {
        assertEquals(ev2, a(k1)(k2).toDouble, d)
      }
    }
  }
}
