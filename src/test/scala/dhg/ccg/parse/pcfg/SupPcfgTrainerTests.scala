package dhg.ccg.parse.pcfg

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.util._
import dhg.ccg.parse.pcfg.mcmc.PcfgProductionCounter

class SupPcfgTrainerTests {

  val A: Cat = cat"A"
  val B: Cat = cat"B"
  val C: Cat = cat"C"
  val D: Cat = cat"D"
  val E: Cat = cat"E"
  val F: Cat = cat"F"
  val G: Cat = cat"G"
  val H: Cat = cat"H"
  val Z: Cat = cat"Z"

  @Test
  def test_UnsmoothedSupPcfgTrainer_train {
    throw new NotImplementedError("Test not written")
  }

  @Test
  def test_AlphaBetaSupPcfgTrainer {
    type Word = String

    val alphaRoot = 2.1
    val alphaProd = 202.2
    val alphaLctx = 213.3
    val alphaRctx = 224.4

    val s1: CcgTree = CcgLeaf(A, "something", "FAKEPOS")
    val s2: CcgTree = CcgLeaf(B, "fake", "FAKEPOS")

    val mockProductionFinder = new PcfgProductionCounter {
      def rootCounts(t: CcgTree): Map[Cat, Double] = t match {
        case _ if t == s1 => Map(A -> 11, B -> 12)
        case _ if t == s2 => Map(B -> 13, C -> 14, D -> 15)
      }
      def binyCounts(t: CcgTree): Map[Cat, Map[BinaryProd, Double]] = ???
      def unryCounts(t: CcgTree): Map[Cat, Map[UnaryProd, Double]] = ???
      def termCounts(t: CcgTree): Map[Cat, Map[TermProd, Double]] = ???
      def prodCounts(t: CcgTree): Map[Cat, Map[Prod, Double]] = t match {
        case _ if t == s1 => Map(
          A -> Map(BinaryProd(B, C) -> 21, UnaryProd(B) -> 24, TermProd("a1") -> 27),
          C -> Map(BinaryProd(D, E) -> 22, UnaryProd(D) -> 25, TermProd("c1") -> 28, TermProd("c2") -> 29),
          D -> Map(BinaryProd(B, C) -> 23, UnaryProd(B) -> 26))
        case _ if t == s2 => Map(
          A -> Map(BinaryProd(B, C) -> 24, BinaryProd(E, F) -> 26, UnaryProd(B) -> 64, UnaryProd(E) -> 66, TermProd("a1") -> 35, TermProd("a2") -> 36),
          C -> Map(BinaryProd(D, E) -> 25, BinaryProd(E, F) -> 27, UnaryProd(D) -> 65, UnaryProd(E) -> 67, TermProd("c1") -> 37, TermProd("c3") -> 38),
          E -> Map(TermProd("e1") -> 39))
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
        case G => 0.97
        case H => 0.98
        case Z => 0.99
      })
      def sample(): Cat = ???
      def defaultProb: LogDouble = ???
    }
    val priorBinyDist = new ConditionalLogProbabilityDistribution[Cat, BinaryProd] {
      def apply(x: BinaryProd, given: Cat): LogDouble = LogDouble((given, x) match {
        case (A, BinaryProd(B, C)) => 0.11
        case (A, BinaryProd(E, D)) => 0.12
        case (A, BinaryProd(E, F)) => 0.13
        case (A, BinaryProd(Z, Z)) => 0.38

        case (C, BinaryProd(A, D)) => 0.14
        case (C, BinaryProd(D, E)) => 0.15
        case (C, BinaryProd(D, F)) => 0.16
        case (C, BinaryProd(E, D)) => 0.17
        case (C, BinaryProd(E, F)) => 0.18

        case (D, BinaryProd(B, C)) => 0.19

        case (E, BinaryProd(D, F)) => 0.21
        case (E, BinaryProd(B, C)) => 0.22

        case (Z, BinaryProd(Z, Z)) => 0.39
      })
      def sample(given: Cat): BinaryProd = ???
    }
    val priorUnryDist = new ConditionalLogProbabilityDistribution[Cat, UnaryProd] {
      def apply(x: UnaryProd, given: Cat): LogDouble = LogDouble((given, x) match {
        case (A, UnaryProd(B)) => 0.23
        case (A, UnaryProd(D)) => 0.24
        case (A, UnaryProd(E)) => 0.25
        case (A, UnaryProd(Z)) => 0.41

        case (C, UnaryProd(A)) => 0.26
        case (C, UnaryProd(D)) => 0.27
        case (C, UnaryProd(E)) => 0.28

        case (D, UnaryProd(B)) => 0.42

        case (E, UnaryProd(B)) => 0.43
        case (E, UnaryProd(D)) => 0.44

        case (Z, UnaryProd(Z)) => 0.45
      })
      def sample(given: Cat): UnaryProd = ???
    }
    val priorTermDist = new ConditionalLogProbabilityDistribution[Cat, TermProd] {
      def apply(x: TermProd, given: Cat): LogDouble = LogDouble((given, x) match {
        case (A, TermProd("a1")) => 0.31
        case (A, TermProd("a2")) => 0.32
        case (A, TermProd("z")) => 0.46

        case (C, TermProd("c1")) => 0.33
        case (C, TermProd("c2")) => 0.34
        case (C, TermProd("c3")) => 0.35

        case (D, TermProd("d2")) => 0.36

        case (E, TermProd("e1")) => 0.37

        case (Z, TermProd("z")) => 0.47
      })
      def sample(given: Cat): TermProd = ???
    }

    val mockResultingParser = new AbstractKBestGuideChartParser {
      def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int): Vector[(CcgTree, LogDouble)] = ???
    }

    val mockPcfgParserInstantiater = new PcfgParserInstantiater {
      def apply(
        rootDist: LogProbabilityDistribution[Cat],
        prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]) = {

        /* ROOTS
         * A: 11      + (2.1 * 0.91) = 12.911  / 72.77 = 0.17742201456644222
         * B: (12+13) + (2.1 * 0.92) = 26.932  / 72.77 = 0.3700975676789886
         * C: 14      + (2.1 * 0.93) = 15.953  / 72.77 = 0.2192249553387385
         * D: 15      + (2.1 * 0.94) = 16.974  / 72.77 = 0.23325546241583073
         *                             ------
         *                             72.77
         * E: 0       + (2.1 * 0.93) = 1.995   / 72.77 = 0.027415143603133157
         * F: 0       + (2.1 * 0.94) =
         */
        assertEqualsLog(LogDouble((11 + (2.1 * 0.91)) / (2.1 + 65)), rootDist(A), 1e-9)
        assertEqualsLog(LogDouble((25 + (2.1 * 0.92)) / (2.1 + 65)), rootDist(B), 1e-9)
        assertEqualsLog(LogDouble((14 + (2.1 * 0.93)) / (2.1 + 65)), rootDist(C), 1e-9)
        assertEqualsLog(LogDouble((15 + (2.1 * 0.94)) / (2.1 + 65)), rootDist(D), 1e-9)
        assertEqualsLog(LogDouble((0 + (2.1 * 0.95)) / (2.1 + 65)), rootDist(E), 1e-9)
        assertEqualsLog(LogDouble((0 + (2.1 * 0.96)) / (2.1 + 65)), rootDist(F), 1e-9)
        assertEqualsLog(LogDouble((0 + (2.1 * 0.97)) / (2.1 + 65)), rootDist(G), 1e-9)
        assertEqualsLog(LogDouble((0 + (2.1 * 0.98)) / (2.1 + 65)), rootDist(H), 1e-9)
        assertEqualsLog(LogDouble((0 + (2.1 * 0.99)) / (2.1 + 65)), rootDist(Z), 1e-9)

        /* PRODS
         * A -> BC  45 + (202.2 * 0.11*0.5) =  56.1210  / 401.858 = 
         * A -> EF  26 + (202.2 * 0.13*0.5) =  39.1430  / 401.858 = 
         * A -> B   88 + (202.2 * 0.23*0.3) = 101.9518  / 401.858 = 
         * A -> E   66 + (202.2 * 0.25*0.3) =  81.1650  / 401.858 = 
         * A -> a1  62 + (202.2 * 0.31*0.2) =  74.5364  / 401.858 = 
         * A -> a2  36 + (202.2 * 0.32*0.2) =  48.9408  / 401.858 = 
         *                                    --------
         *                                     401.858
         * A -> ED   0 + (202.2 * 0.12*0.5) = 
         * A -> D    0 + (202.2 * 0.24*0.3) =
         * 
         * 
         * C -> DE  47 + (202.2 * 0.15*0.5) =  62.1650  / 470.9748 = 
         * C -> EF  27 + (202.2 * 0.18*0.5) =  45.1980  / 470.9748 = 
         * C -> D   90 + (202.2 * 0.27*0.3) = 106.3782  / 470.9748 = 
         * C -> E   67 + (202.2 * 0.28*0.3) =  83.9848  / 470.9748 = 
         * C -> c1  65 + (202.2 * 0.33*0.2) =  78.3452  / 470.9748 = 
         * C -> c2  29 + (202.2 * 0.34*0.2) =  42.7496  / 470.9748 = 
         * C -> c3  38 + (202.2 * 0.35*0.2) =  52.1540  / 470.9748 = 
         *                                    --------
         *                                    470.9748
         * C -> AD   0 + (202.2 * 0.14*0.5) =  14.1540  / 470.9748 = 
         * C -> DF   0 + (202.2 * 0.16*0.5) =  16.1760  / 470.9748 = 
         * C -> ED   0 + (202.2 * 0.17*0.5) =  17.1870  / 470.9748 = 
         * C -> A    0 + (202.2 * 0.26*0.3) =  15.7716  / 470.9748 = 
         * 
         * 
         * D -> BC  23 + (202.2 * 0.19*0.5) =  42.2090  / 93.6862 =
         * D -> B   26 + (202.2 * 0.42*0.3) =  51.4772  / 93.6862 =
         *                                     -------
         *                                     93.6862
         * D -> d2   0 + (202.2 * 0.36*0.2) =  14.5584  / 93.6862 = 0.2692589386776749
         *
         * 
         * E -> e1  39 + (202.2 * 0.37*0.2) =  53.9628  / 53.9628 = 1.0
         *                                     -------
         *                                     53.9628
         * E -> DF   0 + (202.2 * 0.21*0.5) =  21.2310  / 53.9628 = 
         * E -> BC   0 + (202.2 * 0.22*0.5) =  22.2420  / 53.9628 = 
         * E -> B    0 + (202.2 * 0.43*0.3) =  26.0838  / 53.9628 = 
         * E -> D    0 + (202.2 * 0.44*0.3) =  26.6904  / 53.9628 = 
         */

        assertEqualsLog(LogDouble((45 + (202.2 * 0.11 * 0.5)) / (202.2 + 323)), prodDist(BinaryProd(B, C), A), 1e-9)
        assertEqualsLog(LogDouble((26 + (202.2 * 0.13 * 0.5)) / (202.2 + 323)), prodDist(BinaryProd(E, F), A), 1e-9)
        assertEqualsLog(LogDouble((88 + (202.2 * 0.23 * 0.3)) / (202.2 + 323)), prodDist(UnaryProd(B), A), 1e-9)
        assertEqualsLog(LogDouble((66 + (202.2 * 0.25 * 0.3)) / (202.2 + 323)), prodDist(UnaryProd(E), A), 1e-9)
        assertEqualsLog(LogDouble((62 + (202.2 * 0.31 * 0.2)) / (202.2 + 323)), prodDist(TermProd("a1"), A), 1e-9)
        assertEqualsLog(LogDouble((36 + (202.2 * 0.32 * 0.2)) / (202.2 + 323)), prodDist(TermProd("a2"), A), 1e-9)
        assertEqualsLog(LogDouble((0 + (202.2 * 0.12 * 0.5)) / (202.2 + 323)), prodDist(BinaryProd(E, D), A), 1e-9)
        assertEqualsLog(LogDouble((0 + (202.2 * 0.24 * 0.3)) / (202.2 + 323)), prodDist(UnaryProd(D), A), 1e-9)
        assertEqualsLog(LogDouble((0 + (202.2 * 0.38 * 0.5)) / (202.2 + 323)), prodDist(BinaryProd(Z, Z), A), 1e-9)
        assertEqualsLog(LogDouble((0 + (202.2 * 0.41 * 0.3)) / (202.2 + 323)), prodDist(UnaryProd(Z), A), 1e-9)
        assertEqualsLog(LogDouble((0 + (202.2 * 0.46 * 0.2)) / (202.2 + 323)), prodDist(TermProd("z"), A), 1e-9)

        assertEqualsLog(LogDouble((47 + (202.2 * 0.15 * 0.5)) / (202.2 + 363)), prodDist(BinaryProd(D, E), C), 1e-9)
        assertEqualsLog(LogDouble((27 + (202.2 * 0.18 * 0.5)) / (202.2 + 363)), prodDist(BinaryProd(E, F), C), 1e-9)
        assertEqualsLog(LogDouble((90 + (202.2 * 0.27 * 0.3)) / (202.2 + 363)), prodDist(UnaryProd(D), C), 1e-9)
        assertEqualsLog(LogDouble((67 + (202.2 * 0.28 * 0.3)) / (202.2 + 363)), prodDist(UnaryProd(E), C), 1e-9)
        assertEqualsLog(LogDouble((65 + (202.2 * 0.33 * 0.2)) / (202.2 + 363)), prodDist(TermProd("c1"), C), 1e-9)
        assertEqualsLog(LogDouble((29 + (202.2 * 0.34 * 0.2)) / (202.2 + 363)), prodDist(TermProd("c2"), C), 1e-9)
        assertEqualsLog(LogDouble((38 + (202.2 * 0.35 * 0.2)) / (202.2 + 363)), prodDist(TermProd("c3"), C), 1e-9)
        assertEqualsLog(LogDouble((0 + (202.2 * 0.14 * 0.5)) / (202.2 + 363)), prodDist(BinaryProd(A, D), C), 1e-9)
        assertEqualsLog(LogDouble((0 + (202.2 * 0.16 * 0.5)) / (202.2 + 363)), prodDist(BinaryProd(D, F), C), 1e-9)
        assertEqualsLog(LogDouble((0 + (202.2 * 0.17 * 0.5)) / (202.2 + 363)), prodDist(BinaryProd(E, D), C), 1e-9)
        assertEqualsLog(LogDouble((0 + (202.2 * 0.26 * 0.3)) / (202.2 + 363)), prodDist(UnaryProd(A), C), 1e-9)

        assertEqualsLog(LogDouble((23 + (202.2 * 0.19 * 0.5)) / (202.2 + 49)), prodDist(BinaryProd(B, C), D), 1e-9)
        assertEqualsLog(LogDouble((26 + (202.2 * 0.42 * 0.3)) / (202.2 + 49)), prodDist(UnaryProd(B), D), 1e-9)
        assertEqualsLog(LogDouble((0 + (202.2 * 0.36 * 0.2)) / (202.2 + 49)), prodDist(TermProd("d2"), D), 1e-9)

        assertEqualsLog(LogDouble((39 + (202.2 * 0.37 * 0.2)) / (202.2 + 39)), prodDist(TermProd("e1"), E), 1e-9)
        assertEqualsLog(LogDouble((0 + (202.2 * 0.21 * 0.5)) / (202.2 + 39)), prodDist(BinaryProd(D, F), E), 1e-9)
        assertEqualsLog(LogDouble((0 + (202.2 * 0.22 * 0.5)) / (202.2 + 39)), prodDist(BinaryProd(B, C), E), 1e-9)
        assertEqualsLog(LogDouble((0 + (202.2 * 0.43 * 0.3)) / (202.2 + 39)), prodDist(UnaryProd(B), E), 1e-9)
        assertEqualsLog(LogDouble((0 + (202.2 * 0.44 * 0.3)) / (202.2 + 39)), prodDist(UnaryProd(D), E), 1e-9)

        assertEqualsLog(LogDouble(0.39 * 0.5), prodDist(BinaryProd(Z, Z), Z), 1e-9)
        assertEqualsLog(LogDouble(0.45 * 0.3), prodDist(UnaryProd(Z), Z), 1e-9)
        assertEqualsLog(LogDouble(0.47 * 0.2), prodDist(TermProd("z"), Z), 1e-9)

        mockResultingParser
      }
    }

    val sampledTrees = Vector[CcgTree](s1, s2)

    val absct = new AlphaBetaSupPcfgTrainer(
      priorRootDist, priorBinyDist, priorUnryDist, priorTermDist,
      alphaRoot, alphaProd,
      priorBinyProdMix = 0.5, priorUnryProdMix = 0.3, priorTermProdMix = 0.2,
      mockProductionFinder,
      mockPcfgParserInstantiater)

    val parser: GuideChartParser = absct.train(sampledTrees)

    assertSame(mockResultingParser, parser)
  }

  def assertEqualsLog(a: LogDouble, b: LogDouble, e: Double) {
    assertEquals(a.toDouble, b.toDouble, e)
  }
}
