package dhg.ccg.parse.scg

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
import dhg.ccg.parse.scg.mcmc._

class AlphaBetaSupScgTrainerTests {

  //  val S = AtomCat("S")
  //  val NP = AtomCat("NP")
  //  val N = AtomCat("N")
  //  val PP = AtomCat("PP")
  val STA: Cat = cat"<S>"
  val END: Cat = cat"<E>"

  val A = cat"A".asInstanceOf[AtomCat]
  val B = cat"B".asInstanceOf[AtomCat]
  val C = cat"C".asInstanceOf[AtomCat]
  val D = cat"D".asInstanceOf[AtomCat]
  val E = cat"E".asInstanceOf[AtomCat]
  val F = cat"F".asInstanceOf[AtomCat]
  val G = cat"G".asInstanceOf[AtomCat]
  val H = cat"H".asInstanceOf[AtomCat]
  val Z = cat"Z".asInstanceOf[AtomCat]

  @Test
  def test_AlphaBetaSupScgTrainer_train {
    type Word = String
    val se = SimpleStartEndTags[Cat](STA, END)

    val alphaRoot = 2.1
    val alphaProd = 202.2
    val alphaLctx = 213.3
    val alphaRctx = 224.4

    val s1: CcgTree = CcgLeaf(A, "something", "FAKEPOS")
    val s2: CcgTree = CcgLeaf(B, "fake", "FAKEPOS")

    val mockProductionFinder = new ScgProductionFinder {
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
    val priorLctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
      def apply(left: Cat, given: Cat): LogDouble = LogDouble((given, left) match {
        case (A, STA) => 0.51
        case (A, A) => 0.52
        case (A, B) => 0.53
        case (A, D) => 0.54
        case (A, Z) => 0.65

        case (C, STA) => 0.55
        case (C, A) => 0.56
        case (C, B) => 0.57
        case (C, C) => 0.58

        case (D, STA) => 0.61

        case (E, D) => 0.63

        case (F, E) => 0.64

        case (Z, Z) => 0.66
      })
      def sample(given: Cat): Cat = ???
    }
    val priorRctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
      def apply(right: Cat, given: Cat): LogDouble = LogDouble((given, right) match {
        case (A, B) => 0.71
        case (A, C) => 0.72
        case (A, END) => 0.73
        case (A, Z) => 0.86

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

        case (Z, Z) => 0.87
      })
      def sample(given: Cat): Cat = ???
    }

    val mockResultingParser = new AbstractKBestGuideChartParser {
      def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int): Vector[(CcgTree, LogDouble)] = ???
    }

    val mockScgParserInstantiater = new ScgParserInstantiater {
      def apply(
        rootDist: LogProbabilityDistribution[Cat],
        prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
        lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
        rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]) = {

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

        /* LEFT CONTEXTS
         * A -> <S>  68 + (213.3 * 0.51) = 176.783  / 468.748 = 0.37713867579168336
         * A -> A    31 + (213.3 * 0.52) = 141.916  / 468.748 = 0.30275542508981373
         * A -> B    37 + (213.3 * 0.53) = 150.049  / 468.748 = 0.3201058991185029
         *                                 -------
         *                                 468.748
         * A -> D     0 + (213.3 * 0.54) = 115.182  / 468.748 = 0.24572264841663324
         * A -> Z     0 + (213.3 * 0.55) = 117.315  / 468.748 = 0.25027306783175607
         * 
         * 
         * C -> <S>  38 + (213.3 * 0.55) = 155.315  / 467.610 = 0.37713867579168336
         * C -> B    33 + (213.3 * 0.57) = 154.581  / 467.610 = 0.3201058991185029
         * C -> C    34 + (213.3 * 0.58) = 157.714  / 467.610 = 0.24572264841663324
         *                                 -------
         *                                 467.610
         * C -> A     0 + (213.3 * 0.56) = 119.448  / 467.610 = 0.30275542508981373
         * 
         * 
         * D -> <S>  35 + (213.3 * 0.61) = 165.113  / 165.113 = 
         *                                 -------
         *                                 165.113
         *                                 
         * E -> D    39 + (213.3 * 0.63) = 173.379  / 173.379 = 
         *                                 -------
         *                                 173.379
				 */
        assertEqualsLog(LogDouble((68 + (213.3 * 0.51)) / (213.3 + 136)), lctxDist(STA, A), 1e-9)
        assertEqualsLog(LogDouble((31 + (213.3 * 0.52)) / (213.3 + 136)), lctxDist(A, A), 1e-9)
        assertEqualsLog(LogDouble((37 + (213.3 * 0.53)) / (213.3 + 136)), lctxDist(B, A), 1e-9)
        assertEqualsLog(LogDouble((0 + (213.3 * 0.54)) / (213.3 + 136)), lctxDist(D, A), 1e-9)
        assertEqualsLog(LogDouble((0 + (213.3 * 0.65)) / (213.3 + 136)), lctxDist(Z, A), 1e-9)

        assertEqualsLog(LogDouble((38 + (213.3 * 0.55)) / (213.3 + 105)), lctxDist(STA, C), 1e-9)
        assertEqualsLog(LogDouble((0 + (213.3 * 0.56)) / (213.3 + 105)), lctxDist(A, C), 1e-9)
        assertEqualsLog(LogDouble((33 + (213.3 * 0.57)) / (213.3 + 105)), lctxDist(B, C), 1e-9)
        assertEqualsLog(LogDouble((34 + (213.3 * 0.58)) / (213.3 + 105)), lctxDist(C, C), 1e-9)

        assertEqualsLog(LogDouble((35 + (213.3 * 0.61)) / (213.3 + 35)), lctxDist(STA, D), 1e-9)

        assertEqualsLog(LogDouble((39 + (213.3 * 0.63)) / (213.3 + 39)), lctxDist(D, E), 1e-9)

        assertEqualsLog(LogDouble(0.64), lctxDist(E, F), 1e-9)

        assertEqualsLog(LogDouble(0.66), lctxDist(Z, Z), 1e-9)

        /* RIGHT CONTEXTS
         * A -> <E>  88 + (224.4 * 0.73) = 251.812  / 454.38 = 0.5541881244773098
         * A -> C    41 + (224.4 * 0.72) = 202.568  / 454.38 = 0.4458118755226903
         *                                 -------
         *                                 454.38
         * A -> B     0 + (224.4 * 0.71) = 159.324  / 454.38 = 0.35064043311765486
         * A -> Z     0 + (224.4 * 0.86) = 192.984  / 454.38 = 0.42471939786082136
         * 
         * 
         * B -> A    0 + (224.4 * 0.74) = 202.568  / 454.38 = 0.4458118755226903
         * B -> <E>  0 + (224.4 * 0.75) = 251.812  / 454.38 = 0.5541881244773098
         * 
         * 
         * C -> D    43 + (224.4 * 0.77) = 215.788  / 667.584 = 
         * C -> E    48 + (224.4 * 0.78) = 223.032  / 667.584 = 
         * C -> <E>  47 + (224.4 * 0.81) = 228.764  / 667.584 = 
         *                                 -------
         *                                 667.584
         * C -> B     0 + (224.4 * 0.76) = 170.544  / 454.38 = 
         * C -> F     0 + (224.4 * 0.79) = 177.276  / 454.38 = 
         *
         * 
         * D -> C    45 + (224.4 * 0.82) = 229.008  / 459.26 = 0.4458118755226903
         * D -> <E>  44 + (224.4 * 0.83) = 230.252  / 459.26 = 0.5541881244773098
         *                                 -------
         *                                 459.26
         *  
         */
        assertEqualsLog(LogDouble((88 + (224.4 * 0.73)) / (224.4 + 129)), rctxDist(END, A), 1e-9)
        assertEqualsLog(LogDouble((41 + (224.4 * 0.72)) / (224.4 + 129)), rctxDist(C, A), 1e-9)
        assertEqualsLog(LogDouble((0 + (224.4 * 0.71)) / (224.4 + 129)), rctxDist(B, A), 1e-9)
        assertEqualsLog(LogDouble((0 + (224.4 * 0.86)) / (224.4 + 129)), rctxDist(Z, A), 1e-9)

        assertEqualsLog(LogDouble(0.74), rctxDist(A, B), 1e-9)
        assertEqualsLog(LogDouble(0.75), rctxDist(END, B), 1e-9)

        assertEqualsLog(LogDouble((43 + (224.4 * 0.77)) / (224.4 + 138)), rctxDist(D, C), 1e-9)
        assertEqualsLog(LogDouble((48 + (224.4 * 0.78)) / (224.4 + 138)), rctxDist(E, C), 1e-9)
        assertEqualsLog(LogDouble((47 + (224.4 * 0.81)) / (224.4 + 138)), rctxDist(END, C), 1e-9)
        assertEqualsLog(LogDouble((0 + (224.4 * 0.76)) / (224.4 + 138)), rctxDist(B, C), 1e-9)
        assertEqualsLog(LogDouble((0 + (224.4 * 0.79)) / (224.4 + 138)), rctxDist(F, C), 1e-9)

        assertEqualsLog(LogDouble((45 + (224.4 * 0.82)) / (224.4 + 89)), rctxDist(C, D), 1e-9)
        assertEqualsLog(LogDouble((44 + (224.4 * 0.83)) / (224.4 + 89)), rctxDist(END, D), 1e-9)

        assertEqualsLog(LogDouble((49 + (224.4 * 0.84)) / (224.4 + 49)), rctxDist(D, E), 1e-9)

        assertEqualsLog(LogDouble(0.85), rctxDist(E, F), 1e-9)

        assertEqualsLog(LogDouble(0.87), rctxDist(Z, Z), 1e-9)

        mockResultingParser
      }
    }

    val sampledTrees = Vector[CcgTree](s1, s2)

    val absct = new AlphaBetaSupScgTrainer(
      priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, priorLctxDist, priorRctxDist,
      alphaRoot, alphaProd, alphaLctx, alphaRctx,
      priorBinyProdMix = 0.5, priorUnryProdMix = 0.3, priorTermProdMix = 0.2,
      mockProductionFinder,
      mockScgParserInstantiater)(se)

    val parser: GuideChartParser = absct.train(sampledTrees)

    assertSame(mockResultingParser, parser)
  }

  def assertEqualsLog(a: LogDouble, b: LogDouble, e: Double) {
    assertEquals(a.toDouble, b.toDouble, e)
  }
}
