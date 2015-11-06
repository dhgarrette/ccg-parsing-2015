package dhg.ccg.parse.scg.mcmc

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.test.TestUtil._
import dhg.ccg.parse.pcfg._
import dhg.util._
import org.apache.commons.math3.random.MersenneTwister
import dhg.ccg.parse.scg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.tagdict.SimpleStartEndTags
import org.apache.commons.math3.random.RandomGenerator
import dhg.ccg.tagdict.StartEndTags
import scala.collection.parallel.immutable.ParVector
import dhg.util._
import dhg.ccg.math._
import dhg.ccg.parse.scg._
import scalaz._
import Scalaz._

class McmcScgTests {

  val A: AtomCat = cat"A".asInstanceOf[AtomCat]
  val B: AtomCat = cat"B".asInstanceOf[AtomCat]
  val C: AtomCat = cat"C".asInstanceOf[AtomCat]
  val D: AtomCat = cat"D".asInstanceOf[AtomCat]
  val E: AtomCat = cat"E".asInstanceOf[AtomCat]
  val F: AtomCat = cat"F".asInstanceOf[AtomCat]
  val X: AtomCat = cat"X".asInstanceOf[AtomCat]

  val S: AtomCat = cat"S".asInstanceOf[AtomCat]
  val NP: AtomCat = cat"NP".asInstanceOf[AtomCat]
  val N: AtomCat = cat"N".asInstanceOf[AtomCat]
  val PP: AtomCat = cat"PP".asInstanceOf[AtomCat]
  val STA = cat"<S>"
  val END = cat"<E>"
  val SE = new SimpleStartEndTags(STA: Cat, END: Cat)

  val s: AtomCat = cat"S".asInstanceOf[AtomCat]
  val np: AtomCat = cat"NP".asInstanceOf[AtomCat]
  val n: AtomCat = cat"N".asInstanceOf[AtomCat]
  val pp: AtomCat = cat"PP".asInstanceOf[AtomCat]

  val startWord = "<S>"
  val startTag = STA
  val endWord = "<E>"
  val endTag = END

  @Test
  def test_McmcScg_trainGuideChartsWithSomeGold {
    type Word = String
    type Tag = Cat

    val GC1 = CfgGuideChart(Vector.empty, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](Vector(Map(cat"gc1" -> Set.empty))))
    val GC2 = CfgGuideChart(Vector.empty, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](Vector(Map(cat"gc2" -> Set.empty))))
    val GC3 = CfgGuideChart(Vector.empty, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](Vector(Map(cat"gc3" -> Set.empty))))
    val mockGuideCharts = Vector(GC1, GC2, GC3)

    val InitialT1 = CcgLeaf(N, "InitialT1", "FAKEPOS")
    val InitialT2 = CcgLeaf(N, "InitialT2", "FAKEPOS")
    val InitialT3 = CcgLeaf(N, "InitialT3", "FAKEPOS")
    val InitialT1p = InitialT1 -> LogDouble(0.001)
    val InitialT2p = InitialT2 -> LogDouble(0.001)
    val InitialT3p = InitialT3 -> LogDouble(0.001)
    val T1a = CcgLeaf(N, "T1a", "FAKEPOS")
    val T2a = CcgLeaf(N, "T2a", "FAKEPOS")
    val T3a = CcgLeaf(N, "T3a", "FAKEPOS")
    val T1b = CcgLeaf(N, "T1b", "FAKEPOS")
    val T2b = CcgLeaf(N, "T2dup", "FAKEPOS")
    val T3b = CcgLeaf(N, "T3b", "FAKEPOS")
    val T1c = CcgLeaf(N, "T1c", "FAKEPOS")
    val T2c = CcgLeaf(N, "T2dup", "FAKEPOS")
    val T3c = CcgLeaf(N, "T3c", "FAKEPOS")
    val T1d = CcgLeaf(N, "T1dup", "FAKEPOS")
    val T2d = CcgLeaf(N, "T2dup", "FAKEPOS")
    val T3d = CcgLeaf(N, "T3d", "FAKEPOS")
    val T1e = CcgLeaf(N, "T1dup", "FAKEPOS")
    val T2e = CcgLeaf(N, "T2e", "FAKEPOS")
    val T3e = CcgLeaf(N, "T3e", "FAKEPOS")

    val mockGoldTrees = Vector(CcgLeaf(N, "goldLabeledSentence1", "FAKEPOS"))

    val priorRootDist = new LogProbabilityDistribution[Cat] { def apply(b: Cat) = ???; def sample() = ???; def defaultProb = ??? }
    val priorBinyDist = new ConditionalLogProbabilityDistribution[Cat, BinaryProd] { def apply(x: BinaryProd, given: Cat) = ???; def sample(given: Cat) = ??? }
    val priorUnryDist = new ConditionalLogProbabilityDistribution[Cat, UnaryProd] { def apply(x: UnaryProd, given: Cat) = ???; def sample(given: Cat) = ??? }
    val priorTermDist = new ConditionalLogProbabilityDistribution[Cat, TermProd] { def apply(x: TermProd, given: Cat) = ???; def sample(given: Cat) = ??? }
    val priorLctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] { def apply(x: Cat, given: Cat) = ???; def sample(given: Cat) = ??? }
    val priorRctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] { def apply(x: Cat, given: Cat) = ???; def sample(given: Cat) = ??? }

    val mockAlphaRoot = 1.1
    val mockAlphaBiny = 1.2
    val mockAlphaUnry = 1.3
    val mockAlphaTerm = 1.4
    val mockAlphaProd = 1.5
    val mockAlphaLctx = 1.6
    val mockAlphaRctx = 1.7

    val mockAlphaPriorRoot = Map[Cat, LogDouble](
      A -> LogDouble(11.1),
      B -> LogDouble(12.2),
      C -> LogDouble(13.3))
    val mockAlphaPriorBiny = Map(cat"mockAlphaPriorBiny" -> Map.empty[BinaryProd, LogDouble])
    val mockAlphaPriorUnry = Map(cat"mockAlphaPriorUnry" -> Map.empty[UnaryProd, LogDouble])
    val mockAlphaPriorTerm = Map(cat"mockAlphaPriorTerm" -> Map.empty[TermProd, LogDouble])
    val mockAlphaPriorLctx = Map(cat"mockAlphaPriorLctx" -> Map.empty[Cat, LogDouble])
    val mockAlphaPriorRctx = Map(cat"mockAlphaPriorRctx" -> Map.empty[Cat, LogDouble])

    val RD1 = Map(cat"RD1" -> LogDouble(0.1))
    val RD2 = Map(cat"RD2" -> LogDouble(0.1))
    val RD3 = Map(cat"RD2" -> LogDouble(0.1))
    val RD4 = Map(cat"RD2" -> LogDouble(0.1))
    val RD5 = Map(cat"RD2" -> LogDouble(0.1))

    //    val priorRootDist = new LogProbabilityDistribution[Cat] {
    //      def apply(b: Cat): LogDouble = LogDouble(b match {
    //        case A => 0.5
    //        case B => 0.3
    //        case C => 0.2
    //      }); def sample(): Cat = ???; def defaultProb: LogDouble = ???
    //    }
    //    val priorBinyDist = new ConditionalLogProbabilityDistribution[Cat, BinaryProd] {
    //      def apply(x: BinaryProd, given: Cat): LogDouble = LogDouble((given, x) match {
    //        case (A, BinaryProd(B, C)) => 0.21
    //        case (C, BinaryProd(D, E)) => 0.23
    //      }); def sample(given: Cat): BinaryProd = ???
    //    }
    //    val priorUnryDist = new ConditionalLogProbabilityDistribution[Cat, UnaryProd] {
    //      def apply(x: UnaryProd, given: Cat): LogDouble = LogDouble((given, x) match {
    //        case (A, UnaryProd(B)) => 0.21
    //      }); def sample(given: Cat): UnaryProd = ???
    //    }
    //    val priorTermDist = new ConditionalLogProbabilityDistribution[Cat, TermProd] {
    //      def apply(x: TermProd, given: Cat): LogDouble = LogDouble((given, x) match {
    //        case (B, TermProd("b")) => 0.22
    //        case (D, TermProd("d")) => 0.24
    //        case (E, TermProd("e")) => 0.25
    //      }); def sample(given: Cat): TermProd = ???
    //    }
    //    val priorLctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
    //      def apply(left: Cat, given: Cat): LogDouble = LogDouble((left, given) match {
    //        case (STA, A) => 0.31
    //        case (STA, B) => 0.33
    //        case (B, C) => 0.32
    //        case (B, D) => 0.34
    //        case (D, E) => 0.35
    //      }); def sample(given: Cat): Cat = ???
    //    }
    //    val priorRctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
    //      def apply(right: Cat, given: Cat): LogDouble = LogDouble((given, right) match {
    //        case (A, END) => 0.41
    //        case (B, D) => 0.42
    //        case (C, END) => 0.43
    //        case (D, E) => 0.44
    //        case (E, END) => 0.45
    //      }); def sample(given: Cat): Cat = ???
    //    }

    val mockScgProductionFinder = new ScgProductionFinder {
      def rootCounts(t: CcgTree): Map[Cat, Double] = t match {
        case T1a => Map(A -> 1.0)
        case T2a => Map(A -> 1.0)
        case T3a => Map(B -> 1.0)

        case T1b => Map(A -> 1.0)
        case T2b => Map(A -> 1.0) //
        case T3b => Map(C -> 1.0)

        case T1c => Map(B -> 1.0)
        case T2c => Map(A -> 1.0) //
        case T3c => Map(A -> 1.0)

        case T1d => Map(A -> 1.0) //
        case T2d => Map(A -> 1.0) //
        case T3d => Map(A -> 1.0)

        case T1e => Map(A -> 1.0) //
        case T2e => Map(C -> 1.0)
        case T3e => Map(B -> 1.0)
      }
      def binyCounts(t: CcgTree): Map[Cat, Map[BinaryProd, Double]] = t match {
        case T1a => Map.empty
        case T2a => Map.empty
        case T3a => Map.empty

        case T1b => Map.empty
        case T2b => Map.empty
        case T3b => Map.empty

        case T1c => Map.empty
        case T2c => Map.empty
        case T3c => Map.empty

        case T1d => Map.empty
        case T2d => Map.empty
        case T3d => Map.empty

        case T1e => Map.empty
        case T2e => Map.empty
        case T3e => Map.empty
      }
      def unryCounts(t: CcgTree): Map[Cat, Map[UnaryProd, Double]] = t match {
        case T1a => Map.empty
        case T2a => Map.empty
        case T3a => Map.empty

        case T1b => Map.empty
        case T2b => Map.empty
        case T3b => Map.empty

        case T1c => Map.empty
        case T2c => Map.empty
        case T3c => Map.empty

        case T1d => Map.empty
        case T2d => Map.empty
        case T3d => Map.empty

        case T1e => Map.empty
        case T2e => Map.empty
        case T3e => Map.empty
      }
      def termCounts(t: CcgTree): Map[Cat, Map[TermProd, Double]] = t match {
        case T1a => Map.empty
        case T2a => Map.empty
        case T3a => Map.empty

        case T1b => Map.empty
        case T2b => Map.empty
        case T3b => Map.empty

        case T1c => Map.empty
        case T2c => Map.empty
        case T3c => Map.empty

        case T1d => Map.empty
        case T2d => Map.empty
        case T3d => Map.empty

        case T1e => Map.empty
        case T2e => Map.empty
        case T3e => Map.empty
      }
      def prodCounts(t: CcgTree): Map[Cat, Map[Prod, Double]] = t match {
        case T1a => Map.empty
        case T2a => Map.empty
        case T3a => Map.empty

        case T1b => Map.empty
        case T2b => Map.empty
        case T3b => Map.empty

        case T1c => Map.empty
        case T2c => Map.empty
        case T3c => Map.empty

        case T1d => Map.empty
        case T2d => Map.empty
        case T3d => Map.empty

        case T1e => Map.empty
        case T2e => Map.empty
        case T3e => Map.empty
      }
      def lctxCounts(t: CcgTree)(se: StartEndTags[Cat]): Map[Cat, Map[Cat, Double]] = t match {
        case T1a => Map.empty
        case T2a => Map.empty
        case T3a => Map.empty

        case T1b => Map.empty
        case T2b => Map.empty
        case T3b => Map.empty

        case T1c => Map.empty
        case T2c => Map.empty
        case T3c => Map.empty

        case T1d => Map.empty
        case T2d => Map.empty
        case T3d => Map.empty

        case T1e => Map.empty
        case T2e => Map.empty
        case T3e => Map.empty
      }
      def rctxCounts(t: CcgTree)(se: StartEndTags[Cat]): Map[Cat, Map[Cat, Double]] = t match {
        case T1a => Map.empty
        case T2a => Map.empty
        case T3a => Map.empty

        case T1b => Map.empty
        case T2b => Map.empty
        case T3b => Map.empty

        case T1c => Map.empty
        case T2c => Map.empty
        case T3c => Map.empty

        case T1d => Map.empty
        case T2d => Map.empty
        case T3d => Map.empty

        case T1e => Map.empty
        case T2e => Map.empty
        case T3e => Map.empty
      }
    }

    val mockScgAlphaPriorMaker = new ScgAlphaPriorMaker {
      def makeAll(guideCharts: Vector[CfgGuideChart], goldTrees: Vector[CcgTree],
        priorRootDist: LogProbabilityDistribution[Cat],
        priorBinyDist: ConditionalLogProbabilityDistribution[Cat, BinaryProd],
        priorUnryDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd],
        priorTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
        priorLctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
        priorRctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
        alphaRoot: Double, alphaBiny: Double, alphaUnry: Double, alphaTerm: Double, alphaLctx: Double, alphaRctx: Double)(se: StartEndTags[Cat]): //
        (Map[Cat, LogDouble], Map[Cat, Map[BinaryProd, LogDouble]], Map[Cat, Map[UnaryProd, LogDouble]], Map[Cat, Map[TermProd, LogDouble]], Map[Cat, Map[Cat, LogDouble]], Map[Cat, Map[Cat, LogDouble]]) = {

        assertSame(mockGuideCharts, guideCharts)
        assertSame(mockGoldTrees, goldTrees)

        assertSame(priorRootDist, priorRootDist)
        assertSame(priorBinyDist, priorBinyDist)
        assertSame(priorUnryDist, priorUnryDist)
        assertSame(priorTermDist, priorTermDist)
        assertSame(priorLctxDist, priorLctxDist)
        assertSame(priorRctxDist, priorRctxDist)

        assertEquals(mockAlphaRoot, alphaRoot, 1e-9)
        assertEquals(mockAlphaBiny, alphaBiny, 1e-9)
        assertEquals(mockAlphaUnry, alphaUnry, 1e-9)
        assertEquals(mockAlphaTerm, alphaTerm, 1e-9)
        assertEquals(mockAlphaLctx, alphaLctx, 1e-9)
        assertEquals(mockAlphaRctx, alphaRctx, 1e-9)
        assertSame(SE, se)

        (
          mockAlphaPriorRoot,
          mockAlphaPriorBiny,
          mockAlphaPriorUnry,
          mockAlphaPriorTerm,
          mockAlphaPriorLctx,
          mockAlphaPriorRctx)
      }
    }

    val mockMcmcScgResampler = new McmcScgResampler {
      def resample(
        guideCharts: Vector[CfgGuideChart],
        currentTrees: Vector[CcgTree],
        rootDist: LogProbabilityDistribution[Cat],
        prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
        lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
        rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]) = {
        assertSame(mockGuideCharts, guideCharts)
        assertSame(SE, se)
        currentTrees match {
          case Vector(InitialT1, InitialT2, InitialT3) =>
            assertEqualsLog(LogDouble(11.1 / 36.6), rootDist(A), 1e-9)
            assertEqualsLog(LogDouble(12.2 / 36.6), rootDist(B), 1e-9)
            assertEqualsLog(LogDouble(13.3 / 36.6), rootDist(C), 1e-9)
            //  assertEqualsLog(???, prodDist(???, ???), 1e-9) // TODO: THIS
            //  assertEqualsLog(???, lctxDist(???, ???), 1e-9) // TODO: THIS
            //  assertEqualsLog(???, rctxDist(???, ???), 1e-9) // TODO: THIS

            val resampledTrees = ParVector(T1a, T2a, T3a)
            val acceptProportion = 0.21
            val sub1AcceptProportion = Some(0.22)
            val avgRatio = 0.23
            val proportionRatioGt1 = 0.24
            val sub1Ratios = ParVector(0.25, 0.26, 0.27)
            val agreeProportion = 0.28
            (resampledTrees, acceptProportion, sub1AcceptProportion, avgRatio, proportionRatioGt1, sub1Ratios, agreeProportion)

          case Vector(T1a, T2a, T3a) =>
            assertEqualsLog(LogDouble(1.0), rootDist(cat"RD1"), 1e-9)
            //                         4.206010928961748
            //  assertEqualsLog(???, rootDist(???), 1e-9) // TODO: THIS
            //  assertEqualsLog(???, prodDist(???, ???), 1e-9) // TODO: THIS
            //  assertEqualsLog(???, lctxDist(???, ???), 1e-9) // TODO: THIS
            //  assertEqualsLog(???, rctxDist(???, ???), 1e-9) // TODO: THIS
            val resampledTrees = ParVector(T1b, T2b, T3b)
            val acceptProportion = 0.31
            val sub1AcceptProportion = Some(0.32)
            val avgRatio = 0.33
            val proportionRatioGt1 = 0.34
            val sub1Ratios = ParVector(0.35, 0.36, 0.37)
            val agreeProportion = 0.38
            (resampledTrees, acceptProportion, sub1AcceptProportion, avgRatio, proportionRatioGt1, sub1Ratios, agreeProportion)
          case Vector(T1b, T2b, T3b) =>
            //  assertEqualsLog(???, rootDist(???), 1e-9) // TODO: THIS
            //  assertEqualsLog(???, prodDist(???, ???), 1e-9) // TODO: THIS
            //  assertEqualsLog(???, lctxDist(???, ???), 1e-9) // TODO: THIS
            //  assertEqualsLog(???, rctxDist(???, ???), 1e-9) // TODO: THIS
            val resampledTrees = ParVector(T1c, T2c, T3c)
            val acceptProportion = 0.41
            val sub1AcceptProportion = Some(0.42)
            val avgRatio = 0.43
            val proportionRatioGt1 = 0.44
            val sub1Ratios = ParVector(0.45, 0.46, 0.47)
            val agreeProportion = 0.48
            (resampledTrees, acceptProportion, sub1AcceptProportion, avgRatio, proportionRatioGt1, sub1Ratios, agreeProportion)
          case Vector(T1c, T2c, T3c) =>
            //  assertEqualsLog(???, rootDist(???), 1e-9) // TODO: THIS
            //  assertEqualsLog(???, prodDist(???, ???), 1e-9) // TODO: THIS
            //  assertEqualsLog(???, lctxDist(???, ???), 1e-9) // TODO: THIS
            //  assertEqualsLog(???, rctxDist(???, ???), 1e-9) // TODO: THIS
            val resampledTrees = ParVector(T1d, T2d, T3d)
            val acceptProportion = 0.51
            val sub1AcceptProportion = Some(0.52)
            val avgRatio = 0.53
            val proportionRatioGt1 = 0.54
            val sub1Ratios = ParVector(0.55, 0.56, 0.57)
            val agreeProportion = 0.58
            (resampledTrees, acceptProportion, sub1AcceptProportion, avgRatio, proportionRatioGt1, sub1Ratios, agreeProportion)
          case Vector(T1d, T2d, T3d) =>
            //  assertEqualsLog(???, rootDist(???), 1e-9) // TODO: THIS
            //  assertEqualsLog(???, prodDist(???, ???), 1e-9) // TODO: THIS
            //  assertEqualsLog(???, lctxDist(???, ???), 1e-9) // TODO: THIS
            //  assertEqualsLog(???, rctxDist(???, ???), 1e-9) // TODO: THIS
            val resampledTrees = ParVector(T1e, T2e, T3e)
            val acceptProportion = 0.61
            val sub1AcceptProportion = Some(0.62)
            val avgRatio = 0.63
            val proportionRatioGt1 = 0.64
            val sub1Ratios = ParVector(0.65, 0.66, 0.67)
            val agreeProportion = 0.68
            (resampledTrees, acceptProportion, sub1AcceptProportion, avgRatio, proportionRatioGt1, sub1Ratios, agreeProportion)
        }
      }
    }

    val mockResultingParser = new GuideChartParser {
      def parseAndProbFromGuideChart(guideChart: CfgGuideChart): Option[(CcgTree, LogDouble)] = ???
    }
    val mockSupParserTrainer = new SupParserTrainer {
      def train(trees: Vector[CcgTree]): GuideChartParser = {
        assertEquals(Vector(
          T1c, T2c, T3c,
          T1d, T2d, T3d,
          T1e, T2e, T3e), trees)
        mockResultingParser
      }
    }

    val mockInitialParser = new AbstractKBestGuideChartParser {
      def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int): Vector[(CcgTree, LogDouble)] = {
        assertEquals(1, k)
        guideChart match {
          case GC1 => Vector(InitialT1p)
          case GC2 => Vector(InitialT2p)
          case GC3 => Vector(InitialT3p)
        }
      }
    }
    val mockInitialParserInstantiater = new PcfgParserInstantiater {
      def apply(
        rootDist: LogProbabilityDistribution[Cat],
        prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): KBestGuideChartParser = {

        //assertEqualsLog(???, rootDist(???), 1e-9) // TODO: THIS
        //assertEqualsLog(???, prodDist(???, ???), 1e-9) // TODO: THIS

        mockInitialParser
      }
    }

    val mockDirSampler = new DirichletSampler {
      def logDir[T](counts: Map[T, LogDouble]): Map[T, LogDouble] = counts match {
        case c: Map[Cat, LogDouble] =>
          if (eqm[T](c, Map(
            A -> LogDouble(11.1 + 2),
            B -> LogDouble(12.2 + 1),
            C -> LogDouble(13.3 + 0)))) RD1
          else if (eqm[T](c, Map(
            A -> LogDouble(11.1 + 2),
            B -> LogDouble(12.2 + 0),
            C -> LogDouble(13.3 + 1)))) RD2
          else if (c.isEmpty) Map.empty
          else sys.error(f"not found: $counts")
      }
      private[this] def eqm[T](a: Map[T, LogDouble], b: Map[T, LogDouble]) = {
        a.keySet == b.keySet && a.keySet.forall(k => a(k) approx b(k))
      }
      def logDir(counts: Vector[LogDouble]): Vector[LogDouble] = counts match {
        case c: Vector[LogDouble] =>
          if (false) ???
          //          eqm(c, Vector(
          //            LogDouble(11.1 + 2),
          //            LogDouble(12.2 + 1),
          //            LogDouble(13.3 + 0)))) RD1
          else if (eqv(c, Vector(LogDouble(1.0), LogDouble(1.0), LogDouble(1.0)))) Vector(LogDouble(1.1), LogDouble(1.2), LogDouble(1.3))
          else if (c.isEmpty) Vector.empty
          else sys.error(f"not found: $counts")
      }
      private[this] def eqv(a: Vector[LogDouble], b: Vector[LogDouble]) = {
        a.size == b.size && (a zip b).forall { case (x, y) => x approx y }
      }

      def logBeta(a: LogDouble, b: LogDouble): LogDouble = ???
    }

    val mcmcScg = new McmcScg(
      samplingIterations = 3,
      burninIterations = 2,
      alphaPriorMaker = mockScgAlphaPriorMaker,
      productionFinder = mockScgProductionFinder,
      initialParserInstantiater = mockInitialParserInstantiater,
      dirSampler = mockDirSampler,
      resampler = mockMcmcScgResampler,
      priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, priorLctxDist, priorRctxDist,
      alphaRoot = 1.1, alphaBiny = 1.2, alphaUnry = 1.3, alphaTerm = 1.4, alphaLctx = 1.6, alphaRctx = 1.7,
      alphaLambda = ???, priorBinyProdMix = 0.2, priorUnryProdMix = 0.3, priorTermProdMix = 0.5,
      supScgTrainer = mockSupParserTrainer)(SE)

    val resultParser = mcmcScg.trainGuideChartsWithSomeGold(mockGuideCharts, mockGoldTrees)

    assertEquals(mockResultingParser, resultParser)
  }

  @Test
  def test_SimpleMcmcScgResampler {
    type Word = String
    type Tag = Cat

    val GC1 = CfgGuideChart(Vector.empty, Vector(Vector(Map(cat"gc1" -> Set()))))
    val GC2 = CfgGuideChart(Vector.empty, Vector(Vector(Map(cat"gc2" -> Set()))))
    val GC3 = CfgGuideChart(Vector.empty, Vector(Vector(Map(cat"gc3" -> Set()))))

    val RootDist = new SimpleLogProbabilityDistribution[Cat](Map())
    val ProdDist = new SimpleConditionalLogProbabilityDistribution[Cat, Prod](Map())
    val LctxDist = new SimpleConditionalLogProbabilityDistribution[Cat, Cat](Map())
    val RctxDist = new SimpleConditionalLogProbabilityDistribution[Cat, Cat](Map())

    val Rand = DoubleIteratorRandomGenerator(Iterator())

    val T1a = CcgLeaf(cat"t1a", "t1a", "FAKEPOS")
    val T1b = CcgLeaf(cat"t1b", "t1b", "FAKEPOS")
    val T2a = CcgLeaf(cat"t2a", "t2a", "FAKEPOS")
    val T2b = CcgLeaf(cat"t2b", "t2b", "FAKEPOS")
    val T3a = CcgLeaf(cat"t3a", "t3a", "FAKEPOS")
    val T3b = CcgLeaf(cat"t3b", "t3b", "FAKEPOS")

    val mockPcfgTreeSampler = new PcfgTreeSampler {
      def samples(
        guideChart: CfgGuideChart,
        rootDist: LogProbabilityDistribution[Cat],
        prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
        k: Int): Vector[CcgTree] = {
        assertEquals(1, k)
        assertSame(RootDist, rootDist)
        assertSame(ProdDist, prodDist)
        guideChart match {
          case GC1 => Vector(T1b)
          case GC2 => Vector(T2b)
          case GC3 => Vector(T3b)
        }
      }
    }

    val mockAcceptanceSampler = new ScgAcceptanceSampler {
      def accept(newTree: CcgTree, curTree: CcgTree,
        rootDist: LogProbabilityDistribution[Cat],
        prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
        lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
        rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
        rand: RandomGenerator)(se: StartEndTags[Cat]) = {
        assertSame(RootDist, rootDist)
        assertSame(ProdDist, prodDist)
        assertSame(LctxDist, lctxDist)
        assertSame(RctxDist, rctxDist)
        assertSame(Rand, rand)
        assertSame(SE, se)

        (newTree, curTree) match {
          case (T1b, T1a) => (true, 0.8, false)
          case (T2b, T2a) => (false, 0.9, false)
          case (T3b, T3a) => (true, 1.1, true)
        }
      }
    }

    val resampler = new SimpleMcmcScgResampler(
      mockPcfgTreeSampler,
      mockAcceptanceSampler,
      maxAcceptanceTries = 1,
      Rand)

    val guideCharts = Vector(GC1, GC2, GC3)
    val currentTrees = Vector(T1a, T2a, T3a)
    val (resampledTrees, acceptProportion, sub1AcceptProportion, avgRatio, proportionRatioGt1, sub1Ratios, agreeProportion) =
      resampler.resample(guideCharts, currentTrees, RootDist, ProdDist, LctxDist, RctxDist)(SE)
    assertEquals(3, resampledTrees.size)
    assertEquals(T1b, resampledTrees(0))
    assertEquals(T2a, resampledTrees(1))
    assertEquals(T3b, resampledTrees(2))
    assertEquals(2 / 3.0, acceptProportion, 1e-9)
    assertEquals(1 / 2.0, sub1AcceptProportion.get, 1e-9)
    assertEquals((0.8 + 0.9 + 1.1) / 3, avgRatio, 1e-9)
    assertEquals(1 / 3.0, proportionRatioGt1, 1e-9)
    assertEquals(ParVector(0.8, 0.9), sub1Ratios)
    assertEquals(1 / 3.0, agreeProportion, 1e-9)
  }

  @Test
  def test_SimpleMcmcScgResampler_maxAcceptanceTries3 {
    type Word = String
    type Tag = Cat

    val GC1 = CfgGuideChart(Vector.empty, Vector(Vector(Map(cat"gc1" -> Set()))))
    val GC2 = CfgGuideChart(Vector.empty, Vector(Vector(Map(cat"gc2" -> Set()))))
    val GC3 = CfgGuideChart(Vector.empty, Vector(Vector(Map(cat"gc3" -> Set()))))
    val GC4 = CfgGuideChart(Vector.empty, Vector(Vector(Map(cat"gc4" -> Set()))))
    val GC5 = CfgGuideChart(Vector.empty, Vector(Vector(Map(cat"gc5" -> Set()))))

    val RootDist = new SimpleLogProbabilityDistribution[Cat](Map())
    val ProdDist = new SimpleConditionalLogProbabilityDistribution[Cat, Prod](Map())
    val LctxDist = new SimpleConditionalLogProbabilityDistribution[Cat, Cat](Map())
    val RctxDist = new SimpleConditionalLogProbabilityDistribution[Cat, Cat](Map())

    val Rand = DoubleIteratorRandomGenerator(Iterator())

    val T1a = CcgLeaf(cat"t1a", "t1a", "FAKEPOS")
    val T1b = CcgLeaf(cat"t1b", "t1b", "FAKEPOS")
    val T1c = CcgLeaf(cat"t1c", "t1c", "FAKEPOS")
    val T1d = CcgLeaf(cat"t1d", "t1d", "FAKEPOS")

    val T2a = CcgLeaf(cat"t2a", "t2a", "FAKEPOS")
    val T2b = CcgLeaf(cat"t2b", "t2b", "FAKEPOS")
    val T2c = CcgLeaf(cat"t2c", "t2c", "FAKEPOS")
    val T2d = CcgLeaf(cat"t2d", "t2d", "FAKEPOS")

    val T3a = CcgLeaf(cat"t3a", "t3a", "FAKEPOS")
    val T3b = CcgLeaf(cat"t3b", "t3b", "FAKEPOS")
    val T3c = CcgLeaf(cat"t3c", "t3c", "FAKEPOS")
    val T3d = CcgLeaf(cat"t3d", "t3d", "FAKEPOS")

    val T4a = CcgLeaf(cat"t4a", "t4a", "FAKEPOS")
    val T4b = CcgLeaf(cat"t4b", "t4b", "FAKEPOS")
    val T4c = CcgLeaf(cat"t4c", "t4c", "FAKEPOS")
    val T4d = CcgLeaf(cat"t4d", "t4d", "FAKEPOS")

    val T5a = CcgLeaf(cat"t5a", "t5a", "FAKEPOS")
    val T5b = CcgLeaf(cat"t5b", "t5b", "FAKEPOS")
    val T5c = CcgLeaf(cat"t5c", "t5c", "FAKEPOS")
    val T5d = CcgLeaf(cat"t5d", "t5d", "FAKEPOS")

    val mockPcfgTreeSampler = new PcfgTreeSampler {
      def samples(
        guideChart: CfgGuideChart,
        rootDist: LogProbabilityDistribution[Cat],
        prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
        k: Int): Vector[CcgTree] = {
        assertEquals(3, k)
        assertSame(RootDist, rootDist)
        assertSame(ProdDist, prodDist)
        guideChart match {
          case GC1 => Vector(T1b, T1c, T1d)
          case GC2 => Vector(T2b, T2c, T2d)
          case GC3 => Vector(T3b, T3c, T3d)
          case GC4 => Vector(T4b, T4c, T4d)
          case GC5 => Vector(T5b, T5c, T5d)
        }
      }
    }

    val mockAcceptanceSampler = new ScgAcceptanceSampler {
      def accept(newTree: CcgTree, curTree: CcgTree,
        rootDist: LogProbabilityDistribution[Cat],
        prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
        lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
        rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
        rand: RandomGenerator)(se: StartEndTags[Cat]) = {
        assertSame(RootDist, rootDist)
        assertSame(ProdDist, prodDist)
        assertSame(LctxDist, lctxDist)
        assertSame(RctxDist, rctxDist)
        assertSame(Rand, rand)
        assertSame(SE, se)

        (curTree, newTree) match {
          case (T1a, T1b) => (false, 0.11, true)
          case (T1a, T1c) => (true, 0.12, false)
          case (T1c, T1d) => (true, 1.13, true)

          case (T2a, T2b) => (true, 0.22, false)
          case (T2b, T2c) => (true, 0.23, false)
          case (T2c, T2d) => (false, 0.24, true)

          case (T3a, T3b) => (false, 0.35, true)
          case (T3a, T3c) => (false, 0.36, true)
          case (T3a, T3d) => (false, 0.37, false)

          case (T4a, T4b) => (true, 0.41, false)
          case (T4b, T4c) => (true, 0.42, true)
          case (T4c, T4d) => (true, 2.43, true)

          case (T5a, T5b) => (false, 0.51, true)
          case (T5a, T5c) => (false, 0.52, true)
          case (T5a, T5d) => (true, 0.53, true)
        }
      }
    }

    val resampler = new SimpleMcmcScgResampler(
      mockPcfgTreeSampler,
      mockAcceptanceSampler,
      maxAcceptanceTries = 3,
      Rand)

    val guideCharts = Vector(GC1, GC2, GC3, GC4, GC5)
    val currentTrees = Vector(T1a, T2a, T3a, T4a, T5a)
    val (resampledTrees, acceptProportion, sub1AcceptProportion, avgRatio, proportionRatioGt1, sub1Ratios, agreeProportion) =
      resampler.resample(guideCharts, currentTrees, RootDist, ProdDist, LctxDist, RctxDist)(SE)
    assertEquals(5, resampledTrees.size)
    assertEquals(T1d, resampledTrees(0))
    assertEquals(T2c, resampledTrees(1))
    assertEquals(T3a, resampledTrees(2))
    assertEquals(T4d, resampledTrees(3))
    assertEquals(T5d, resampledTrees(4))
    assertEquals(3 / 5.0, acceptProportion, 1e-9)
    assertEquals(1 / 3.0, sub1AcceptProportion.get, 1e-9)
    assertEquals((1.13 + 0.24 + 0.37 + 2.43 + 0.53) / 5, avgRatio, 1e-9)
    assertEquals(2 / 5.0, proportionRatioGt1, 1e-9)
    assertEquals(ParVector(0.24, 0.37, 0.53), sub1Ratios)
    assertEquals(4 / 5.0, agreeProportion, 1e-9)
  }

  @Test
  def integration_againstEM {
    type Word = String
    type Tag = Cat

    val Det: Set[Cat] = Set(
      np / n)
    val Adj: Set[Cat] = Set(
      n / n)
    val IV: Set[Cat] = Set(
      s \ np,
      (s \ np) / pp)
    val TV: Set[Cat] = Set(
      (s \ np) / np,
      ((s \ np) / pp) / np,
      (((s \ np) / pp) / pp) / np)
    val N: Set[Cat] = Set(
      n)
    val NNP: Set[Cat] = Set(
      np,
      np / pp,
      (np / pp) / pp)
    val Prep: Set[Cat] = Set(
      pp / np)

    val tagdict = SimpleTagDictionary.apply(
      Map[Word, Set[Tag]](
        "the" -> Det,
        "a" -> Det,
        "big" -> Adj,
        "man" -> N,
        "dog" -> N,
        "dogs" -> N,
        "cat" -> N,
        "cats" -> N,
        "telescope" -> N,
        "saw" -> (IV | TV),
        "walked" -> (IV | TV),
        "chase" -> TV,
        "run" -> IV,
        "ran" -> IV,
        "John" -> NNP,
        "Mary" -> NNP,
        "with" -> Prep,
        "nnp" -> Set(n, np)),
      "<S>", cat"<S>", "<E>", cat"<E>")

    //    val sentences = Vector(
    //      "the dogs walked",
    //      "the man walked the dog",
    //      "dogs chase cats",
    //      "the man ran with a dog",
    //      "big dogs run",
    //      "the big dogs run",
    //      "John saw Mary with the dog",
    //      "John saw a cat with the dog",
    //      "John saw Mary with the telescope",
    //      "John saw Mary with the dog with the telescope")
    //    val gcBuilder = new SimpleCfgGuideChartBuilder(Vector(FA, BA, N2NP), additionalSupertagAdder = new PresentTagdictAdditionalTagAdder)
    //    val rawDataGC = sentences.flatMap(s => gcBuilder.build(s.splitWhitespace, None, tagdict))

    //    val B1 = (B / C) \ A
    //    val B2 = (B \ A) / C
    //    val X = C / D
    //    val XS: Vector[(Word, Set[Cat])] = Vector(("s", Set(S / E)), ("x", Set(B / C)), ("y", Set(X)), ("z", Set((E \ (B / C)) \ X)))
    //    val sentences: Vector[Vector[(Word, Set[Cat])]] = (1 to 100).toVector.flatMap(_ => Vector[Vector[(Word, Set[Cat])]](
    //      XS, XS, XS, XS, XS, XS, XS,
    //      Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B1)), ("c", Set(C / D)), ("d", Set(D))),
    //      Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B2)), ("c", Set(C / D)), ("d", Set(D))),
    //      Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B1, B2)), ("c", Set(C / D)), ("d", Set(D))),
    //      Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B1)), ("c", Set(C / D)), ("d", Set(D)))))

    val B1 = (B / C) \ A
    val B2 = (B \ A) / C
    val X = F //C / D
    val XS: Vector[(Word, Set[Cat])] = Vector(("s", Set(S / E)), ("x", Set(B / C)), ("y", Set(X)), ("z", Set((E \ (B / C)) \ X)))
    val sentences: Vector[Vector[(Word, Set[Cat])]] = (1 to 100).toVector.flatMap(_ => Vector[Vector[(Word, Set[Cat])]](
      XS, XS, XS, XS, XS, XS, XS,
      Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B1)), ("c", Set(C / D)), ("d", Set(D))),
      Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B2)), ("c", Set(C / D)), ("d", Set(D))),
      Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B1, B2)), ("c", Set(C / D)), ("d", Set(D))),
      Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B1)), ("c", Set(C / D)), ("d", Set(D)))))

    val gcBuilder = new SimpleCfgGuideChartBuilder(Vector(FA, BA, N2NP), additionalSupertagAdder = new NoOpAdditionalTagAdder, allowTerminalDeletion = false)
    val rawDataGC = sentences.flatMap(s => gcBuilder.buildFromSupertagSetSentence(s, None, tagdict))

    //

    val pcfgProductionFinder = new SimplePcfgProductionCounter()
    val scgProductionFinder = new SimpleScgProductionFinder(pcfgProductionFinder)
    val pcfgGuideChartProdFinder = new SimplePcfgGuideChartProdFinder()
    val scgGuideChartProdFinder = new SimpleScgGuideChartProdFinder(pcfgGuideChartProdFinder)
    val pcfgAlphaPriorMaker = new TrainDataPcfgAlphaPriorMaker(pcfgProductionFinder, pcfgGuideChartProdFinder)
    val scgAlphaPriorMaker = new TrainDataScgAlphaPriorMaker(scgProductionFinder, scgGuideChartProdFinder)
    val pcfgInsideChartBuilder = new SimpleScgInsideChartBuilder()
    val pcfgTreeSampler = new SimplePcfgTreeSampler(new SimplePcfgInsideChartBuilder())
    val pcfgParserInstantiater = new ExactPcfgParserInstantiater()
    val scgParserInstantiater = new ExactScgParserInstantiater()
    val scgGuideChartProdFinders = new SimpleScgGuideChartProdFinder(pcfgGuideChartProdFinder)

    //
    val priorRootDist = new UniformDefaultLogProbabilityDistribution[Cat](LogDouble.one)
    val priorBinyDist = new SimpleConditionalLogProbabilityDistribution[Cat, BinaryProd](rawDataGC.map(scgGuideChartProdFinders.binys).reduce(_ |+| _).mapVals(c => new SimpleLogProbabilityDistribution(c.mapToVal(LogDouble(scala.util.Random.nextDouble)).toMap)))
    val priorUnryDist = new SimpleConditionalLogProbabilityDistribution[Cat, UnaryProd](rawDataGC.map(scgGuideChartProdFinders.unrys).reduce(_ |+| _).mapVals(c => new SimpleLogProbabilityDistribution(c.mapToVal(LogDouble(scala.util.Random.nextDouble)).toMap)))
    val priorTermDist = new SimpleConditionalLogProbabilityDistribution[Cat, TermProd](rawDataGC.map(scgGuideChartProdFinders.terms).reduce(_ |+| _).mapVals(c => new SimpleLogProbabilityDistribution(c.mapToVal(LogDouble(scala.util.Random.nextDouble)).toMap)))
    val priorProdDist = new SimpleConditionalLogProbabilityDistribution[Cat, Prod](rawDataGC.map(scgGuideChartProdFinders.prods).reduce(_ |+| _).mapVals(c => new SimpleLogProbabilityDistribution(c.mapToVal(LogDouble(scala.util.Random.nextDouble)).toMap)))
    val priorLctxDist = new SimpleConditionalLogProbabilityDistribution[Cat, Cat](rawDataGC.map(scgGuideChartProdFinders.lctxs(_)(SE)).reduce(_ |+| _).mapVals(c => new SimpleLogProbabilityDistribution(c.mapToVal(LogDouble(scala.util.Random.nextDouble)).toMap)))
    val priorRctxDist = new SimpleConditionalLogProbabilityDistribution[Cat, Cat](rawDataGC.map(scgGuideChartProdFinders.rctxs(_)(SE)).reduce(_ |+| _).mapVals(c => new SimpleLogProbabilityDistribution(c.mapToVal(LogDouble(scala.util.Random.nextDouble)).toMap)))

    val uniPriorRootDist = new UniformDefaultLogProbabilityDistribution[Cat](LogDouble.one)
    val uniPriorBinyDist = new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, BinaryProd](new UniformDefaultLogProbabilityDistribution(LogDouble.one))
    val uniPriorUnryDist = new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, UnaryProd](new UniformDefaultLogProbabilityDistribution(LogDouble.one))
    val uniPriorTermDist = new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, TermProd](new UniformDefaultLogProbabilityDistribution(LogDouble.one))
    val uniPriorProdDist = new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, Prod](new UniformDefaultLogProbabilityDistribution(LogDouble.one))
    val uniPriorLctxDist = new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, Cat](new UniformDefaultLogProbabilityDistribution(LogDouble.one))
    val uniPriorRctxDist = new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, Cat](new UniformDefaultLogProbabilityDistribution(LogDouble.one))
    //
    val accumulate = false

    val em = new DumbScgEm(100, pcfgOnly = false)
    val (sc, pc, lc, rc) = em.trainFromInit(rawDataGC, priorRootDist, priorProdDist, priorLctxDist, priorRctxDist)(SE)
    val esd = new SimpleLogProbabilityDistribution(sc)
    val epd = new SimpleConditionalLogProbabilityDistribution(pc.mapVals(new SimpleLogProbabilityDistribution(_)))
    val eld = new SimpleConditionalLogProbabilityDistribution(lc.mapVals(new SimpleLogProbabilityDistribution(_)))
    val erd = new SimpleConditionalLogProbabilityDistribution(rc.mapVals(new SimpleLogProbabilityDistribution(_)))

    val alpha = 1e-100
    val mcmc = new McmcScg(
      samplingIterations = 1000,
      burninIterations = 10,
      alphaPriorMaker = scgAlphaPriorMaker,
      productionFinder = scgProductionFinder,
      initialParserInstantiater = pcfgParserInstantiater,
      dirSampler = DirSampler,
      resampler = new SimpleMcmcScgResampler(
        pcfgTreeSampler,
        acceptanceSampler = new ContextScgAcceptanceSampler(scgWeighter = new SimpleScgWeighter()), // TODO:
        maxAcceptanceTries = 10,
        rand = new MersenneTwister),
      uniPriorRootDist, uniPriorBinyDist, uniPriorUnryDist, uniPriorTermDist, uniPriorLctxDist, uniPriorRctxDist,
      alphaRoot = alpha, alphaBiny = alpha, alphaUnry = alpha, alphaTerm = alpha, alphaLctx = alpha, alphaRctx = alpha,
      alphaLambda = 3.0, priorBinyProdMix = 1.0 / 3, priorUnryProdMix = 1.0 / 3, priorTermProdMix = 1.0 / 3,
      supScgTrainer = new UnsmoothedSupScgTrainer(scgProductionFinder, scgParserInstantiater)(SE),
      accumulate = accumulate)(SE)
    val mcmcparser: ExactScgParser = mcmc.trainGuideChartsWithSomeGold(rawDataGC, Vector.empty).asInstanceOf[ExactScgParser]
    val msd = mcmcparser.rootDist
    val mpd = mcmcparser.prodDist
    val mld = mcmcparser.lctxDist
    val mrd = mcmcparser.rctxDist

    val pem = new DumbPcfgEm(100)
    val (pesc, pepc) = pem.trainFromInit(rawDataGC, priorRootDist, priorProdDist)
    val pesd = new SimpleLogProbabilityDistribution(pesc)
    val pepd = new SimpleConditionalLogProbabilityDistribution(pepc.mapVals(new SimpleLogProbabilityDistribution(_)))

    val dumbMcmcPcfg = new DumbPcfgMcmc(
      samplingIterations = 1000,
      burninIterations = 100,
      pcfgTreeSampler,
      alphaRoot = alpha, alphaProd = alpha,
      accumulate)
    val dumbMcmcPcfgParser: PcfgParser = dumbMcmcPcfg.train(rawDataGC, uniPriorRootDist, uniPriorProdDist)
    val dpmsd = dumbMcmcPcfgParser.rootDist
    val dpmpd = dumbMcmcPcfgParser.prodDist

    val mcmcPcfg = new McmcPcfg(
      samplingIterations = 1000,
      burninIterations = 100,
      pcfgAlphaPriorMaker,
      pcfgProductionFinder,
      initialParserInstantiater = pcfgParserInstantiater,
      DirSampler,
      pcfgTreeSampler,
      uniPriorRootDist, uniPriorBinyDist, uniPriorUnryDist, uniPriorTermDist,
      alphaRoot = alpha, alphaBiny = alpha, alphaUnry = alpha, alphaTerm = alpha,
      alphaLambda = 3.0, priorBinyProdMix = 1.0 / 3, priorUnryProdMix = 1.0 / 3, priorTermProdMix = 1.0 / 3,
      supPcfgTrainer = new UnsmoothedSupPcfgTrainer(pcfgProductionFinder, pcfgParserInstantiater),
      accumulate = accumulate)
    val mcmcPcfgParser: PcfgParser = mcmcPcfg.trainGuideChartsWithSomeGold(rawDataGC, Vector.empty).asInstanceOf[PcfgParser]
    val pmsd = mcmcPcfgParser.rootDist
    val pmpd = mcmcPcfgParser.prodDist

    //    // TODO: Directly sample from pcfgTreeSampler w/ em params, average => get back the EM parameters
    //    val pcfgSamples = rawDataGC.flatMap(gc => pcfgTreeSampler.samples(gc, esd, epd, 1000))
    //    val msd = new SimpleLogProbabilityDistribution[Cat](pcfgSamples.map(pcfgProductionFinder.rootCounts).reduce(_ |+| _).mapVals(LogDouble(_)))
    //    val mpd = new SimpleConditionalLogProbabilityDistribution[Cat, Prod](pcfgSamples.map(pcfgProductionFinder.prodCounts).reduce(_ |+| _).mapVals(c => new SimpleLogProbabilityDistribution(c.mapVals(LogDouble(_)))))

    //

    println(f"${" " * 35}${"scg-em ex cnt"}%-15s${"dumb scg-em"}%-15s${"scg-mcmc"}%-15s${"dumb pcfg-em"}%-15s${"dumb pcfg-mcmc"}%-15s${"pcfg-mcmc"}%-15s")
    println("\nRoots"); /*                                                                                     */ for ((y, c) <- sc.toVector.sortBy(_._1.toString)) { println( /*  */ f"  $y -> ".padRight(35) + f"${c.toDouble}%.4f       ${esd(y).toDouble}%.4f         ${msd(y).toDouble}%.4f         ${pesd(y).toDouble}%.4f         ${dpmsd(y).toDouble}%.4f         ${pmsd(y).toDouble}%.4f") }
    println("\nProds"); for ((x, ys) <- pc.toVector.sortBy(_._1.toString) if ys.nonEmpty) { println(f"  $x -> "); for ((y, c) <- ys.toVector.sortBy(_._1.toString)) { println( /**/ f"         $y -> ".padRight(35) + f"${c.toDouble}%.4f       ${epd(y, x).toDouble}%.4f         ${mpd(y, x).toDouble}%.4f         ${pepd(y, x).toDouble}%.4f         ${dpmpd(y, x).toDouble}%.4f         ${pmpd(y, x).toDouble}%.4f".padLeft(30)) } }
    ////    println("\nBinary Prods"); for ((x, ys) <- pc.toVector.sortBy(_._1.toString) if ys.exists(_._1.isInstanceOf[BinaryProd])) { println(f"  $x -> "); for ((BinaryProd(y, z), c) <- ys.toVector.sortBy(_._1.toString)) { println(f"    [$y $z] -> ${c.toDouble}%.4f") } }
    ////    println("\nUnary Prods"); for ((x, ys) <- pc.toVector.sortBy(_._1.toString) if ys.exists(_._1.isInstanceOf[UnaryProd])) { println(f"  $x -> "); for ((UnaryProd(y), c) <- ys.toVector.sortBy(_._1.toString)) { println(f"         $y -> ${c.toDouble}%.4f") } }
    ////    println("\nTerminal Prods"); for ((x, ys) <- pc.toVector.sortBy(_._1.toString) if ys.exists(_._1.isInstanceOf[TermProd])) { println(f"  $x -> "); for ((TermProd(y), c) <- ys.toVector.sortBy(_._1.toString)) { println(f"         $y -> ${c.toDouble}%.4f") } }
    println("\nLeft-Contexts"); for ((x, ys) <- lc.toVector.sortBy(_._1.toString) if ys.nonEmpty) { println(f"  $x -> "); for ((y, c) <- ys.toVector.sortBy(_._1.toString)) { println(f"         $y -> ".padRight(35) + f"${c.toDouble}%.4f       ${eld(y, x).toDouble}%.4f         ${mld(y, x).toDouble}%.4f") } }
    println("\nRight-Contexts"); for ((x, ys) <- rc.toVector.sortBy(_._1.toString) if ys.nonEmpty) { println(f"  $x -> "); for ((y, c) <- ys.toVector.sortBy(_._1.toString)) { println(f"         $y -> ".padRight(35) + f"${c.toDouble}%.4f       ${erd(y, x).toDouble}%.4f         ${mrd(y, x).toDouble}%.4f") } }

    ???
  }

  def assertEqualsLog(a: LogDouble, b: LogDouble, t: Double) {
    assertEquals(a.toDouble, b.toDouble, t)
  }
}

class XPcfgTreeSampler()
    extends PcfgTreeSampler {

  def samples(
    guideChart: CfgGuideChart,
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    k: Int): Vector[CcgTree] = {
    assert(k == 1)

    val pcfgParser = new PcfgParser(rootDist, prodDist)
    val treesAndProbs = pcfgParser.parseAndProbKBestFromGuideChart(guideChart, guideChart.numPossibleParses.toInt * 2)
    if (treesAndProbs.size > 1) {
      val x = new SimpleLogProbabilityDistribution(treesAndProbs.toMap).sample()
      val totalProb = treesAndProbs.sumBy(_._2)
      for (((t, p), j) <- treesAndProbs.sortBy(_._1.toString).zipWithIndex) {
        println(f"  --${j + 1}  ${(p / totalProb).toDouble}%.4f  ${t}  ${if (t == x) f"${j + 1}" else ""}")
      }
      Vector(x)
    }
    else {
      Vector(treesAndProbs.only._1)
    }
  }

}
