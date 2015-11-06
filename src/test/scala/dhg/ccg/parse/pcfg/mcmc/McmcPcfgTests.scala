package dhg.ccg.parse.pcfg.mcmc

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.parse.pcfg._
import dhg.ccg.math._
import dhg.util._
import org.apache.commons.math3.random.MersenneTwister
import dhg.ccg.parse.pcfg.typesup.UniformRootDistInitializer
import dhg.ccg.parse.pcfg.typesup.UniformUnaryDistInitializer
import dhg.ccg.tag.learn.EmUniform
import dhg.ccg.parse.pcfg.typesup.UniformBinaryDistInitializer
import dhg.ccg.parse.scg.exp.Em2TermProdDist
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tagdict.SimpleStartEndTags
import scalaz._
import Scalaz._

class McmcPcfgTests {

  val A: AtomCat = cat"A".asInstanceOf[AtomCat]
  val B: AtomCat = cat"B".asInstanceOf[AtomCat]
  val C: AtomCat = cat"C".asInstanceOf[AtomCat]
  val D: AtomCat = cat"D".asInstanceOf[AtomCat]
  val E: AtomCat = cat"E".asInstanceOf[AtomCat]
  val F: AtomCat = cat"F".asInstanceOf[AtomCat]
  val X: Cat = cat"X".asInstanceOf[AtomCat]

  val S: AtomCat = cat"S".asInstanceOf[AtomCat]
  val NP: Cat = cat"NP".asInstanceOf[AtomCat]
  val N: Cat = cat"N".asInstanceOf[AtomCat]
  val PP: Cat = cat"PP".asInstanceOf[AtomCat]
  val STA: Cat = cat"<S>"
  val END: Cat = cat"<E>"
  val SE = new SimpleStartEndTags(STA, END)

  val s: Cat = cat"S".asInstanceOf[AtomCat]
  val np: Cat = cat"NP".asInstanceOf[AtomCat]
  val n: Cat = cat"N".asInstanceOf[AtomCat]
  val pp: Cat = cat"PP".asInstanceOf[AtomCat]

  val startWord = "<S>"
  val startTag = STA
  val endWord = "<E>"
  val endTag = END

  @Test
  def test_McmcPcfg_trainGuideChartsWithSomeGold {
    throw new NotImplementedError("Test not written")
  }

  @Test
  def test_SimpleMcmcPcfgResampler {
    throw new NotImplementedError("Test not written")
  }

  @Test
  def integration_posterior {
    type Word = String
    type Tag = Cat

    val tagdict = SimpleTagDictionary[Tag](Map(), "<S>", cat"<S>", "<E>", cat"<E>")

    val B1 = (B / C) \ A
    val B2 = (B \ A) / C
    val sentences: Vector[Vector[(Word, Set[Cat])]] = Vector(
      //      Vector(("a", Set(A)), ("b", Set(B1)), ("c", Set(C))),
      //      Vector(("a", Set(A)), ("b", Set(B2)), ("c", Set(C))),
      //      Vector(("a", Set(A)), ("b", Set(B1, B2)), ("c", Set(C))),
      //      Vector(("a", Set(A)), ("b", Set(B1)), ("c", Set(C/D)), ("d", Set(D))))
      Vector(("a", Set(A)), ("b", Set(B1)), ("c", Set(C))),
      Vector(("a", Set(A)), ("b", Set(B2)), ("c", Set(C / D)), ("d", Set(D))),
      Vector(("a", Set(A)), ("b", Set(B1, B2)), ("c", Set(C / D)), ("d", Set(D))),
      Vector(("a", Set(A)), ("b", Set(B1)), ("c", Set(C / D)), ("d", Set(D))))
    val gcBuilder = new SimpleCfgGuideChartBuilder(Vector(FA, BA, N2NP), additionalSupertagAdder = new NoOpAdditionalTagAdder, allowTerminalDeletion = false)
    val rawDataGC = sentences.flatMap(s => gcBuilder.buildFromSupertagSetSentence(s, None, tagdict))

    //

    println(f"sentences.length = ${sentences.length}")
    println(f"rawDataGC.length = ${rawDataGC.length}")
    println

    val pcfgProductionFinder = new SimplePcfgProductionCounter()
    val pcfgGuideChartProdFinder = new SimplePcfgGuideChartProdFinder()
    val pcfgAlphaPriorMaker = new TrainDataPcfgAlphaPriorMaker(pcfgProductionFinder, pcfgGuideChartProdFinder)
    val pcfgInsideChartBuilder = new SimplePcfgInsideChartBuilder()
    val pcfgTreeSampler = new SimplePcfgTreeSampler(pcfgInsideChartBuilder)
    val pcfgParserInstantiater = new ExactPcfgParserInstantiater()

    val priorRootDist = new UniformDefaultLogProbabilityDistribution[Cat](LogDouble.one)
    val priorBinyDist = new SimpleConditionalLogProbabilityDistribution[Cat, BinaryProd](rawDataGC.map(pcfgGuideChartProdFinder.binys).reduce(_ |+| _).mapVals(c => new SimpleLogProbabilityDistribution(c.mapToVal(LogDouble(scala.util.Random.nextDouble)).toMap)))
    val priorUnryDist = new SimpleConditionalLogProbabilityDistribution[Cat, UnaryProd](rawDataGC.map(pcfgGuideChartProdFinder.unrys).reduce(_ |+| _).mapVals(c => new SimpleLogProbabilityDistribution(c.mapToVal(LogDouble(scala.util.Random.nextDouble)).toMap)))
    val priorTermDist = new SimpleConditionalLogProbabilityDistribution[Cat, TermProd](rawDataGC.map(pcfgGuideChartProdFinder.terms).reduce(_ |+| _).mapVals(c => new SimpleLogProbabilityDistribution(c.mapToVal(LogDouble(scala.util.Random.nextDouble)).toMap)))
    val priorProdDist = new SimpleConditionalLogProbabilityDistribution[Cat, Prod](rawDataGC.map(pcfgGuideChartProdFinder.prods).reduce(_ |+| _).mapVals(c => new SimpleLogProbabilityDistribution(c.mapToVal(LogDouble(scala.util.Random.nextDouble)).toMap)))

    val alpha = 0.0000000001
    val supScgTrainer = new AlphaBetaSupPcfgTrainer(
      priorRootDist, priorBinyDist, priorUnryDist, priorTermDist,
      alphaRoot = 0.01, alphaProd = 0.01,
      priorBinyProdMix = 1.0 / 3, priorUnryProdMix = 1.0 / 3, priorTermProdMix = 1.0 / 3,
      pcfgProductionFinder, pcfgParserInstantiater)
    val mcmcPcfg = new McmcPcfg(
      samplingIterations = 50,
      burninIterations = 0,
      pcfgAlphaPriorMaker,
      pcfgProductionFinder,
      initialParserInstantiater = pcfgParserInstantiater,
      DirSampler,
      pcfgTreeSampler,
      priorRootDist, priorBinyDist, priorUnryDist, priorTermDist,
      alphaRoot = alpha, alphaBiny = alpha, alphaUnry = alpha, alphaTerm = alpha,
      alphaLambda = 3.0, priorBinyProdMix = 1.0 / 3, priorUnryProdMix = 1.0 / 3, priorTermProdMix = 1.0 / 3,
      supScgTrainer)

    val em = new DumbPcfgEm(20)
    val (sc, pc) = em.trainFromInit(rawDataGC, priorRootDist, priorProdDist)
    val esd = new SimpleLogProbabilityDistribution(sc)
    val epd = new SimpleConditionalLogProbabilityDistribution(pc.mapVals(new SimpleLogProbabilityDistribution(_)))

    val mcmcPcfgParser: PcfgParser = mcmcPcfg.trainGuideChartsWithSomeGold(rawDataGC, Vector.empty).asInstanceOf[PcfgParser]
    val msd = mcmcPcfgParser.rootDist
    val mpd = mcmcPcfgParser.prodDist
    println("\nRoots"); for ((y, c) <- sc.toVector.sortBy(_._1.toString)) { println(f"  $y -> ${c.toDouble}%.4f    ${esd(y).toDouble}%.4f    ${msd(y).toDouble}%.4f") }
    println("\nProds"); for ((x, ys) <- pc.toVector.sortBy(_._1.toString) if ys.nonEmpty) { println(f"  $x -> "); for ((y, c) <- ys.toVector.sortBy(_._1.toString)) { println(f"    $y -> ".padRight(35) + f"${c.toDouble}%.4f    ${epd(y, x).toDouble}%.4f    ${mpd(y, x).toDouble}%.4f".padLeft(30)) } }

    ???
  }

  //  @Test
  //  def test1() {
  //    /*
  //     *  +-------------+--------------+--------------+
  //     *  | np/n -> 0.8 | np ->        | s ->         |
  //     *  | n/n -> 0.1  | n ->         | n ->         |
  //     *  | n/s -> 0.1  |              |              |
  //     *  +-------------+--------------+--------------+
  //     *                | n -> 0.6     | s ->         |
  //     *                | np -> 0.4    |              |
  //     *                |              |              |
  //     *                +--------------+--------------+
  //     *                               | s\np -> 0.7  |
  //     *                               | s\n -> 0.2   |
  //     *                               | s -> 0.1     |
  //     *                               +--------------+
  //     *
  //     * np/n -> the     : 0.21
  //     * n/n  -> the     : 0.02
  //     * n/s  -> the     : 0.01
  //     * n    -> dogs    : 0.05
  //     * np   -> dogs    : 0.04
  //     * s\np -> run     : 0.06
  //     * s\n  -> run     : 0.01
  //     * s    -> run     : 0.03
  //     * 
  //     * np -> np/n n    : 0.45
  //     * np -> X         : 0.55
  //     * n  -> n/n  n    : 0.25
  //     * n  -> n/s  s    : 0.10
  //     * n  -> X         : 0.65
  //     * s  -> np   s\np : 0.65
  //     * s  -> n    s\n  : 0.15
  //     * s  -> X         : 0.20
  //     * 
  //     * p(s) : 0.7
  //     * p(n) : 0.2
  //     * p(X) : 0.1
  //     * 
  //     * 
  //     * v02(np): p(np -> np/n n) * v01(np/n) * v12(n) = 0.45 * 0.21 * 0.05 = 0.004725
  //     * v02(n):  p(n  -> n/n  n) * v01(n/n)  * v12(n) = 0.25 * 0.02 * 0.05 = 0.000250
  //     * 
  //     * v13(s): p(s -> np s\np) * v12(np) * v23(s\np) = 0.65 * 0.04 * 0.06 = 0.001560
  //     * v13(s): p(s -> n  s\n ) * v12(n)  * v23(s\n)  = 0.15 * 0.05 * 0.01 = 0.000075
  //     * 
  //     * v03(s): p(s -> np s\np) * v02(np) * v23(s\np) = 0.65 * 0.004725 * 0.06 = 0.000184275
  //     * v03(s): p(s -> n  s\n ) * v02(n)  * v23(s\n)  = 0.15 * 0.000250 * 0.01 = 0.000000375
  //     * v03(n): p(n -> n/s  s) * v01(n/s) * v13(s)    = 0.10 * 0.01 * 0.001560 = 0.000001560
  //     * 
  //     * p(s) * v03(s) = 0.7 * 0.000184275 = 0.0001289925
  //     * p(n) * v03(n) = 0.2 * 0.000001560 = 0.0000003120
  //     * 
  //     */
  //
  //    val tagdict = SimpleTagDictionary[Cat](Map(
  //      "the" -> Set(n / n, np / n, n / s),
  //      "dogs" -> Set(np, n),
  //      "run" -> Set(s \ n, s \ np, s)),
  //      "<S>", cat"<S>", "<E>", cat"<E>")
  //
  //    val rootDist = new SimpleExpProbabilityDistribution[Cat](Map(
  //      s -> 0.7,
  //      n -> 0.2,
  //      A -> 0.1))
  //    val prodDist = new SimpleConditionalExpProbabilityDistribution[Cat, Prod](Map(
  //      np -> new DefaultedExpProbabilityDistribution(Map(
  //        NontermProd(np / n, n) -> 0.45,
  //        NontermProd(A, A) -> 0.55,
  //        TermProd("dogs") -> 0.04,
  //        TermProd("X") -> 0.96)),
  //      n -> new DefaultedExpProbabilityDistribution(Map(
  //        NontermProd(n / n, n) -> 0.25,
  //        NontermProd(n / s, s) -> 0.10,
  //        NontermProd(A, A) -> 0.65,
  //        TermProd("dogs") -> 0.05,
  //        TermProd("X") -> 0.95)),
  //      s -> new DefaultedExpProbabilityDistribution(Map(
  //        NontermProd(np, s \ np) -> 0.65,
  //        NontermProd(n, s \ n) -> 0.15,
  //        NontermProd(A, A) -> 0.2,
  //        TermProd("run") -> 0.03,
  //        TermProd("X") -> 0.97)),
  //      (np / n) -> new DefaultedExpProbabilityDistribution(Map(
  //        TermProd("the") -> 0.21,
  //        TermProd("X") -> 0.79)),
  //      (n / n) -> new DefaultedExpProbabilityDistribution(Map(
  //        TermProd("the") -> 0.02,
  //        TermProd("X") -> 0.98)),
  //      (n / s) -> new DefaultedExpProbabilityDistribution(Map(
  //        TermProd("the") -> 0.01,
  //        TermProd("X") -> 0.99)),
  //      (s \ np) -> new DefaultedExpProbabilityDistribution(Map(
  //        TermProd("run") -> 0.06,
  //        TermProd("X") -> 0.94)),
  //      (s \ n) -> new DefaultedExpProbabilityDistribution(Map(
  //        TermProd("run") -> 0.01,
  //        TermProd("X") -> 0.99))))
  //    val rules = Vector(FA, BA)
  //    val sentences = Vector(
  //      "the dogs run").map(_.splitWhitespace)
  //
  //    val trainer = new McmcPcfg(2, 1, new UnsmoothedPcfgParserTrainer(rules), parseCountCutoff = 1000, numSamples = 1000, rules = rules, 
  //        alphaRoots = 0.1, alphaProds = 0.1,
  //        verbose = true)
  //    val pcfg = trainer.train(sentences, tagdict, rootDist, prodDist)
  //
  //    //    val ot = hard1.parse("the dogs run".split("\\s+").toVector)
  //    //    println(ot)
  //    //    ot.foreach(t => println(t.pretty))
  //  }

}
