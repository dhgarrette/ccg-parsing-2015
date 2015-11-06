package dhg.ccg.parse.inf

import scala.annotation.tailrec
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.prob._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.tagdict.TagDictionary
import dhg.util._
import scalaz._
import Scalaz._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import org.apache.commons.math3.random.RandomGenerator
import dhg.ccg.math._
import dhg.ccg.math.Util._
import dhg.ccg.tagdict.StartEndTags
import dhg.ccg.tagdict.SimpleStartEndTags
import scala.collection.parallel.immutable.ParVector
import dhg.ccg.math.DirichletSampler
import dhg.ccg.parse.dep.ParserEvaluator
import dhg.gfl.FudgSentence
import dhg.ccg.math.SimpleDirichletSampler
import breeze.stats.distributions.Rand
import scala.util.control.Breaks._
import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.random.SynchronizedRandomGenerator
import dhg.util.viz._
import dhg.ccg.parse.pcfg.typesup._

class McmcInfPcfg(
    samplingIterations: Int,
    burninIterations: Int,
    alphaPriorMaker: PcfgAlphaPriorMaker,
    productionFinder: PcfgProductionCounter,
    initialParserInstantiater: PcfgParserInstantiater,
    dirSampler: DirichletSampler,
    termPrior: ConditionalLogProbabilityDistribution[Cat, TermProd],
    rootSet: Set[Cat],
    binaryRules: Set[BinaryCcgRule], unaryRules: Set[UnaryCcgRule],
    alphaRoot: Double, alphaBiny: Double, alphaUnry: Double, alphaTerm: Double,
    alphaLambda: Double, priorBinyProdMix: Double, priorUnryProdMix: Double, priorTermProdMix: Double,
    qBetaA: LogDouble, qBetaB: LogDouble,
    initialGudeChartBuilder: CfgGuideChartBuilder, // should be forward-backward adding
    guideChartProdFinder: PcfgGuideChartProdFinder,
    catPrior: InfCatPrior,
    rand: RandomGenerator,
    additionalKnownCats: Set[Cat] = Set.empty,
    fullAllowedCatSet: Option[Set[Cat]] = None,
    accumulate: Boolean = false,
    verbose: Boolean = false) {
  type Word = String

  private[this] val unaryProductions: Map[Cat, Set[Cat]] = unaryRules.map(r => r.parent -> r.child).groupByKey.withDefaultValue(Set.empty)
  private[this] val allUnrySet: Map[Cat, Set[UnaryProd]] = unaryProductions.mapVals(_.map(UnaryProd(_)))

  private[this] val paramResampler = new SimpleParamResampler(
    productionFinder: PcfgProductionCounter,
    catPrior: InfCatPrior,
    termPrior: ConditionalLogProbabilityDistribution[Cat, TermProd],
    alphaRoot: Double, alphaBiny: Double, alphaUnry: Double, alphaTerm: Double,
    alphaLambda: Double, priorBinyProdMix: Double, priorUnryProdMix: Double, priorTermProdMix: Double,
    rootSet: Set[Cat],
    allUnrySet: Map[Cat, Set[UnaryProd]],
    dirSampler)

  private[this] val infTreeResampler = new SlicingInfTreeResampler(
    binaryRules, unaryRules,
    catPrior,
    rootSet,
    dirSampler,
    qBetaA, qBetaB,
    rand,
    fullAllowedCatSet.getOrElse(UniversalSet()))

  def trainGuideChartsWithSomeGold(
    guideCharts: Vector[CfgGuideChart], annotations: Vector[Option[FudgSentence]],
    goldTrees: Vector[CcgTree]) = {

    val goldRootCounts = goldTrees.map(productionFinder.rootCounts).fold(Map.empty[Cat, Double])(_ |+| _).withDefaultValue(0.0)
    val goldBinyCounts = goldTrees.map(productionFinder.binyCounts).fold(Map.empty[Cat, Map[BinaryProd, Double]])(_ |+| _).mapVals(_.withDefaultValue(0.0)).withDefaultValue(Map.empty.withDefaultValue(0.0))
    val goldUnryCounts = goldTrees.map(productionFinder.unryCounts).fold(Map.empty[Cat, Map[UnaryProd, Double]])(_ |+| _).mapVals(_.withDefaultValue(0.0)).withDefaultValue(Map.empty.withDefaultValue(0.0))
    val goldTermCounts = goldTrees.map(productionFinder.termCounts).fold(Map.empty[Cat, Map[TermProd, Double]])(_ |+| _).mapVals(_.withDefaultValue(0.0)).withDefaultValue(Map.empty.withDefaultValue(0.0))

    val sentences: Vector[Vector[Word]] = guideCharts.map(_.words)
    val allWordSet: Set[TermProd] = (sentences.flatten.map(TermProd(_)) ++ goldTermCounts.values.flatMap(_.keys)).toSet

    //    val (guideCharts, newAnnotations) =
    //      (for ((sentence, annotation) <- (sentences zipSafe annotations)) yield {
    //        val gcWithAnnotation = initialGudeChartBuilder.build(sentence, annotation, tagdict)
    //        if (gcWithAnnotation.isDefined)
    //          gcWithAnnotation.map(_ -> annotation)
    //        else
    //          initialGudeChartBuilder.build(sentence, None, tagdict).map(_ -> None)
    //      }).flatten.unzip
    val newAnnotations = annotations

    val initialTrees = time("parse for initial trees", getInitialTrees(guideCharts, annotations, goldRootCounts, goldBinyCounts, goldUnryCounts, goldTermCounts, goldTrees))
    val (initRootDist, initKnownBinyProbs, initUnryDist, initTermDist, initProdMixes) =
      //time("resample params", 
      paramResampler.resampleParameters(initialTrees, goldRootCounts, goldBinyCounts, goldUnryCounts, goldTermCounts, allWordSet)
    //)

    //TreeViz.drawTree(initialTrees.head)

    val infCatPriorsDesc =
      fullAllowedCatSet.map { set =>
        set.mapTo(catPrior)
      }.getOrElse {
        time(f"infCatPrior.allAbove(minQ=default)", catPrior.allAbove(LogDouble(1e-8), maxSize = 8)).toSet ++ additionalKnownCats.mapTo(catPrior)
      }.toVector.desc

    val sampledTrees = iterate(sentences, annotations,
      initialTrees,
      initRootDist, initKnownBinyProbs, initUnryDist, initTermDist, initProdMixes,
      infCatPriorsDesc,
      goldRootCounts, goldBinyCounts, goldUnryCounts, goldTermCounts,
      allWordSet,
      Vector.empty, -burninIterations)
    sampledTrees
  }

  @tailrec private[this] def iterate(
    sentences: Vector[Vector[Word]], annotations: Vector[Option[FudgSentence]],
    currentTrees: Vector[CcgTree], // ParVector[CcgTree],
    rootDist: LogProbabilityDistribution[Cat],
    knownBinyProds: Map[Cat, (Map[BinaryProd, LogDouble], LogDouble)],
    unryDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd],
    termDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
    prodMixes: Map[Cat, (LogDouble, LogDouble, LogDouble)], // binyProdMix: LogDouble, unryProdMix: LogDouble, termProdMix: LogDouble, // guaranteed to sum to 1
    infCatPriorsDesc: Vector[(Cat, LogDouble)],
    goldRootCounts: Map[Cat, Double],
    goldBinyCounts: Map[Cat, Map[BinaryProd, Double]],
    goldUnryCounts: Map[Cat, Map[UnaryProd, Double]],
    goldTermCounts: Map[Cat, Map[TermProd, Double]],
    allWordSet: Set[TermProd],
    runningTrees: Vector[CcgTree],
    iteration: Int): // 
    Vector[CcgTree] = {

    //TreeViz.drawTree(currentTrees.head)

    if (iteration < samplingIterations) {
      val startTime = System.currentTimeMillis()

      //  var infCatPriorsDesc = Vector.empty[(Cat, LogDouble)]
      //  if (infCatPriorsDesc.isEmpty) infCatPriorsDesc = time(f"infCatPrior.allAbove(minQ=default)", infCatPrior.allAbove(LogDouble(1e-7), maxSize = 3).toVector.desc)
      //  //if (infCatPriorsDesc.isEmpty || minQs.min < infCatPriorsDesc.last._2) infCatPriorsDesc = time(f"infCatPrior.allAbove(minQ=${minQs.min})", infCatPrior.allAbove(minQs.min).toVector.desc)

      val prodDist: ConditionalLogProbabilityDistribution[Cat, Prod] = new ProdCcgDP(knownBinyProds, unryDist, termDist, prodMixes, catPrior)

      val resampledTrees = zipSafe(sentences, currentTrees, annotations).zipWithIndex.par.map {
        case ((sentence, currentTree, annotation), i) =>
          val brackets: Vector[(Int, Int)] = annotation.map(_.brackets.toVector).getOrElse(Vector.empty)
          val invalidSpans: Set[(Int, Int)] = Set.empty
          //println(f" sentence--> $sentence")
          //time(f"resample tree $i", 
          infTreeResampler.resampleTree(sentence, currentTree, brackets, invalidSpans, knownBinyProds, unryDist, termDist, prodMixes, infCatPriorsDesc, rootDist)
        //)
      }.seq

      println(f"iteration ${((if (iteration < 0) iteration else iteration + 1) + ":").padRight(4)} ${(System.currentTimeMillis() - startTime) / 1000.0}%.3f sec")

      val treesToEstimate = if (accumulate) (resampledTrees ++ runningTrees) else resampledTrees
      val (newRootDist, newKnownBinyProbs, newUnryDist, newTermDist, newProdMixes) =
        //time("resample params", 
        paramResampler.resampleParameters(treesToEstimate, goldRootCounts, goldBinyCounts, goldUnryCounts, goldTermCounts, allWordSet)
      //)

      iterate(sentences, annotations,
        resampledTrees,
        newRootDist, newKnownBinyProbs, newUnryDist, newTermDist, newProdMixes,
        infCatPriorsDesc,
        goldRootCounts, goldBinyCounts, goldUnryCounts, goldTermCounts,
        allWordSet,
        if (iteration >= 0) runningTrees ++ resampledTrees else Vector.empty, // add new trees during sampling iterations only
        iteration + 1)
    }
    else {
      println(f"MAX ITERATIONS REACHED")
      runningTrees
    }
  }

  def getInitialTrees(
    guideCharts: Vector[CfgGuideChart], annotations: Vector[Option[FudgSentence]],
    goldRootCounts: Map[Cat, Double],
    goldBinyCounts: Map[Cat, Map[BinaryProd, Double]],
    goldUnryCounts: Map[Cat, Map[UnaryProd, Double]],
    goldTermCounts: Map[Cat, Map[TermProd, Double]],
    goldTrees: Vector[CcgTree]) = {

    val priorRootDist = catPrior
    val priorBinyDist = new BinaryCondPriorDist(catPrior)
    val priorUnryDist = new UnaryCondPriorDist(catPrior)

    val (alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts) =
      alphaPriorMaker.makeAll(guideCharts, goldTrees,
        priorRootDist, priorBinyDist, priorUnryDist, termPrior,
        alphaRoot, alphaBiny, alphaUnry, alphaTerm)

    val allProdHeadCats = alphaPriorBinyCounts.keySet | alphaPriorUnryCounts.keySet | alphaPriorTermCounts.keySet
    val initRootDist = new SimpleLogProbabilityDistribution(alphaPriorRootCounts)
    val initProdDist = new SimpleConditionalLogProbabilityDistribution(allProdHeadCats.mapTo { cat =>
      val binyDist = new SimpleLogProbabilityDistribution(alphaPriorBinyCounts.getOrElse(cat, Map.empty))
      val unryDist = new SimpleLogProbabilityDistribution(alphaPriorUnryCounts.getOrElse(cat, Map.empty))
      val termDist = new SimpleLogProbabilityDistribution(alphaPriorTermCounts.getOrElse(cat, Map.empty))
      new IPDU(binyDist, unryDist, termDist, LogDouble(priorBinyProdMix), LogDouble(priorUnryProdMix), LogDouble(priorTermProdMix))
    }.toMap)
    val initPcfgParser = initialParserInstantiater(initRootDist, initProdDist)
    val trees = guideCharts.map { gc =>
      val (t, p) = initPcfgParser.parseAndProbKBestFromGuideChart(gc, k = 1).only
      assert(p.nonZero, "Sentence parsed with zero probability")
      t
    }
    trees
  }

  //  /**
  //   * memoizingProdProbs = Map(* -> MemoizingDP(counts, LogProbabilityDistribution[BinaryProd]))
  //   */
  //  private[this] def getSupertagSets(t: CcgTree,
  //    memoizingRootProbs: MemoizingDP[Cat],
  //    memoizingProdProbs: Map[Cat, MemoizingDP[Prod]], defaultMemoizingProdProbs: => MemoizingDP[Prod],
  //    rand: RandomGenerator) = {
  //
  //    var infCatPriorsDesc = Vector[(Cat, LogDouble)]()
  //
  //    def f2(t: CcgTree, i: Int, j: Int, tsToUse: Set[Cat]): Vector[(Int, Set[Cat])] = {
  //      t match {
  //        case CcgBinode(cat, l, r) =>
  //          val prevProd = BinaryProd(l.cat, r.cat)
  //          val q = LogDouble(rand.nextDouble()) * newProdProbs(t.cat)(prevProd)
  //          println(f"q=$q")
  //          if (q < infCatPriorsDesc.last._2) infCatPriorsDesc = infCatPrior.allAbove(q).toVector.desc
  //          tsToUse.mapTo { t =>
  //            val knownProdsToUse =
  //              for {
  //                probs <- newProdProbs.get(t)
  //                (uvProd @ BinaryProd(u, v), uvp) <- probs if uvp > q
  //              } yield {
  //                uvProd
  //              }
  //
  //            for {
  //              (u, up) <- infCatPriorsDesc if up > q
  //              (v, vp) <- infCatPriorsDesc if up * vp > q
  //              uvProd = BinaryProd(u, v)
  //            } {
  //              newProdProbs.get(t).map { probs =>
  //                probs.getOrElseUpdate(uvProd, prodZ * prodPrior(uvProd, t) / (LogDouble.one - probs.keys.sumBy(prodPrior(_, t))))
  //              }.getOrElse(prodPrior(uvProd, t))
  //            }
  //          }
  //
  //          val ly = l match {
  //            case CcgLeaf(_, _, _) => Vector.empty
  //            case _ => f2(l, i, i + l.length)
  //          }
  //          val ry = r match {
  //            case CcgLeaf(_, _, _) => Vector.empty
  //            case _ => f2(r, i + l.length, j)
  //          }
  //          val p = prodDist(BinaryProd(l.cat, r.cat), cat)
  //          ly ++ ry ++ Vector(lx.option((i, Some(true), p)), rx.option((j - 1, Some(false), p))).flatten
  //        case CcgUnode(cat, s) =>
  //          s match {
  //            case CcgLeaf(_, _, _) => Vector((i, None, prodDist(UnaryProd(s.cat), cat)))
  //            case _ => f2(s, i, j)
  //          }
  //        case CcgLeaf(cat, word, _) => ???
  //      }
  //    }
  //
  //    val qRoot = LogDouble(rand.nextDouble()) * newRootProbs(t.cat)
  //    val rootsToUse = rootSet.filter { s =>
  //      val p = newRootProbs.getOrElseUpdate(s, rootZ * rootPrior(s) / (LogDouble.one - newRootProbs.keys.sumBy(rootPrior(_))))
  //      p >= qRoot
  //    }
  //    f2(t, 0, t.length, rootsToUse)))
  //
  //  }

  override def toString = f"McmcInfPcfg(samplingIterations=${samplingIterations}, burninIterations=${burninIterations}"
}

class ProdCcgDP(
  knownBinyProds: Map[Cat, (Map[BinaryProd, LogDouble], LogDouble)],
  unryDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd],
  termDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
  prodMixes: Map[Cat, (LogDouble, LogDouble, LogDouble)],
  catPrior: LogProbabilityDistribution[Cat])
    extends ConditionalLogProbabilityDistribution[Cat, Prod] {

  def apply(prod: Prod, t: Cat): LogDouble = {
    val (binyProdMix, unryProdMix, termProdMix) = prodMixes(t)
    val prodMixSum = binyProdMix + unryProdMix + termProdMix
    prod match {
      case prod @ BinaryProd(u, v) =>
        knownBinyProds.get(t)
          .map {
            case (knownProdProbs, scaleFactor) => //                 t is known:
              knownProdProbs.getOrElse(
                prod, //                                                 <u,v> is known, return the directly-sampled probability
                scaleFactor * (catPrior(u) * catPrior(v))) //      <u,v> is unknown, fall back to (scaled) cat prior;
          }
          .getOrElse { catPrior(u) * catPrior(v) } * //        t is unknown, just use cat prior
          binyProdMix / prodMixSum
      case prod @ UnaryProd(u) => unryDist(prod, t) * unryProdMix / prodMixSum
      case prod @ TermProd(w) => termDist(prod, t) * termProdMix / prodMixSum
    }
  }

  def sample(given: Cat): Prod = ???
}

//
//
//

object McmcInfPcfg {

  val A: AtomCat = cat"A".asInstanceOf[AtomCat]
  val B: AtomCat = cat"B".asInstanceOf[AtomCat]
  val C: AtomCat = cat"C".asInstanceOf[AtomCat]
  val D: AtomCat = cat"D".asInstanceOf[AtomCat]
  val E: AtomCat = cat"E".asInstanceOf[AtomCat]
  val F: AtomCat = cat"F".asInstanceOf[AtomCat]
  //val X: Cat = cat"X".asInstanceOf[AtomCat]

  val s: AtomCat = cat"S".asInstanceOf[AtomCat]
  val n: AtomCat = cat"N".asInstanceOf[AtomCat]
  val np: AtomCat = cat"NP".asInstanceOf[AtomCat]
  val pp: AtomCat = cat"PP".asInstanceOf[AtomCat]
  val STA: Cat = cat"<S>"
  val END: Cat = cat"<E>"
  val SE = new SimpleStartEndTags(STA, END)

  val startWord = "<S>"
  val startTag = STA
  val endWord = "<E>"
  val endTag = END

  def main(args: Array[String]): Unit = {
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

    val sentences: Vector[Vector[(Word, Set[Cat])]] = Vector(Vector("the", "dog", "walks").mapTo(tagdict))

    val rootSet: Set[Cat] = Set(s)
    val binaryRules: Set[BinaryCcgRule] = Set(FA, BA)
    val unaryRules: Set[UnaryCcgRule] = Set(N2NP)
    val gcBuilder = new SimpleCfgGuideChartBuilder((binaryRules.toVector ++ unaryRules), additionalSupertagAdder = new NoOpAdditionalTagAdder, rootSet, allowTerminalDeletion = false)
    val rawDataGC = {
      val gcs = sentences.flatMap(s => gcBuilder.buildFromSupertagSetSentence(s, None, tagdict))
      gcs
      //Vector.fill(10)(gcs).flatten
    }

    val infCatPrior = new MemoizingInfCatPrior(new SimpleInfCatPrior(
      allPunc = Set(cat".", cat",", cat";", cat":", cat"LRB", cat"RRB").map(_.asInstanceOf[PuncCat]), puncDist = new SimpleLogProbabilityDistribution(Map(
        cat"," -> 0.50,
        cat"." -> 0.45,
        cat":" -> 0.02,
        cat";" -> 0.01,
        cat"LRB" -> 0.01,
        cat"RRB" -> 0.01).mapt((c, p) => c.asInstanceOf[PuncCat] -> LogDouble(p)).normalizeValues),
      pPunc = 0.1,
      allAtoms = Set(s, n, np, pp), pAtom = new SimpleLogProbabilityDistribution(Map(
        s -> 0.20,
        n -> 0.40,
        np -> 0.35,
        pp -> 0.05).mapt((c, p) => c.asInstanceOf[AtomCat] -> LogDouble(p)).normalizeValues),
      pTerm = 0.8, pMod = 0.1, pFwd = 0.5,
      tagdict.excludedTags))

    //

    val pcfgProductionFinder = new SimplePcfgProductionCounter()
    val pcfgGuideChartProdFinder = new SimplePcfgGuideChartProdFinder()
    val pcfgAlphaPriorMaker = new TrainDataPcfgAlphaPriorMaker(pcfgProductionFinder, pcfgGuideChartProdFinder)
    val pcfgTreeSampler = new SimplePcfgTreeSampler(new SimplePcfgInsideChartBuilder())
    val pcfgParserInstantiater = new ExactPcfgParserInstantiater()

    //
    val termPrior = new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, TermProd](new UniformDefaultLogProbabilityDistribution(LogDouble(1.0 / (rawDataGC.flatMap(pcfgGuideChartProdFinder.terms(_).values).flatten.map(_.word).toSet | tagdict.allWords).size)))

    //
    val accumulate = false

    val alpha = 1e0

    val rand = new SynchronizedRandomGenerator(new MersenneTwister)

    val infMcmcPcfg = new McmcInfPcfg(
      samplingIterations = 50,
      burninIterations = 1,
      pcfgAlphaPriorMaker,
      pcfgProductionFinder,
      initialParserInstantiater = pcfgParserInstantiater,
      DirSampler,
      /*priorRootDist, priorBinyDist, priorUnryDist,*/ termPrior,
      rootSet,
      binaryRules, unaryRules,
      alphaRoot = alpha, alphaBiny = alpha, alphaUnry = alpha, alphaTerm = alpha,
      alphaLambda = 3.0, priorBinyProdMix = 1.0 / 3, priorUnryProdMix = 1.0 / 3, priorTermProdMix = 1.0 / 3,
      qBetaA = LogDouble(0.1), qBetaB = LogDouble.one,
      gcBuilder,
      pcfgGuideChartProdFinder,
      //supPcfgTrainer = new UnsmoothedSupPcfgTrainer(pcfgProductionFinder, pcfgParserInstantiater),
      infCatPrior,
      rand,
      accumulate = accumulate)

    val infmcmcparser: PcfgParser = infMcmcPcfg.trainGuideChartsWithSomeGold(rawDataGC, rawDataGC.map(_ => None), Vector.empty).asInstanceOf[PcfgParser]
    val msd = infmcmcparser.rootDist
    val mpd = infmcmcparser.prodDist

    //    val pem = new DumbPcfgEm(100)
    //    val (pesc, pepc) = pem.trainFromInit(rawDataGC, priorRootDist, priorProdDist)
    //    val pesd = new SimpleLogProbabilityDistribution(pesc)
    //    val pepd = new SimpleConditionalLogProbabilityDistribution(pepc.mapVals(new SimpleLogProbabilityDistribution(_)))
    //
    //    val dumbMcmcPcfg = new DumbPcfgMcmc(
    //      samplingIterations = 1000,
    //      burninIterations = 100,
    //      pcfgTreeSampler,
    //      alphaRoot = alpha, alphaProd = alpha,
    //      accumulate)
    //    val dumbMcmcPcfgParser: PcfgParser = dumbMcmcPcfg.train(rawDataGC, priorRootDist, priorProdDist)
    //    val dpmsd = dumbMcmcPcfgParser.rootDist
    //    val dpmpd = dumbMcmcPcfgParser.prodDist
    //
    //    val mcmcPcfg = new McmcPcfg(
    //      samplingIterations = 1000,
    //      burninIterations = 100,
    //      pcfgAlphaPriorMaker,
    //      pcfgProductionFinder,
    //      initialParserInstantiater = pcfgParserInstantiater,
    //      DirSampler,
    //      pcfgTreeSampler,
    //      priorRootDist, priorBinyDist, priorUnryDist, termPrior,
    //      alphaRoot = alpha, alphaBiny = alpha, alphaUnry = alpha, alphaTerm = alpha,
    //      alphaLambda = 3.0, priorBinyProdMix = 1.0 / 3, priorUnryProdMix = 1.0 / 3, priorTermProdMix = 1.0 / 3,
    //      supPcfgTrainer = new UnsmoothedSupPcfgTrainer(pcfgProductionFinder, pcfgParserInstantiater),
    //      accumulate = accumulate)
    //    val mcmcPcfgParser: PcfgParser = mcmcPcfg.trainGuideChartsWithSomeGold(rawDataGC, Vector.empty).asInstanceOf[PcfgParser]
    //    val pmsd = mcmcPcfgParser.rootDist
    //    val pmpd = mcmcPcfgParser.prodDist
    //
    //    //
    //
    //    println(f"${" " * 35}${"pcfg dumb-em cnt"}%-15s${"inf pcfg-mcmc"}%-15s${"dumb pcfg-em"}%-15s${"dumb pcfg-mcmc"}%-15s${"pcfg-mcmc"}%-15s")
    //    println("\nRoots"); /*                                                                                       */ for ((y, c) <- pesc.toVector.sortBy(_._1.toString)) { println( /*     */ f"  $y -> ".padRight(35) + f"${c.toDouble}%.4f       ${msd(y).toDouble}%.4f         ${pesd(y).toDouble}%.4f         ${dpmsd(y).toDouble}%.4f         ${pmsd(y).toDouble}%.4f") }
    //    println("\nProds"); for ((x, ys) <- pepc.toVector.sortBy(_._1.toString) if ys.nonEmpty) { println(f"  $x -> "); for ((y, c) <- ys.toVector.sortBy(_._1.toString)) { println( /**/ f"         $y -> ".padRight(35) + f"${c.toDouble}%.4f       ${mpd(y, x).toDouble}%.4f         ${pepd(y, x).toDouble}%.4f         ${dpmpd(y, x).toDouble}%.4f         ${pmpd(y, x).toDouble}%.4f".padLeft(30)) } }

  }
}

//

trait InfTreeResampler {
  type Word = String
  def resampleTrees(
    currentTrees: Vector[CcgTree], // ParVector[CcgTree],    
    knownRootProds: Map[Cat, LogDouble],
    knownBinyProds: Map[Cat, Map[BinaryProd, LogDouble]],
    knownUnryProds: Map[Cat, Map[UnaryProd, LogDouble]],
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    prodMixes: Map[Cat, (LogDouble, LogDouble, LogDouble)], // binyProdMix: LogDouble, unryProdMix: LogDouble, termProdMix: LogDouble, // guaranteed to sum to 1
    infCatPriorsDesc: Vector[(Cat, LogDouble)],
    binyScaleFactors: Map[Cat, LogDouble],
    unryScaleFactors: Map[Cat, LogDouble],
    sentences: Vector[Vector[Word]],
    annotations: Vector[Option[FudgSentence]]): Vector[CcgTree]
}
