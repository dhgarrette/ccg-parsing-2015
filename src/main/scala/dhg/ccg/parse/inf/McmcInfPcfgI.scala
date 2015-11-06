//package dhg.ccg.parse.inf
//
//import scala.annotation.tailrec
//import scala.collection.immutable.BitSet
//import scala.collection.mutable.{ BitSet => MBitSet }
//import scala.collection.mutable.{ Map => MMap }
//import scala.collection.mutable.{ Set => MSet }
//import dhg.ccg.cat._
//import dhg.ccg.rule._
//import dhg.ccg.prob._
//import dhg.ccg.util._
//import dhg.ccg.tagdict.SimpleTagDictionary
//import dhg.ccg.tagdict.TagDictionary
//import dhg.util._
//import scalaz._
//import Scalaz._
//import dhg.ccg.parse._
//import dhg.ccg.parse.pcfg._
//import dhg.ccg.parse.pcfg.mcmc._
//import org.apache.commons.math3.random.RandomGenerator
//import dhg.ccg.math._
//import dhg.ccg.math.Util._
//import dhg.ccg.tagdict.StartEndTags
//import dhg.ccg.tagdict.SimpleStartEndTags
//import scala.collection.parallel.immutable.ParVector
//import dhg.ccg.math.DirichletSampler
//import dhg.ccg.parse.dep.ParserEvaluator
//import dhg.gfl.FudgSentence
//import dhg.ccg.math.SimpleDirichletSampler
//import breeze.stats.distributions.Rand
//import scala.util.control.Breaks._
//import org.apache.commons.math3.random.MersenneTwister
//import org.apache.commons.math3.random.SynchronizedRandomGenerator
//import dhg.util.viz._
//import scala.collection.mutable.ArrayBuffer
//
//class McmcInfPcfgI(
//    samplingIterations: Int,
//    burninIterations: Int,
//    //    infTreeResampler: SlicingInfTreeResamplerI,
//    paramResampler: ParamResamplerI,
//    alphaPriorMaker: PcfgAlphaPriorMaker,
//    productionFinder: PcfgProductionCounterI,
//    initialParserInstantiater: PcfgParserInstantiater,
//    dirSampler: DirichletSampler,
//    priorTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
//    alphaRoot: Double, alphaBiny: Double, alphaUnry: Double, alphaTerm: Double,
//    alphaLambda: Double, priorBinyProdMix: Double, priorUnryProdMix: Double, priorTermProdMix: Double,
//    initialGudeChartBuilder: CfgGuideChartBuilder, // should be forward-backward adding
//    guideChartProdFinder: PcfgGuideChartProdFinder,
//    supPcfgTrainer: SupParserTrainer,
//    infCatPrior: InfCatPrior,
//    rootSet: Set[Cat],
//    binaryRuleSet: Set[BinaryCcgRule],
//    unaryRuleSet: Set[UnaryCcgRule],
//    qBetaA: LogDouble, qBetaB: LogDouble,
//    rand: RandomGenerator,
//    accumulate: Boolean = false,
//    verbose: Boolean = false) {
//  type Word = String
//  type WordI = Int
//  type CatI = Int
//  type CatSet = BitSet
//  type MCatSet = MBitSet
//  type CatMap[A] = Vec[A]
//  type SortedCatMap[A] = OrderedIndirectSparseVec[A]
//
//  def trainGuideChartsWithSomeGold(
//    guideCharts: Vector[CfgGuideChart], annotations: Vector[Option[FudgSentence]],
//    goldTrees: Vector[CcgTree]): GuideChartParser = {
//
//    val generatedCats = time(f"infCatPrior.allAbove(minQ=default)", infCatPrior.allAbove(LogDouble(1e-7), maxSize = 3).toMap)
//
//    trainGuideChartsWithSomeGold(guideCharts, annotations, goldTrees, generatedCats)
//  }
//
//  def trainGuideChartsWithSomeGold(
//    guideCharts: Vector[CfgGuideChart], annotations: Vector[Option[FudgSentence]],
//    goldTrees: Vector[CcgTree],
//    generatedCats: Map[Cat, LogDouble]): GuideChartParser = {
//
//    val knownCats = (guideCharts.flatMap(gc => gc.matrix.flatMap(_.flatMap(_.keys))) ++ goldTrees.flatMap(_.supertags)).toSet
//    val catPriorsMap = generatedCats ++ (generatedCats.keySet -- knownCats).mapTo(infCatPrior)
//    val allCats = catPriorsMap.toVector.sortBy(_._2).reverse.map(_._1)
//    val catIndexer = SimpleIndexer(allCats)
//
//    val sentences: Vector[Vector[Word]] = guideCharts.map(_.words)
//    val wordIndexer = new SimpleIndexer((sentences ++ goldTrees.map(_.words)).flatten.distinct.sorted)
//
//    val unaryRules: CatMap[CatSet] = IndirectSparseVec((for { r <- unaryRuleSet; ci <- catIndexer.getIndex(r.child); pi <- catIndexer.getIndex(r.parent) } yield (ci -> pi)).groupByKey.mapVals(BitSet() ++ _))
//    val inferRight: Array[Array[CatSet]] = { //  Map[ParentCat, Map[LeftCat, Set[RightCat]]]
//      allCats.zipWithIndex.mapt { (t, ti) =>
//        allCats.zipWithIndex.mapt { (u, ui) =>
//          BitSet.empty ++ binaryRuleSet.flatMap { r => r.inferRight(t, u).map(catIndexer.get) }.flatten
//        }.toArray
//      }.toArray
//    }
//
//    val goldTreesI: Vector[CcgTreeI] = goldTrees.map(t => CcgTreeI(t, catIndexer, wordIndexer))
//
//    //    val tagdict = initialTagdict.withWords(sentences.flatten.toSet | goldTrees.flatMap(_.words).toSet).withTags(goldTrees.flatMap(_.supertags).toSet)
//
//    //    val (guideCharts, newAnnotations) =
//    //      (for ((sentence, annotation) <- (sentences zipSafe annotations)) yield {
//    //        val gcWithAnnotation = initialGudeChartBuilder.build(sentence, annotation, tagdict)
//    //        if (gcWithAnnotation.isDefined)
//    //          gcWithAnnotation.map(_ -> annotation)
//    //        else
//    //          initialGudeChartBuilder.build(sentence, None, tagdict).map(_ -> None)
//    //      }).flatten.unzip
//    val newAnnotations = annotations
//
//    val initialTrees: Vector[CcgTreeI] = {
//      val priorRootDist: LogProbabilityDistribution[Cat] = infCatPrior
//      val priorBinyDist: ConditionalLogProbabilityDistribution[Cat, BinaryProd] = new ConditionalLogProbabilityDistribution[Cat, BinaryProd] {
//        def apply(x: BinaryProd, given: Cat): LogDouble = infCatPrior(x.left) * infCatPrior(x.right)
//        def sample(given: Cat): BinaryProd = ???
//      }
//      val priorUnryDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd] = new ConditionalLogProbabilityDistribution[Cat, UnaryProd] {
//        def apply(x: UnaryProd, given: Cat): LogDouble = infCatPrior(x.sub)
//        def sample(given: Cat): UnaryProd = ???
//      }
//      val (alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts) = {
//        alphaPriorMaker.makeAll(guideCharts, goldTrees,
//          priorRootDist, priorBinyDist, priorUnryDist, priorTermDist,
//          alphaRoot = 1.0, alphaBiny = 1.0, alphaUnry = 1.0, alphaTerm = 1.0)
//      }
//      val allProdHeadCats = alphaPriorBinyCounts.keySet | alphaPriorUnryCounts.keySet | alphaPriorTermCounts.keySet
//      val initRootDist = new SimpleLogProbabilityDistribution(alphaPriorRootCounts)
//      val initProdDist = new SimpleConditionalLogProbabilityDistribution(allProdHeadCats.mapTo { cat =>
//        val binyDist = new SimpleLogProbabilityDistribution(alphaPriorBinyCounts.getOrElse(cat, Map.empty))
//        val unryDist = new SimpleLogProbabilityDistribution(alphaPriorUnryCounts.getOrElse(cat, Map.empty))
//        val termDist = new SimpleLogProbabilityDistribution(alphaPriorTermCounts.getOrElse(cat, Map.empty))
//        new IPDU(binyDist, unryDist, termDist, binyProdMix = LogDouble(1 / 3.0), unryProdMix = LogDouble(1 / 3.0), termProdMix = LogDouble(1 / 3.0))
//      }.toMap)
//      val initPcfgParser = initialParserInstantiater(initRootDist, initProdDist)
//      guideCharts.map { gc =>
//        val (t, p) = initPcfgParser.parseAndProbKBestFromGuideChart(gc, k = 1).only
//        assert(p.nonZero, "Sentence parsed with zero probability")
//        CcgTreeI(t, catIndexer, wordIndexer)
//      }
//    }
//
//    //val termPrior: Array[LogDouble] = { val uniformP = LogDouble(1.0 / wordIndexer.size); Vec.empty[Array[LogDouble]].withDefaultValue(Array.fill(wordIndexer.size)(uniformP)) } // known cats -> words -> prior prob 
//    val termPrior: Array[LogDouble] = { val uniformP = LogDouble(1.0 / wordIndexer.size); Array.fill(wordIndexer.size)(uniformP) } // known cats -> words -> prior prob 
//
//    val goldRootCounts = goldTreesI.map(productionFinder.rootCounts).fold(Map.empty[CatI, /*                */ Double])(_ |+| _).withDefaultValue(0.0)
//    val goldBinyCounts = goldTreesI.map(productionFinder.binyCounts).fold(Map.empty[CatI, Map[BinaryProdI, /**/ Double]])(_ |+| _).mapVals(_.withDefaultValue(0.0)).withDefaultValue(Map.empty.withDefaultValue(0.0))
//    val goldUnryCounts = goldTreesI.map(productionFinder.unryCounts).fold(Map.empty[CatI, Map[UnaryProdI, /* */ Double]])(_ |+| _).mapVals(_.withDefaultValue(0.0)).withDefaultValue(Map.empty.withDefaultValue(0.0))
//    val goldTermCounts = goldTreesI.map(productionFinder.termCounts).fold(Map.empty[CatI, Map[TermProdI, /*  */ Double]])(_ |+| _).mapVals(_.withDefaultValue(0.0)).withDefaultValue(Map.empty.withDefaultValue(0.0))
//
//    val catPrior = catIndexer.objects.toArray.map(catPriorsMap)
//
//    val infTreeResampler =
//      new SlicingInfTreeResamplerI(
//        //canCombine: Array[BitSet], //       Map[LeftCat, Set[RightCat]]
//        //binaryRules: Array[Vec[Int]], //    Map[LeftCat, Map[RightCat, ParentCat]]
//        unaryRules: Vec[BitSet], //           Map[ChildCat, Set[ParentCat]]
//        inferRight: Array[Array[BitSet]], //  Map[ParentCat, Map[LeftCat, Set[RightCat]]]
//        catPrior: Array[LogDouble], //        InfCatPrior,  
//        rootSet = BitSet.empty ++ rootSet.map(catIndexer), // Set[Cat],
//        dirSampler: DirichletSampler,
//        qBetaA: LogDouble, qBetaB: LogDouble,
//        rand: RandomGenerator)
//
//    @tailrec def iterate(
//      sentences: Vector[Array[WordI]], annotations: Vector[Option[FudgSentence]],
//      currentTrees: Vector[CcgTreeI], // ParVector[CcgTree],
//      rootDist: CatMap[LogDouble], // LogProbabilityDistribution[Cat],
//      knownBinyProbs: CatMap[CatMap[CatMap[LogDouble]]], // Map[Cat, Map[BinaryLeft, Map[BinaryRight, LogDouble]]], // Map[Cat, Map[BinaryProd, LogDouble]],
//      unaryDist: CatMap[CatMap[LogDouble]], // Map[Cat, Map[SubCat, LogDouble]] // has prior means as defaults
//      termDist: CatMap[Array[LogDouble]], // Map[Cat, Map[Word, LogDouble]] // has prior means as defaults
//      prodMixes: CatMap[(LogDouble, LogDouble, LogDouble)], // has prior means as defaults
//      catPrior: Array[LogDouble],
//      runningTrees: Vector[CcgTreeI],
//      iteration: Int): // 
//      Vector[CcgTreeI] = {
//
//      if (iteration < samplingIterations) {
//        val startTime = System.currentTimeMillis()
//
//        val binyScaleFactors = getBinyScaleFactors(knownBinyProbs, catPrior)
//        val resampledTrees =
//          zipSafe(sentences, currentTrees, annotations).mapt { (sentence, currentTree, fudgAnnotation) =>
//            val brackets = fudgAnnotation.map(_.brackets.toVector).getOrElse(Vector.empty)
//            val invalidSpans: Set[(Int, Int)] = Set.empty
//            infTreeResampler.resampleTree(sentence, currentTree, brackets, invalidSpans, knownBinyProbs, binyScaleFactors, unaryDist, termDist, prodMixes, rootDist)
//          }
//
//        println(f"iteration ${((if (iteration < 0) iteration else iteration + 1) + ":").padRight(4)} ${(System.currentTimeMillis() - startTime) / 1000.0}%.3f sec")
//
//        val treesToEstimate: Vector[CcgTreeI] = if (accumulate) (resampledTrees ++ runningTrees) else (resampledTrees)
//        val estRootCounts = (treesToEstimate.map(productionFinder.rootCounts).reduce(_ |+| _) |+| goldRootCounts).withDefaultValue(0.0)
//        val estBinyCounts = (treesToEstimate.map(productionFinder.binyCounts).reduce(_ |+| _) |+| goldBinyCounts).mapVals(_.withDefaultValue(0.0)).withDefaultValue(Map.empty.withDefaultValue(0.0))
//        val estUnryCounts = (treesToEstimate.map(productionFinder.unryCounts).reduce(_ |+| _) |+| goldUnryCounts).mapVals(_.withDefaultValue(0.0)).withDefaultValue(Map.empty.withDefaultValue(0.0))
//        val estTermCounts = (treesToEstimate.map(productionFinder.termCounts).reduce(_ |+| _) |+| goldTermCounts).mapVals(_.withDefaultValue(0.0)).withDefaultValue(Map.empty.withDefaultValue(0.0))
//
//        val (newRootDist, newKnownBinyProbs, newUnryDist, newTermDist, newProdMixes) = paramResampler.resampleParameters(estRootCounts, estBinyCounts, estUnryCounts, estTermCounts, catPrior, termPrior, unaryRules, catIndexer)
//
//        iterate(sentences, annotations,
//          resampledTrees,
//          newRootDist,
//          newKnownBinyProbs,
//          newUnryDist,
//          newTermDist,
//          newProdMixes,
//          catPrior,
//          if (iteration >= 0) runningTrees ++ resampledTrees else Vector.empty, // add new trees during sampling iterations only
//          iteration + 1)
//      }
//      else {
//        println(f"MAX ITERATIONS REACHED")
//        runningTrees
//      }
//    }
//
//    val (initRootDist, initKnownBinyProbs, initUnryDist, initTermDist, initProdMixes) = paramResampler.resampleParameters(goldRootCounts, goldBinyCounts, goldUnryCounts, goldTermCounts, catPrior, termPrior, unaryRules, catIndexer)
//    val sampledTrees = iterate(sentences.map(_.toArray.map(wordIndexer)), newAnnotations,
//      initialTrees /*.PARALLEL*/ ,
//      initRootDist, initKnownBinyProbs, initUnryDist, initTermDist, initProdMixes,
//      catPrior,
//      Vector.empty, -burninIterations)
//    supPcfgTrainer.train(sampledTrees.map(CcgTreeI.toCcgTree(_, catIndexer, wordIndexer)))
//  }
//
//  def getBinyScaleFactors(
//    knownBinyProbs: CatMap[CatMap[CatMap[LogDouble]]], // Map[Cat, Map[BinaryLeft, Map[BinaryRight, LogDouble]]], // Map[Cat, Map[BinaryProd, LogDouble]],
//    catPrior: Array[LogDouble] // InfCatPrior,
//    ): CatMap[LogDouble] = {
//    val ts = knownBinyProbs.keys
//    val values = knownBinyProbs.values
//    var ti = 0
//    val tLen = ts.length
//    val resultValues = new Array[LogDouble](tLen)
//    while (ti < tLen) {
//      var logToSum = new ArrayBuffer[Double]
//
//      val uMap = values(ti)
//      val us = uMap.keys
//      val vMaps = uMap.values
//      var ui = 0
//      val uLen = us.length
//      while (ui < uLen) {
//        val u = us(ui)
//        val uP: LogDouble = catPrior(u)
//        val vs = vMaps(ui).keys
//        var vi = 0
//        val vLen = vs.length
//        while (vi < vLen) {
//          val v = vs(vi)
//          val vP: LogDouble = catPrior(v)
//          logToSum += (uP * vP).logValue
//          vi += 1
//        }
//        ui += 1
//      }
//      resultValues(ti) = new LogDouble(logSum(logToSum.toArray, logToSum.length))
//
//      ti += 1
//    }
//    IndirectSparseVec(ts, resultValues)
//  }
//
//  override def toString = f"McmcInfPcfg(samplingIterations=${samplingIterations}, burninIterations=${burninIterations}"
//}
//
////
////
////
//
//object McmcInfPcfgI {
//
//  val A: AtomCat = cat"A".asInstanceOf[AtomCat]
//  val B: AtomCat = cat"B".asInstanceOf[AtomCat]
//  val C: AtomCat = cat"C".asInstanceOf[AtomCat]
//  val D: AtomCat = cat"D".asInstanceOf[AtomCat]
//  val E: AtomCat = cat"E".asInstanceOf[AtomCat]
//  val F: AtomCat = cat"F".asInstanceOf[AtomCat]
//  //val X: Cat = cat"X".asInstanceOf[AtomCat]
//
//  val S: AtomCat = cat"S".asInstanceOf[AtomCat]
//  val NP: Cat = cat"NP".asInstanceOf[AtomCat]
//  val N: Cat = cat"N".asInstanceOf[AtomCat]
//  val PP: Cat = cat"PP".asInstanceOf[AtomCat]
//  val STA: Cat = cat"<S>"
//  val END: Cat = cat"<E>"
//  val SE = new SimpleStartEndTags(STA, END)
//
//  val s: Cat = cat"S".asInstanceOf[AtomCat]
//  val np: Cat = cat"NP".asInstanceOf[AtomCat]
//  val n: Cat = cat"N".asInstanceOf[AtomCat]
//  val pp: Cat = cat"PP".asInstanceOf[AtomCat]
//
//  val startWord = "<S>"
//  val startTag = STA
//  val endWord = "<E>"
//  val endTag = END
//
//  def main(args: Array[String]): Unit = {
//    type Word = String
//    type Tag = Cat
//
//    val Det: Set[Cat] = Set(
//      np / n)
//    val Adj: Set[Cat] = Set(
//      n / n)
//    val IV: Set[Cat] = Set(
//      s \ np,
//      (s \ np) / pp)
//    val TV: Set[Cat] = Set(
//      (s \ np) / np,
//      ((s \ np) / pp) / np,
//      (((s \ np) / pp) / pp) / np)
//    val N: Set[Cat] = Set(
//      n)
//    val NNP: Set[Cat] = Set(
//      np,
//      np / pp,
//      (np / pp) / pp)
//    val Prep: Set[Cat] = Set(
//      pp / np)
//
//    val tagdict = SimpleTagDictionary.apply(
//      Map[Word, Set[Tag]](
//        "the" -> Det,
//        "a" -> Det,
//        "big" -> Adj,
//        "man" -> N,
//        "dog" -> N,
//        "dogs" -> N,
//        "cat" -> N,
//        "cats" -> N,
//        "telescope" -> N,
//        "saw" -> (IV | TV),
//        "walked" -> (IV | TV),
//        "chase" -> TV,
//        "run" -> IV,
//        "ran" -> IV,
//        "John" -> NNP,
//        "Mary" -> NNP,
//        "with" -> Prep,
//        "nnp" -> Set(n, np)),
//      "<S>", cat"<S>", "<E>", cat"<E>")
//
//    val B1 = (B / C) \ A
//    val B2 = (B \ A) / C
//    val X = C / D
//    val XS: Vector[(Word, Set[Cat])] = Vector(("s", Set(S / E)), ("x", Set(B / C)), ("y", Set(X)), ("z", Set((E \ (B / C)) \ X)))
//    val sentences: Vector[Vector[(Word, Set[Cat])]] = Vector[Vector[(Word, Set[Cat])]](
//      XS, XS, XS, XS, XS, XS, XS,
//      Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B1)), ("c", Set(C / D)), ("d", Set(D))),
//      Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B2)), ("c", Set(C / D)), ("d", Set(D))),
//      Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B1, B2)), ("c", Set(C / D)), ("d", Set(D))),
//      Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B1)), ("c", Set(C / D)), ("d", Set(D))))
//
//    //    val B1 = (B / C) \ A
//    //    val B2 = (B \ A) / C
//    //    val X = F//C / D
//    //    val XS: Vector[(Word, Set[Cat])] = Vector(("s", Set(S/E)), ("x", Set(B / C)), ("y", Set(X)), ("z", Set((E \ (B / C)) \ X)))
//    //    val sentences: Vector[Vector[(Word, Set[Cat])]] = Vector(
//    //      XS,XS,XS,XS,XS,XS,XS,
//    //      Vector(("s", Set(S/B)), ("a", Set(A)), ("b", Set(B1)), ("c", Set(C / D)), ("d", Set(D))),
//    //      Vector(("s", Set(S/B)), ("a", Set(A)), ("b", Set(B2)), ("c", Set(C / D)), ("d", Set(D))),
//    //      Vector(("s", Set(S/B)), ("a", Set(A)), ("b", Set(B1, B2)), ("c", Set(C / D)), ("d", Set(D))),
//    //      Vector(("s", Set(S/B)), ("a", Set(A)), ("b", Set(B1)), ("c", Set(C / D)), ("d", Set(D))))
//
//    val rootSet: Set[Cat] = Set(S)
//    val gcBuilder = new SimpleCfgGuideChartBuilder(Vector(FA, BA, N2NP), additionalSupertagAdder = new NoOpAdditionalTagAdder, rootSet)
//    val rawDataGC = {
//      val gcs = sentences.flatMap(s => gcBuilder.buildFromSupertagSetSentence(s, None, tagdict))
//      Vector.fill(10)(gcs).flatten
//    }
//
//    val infCatPrior = new SimpleInfCatPrior(
//      tagdict,
//      Set(S, A, B, C, D, E),
//      new SimpleLogProbabilityDistribution(Map(S -> LogDouble(1 / 6.0), A -> LogDouble(1 / 6.0), B -> LogDouble(1 / 6.0), C -> LogDouble(1 / 6.0), D -> LogDouble(1 / 6.0), E -> LogDouble(1 / 6.0))),
//      pTerm = 0.8, pMod = 0.1, pFwd = 0.5)
//
//    //
//
//    val pcfgProductionFinder = new SimplePcfgProductionCounter()
//    val pcfgGuideChartProdFinder = new SimplePcfgGuideChartProdFinder()
//    val pcfgAlphaPriorMaker = new TrainDataPcfgAlphaPriorMaker(pcfgProductionFinder, pcfgGuideChartProdFinder)
//    val pcfgTreeSampler = new SimplePcfgTreeSampler(new SimplePcfgInsideChartBuilder())
//    val pcfgParserInstantiater = new ExactPcfgParserInstantiater()
//
//    //
//    val priorRootDist: LogProbabilityDistribution[Cat] = infCatPrior
//    val priorBinyDist: ConditionalLogProbabilityDistribution[Cat, BinaryProd] = new ConditionalLogProbabilityDistribution[Cat, BinaryProd] { def apply(x: BinaryProd, given: Cat): LogDouble = infCatPrior(x.left) * infCatPrior(x.right); def sample(given: Cat): BinaryProd = ??? }
//    val priorUnryDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd] = new ConditionalLogProbabilityDistribution[Cat, UnaryProd] { def apply(x: UnaryProd, given: Cat): LogDouble = infCatPrior(x.sub); def sample(given: Cat): UnaryProd = ??? }
//    val priorTermDist = new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, TermProd](new UniformDefaultLogProbabilityDistribution(LogDouble(1.0 / (rawDataGC.flatMap(pcfgGuideChartProdFinder.terms(_).values).flatten.map(_.word).toSet | tagdict.allWords).size)))
//    val priorProdDist = new ICPDU(priorBinyDist, priorUnryDist, priorTermDist, Map.empty.withDefaultValue((LogDouble.one, LogDouble.one, LogDouble.one)))
//
//    //
//    val accumulate = false
//
//    val alpha = 1.0
//
//    val binaryRuleSet: Set[BinaryCcgRule] = Set(FA, BA)
//    val unaryRuleSet: Set[UnaryCcgRule] = Set(N2NP)
//    val rules = binaryRuleSet ++ unaryRuleSet
//    val rand = new SynchronizedRandomGenerator(new MersenneTwister)
//
//    val infMcmcPcfg = new McmcInfPcfgI(
//      samplingIterations = 1000,
//      burninIterations = 100,
//      //    infTreeResampler: SlicingInfTreeResamplerI,
//      paramResampler = new SimpleParamResamplerI(
//        DirSampler,
//        alphaRoot = alpha, alphaBiny = alpha, alphaUnry = alpha, alphaTerm = alpha,
//        alphaLambda = 3.0, priorBinyProdMix = 1.0 / 3, priorUnryProdMix = 1.0 / 3, priorTermProdMix = 1.0 / 3,
//        rootSet),
//      pcfgAlphaPriorMaker,
//      new SimplePcfgProductionCounterI(),
//      initialParserInstantiater = pcfgParserInstantiater,
//      DirSampler,
//      /*priorRootDist, priorBinyDist, priorUnryDist,*/ priorTermDist,
//      alphaRoot = alpha, alphaBiny = alpha, alphaUnry = alpha, alphaTerm = alpha,
//      alphaLambda = 3.0, priorBinyProdMix = 1.0 / 3, priorUnryProdMix = 1.0 / 3, priorTermProdMix = 1.0 / 3,
//      gcBuilder,
//      pcfgGuideChartProdFinder,
//      supPcfgTrainer = new UnsmoothedSupPcfgTrainer(pcfgProductionFinder, pcfgParserInstantiater),
//      infCatPrior,
//      rootSet,
//      binaryRuleSet: Set[BinaryCcgRule],
//      unaryRuleSet: Set[UnaryCcgRule],
//      qBetaA = LogDouble(0.1), qBetaB = LogDouble.one,
//      rand: RandomGenerator)
//
//    val infmcmcparser: PcfgParser = infMcmcPcfg.trainGuideChartsWithSomeGold(rawDataGC, rawDataGC.map(_ => None), Vector.empty).asInstanceOf[PcfgParser]
//    val msd = infmcmcparser.rootDist
//    val mpd = infmcmcparser.prodDist
//
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
//      priorRootDist, priorBinyDist, priorUnryDist, priorTermDist,
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
//
//  }
//}
