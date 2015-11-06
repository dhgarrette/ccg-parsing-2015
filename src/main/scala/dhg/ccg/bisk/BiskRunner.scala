package dhg.ccg.bisk

import dhg.util._
import dhg.gfl.FudgSentence
import dhg.gfl.{ Sentence => GflSentence }
import dhg.gfl.{ Edge => GflEdge }
import dhg.ccg.cat._
import dhg.ccg.data._
import dhg.ccg.data.Conll2009Reader._
import dhg.ccg.math._
import dhg.ccg.rule._
import dhg.ccg.parse._
import dhg.ccg.parse.dep._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.parse.pcfg.typesup._
import dhg.ccg.parse.scg._
import dhg.ccg.parse.scg.mcmc._
import dhg.ccg.parse.scg.exp._
import dhg.ccg.prob._
import dhg.ccg.tag.learn._
import dhg.ccg.tagdict._
import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.random.SynchronizedRandomGenerator
import scalaz._
import Scalaz._
import dhg.util.GraphUtil

object BiskRunner {
  type Word = String
  type Pos = String

  def main(args: Array[String]): Unit = {
    val lang = "English"
    val useSrl = false
    val maxTrainingSentenceLength = 10

    //    val baseReader = new EnglishCcgTreeBankReader
    //    //val baseReader = new ChineseCcgTreeBankReader
    //    val reader: TreeBankReader =
    //      new RuleViolatedRemovingTreeBankReader(new SimpleRuleViolationFinder(CcgRules.all ++ Vector(AnyPunctRight, AnyPunctLeft), allowAllUnary = true),
    //        new MaxLengthRemovingTreeBankReader(10,
    //          new RebankingTreeBankReader(new SimpleRebanker(RebankRules.standard),
    //            baseReader)))
    //    val raw: Vector[(Vector[(Word, Pos)], Option[FudgSentence])] = time("read raw data",
    //    		(reader.rawCHEATING() ++ reader.tdData()).filter(_.length <= maxTrainingSentenceLength).take(args.headOption.fold(Int.MaxValue)(_.toInt)).map(_.wordposcats.mapt((w, p, c) => (w, p)) -> none[FudgSentence]).toVector)

    val applicationRules = Vector[CcgRule](FA, BA,
      new UnaryCcgRule { val child: Cat = cat"N"; val parent: Cat = cat"(S/(S\N))" },
      new UnaryCcgRule { val child: Cat = cat"S"; val parent: Cat = TopCat },
      new UnaryCcgRule { val child: Cat = cat"N"; val parent: Cat = TopCat })
    val allRules = applicationRules ++ Vector(FC, BX /*, BC, FX*/ , FC2, BX2 /*, BC2, FX2*/ )
    val applicationGCB = new SimpleCfgGuideChartBuilder(applicationRules,
      rootSet = Set(TopCat),
      additionalSupertagAdder = new NoOpAdditionalTagAdder,
      allowTerminalDeletion = false)

    //

    val raw: Vector[(Vector[(Word, Pos)], Option[FudgSentence])] = time("read conll train data", {
      new Conll2009Reader("data/conll-2009/CoNLL2009-ST-traindev/CoNLL2009-ST-English-traindev/CoNLL2009-ST-English-train.txt").read(maxTrainingSentenceLength).map { s =>
        val words = s.words
        val pos = s.pos
        val semDeps: Set[(Int, Int)] = s.semEdges.map { case Edge(p, c, _) => p.index -> c.index }.toSet

        //        // Fix cycles
        //        val cycles = GraphUtil.findUniqueCycles(semDeps)
        //        def findNodesWithMultipleParents(edges: Iterable[(Int,Int)]) = edges.map(_.swap).groupByKey.collect{ case (k,vs) if vs.size > 1 => k}
        //        val nodesWithMultipleParents1 = findNodesWithMultipleParents(semDeps)
        //        cycles.map { c =>
        //          c.sliding2.mapt { case e @ (a,b) =>
        //            val nwmp2 = findNodesWithMultipleParents(semDeps - e)
        //            val fixedNodes = nodesWithMultipleParents1 -- nwmp2
        //            if(fixedNodes == 1){
        //              // eliminate the edge (across all cycles) that produces the max fixedNodes, then findUniqueCycles again and repeat until no cycles. 
        //            }
        //          }
        //        }

        ((words zipSafe pos), useSrl.option(GflSentence.fromDepIndices(words, semDeps.toVector)))
      }.toVector
    })
    val eval: TraversableOnce[(Vector[Word], DepTree)] = time("read conll eval data", {
      new Conll2009Reader("data/conll-2009/CoNLL2009-ST-traindev/CoNLL2009-ST-English-traindev/CoNLL2009-ST-English-development.txt").read().map { s =>
        (s.words, s.depTree)
      }
    })

    //

    val rules = Vector[CcgRule](
      FA, BA,
      //FC, BX,
      //BC, FX,
      //FC2, BX2,
      //BC2, FX2,
      new UnaryCcgRule { val child: Cat = cat"N"; val parent: Cat = cat"(S/(S\N))" },
      new UnaryCcgRule { val child: Cat = cat"S"; val parent: Cat = TopCat },
      new UnaryCcgRule { val child: Cat = cat"N"; val parent: Cat = TopCat })
    val gcb =
      if (useSrl) new DependencyTrackingCfgGuideChartBuilder(rules, rootSet = Set(TopCat), additionalSupertagAdder = new NoOpAdditionalTagAdder)
      else new SimpleCfgGuideChartBuilder(rules, rootSet = Set(TopCat), additionalSupertagAdder = new NoOpAdditionalTagAdder)

    def p2cWithDefault(p2c: Map[String, Set[Cat]]) = { val allCats = p2c.flatMap(_._2).toSet; p2c.withDefaultValue(allCats) }

    println(s"${raw.size} raw sentences")
    val biskInducer = new BiskPosCatMappingInducer(gcb, quick = false)
    val biskPos2Cat_ = time("bisk cat induction", biskInducer.induceWithConstraints(raw, 2)).filter(_._2.nonEmpty)
    val allBiskCats = biskPos2Cat_.flatMap(_._2).toSet
    val biskPos2Cat = biskPos2Cat_.withDefaultValue(allBiskCats)
    assert(allBiskCats.nonEmpty, "bisk induction returned nothing")

    //
    //

    //val tagdict = DummyCatTagDictionary
    val rawWordSupertagSets = raw.map { case (wordpos, annotation) => (wordpos.map { case (w, p) => (w, biskPos2Cat(p)) }, annotation) }
    val tagdict = SimpleTagDictionary(rawWordSupertagSets.map(_._1).flatten.groupByKey.mapVals(_.flatten.toSet).filter { case (w, ts) => ts.nonEmpty }, "<S>", StartCat, "<E>", EndCat)
    val rawDataGC = time("build raw charts", rawWordSupertagSets.par.flatMap { case (sentence, annotation) => annotation.flatMap(_ => gcb.buildFromSupertagSetSentence(sentence, annotation)).orElse(gcb.buildFromSupertagSetSentence(sentence, None)) }).seq
    println(f"rawDataGC: ${rawDataGC.size}/${raw.size} parsable")
    val trainer = time("get trainer", getMcmcTrainer("pcfg", tagdict, rawDataGC))
    val (model, sampledTrees) = time("train parser", trainer.trainGuideChartsWithSomeGoldReturnTrees(rawDataGC, Vector.empty))
    val extractedPcfgModel = pcfgExtracter(model)

    val tagdict2 = new SimpleTagDictionaryFactory().apply(sampledTrees.map(_.tagged), "<S>", StartCat, "<E>", EndCat)
    val rawWordSupertagSets2 = raw.map { case (wordpos, annotation) => (wordpos.map { case (w, p) => (w, tagdict2.entries.get(w).orElse(biskPos2Cat.get(p).map(_ | tagdict2.allTags)).getOrElse(tagdict2.allTags)) }, annotation) }
//    val sampledTreesWords2Trees = sampledTrees.map(t => t.words -> t).groupByKey
//    val biskPos2Cat2_ = raw.flatMap { s =>
//      val sTrees = sampledTreesWords2Trees.getOrElse(s._1.map(_._1), Vector.empty)
//      sTrees.flatMap { t => s._1.map(_._2) zipSafe t.supertags }
//    }.to[Set].groupByKey
//    val allBiskCats2 = biskPos2Cat2_.flatMap(_._2).toSet
//    val biskPos2Cat2 = biskPos2Cat2_.withDefaultValue(allBiskCats2)
//    val rawWordSupertagSets2 = raw.map { case (wordpos, annotation) => (wordpos.map { case (w, p) => (w, biskPos2Cat2(p)) }, annotation) }

    val rawDataGC2 = time("build raw charts", rawWordSupertagSets2.par.flatMap { case (sentence, annotation) => annotation.flatMap(_ => gcb.buildFromSupertagSetSentence(sentence, annotation)).orElse(gcb.buildFromSupertagSetSentence(sentence, None)) }).seq
    val autoparsedRaw = rawDataGC2.flatMap(extractedPcfgModel.parseAndProbFromGuideChart).map(_._1)
    val autoparsedWords2Trees = autoparsedRaw.map(t => t.words -> t).toMap

    val biskPos2Cat3_ = raw.flatMap { s =>
      val to = autoparsedWords2Trees.get(s._1.map(_._1))
      to.map(t => s._1.map(_._2) zipSafe t.supertags)
    }.flatten.to[Set].groupByKey
    val allBiskCats3 = biskPos2Cat3_.flatMap(_._2).toSet
    val biskPos2Cat3 = biskPos2Cat3_.withDefaultValue(allBiskCats3)
    val rawWordSupertagSets3 = raw.map { case (wordpos, annotation) => (wordpos.map { case (w, p) => (w, biskPos2Cat3(p)) }, annotation) }

    //val tagdict3 = new SimpleTagDictionaryFactory().apply(autoparsedRaw.map(_.tagged), "<S>", StartCat, "<E>", EndCat)
    val tagdict3 = SimpleTagDictionary(rawWordSupertagSets3.flatMap(_._1).groupByKey.mapVals(_.flatten.toSet), "<S>", StartCat, "<E>", EndCat)
    val evalDataGC: TraversableOnce[(Option[CfgGuideChart], Vector[String], Int, DepTree)] = time("build eval charts", eval.toVector.par.map {
      case (words, depTree) =>
        val supertagSetSentence = words.mapTo(tagdict3)
        //  gc = applicationGCB.buildFromSupertagSetSentence(supertagSetSentence)
        val gc = applicationGCB.buildFromSupertagSetSentence(supertagSetSentence)
        if (gc.isEmpty && supertagSetSentence.length < 10) { supertagSetSentence.foreach { case (w, ts) => println(f"$w  ${ts.mkString(", ")}") }; println }
        (gc, words, words.length, depTree)
    }.seq)
    println(f"EVAL DATA GC defined ${evalDataGC.count(_._1.isDefined)}/${evalDataGC.size}")
    val evaluator = new DepDepParserEvaluator(None)
    println("Extracted PCFG Parser on eval data"); time("evaluate on test data", evaluator.evaluate(extractedPcfgModel, evalDataGC, tagdict3, verbose = false))

  }

  def getMcmcTrainer(
    modelType: String,
    tagdict: TagDictionary[Cat],
    rawDataGC: Vector[CfgGuideChart]) = {

    val rootinit = "uniform" // "tdecatprior"
    val ntprodInit = "uniform" // "tdecatprior"
    val termprodInit = "uniform" // "tdentry"
    val trinit = "uniform" //"combine-tdentry"

    val alphaRoot = 1.0
    val alphaBiny = 100.0
    val alphaUnry = 100.0
    val alphaTerm = 10000.0
    val alphaProd = 100.0
    val alphaLctx = 1000.0
    val alphaRctx = 1000.0
    val alphaTran = 10.0
    val alphaEmis = 10.0
    val alphaLambda = 3.0
    val priorBinyProdMix = 1.0 / 3
    val priorUnryProdMix = 1.0 / 3
    val priorTermProdMix = 1.0 / 3

    val atomLambda = 1000.0
    val emLambda = 0.1
    val emTdCountLambda = 0.1
    val deltaConst = 1.0

    val pTerm = 0.9
    val pMod = 0.1
    val pFwd = 0.5

    val canCombine = new SimpleCatCanCombine(Vector(FA, FAn2np, BA, BAn2np, FC, FC2, BC, /*FX,*/ BX, BX2), StartCat, EndCat)
    val combinableTransitionMass = 0.85

    val uniformCatpriorInit = new UniformTagPriorInitializer[Cat]()
    val uniatomCatpriorInit = new CatgramCatPriorInitializer(new UniformAtomCatDistInitializer(), pTerm, pMod, pFwd)
    val tdeatomCatpriorInit = new CatgramCatPriorInitializer(new TagdictInformedAtomCatDistInitializer(atomLambda = 1000.0), pTerm, pMod, pFwd)

    val uniformTrInit = new TrUniform[Cat]()
    val tdentryTrInit = new TrTagDictEntriesPossibilities[Cat](new AddLambdaTransitionDistributioner(0.1))

    val rootDistInit = rootinit match {
      case "uniform" => new UniformRootDistInitializer()
      case "catprior" => new CatpriorRootDistInitializer(uniatomCatpriorInit)
      case "tdecatprior" => new CatpriorRootDistInitializer(tdeatomCatpriorInit)
    }

    val (binyDistInit: BinaryDistInitializer, unryDistInit: UnaryDistInitializer) = ntprodInit match {
      case "uniform" => (
        new UniformBinaryDistInitializer(),
        new UniformUnaryDistInitializer())
      case "catprior" => (
        new CatpriorBinaryDistInitializer(uniatomCatpriorInit),
        new CatpriorUnaryDistInitializer(uniatomCatpriorInit))
      case "tdecatprior" => (
        new CatpriorBinaryDistInitializer(tdeatomCatpriorInit),
        new CatpriorUnaryDistInitializer(tdeatomCatpriorInit))
      //      case "zcheat" => (
      //        new CheatingNontermProdDistInitializer(CHEATING_SUPERVISED_DATA),
      //        new CheatingNontermProdDistInitializer(CHEATING_SUPERVISED_DATA))
    }

    val emisDistInit: EmissionInitializer[Cat] = termprodInit match {
      case "x" => null
      case "uniform" => new EmUniform[Cat]()
      case "tdentry" => new EmTagDictionaryEstimate[Cat](tdeatomCatpriorInit, emLambda, emTdCountLambda)
    }

    val (lctxDistInit, rctxDistInit) = trinit match {
      case "uniform" =>
        val uniformCtxInit = new TrUniform[Cat]()
        (new ReversingTrInit(uniformCtxInit), uniformCtxInit)
      case "tdentry" =>
        val combLctxInit = new ReversingTrInit(tdentryTrInit)
        val combRctxInit = /*               */ tdentryTrInit
        (combLctxInit, combRctxInit)
      //      case "catprior" =>
      //        val catpriorCtxInit = new TagPriorTrInitializer[String, Cat](catpriorInit)
      //        (new ReversingTrInit(catpriorCtxInit), catpriorCtxInit)
      case "combine" =>
        val combLctxInit = new ReversingTrInit(new CcgCombinabilityTrInitializer(uniformTrInit, new BackwardCatCanCombine(canCombine), combinableTransitionMass, totalSmoothing = LogDouble.zero))
        val combRctxInit = /*               */ new CcgCombinabilityTrInitializer(uniformTrInit, /*                      */ canCombine, combinableTransitionMass, totalSmoothing = LogDouble.zero)
        (combLctxInit, combRctxInit)
      case "combine-tdentry" =>
        val combLctxInit = new ReversingTrInit(new CcgCombinabilityTrInitializer(tdentryTrInit, new BackwardCatCanCombine(canCombine), combinableTransitionMass, totalSmoothing = LogDouble.zero))
        val combRctxInit = /*               */ new CcgCombinabilityTrInitializer(tdentryTrInit, /*                      */ canCombine, combinableTransitionMass, totalSmoothing = LogDouble.zero)
        (combLctxInit, combRctxInit)
      //      case "catprior-combine" => (
      //        new InterpolatingLctxInit[String](catpriorInit, new CcgCombinabilityTrInitializer(uniformTrInit, new BackwardCatCanCombine(canCombine), combinableTransitionMass, totalSmoothing = LogDouble.zero), catpriorMass),
      //        new InterpolatingRctxInit[String](catpriorInit, new CcgCombinabilityTrInitializer(uniformTrInit, /*                      */ canCombine, combinableTransitionMass, totalSmoothing = LogDouble.zero), catpriorMass))
      //      case "catprior-combine-tdentry" => (
      //        new InterpolatingLctxInit[String](catpriorInit, new CcgCombinabilityTrInitializer(tdentryTrInit, new BackwardCatCanCombine(canCombine), combinableTransitionMass, totalSmoothing = LogDouble.zero), catpriorMass),
      //        new InterpolatingRctxInit[String](catpriorInit, new CcgCombinabilityTrInitializer(tdentryTrInit, /*                      */ canCombine, combinableTransitionMass, totalSmoothing = LogDouble.zero), catpriorMass))
    }

    def gc2supertagSets(gc: CfgGuideChart, td: TagDictionary[Cat]) = {
      (gc.words zipSafe gc.supertagSets).mapt { (w, tags) => (w, tags & td.entries.getOrElse(w, Set.empty)) }
    }

    val priorRootDist = rootDistInit(rawDataGC, tagdict)
    val priorBinyDist = binyDistInit(rawDataGC, tagdict)
    val priorUnryDist = unryDistInit(rawDataGC, tagdict)
    val priorEmisDist = emisDistInit.fromKnownSupertagSets(rawDataGC.map(gc2supertagSets(_, tagdict)), tagdict)
    val priorTermDist = Em2TermProdDist(priorEmisDist)
    val priorProdDist = new ICPDU(priorBinyDist, priorUnryDist, priorTermDist, Map().withDefaultValue((LogDouble(1.0 / 3), LogDouble(1.0 / 3), LogDouble(1.0 / 3))))
    val priorLctxDist: ConditionalLogProbabilityDistribution[Cat, Cat] = if (lctxDistInit != null) lctxDistInit.fromKnownSupertagSets(rawDataGC.map(gc2supertagSets(_, tagdict)), tagdict) else null
    val priorRctxDist: ConditionalLogProbabilityDistribution[Cat, Cat] = if (rctxDistInit != null) rctxDistInit.fromKnownSupertagSets(rawDataGC.map(gc2supertagSets(_, tagdict)), tagdict) else null
    val priorTranDist = priorRctxDist

    val pcfgProductionFinder = new SimplePcfgProductionCounter()
    val pcfgGuideChartProdFinder = new SimplePcfgGuideChartProdFinder()
    val pcfgAlphaPriorMaker = new TrainDataPcfgAlphaPriorMaker(pcfgProductionFinder, pcfgGuideChartProdFinder)
    val pcfgInsideChartBuilder = new SimplePcfgInsideChartBuilder()
    val pcfgTreeSampler = new SimplePcfgTreeSampler(pcfgInsideChartBuilder)
    val scgProductionFinder = new SimpleScgProductionFinder(pcfgProductionFinder)
    val scgGuideChartProdFinder = new SimpleScgGuideChartProdFinder(pcfgGuideChartProdFinder)
    val scgAlphaPriorMaker = new TrainDataScgAlphaPriorMaker(scgProductionFinder, scgGuideChartProdFinder)

    val pcfgParserInstantiater = new ExactPcfgParserInstantiater()
    val samplingPcfgParserInstantiater = new SamplingPcfgParserInstantiater(new SimplePcfgTreeSampler(new SimplePcfgInsideChartBuilder()), new SimplePcfgWeighter())
    val compositePcfgParserInstantiater = new CompositePcfgParserInstantiater(pcfgParserInstantiater, kA = 100, samplingPcfgParserInstantiater)
    val scgParserInstantiater = new RerankScgParserInstantiater(maxExactPossibleParseCount = 1000, numDelegateParses = 10000, compositePcfgParserInstantiater)
    def supPcfgTrainer(delta: Double) = new AlphaBetaSupPcfgTrainer(priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, alphaRoot, alphaProd, priorBinyProdMix, priorUnryProdMix, priorTermProdMix, pcfgProductionFinder, pcfgParserInstantiater)
    def supScgTrainer(delta: Double) = new AlphaBetaSupScgTrainer(priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, priorLctxDist, priorRctxDist, alphaRoot, alphaProd, alphaLctx, alphaRctx, priorBinyProdMix, priorUnryProdMix, priorTermProdMix, scgProductionFinder, scgParserInstantiater)(CatStartEndTags)

    val mcmcPcfgTrainer = new McmcPcfg(
      samplingIterations = 100,
      burninIterations = 50,
      pcfgAlphaPriorMaker,
      pcfgProductionFinder,
      initialParserInstantiater = new ExactPcfgParserInstantiater(),
      DirSampler,
      pcfgTreeSampler,
      priorRootDist, priorBinyDist, priorUnryDist, priorTermDist,
      alphaRoot = alphaRoot, alphaBiny = alphaBiny, alphaUnry = alphaUnry, alphaTerm = alphaTerm,
      alphaLambda = alphaLambda, priorBinyProdMix = priorBinyProdMix, priorUnryProdMix = priorUnryProdMix, priorTermProdMix = priorTermProdMix,
      supPcfgTrainer(1.0),
      None,
      verbose = false)

    val mcmcScgTrainer = new McmcScg(
      samplingIterations = 300,
      burninIterations = 100,
      scgAlphaPriorMaker,
      scgProductionFinder,
      initialParserInstantiater = new ExactPcfgParserInstantiater(),
      DirSampler,
      new SimpleMcmcScgResampler(
        pcfgTreeSampler,
        new ContextScgAcceptanceSampler(new SimpleScgWeighter()),
        maxAcceptanceTries = 1,
        rand = new SynchronizedRandomGenerator(new MersenneTwister())),
      priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, priorLctxDist, priorRctxDist,
      alphaRoot = alphaRoot, alphaBiny = alphaBiny, alphaUnry = alphaUnry, alphaTerm = alphaTerm, alphaLctx = alphaLctx, alphaRctx = alphaRctx,
      alphaLambda = alphaLambda, priorBinyProdMix = priorBinyProdMix, priorUnryProdMix = priorUnryProdMix, priorTermProdMix = priorTermProdMix,
      supScgTrainer(1.0),
      None,
      verbose = false)(CatStartEndTags)

    //    if (modelType == "pcfg") 
    mcmcPcfgTrainer
    //    else if (modelType == "scg") 
    //      mcmcScgTrainer
    //    else sys.error(s"modelType type not supported: $modelType")
  }

  def pcfgExtracter(p: GuideChartParser): PcfgParser = p match {
    case x: PcfgParser => x
    case x: ExactScgParser => new PcfgParser(x.rootDist, x.prodDist)
    case x: NumPossibleParsesDelegatingKBestGuideChartParser => pcfgExtracter(x.exactParser)
    //case x: RerankParser => pcfgExtracter(x.delegate)
  }

}

