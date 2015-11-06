package dhg.ccg.run

import dhg.ccg.cat._
import dhg.ccg.data._
import dhg.ccg.rule._
import dhg.ccg.parse._
import dhg.ccg.parse.dep._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.parse.pcfg.typesup._
import dhg.ccg.parse.scg._
import dhg.ccg.parse.scg.mcmc._
import dhg.ccg.prob._
import dhg.ccg.tagdict._
import dhg.ccg.tag.learn._
import dhg.util._
import dhg.util.viz._
import dhg.ccg.math.DirSampler
import dhg.gfl.FudgSentence
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import scalaz._
import Scalaz._
import scala.collection.breakOut
import scala.annotation.tailrec
import java.io.FileOutputStream
import java.io.FileInputStream
import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.random.SynchronizedRandomGenerator
import java.io.{ File => _, _ }
import java.util.zip._
import scala.collection.immutable.VectorBuilder
import scala.collection.parallel.immutable.ParVector
import scala.util.control.Breaks._
import dhg.ccg.parse.inf.CatgramInfCatPriorInitializer

class Parse2015Runner(
    rules: Vector[CcgRule],
    reader: TreeBankReader,
    tdTokToUse: Int,
    numTrainSentences: Int,
    numTestSentences: Int,

    samplingIterations: Int,
    burninIterations: Int,

    alphaRoot: Double,
    alphaBiny: Double,
    alphaUnry: Double,
    alphaTerm: Double,
    alphaProd: Double,
    alphaLctx: Double,
    alphaRctx: Double,
    alphaLambda: Double,
    priorBinyProdMix: Double,
    priorUnryProdMix: Double,
    priorTermProdMix: Double,

    modelOpt: String,
    learningOpt: String,

    rootinit: String,
    ntprodInit: String,
    termprodInit: String,
    trinit: String,

    pTerm: Double,
    pMod: Double,
    pFwd: Double,

    //catpriorMass: Double,
    combinableTransitionMass: Double,

    tdCutoff: Double,

    maxAcceptanceTries: Int,

    evaluator: ParserEvaluator,

    trainAllowTerminalDeletion: Boolean,
    testAllowTerminalDeletion: Boolean,

    sentencesToTake: Int,
    maxTrainTokens: Int,

    argString: String,

    mcmcOutputCountFile: Option[String] = None,

    verbose: Boolean = false) {

  private[this] val random: RandomGenerator = new MersenneTwister

  val atomLambda = 1000.0
  val emLambda = 0.1
  val emTdCountLambda = 0.1
  val deltaConst = 1.0

  def makeInits(tagdict: TagDictionary[Cat]) = {
    val canCombine = new SimpleCatCanCombine(Vector(FA, FAn2np, BA, BAn2np, FC, FC2, BC, /*FX,*/ BX, BX2), tagdict.startTag, tagdict.endTag)

    val uniformTrInit = new TrUniform[Cat]()
    val tdentryTrInit = new TrTagDictEntriesPossibilities[Cat](new AddLambdaTransitionDistributioner(0.1))

    //

    def makeCatPrior(atomCatInit: AtomCatDistInitializer) = new CatgramInfCatPriorInitializer(
      atomCatInit,
      allPunc = Set(cat".", cat",", cat";", cat":", cat"LRB", cat"RRB").map(_.asInstanceOf[PuncCat]),
      puncDist = new SimpleLogProbabilityDistribution(Map(
        cat"," -> 0.50,
        cat"." -> 0.45,
        cat":" -> 0.02,
        cat";" -> 0.01,
        cat"LRB" -> 0.01,
        cat"RRB" -> 0.01).mapt((c, p) => c.asInstanceOf[PuncCat] -> LogDouble(p)).normalizeValues),
      pPunc = 0.1,
      pTerm, pMod, pFwd)
      
    val uniformCatpriorInit = new UniformTagPriorInitializer[Cat]()
    val uniatomCatpriorInit = makeCatPrior(new UniformAtomCatDistInitializer())
    val tdeatomCatpriorInit = makeCatPrior(new TagdictInformedAtomCatDistInitializer(atomLambda = 1000.0))

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
    }

    val emisDistInit: EmissionInitializer[Cat] = termprodInit match {
      case "uniform" => new EmUniform[Cat]()
      case "tdentry" => new EmTagDictionaryEstimate[Cat](tdeatomCatpriorInit, emLambda, emTdCountLambda)
    }

    val (lctxDistInit, rctxDistInit) = trinit match {
      case _ if modelOpt != "scg" => (None, None)

      case "uniform" =>
        val lctxInit = new ReversingTrInit(uniformTrInit)
        val rctxInit = /*               */ uniformTrInit
        (some(lctxInit), some(rctxInit))
      case "tdentry" =>
        val lctxInit = new ReversingTrInit(tdentryTrInit)
        val rctxInit = /*               */ tdentryTrInit
        (some(lctxInit), some(rctxInit))
      //      case "catprior" =>
      //        val catpriorCtxInit = new TagPriorTrInitializer[String, Cat](catpriorInit)
      //        (new ReversingTrInit(catpriorCtxInit), catpriorCtxInit)
      case "combine-uniform" =>
        val lctxInit = new ReversingTrInit(new CcgCombinabilityTrInitializer(uniformTrInit, new BackwardCatCanCombine(canCombine), combinableTransitionMass, totalSmoothing = LogDouble.zero))
        val rctxInit = /*               */ new CcgCombinabilityTrInitializer(uniformTrInit, /*                      */ canCombine, combinableTransitionMass, totalSmoothing = LogDouble.zero)
        (some(lctxInit), some(rctxInit))
      case "combine-tdentry" =>
        val lctxInit = new ReversingTrInit(new CcgCombinabilityTrInitializer(tdentryTrInit, new BackwardCatCanCombine(canCombine), combinableTransitionMass, totalSmoothing = LogDouble.zero))
        val rctxInit = /*               */ new CcgCombinabilityTrInitializer(tdentryTrInit, /*                      */ canCombine, combinableTransitionMass, totalSmoothing = LogDouble.zero)
        (some(lctxInit), some(rctxInit))
      //      case "catprior-combine" => (
      //        new InterpolatingLctxInit[String](catpriorInit, new CcgCombinabilityTrInitializer(uniformTrInit, new BackwardCatCanCombine(canCombine), combinableTransitionMass, totalSmoothing = LogDouble.zero), catpriorMass),
      //        new InterpolatingRctxInit[String](catpriorInit, new CcgCombinabilityTrInitializer(uniformTrInit, /*                      */ canCombine, combinableTransitionMass, totalSmoothing = LogDouble.zero), catpriorMass))
      //      case "catprior-combine-tdentry" => (
      //        new InterpolatingLctxInit[String](catpriorInit, new CcgCombinabilityTrInitializer(tdentryTrInit, new BackwardCatCanCombine(canCombine), combinableTransitionMass, totalSmoothing = LogDouble.zero), catpriorMass),
      //        new InterpolatingRctxInit[String](catpriorInit, new CcgCombinabilityTrInitializer(tdentryTrInit, /*                      */ canCombine, combinableTransitionMass, totalSmoothing = LogDouble.zero), catpriorMass))

      case _ => sys.error(f"invalid option: --tr-init trinit : $argString")
    }

    (rootDistInit, binyDistInit, unryDistInit, emisDistInit, lctxDistInit, rctxDistInit)
  }

  def run(): Unit = {

    println(f"reader=$reader")

    def allRawTextF: Iterator[Vector[String]] = reader.raw.take(sentencesToTake)
    val allRawWordTypes = time("all raw text", allRawTextF.flatten.toSet)

    println(f"sentencesToTake=$sentencesToTake")
    println(f"numTrainSentences=$numTrainSentences")
    println(f"maxTrainTokens=$maxTrainTokens")
    println(f"numTestSentences=$numTestSentences")

    val tdData = time("read td data", reader.tdData /*.take(sentencesToTake)*/ .map(_.tagged).takeSub(tdTokToUse).toVector) // total: 323190
    println(f"TD data tokens: ${tdData.sumBy(_.size)}")
    println(f"TD data sentences: ${tdData.size}")
    val fullWordSet = allRawWordTypes ++ tdData.flatten.map(_._1)
    val fullSupertagSet = tdData.flatten.map(_._2).toSet
    val fullTdData = tdData
    println(f"Full TD data sentences: ${fullTdData.size}")
    println(f"Full TD data tokens: ${fullTdData.sumBy(_.size)}")

    val tagdict: TagDictionary[Cat] = time("make td", new SimpleTagDictionaryFactory(Some(tdCutoff))(fullTdData, "<S>", CatStartEndTags.startTag, "<E>", CatStartEndTags.endTag)).withWords(fullWordSet) //.withTags(fullSupertagSet)
    println(f"num td entries = ${tagdict.entries.values.sumBy(_.size)}")

    val ata = new StandardTagDictAdditionalTagAdder[Cat]()
    val fbata = new StandardTagDictAndFwdBkdAdditionalTagAdder()
    val applicationGCB = new SimpleCfgGuideChartBuilder(rules, ata, allowTerminalDeletion = false)
    //val fbApplicationGCB = new SimpleCfgGuideChartBuilder(rules, fbata, allowTerminalDeletion = false)
    val fbApplicationGCB = new SimpleCfgGuideChartBuilder(rules, ata, allowTerminalDeletion = true)
    def getGCB(allowTerminalDeletion: Boolean) = {
      val gcb1 =
        if (allowTerminalDeletion) Vector(applicationGCB, fbApplicationGCB)
        else Vector(applicationGCB)
      new CascadingAttemptsCfgGuideChartBuilder(gcb1)
    }
    val trainerGuideChartBuilder = getGCB(trainAllowTerminalDeletion)
    val parserGuideChartBuilder = getGCB(testAllowTerminalDeletion)
    println("GCBs constructed:")
    println(s"    ${trainerGuideChartBuilder}")
    println(s"    ${parserGuideChartBuilder}")

    def makeGC(dataIterator: Iterator[CcgTree], gcb: CfgGuideChartBuilder, filterOutFailedGCs: Boolean, maxNumSentences: Int, maxNumTokens: Int): Vector[(Option[CfgGuideChart], CcgTree)] = {
      val chunkSize = 10
      dataIterator
        .grouped(chunkSize).zipWithIndex.flatMap { case (chunk, chunkNum) => 
          chunk.PARALLEL
            .zipWithIndex
            .mapt {  (t, i) =>
              val gc = //time(f"building gc ${chunkNum*chunkSize + i} (length=${t.length})", 
                  gcb.build(t.words, None, tagdict)//)
              //println(f"    gc $i  ${if(gc.isDefined) "SUCCESS" else "FAIL"}")
              (gc, t)
            }
            .filter { case (gc, t) => !filterOutFailedGCs || gc.isDefined }
          }
          .take(maxNumSentences)
          .takeWhileAg(_.sumBy(_._2.length) <= maxNumTokens)
          .toVector
    }

    val rawDataGC: Vector[CfgGuideChart] = {
      val parseTrainDataIterator: Iterator[CcgTree] = reader.rawCHEATING.take(sentencesToTake)
      val trainDataGC: Vector[(Option[CfgGuideChart], CcgTree)] = time("make raw data GCs", makeGC(parseTrainDataIterator, trainerGuideChartBuilder, filterOutFailedGCs = false, numTrainSentences, maxTrainTokens))
      println(f"train data sentences: ${trainDataGC.size}  (parses found for ${trainDataGC.count(_._1.isDefined)} of them)")
      trainDataGC.flatMap(_._1)
    }
    println(f"raw data sentences: ${rawDataGC.size}")
    println(f"raw data tokens: ${rawDataGC.sumBy(_.words.size)}")

    println(f"tdCutoff=$tdCutoff")
    println(f"evaluator=$evaluator")

    println(f"Number of tags: ${tagdict.allTags.size}")

    val (rootDistInit, binyDistInit, unryDistInit, emisDistInit, lctxDistInit, rctxDistInit) = makeInits(tagdict)

    def gc2supertagSets(gc: CfgGuideChart, td: TagDictionary[Cat]) = {
      (gc.words zipSafe gc.supertagSets).mapt { (w, tags) => (w, tags & td.entries.getOrElse(w, Set.empty)) }
    }

    val priorRootDist = rootDistInit(rawDataGC, tagdict)
    val priorBinyDist = binyDistInit(rawDataGC, tagdict)
    val priorUnryDist = unryDistInit(rawDataGC, tagdict)
    val priorEmisDist = emisDistInit.fromKnownSupertagSets(rawDataGC.map(gc2supertagSets(_, tagdict)), tagdict)
    val priorTermDist = Em2TermProdDist(priorEmisDist)
    val priorProdDist = new ICPDU(priorBinyDist, priorUnryDist, priorTermDist, Map().withDefaultValue((LogDouble(1.0 / 3), LogDouble(1.0 / 3), LogDouble(1.0 / 3))))
    val priorLctxDist: Option[ConditionalLogProbabilityDistribution[Cat, Cat]] = lctxDistInit.map(_.fromKnownSupertagSets(rawDataGC.map(gc2supertagSets(_, tagdict)), tagdict))
    val priorRctxDist: Option[ConditionalLogProbabilityDistribution[Cat, Cat]] = rctxDistInit.map(_.fromKnownSupertagSets(rawDataGC.map(gc2supertagSets(_, tagdict)), tagdict))
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
    def pcfgExtracter(p: GuideChartParser): PcfgParser = p match {
      case x: PcfgParser => x
      case x: ExactScgParser => new PcfgParser(x.rootDist, x.prodDist)
      case x: NumPossibleParsesDelegatingKBestGuideChartParser => pcfgExtracter(x.exactParser)
      //case x: RerankParser => pcfgExtracter(x.delegate)
    }

    val smoothingParameter = 1.0 / rawDataGC.size
    def supPcfgTrainer(delta: Double) = new AlphaBetaSupPcfgTrainer(priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, alphaRoot, alphaProd, priorBinyProdMix, priorUnryProdMix, priorTermProdMix, pcfgProductionFinder, pcfgParserInstantiater)
    def supScgTrainer(delta: Double) = new AlphaBetaSupScgTrainer(priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, priorLctxDist.get, priorRctxDist.get, alphaRoot, alphaProd, alphaLctx, alphaRctx, priorBinyProdMix, priorUnryProdMix, priorTermProdMix, scgProductionFinder, scgParserInstantiater)(CatStartEndTags)

    val testDataGC: Vector[(Option[CfgGuideChart], CcgTree)] = {
      val testDataIterator: Iterator[CcgTree] = time("read test data", reader.testData.take(sentencesToTake))
      val testDataGcAndTrees: Vector[(Option[CfgGuideChart], CcgTree)] = time("make test data GCs", makeGC(testDataIterator, parserGuideChartBuilder, filterOutFailedGCs = false, numTestSentences, Int.MaxValue))
      println(f"test data sentences: ${testDataGcAndTrees.size}  (parses found for ${testDataGcAndTrees.count(_._1.isDefined)} of them)")
      testDataGcAndTrees
    }

    learningOpt match {
      case "mcmc" =>
        val initPcfg = pcfgParserInstantiater(priorRootDist, priorProdDist)

        modelOpt match {

          case "pcfg" =>
            val mcmcPcfgTrainer = new McmcPcfg(
              samplingIterations,
              burninIterations,
              pcfgAlphaPriorMaker,
              pcfgProductionFinder,
              initialParserInstantiater = new ExactPcfgParserInstantiater(),
              DirSampler,
              pcfgTreeSampler,
              priorRootDist, priorBinyDist, priorUnryDist, priorTermDist,
              alphaRoot = alphaRoot, alphaBiny = alphaBiny, alphaUnry = alphaUnry, alphaTerm = alphaTerm,
              alphaLambda = alphaLambda, priorBinyProdMix = priorBinyProdMix, priorUnryProdMix = priorUnryProdMix, priorTermProdMix = priorTermProdMix,
              supPcfgTrainer(1.0),
              Some(((parser: GuideChartParser) => {
                println("Full PCFG Parser on test"); println(argString); evaluator.evaluate(parser, testDataGC, tagdict, verbose = false)
              }, Set(-1, 0, 5, 10, 20, 50, 100, 200, 300, 400) ++ (500 until 10000 by 500))),
              verbose)

            println(f"mcmcTrainer = $mcmcPcfgTrainer")
            println(f"  rootDistInit=$rootDistInit")
            println(f"  binyDistInit=$binyDistInit")
            println(f"  unryDistInit=$unryDistInit")
            println(f"  emisDistInit=$emisDistInit")
            println(f"\nMCMC PCFG")
            val mcmcPcfgModel: GuideChartParser = mcmcPcfgTrainer.trainGuideChartsWithSomeGold(rawDataGC, Vector.empty)
            println("Full PCFG Parser on test"); println(argString); evaluator.evaluate(mcmcPcfgModel, testDataGC, tagdict, verbose = false)

          case "scg" =>
            val mcmcScgTrainer = new McmcScg(
              samplingIterations,
              burninIterations,
              scgAlphaPriorMaker,
              scgProductionFinder,
              initialParserInstantiater = new ExactPcfgParserInstantiater(),
              DirSampler,
              new SimpleMcmcScgResampler(
                pcfgTreeSampler,
                new ContextScgAcceptanceSampler(new SimpleScgWeighter()),
                maxAcceptanceTries,
                rand = new SynchronizedRandomGenerator(new MersenneTwister())),
              priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, priorLctxDist.get, priorRctxDist.get,
              alphaRoot = alphaRoot, alphaBiny = alphaBiny, alphaUnry = alphaUnry, alphaTerm = alphaTerm, alphaLctx = alphaLctx, alphaRctx = alphaRctx,
              alphaLambda = alphaLambda, priorBinyProdMix = priorBinyProdMix, priorUnryProdMix = priorUnryProdMix, priorTermProdMix = priorTermProdMix,
              supScgTrainer(1.0),
              Some(((parser: GuideChartParser) => {
                println("Extracted PCFG Parser on test"); println(argString); evaluator.evaluate(pcfgExtracter(parser), testDataGC, tagdict, verbose = false)
              }, Set(-1, 0, 5, 10, 20, 50, 100, 200, 300, 400) ++ (500 until 10000 by 500))),
              mcmcOutputCountFile,
              verbose)(CatStartEndTags)

            //val trainEval = (p: GuideChartParser) => { println(argString); evaluator.evaluate(new GuideChartParserAdapter(parserGuideChartBuilder, p), parseTrainData, tagdict, verbose = false) }: Unit
            println(f"mcmcTrainer = $mcmcScgTrainer")
            println(f"  rootDistInit=$rootDistInit")
            println(f"  binyDistInit=$binyDistInit")
            println(f"  unryDistInit=$unryDistInit")
            println(f"  emisDistInit=$emisDistInit")
            println(f"  lctxDistInit=$lctxDistInit")
            println(f"  rctxDistInit=$rctxDistInit")
            println(f"")
            //println(f"\nInit SCG")
            //val initScgModel = scgParserInstantiater(priorRootDist, priorProdDist, priorLctxDist.get, priorRctxDist.get)(CatStartEndTags)
            //println(argString); evaluator.evaluate(initScgModel, testDataGC, tagdict, verbose = false)
            println(f"\nMCMC SCG")
            val mcmcScgModel = mcmcScgTrainer.trainGuideChartsWithSomeGold(rawDataGC, Vector.empty)
            val extractedPcfgModel = pcfgExtracter(mcmcScgModel)
            println("Extracted PCFG Parser on test"); println(argString); evaluator.evaluate(extractedPcfgModel, testDataGC, tagdict, verbose = false)
          //println("Full SCG Parser on test"); println(argString); evaluator.evaluate(mcmcScgModel, testDataGC, tagdict, verbose = false)

        }
    }
  }
}

//
//
//

object Parse2015Run {
  def main(args: Array[String]): Unit = {
    val argString = args.mkString(" ")
    println(argString)

    val (arguments, options) = parseArgs(args)
    assert(arguments.isEmpty, s"Argument `${arguments.head}` was not preceded by an option flag.")
    Parse2015Run(options, argString).run()
  }

  def apply(
    options: CommandLineOptions, argString: String) = {

    val additionalRules = options.get("additional-rules") /*.orElse(Some("FC,BX,FC2,BX2"))*/ .filterNot(Set(".", "x")).getOrElse("x")
    val language = options.s("lang", "en")

    val rules = CcgRules.nonComp ++ ((if (additionalRules == "x") Vector.empty else additionalRules.lsplit(",")).map { case "FC" => FC; case "BX" => BX; case "FC2" => FC2; case "BX2" => BX2 })

    val reader: TreeBankReader = {
      val standardReader = getReader(language)
      val dependencyConvertableTreeReader = new RuleViolatedRemovingTreeBankReader(new SimpleRuleViolationFinder(CcgRules.all, allowAllUnary = true), standardReader)
      new SeparateTrainTestTreeBankReader(
        tdReader = dependencyConvertableTreeReader,
        rawReader = dependencyConvertableTreeReader,
        testReader = dependencyConvertableTreeReader)
    }

    val runner = new Parse2015Runner(
      rules,
      options.get("max-sent-len").map(_.toLowerCase).filterNot(Set("max", "all", "inf", "x")).fold(reader)(l => new MaxLengthRemovingTreeBankReader(l.toInt, reader)),
      tdTokToUse = options.get("td-tok").map(_.toLowerCase.replaceAll("(\\d+)k", "$1000")).filterNot(Set("max", "all", "inf", "x")).map(_.toInt).getOrElse(Int.MaxValue),
      numTrainSentences = options.get("train-sent").map(_.toLowerCase).filterNot(Set("max", "all", "inf", "x")).map(_.toInt).getOrElse(Int.MaxValue),
      numTestSentences = options.get("test-sent").map(_.toLowerCase).filterNot(Set("max", "all", "inf", "x")).map(_.toInt).getOrElse(Int.MaxValue),

      samplingIterations = options.i("sampling-iterations", 500),
      burninIterations = options.i("burnin-iterations", 0),

      alphaRoot = options.d("alpha-root", 1.0),
      alphaBiny = options.d("alpha-biny", 100.0),
      alphaUnry = options.d("alpha-unry", 100.0),
      alphaTerm = options.d("alpha-term", 10000.0),
      alphaProd = options.d("alpha-prod", 100.0),
      alphaLctx = options.d("alpha-cntx", 1000.0),
      alphaRctx = options.d("alpha-cntx", 1000.0),
      alphaLambda = 3.0,
      priorBinyProdMix = 1.0 / 3,
      priorUnryProdMix = 1.0 / 3,
      priorTermProdMix = 1.0 / 3,

      modelOpt = options.s("model"),
      learningOpt = options.s("learning", "mcmc"),

      rootinit = options.s("root-init", "tdecatprior"),
      ntprodInit = options.s("nt-prod-init", "tdecatprior"),
      termprodInit = options.s("term-prod-init", "tdentry"),
      trinit = options.s("tr-init", "combine-tdentry"),

      pTerm = options.d("pterm", 0.7),
      pMod = options.d("pmod", 0.1),
      pFwd = options.d("pfwd", 0.5),

      combinableTransitionMass = options.d("comb-tr-mass", 0.85),

      tdCutoff = options.d("td-cutoff", 0.0),

      maxAcceptanceTries = options.i("max-accept-tries", 1),

      evaluator = new DepParserEvaluator(outputFile = options.get("output-file") /*.map { f => pathjoin(f, options("num") + ".txt") }*/ /*, goldSupertags = options.b("gold-test-supertags" , false*/ ),

      trainAllowTerminalDeletion = options.b("train-termdel", false),
      testAllowTerminalDeletion = options.b("test-termdel", false),

      sentencesToTake = options.i("takemax", Int.MaxValue),
      maxTrainTokens = options.get("max-train-tok").map(_.toLowerCase.replaceAll("(\\d+)k", "$1000")).filterNot(Set("max", "all", "inf", "x")).map(_.toInt).getOrElse(Int.MaxValue),

      argString = argString,

      mcmcOutputCountFile = options.get("mcmc-output-count-file"))

    println("Seen option values"); for ((o, v) <- options.getSeenEntries) println(f"    --$o  $v")
    assert(options.unusedOptions.isEmpty, "Unused options: " + options.unusedOptions)

    runner
  }

  def getReader(language: String) = {
    val baseReader = language.take(2) match {
      case "en" => EnglishCcgTreeBankReader()
      case "ch" => ChineseCcgTreeBankReader()
      case "it" => ItalianCcgTreeBankReader()
    }
    new RebankingTreeBankReader(new SimpleRebanker(RebankRules.standard), baseReader)
  }

}

case class Em2TermProdDist(emissionDist: ConditionalLogProbabilityDistribution[Cat, String]) extends ConditionalLogProbabilityDistribution[Cat, TermProd] {
  def apply(x: TermProd, given: Cat) = emissionDist(x.word, given)
  def sample(given: Cat) = TermProd(emissionDist.sample(given))
  override def toString = f"Em2TermProdDist($emissionDist)"
}
