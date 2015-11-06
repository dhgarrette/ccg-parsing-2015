package dhg.ccg.parse.scg.exp

import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.parse._
import dhg.ccg.parse.dep._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.parse.pcfg.typesup._
import dhg.ccg.parse.scg._
import dhg.ccg.parse.scg.mcmc._
import dhg.ccg.data._
import dhg.util._
import dhg.util.viz._
import dhg.ccg.parse._
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import scalaz._
import Scalaz._
import scala.collection.breakOut
import scala.annotation.tailrec
import java.io.FileOutputStream
import java.io.FileInputStream
import dhg.condor.Condor
import org.apache.commons.math3.random.MersenneTwister
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tagdict.SimpleTagDictionaryFactory
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.tag.HmmTagger
import dhg.ccg.tag.WeightedTagger
import dhg.ccg.tag.SemisupervisedTaggerTrainer
import dhg.ccg.tag.AddLambdaSmoothedHmmTaggerTrainer
import dhg.ccg.tag.TaggerEvaluator
import dhg.ccg.tag.learn.AddLambdaTransitionDistributioner
import dhg.ccg.tag.learn.AddLambdaEmissionDistributioner
import dhg.ccg.tag.learn.CatgramCatPriorInitializer
import dhg.ccg.tag.learn.CcgCombinabilityTrInitializer
import dhg.ccg.tag.learn.CheatingTagPriorInitializer
import dhg.ccg.tag.learn.EmissionInitializer
import dhg.ccg.tag.learn.EmTagDictionaryEstimate
import dhg.ccg.tag.learn.EmUniform
import dhg.ccg.tag.learn.FfbsHmmTaggerTrainer
import dhg.ccg.tag.learn.TagdictInformedAtomCatDistInitializer
import dhg.ccg.tag.learn.TrTagDictEntriesPossibilities
import dhg.ccg.tag.learn.TrUniform
import dhg.ccg.tag.learn.UniformAtomCatDistInitializer
import dhg.ccg.tag.learn.UniformTagPriorInitializer
import dhg.ccg.tagdict.SimpleStartEndTags
import dhg.ccg.tagdict.TagDictionaryFactory
import dhg.ccg.tag.learn.TagPriorTrInitializer
import dhg.ccg.math.DirSampler
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.random.SynchronizedRandomGenerator
import dhg.ccg.parse.gfl._
import dhg.gfl.FudgSentence
import dhg.ccg.chunk._
import java.io.{ File => _, _ }
import java.util.zip._
import scala.collection.immutable.VectorBuilder
import scala.collection.parallel.immutable.ParVector
import scala.util.control.Breaks._
import dhg.ccg.parse.inf._

trait ScgExpRunner {

}

class SimpleScgExpRunner(
  rules: Vector[CcgRule],
  reader: TreeBankReader,
  tdTokToUse: Int,
  numTrainSentences: Int,
  numTestSentences: Int,

  samplingIterations: Int,
  burninIterations: Int,
  //  maxEmIterations: Int,
  //  maxDecompIterations: Int,

  alphaRoot: Double,
  alphaBiny: Double,
  alphaUnry: Double,
  alphaTerm: Double,
  alphaProd: Double,
  alphaLctx: Double,
  alphaRctx: Double,
  alphaTran: Double,
  alphaEmis: Double,
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

  tdIncludesRequiredMappings: Boolean,
  tdCutoff: Double,
  tdIncludesTest: Boolean,

  maxAcceptanceTries: Int,
  parseCountCutoff: Long,
  //numSamples :Int,

  evaluator: ParserEvaluator,

  punctSplit: Boolean,
  trainAnnotator: String,
  trainAnnotatedSentencesProportion: Double,
  trainBracketProportion: Double,
  trainBracketHighMatchBase: Boolean,
  trainBracketCats: (Cat => Boolean),
  trainBaseCats: Boolean,
  trainHighCats: Boolean,
  trainDepProportion: Double,
  trainGCB: String,
  testAnnotator: String,
  testAnnotatedSentencesProportion: Double,
  testBracketProportion: Double,
  testBracketHighMatchBase: Boolean,
  testBracketCats: (Cat => Boolean),
  testBaseCats: Boolean,
  testHighCats: Boolean,
  testDepProportion: Double,
  testGCB: String,

  numClusters: Option[Int],

  useSerializedTrainGC: Boolean,
  useSerializedTestGC: Boolean,
  trainSerializedGC: Option[String],
  testSerializedGC: Option[String],

  sentencesToTake: Int,
  maxTrainTokens: Int,

  argString: String,

  mcmcOutputCountFile: Option[String] = None,

  justSerialize: Boolean = false,
  verbose: Boolean = false,
  dummyGcb: Boolean = false)
    extends ScgExpRunner {
  private[this] val random: RandomGenerator = new MersenneTwister

  val atomLambda = 1000.0
  val emLambda = 0.1
  val emTdCountLambda = 0.1
  val deltaConst = 1.0

  def makeInits(tagdict: TagDictionary[Cat], CHEATING_SUPERVISED_DATA: Vector[CcgTree]) = {
    val canCombine = new SimpleCatCanCombine(Vector(FA, FAn2np, BA, BAn2np, FC, FC2, BC, /*FX,*/ BX, BX2), tagdict.startTag, tagdict.endTag)

    val uniformTrInit = new TrUniform[Cat]()
    val tdentryTrInit = new TrTagDictEntriesPossibilities[Cat](new AddLambdaTransitionDistributioner(0.1))

    //

    if (modelOpt == "pcfg") {
      assert(trinit == "x", f"if model=pcfg, --tr-init must be 'x'; was $trinit : $argString")
    }
    //    if (learningOpt == "sup") {
    //      assert(catprior == "x", f"if learning=sup, --cat-prior must be 'x'; was $catprior : $argString")
    //      assert(rootinit == "x", f"if learning=sup, --root-init must be 'x'; was $rootinit : $argString")
    //      assert(ntprodInit == "x", f"if learning=sup, --nt-prod-init must be 'x'; was $ntprodInit : $argString")
    //      assert(termprodInit == "x", f"if learning=sup, --term-prod-init must be 'x'; was $termprodInit : $argString")
    //      assert(trinit == "x", f"if learning=sup, --tr-init must be 'x'; was $trinit : $argString")
    //    }

    val uniformCatpriorInit = new UniformTagPriorInitializer[Cat]()
    val uniatomCatpriorInit = new CatgramCatPriorInitializer(new UniformAtomCatDistInitializer(), pTerm, pMod, pFwd)
    val tdeatomCatpriorInit = new CatgramCatPriorInitializer(new TagdictInformedAtomCatDistInitializer(atomLambda = 1000.0), pTerm, pMod, pFwd)

    val rootDistInit = rootinit match {
      case "x" if learningOpt == "sup" => null
      case "uniform" => new UniformRootDistInitializer()
      case "catprior" => new CatpriorRootDistInitializer(uniatomCatpriorInit)
      case "tdecatprior" => new CatpriorRootDistInitializer(tdeatomCatpriorInit)
      case "zcheat" => null
    }

    val (binyDistInit: BinaryDistInitializer, unryDistInit: UnaryDistInitializer) = ntprodInit match {
      case "x" if learningOpt == "sup" => null
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
      //      case "zcheat" => new TermProdDistInitializerEmInitializer(new CheatingTermProdDistInitializer(CHEATING_SUPERVISED_DATA))
    }

    val (lctxDistInit, rctxDistInit) = trinit match {
      case "x" if learningOpt == "sup" || modelOpt == "pcfg" => (null, null)
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

      case _ => sys.error(f"invalid option: --tr-init trinit : $argString")
    }

    (rootDistInit, binyDistInit, unryDistInit, emisDistInit, lctxDistInit, rctxDistInit)
  }

  def makeGoldAnnoExtractor(
    annotator: String,
    bracketProportion: Double,
    bracketHighMatchBase: Boolean,
    bracketCats: (Cat => Boolean),
    trainBaseCats: Boolean,
    trainHighCats: Boolean,
    depProportion: Double,
    allRawText: Iterator[Vector[String]],
    name: String): Option[GoldAnnoExtractor] = {
    if (annotator != "none") { // && (((bracketProportion > 0.0 || bracketHighMatchBase) && (trainBaseCats || trainHighCats)) || depProportion > 0.0)) {
      val bracketTreeAug = new BracketAnnotationTreeAugmenter(bracketProportion, bracketHighMatchBase, bracketCats, trainBaseCats, trainHighCats)
      annotator match {
        case "gold" =>
          var e: GoldAnnoExtractor =
            if (bracketProportion > 0.0) new AugDelegateGoldAnnoExtractor(bracketTreeAug)
            else new NoOpGoldAnnoExtractor()
          if (depProportion > 0.0) e = new DepAddingGoldAnnoExtractor(depProportion, e)
          Some(e)
        case "upp" =>
          var e: GoldAnnoExtractor =
            if (bracketProportion > 0.0) {
              new ChunkerPseudoGoldAnnoExtractor(
                if (trainHighCats) {
                  //val partialParser: PartialParser = new UpparsePartialParserTrainer().trainParser(allRawText)
                  val m = File("data/raw.upp").readLines.grouped(3).take(sentencesToTake).map { case Seq(s, a, _) => s -> a }.toMap
                  val partialParser: PartialParser = new PartialParser() {
                    def parseAll(sentences: Vector[Vector[Word]]): Vector[AnnotationTree] = sentences.map { s =>
                      UpparseTool.toAnnotationTree(s, m(s.mkString(" ")).replace("(", "{|{").replace(")", "}|}"))
                    }
                  }
                  new AugDelegatePartialParserAnnoExtractor(partialParser, bracketTreeAug)
                }
                else {
                  //                  val chunker: Chunker = new UpparseChunkerTrainer(name).trainChunker(allRawText)
                  //                	new AugDelegateChunkerAnnoExtractor(chunker, bracketTreeAug)

                  val m = File("data/raw.upp").readLines.grouped(3).take(sentencesToTake).map { case Seq(s, a, _) => s -> a }.toMap
                  val partialParser: PartialParser = new PartialParser() {
                    val baseOnly = new BracketAnnotationTreeAugmenter(baseCats = true, highCats = false)
                    def parseAll(sentences: Vector[Vector[Word]]): Vector[AnnotationTree] = sentences.map { s =>
                      baseOnly(UpparseTool.toAnnotationTree(s, m(s.mkString(" ")).replace("(", "{|{").replace(")", "}|}")))
                    }
                  }
                  new AugDelegatePartialParserAnnoExtractor(partialParser, bracketTreeAug)
                })
            }
            else new NoOpGoldAnnoExtractor()
          if (depProportion > 0.0) throw new RuntimeException("annotator is upp, so depProportion can't be non-zero")
          Some(e)
        case "hum" => None
      }
    }
    else {
      None
    }
  }

  def run(): Unit = {

    println(f"reader=$reader")

    def allRawTextF: Iterator[Vector[String]] = reader.raw.take(sentencesToTake)
    val allRawWordTypes = time("all raw text", allRawTextF.flatten.toSet)

    val trainGoldAnnoExtractor = makeGoldAnnoExtractor(trainAnnotator, trainBracketProportion, trainBracketHighMatchBase, trainBracketCats, trainBaseCats, trainHighCats, trainDepProportion, allRawTextF, trainSerializedGC.getOrElse("upp"))
    val testGoldAnnoExtractor = makeGoldAnnoExtractor(testAnnotator, testBracketProportion, testBracketHighMatchBase, testBracketCats, testBaseCats, testHighCats, testDepProportion, allRawTextF, testSerializedGC.getOrElse("upp"))
    println(f"trainGoldAnnoExtractor = $trainGoldAnnoExtractor")
    println(f"testGoldAnnoExtractor  = $testGoldAnnoExtractor")

    println(f"reader = $reader")
    println(f"sentencesToTake=$sentencesToTake")
    println(f"numTrainSentences=$numTrainSentences")
    println(f"maxTrainTokens=$maxTrainTokens")
    println(f"numTestSentences=$numTestSentences")

    val tdData = time("read td data", reader.tdData /*.take(sentencesToTake)*/ .map(_.tagged).takeSub(tdTokToUse).toVector) // total: 323190
    println(f"TD data tokens: ${tdData.sumBy(_.size)}")
    println(f"TD data sentences: ${tdData.size}")
    val requiredTagMappings = /*if (tdIncludesRequiredMappings) (testData ++ parseTrainData).flatMap(_.tagged).groupByKey.mapVals(_.toSet) else*/ Map.empty[String, Set[Cat]]
    val fullWordSet = allRawWordTypes ++ (tdData.flatten /*++ (testData ++ parseTrainData).flatMap(_.tagged)*/ ).map(_._1)
    val fullSupertagSet = (tdData.flatten /*++ (testData ++ parseTrainData).flatMap(_.tagged)*/ ).map(_._2).toSet
    val fullTdData = tdData /*++ (if (tdIncludesTest) (testData ++ parseTrainData).map(_.tagged) else Vector())*/
    println(f"Full TD data sentences: ${fullTdData.size}")
    println(f"Full TD data tokens: ${fullTdData.sumBy(_.size)}")

    val se = CatStartEndTags
    val unclusteredTagdictWithoutAllRequired: TagDictionary[Cat] = time("make td", new SimpleTagDictionaryFactory(Some(tdCutoff))(fullTdData, "<S>", se.startTag, "<E>", se.endTag)).withWords(fullWordSet) //.withTags(fullSupertagSet)
    val unclusteredTagdict = SimpleTagDictionary(
      unclusteredTagdictWithoutAllRequired.entries.mapt { (w, ts) => w -> (ts | requiredTagMappings.getOrElse(w, Set.empty)) }, // don't narrow down the tag choices, but don't exclude any required tags either. if a word is in the TD, add its required tags.  if it's not found, then don't do anything since the default tag set will contain any required tags. 
      unclusteredTagdictWithoutAllRequired.startWord, unclusteredTagdictWithoutAllRequired.startTag, unclusteredTagdictWithoutAllRequired.endWord, unclusteredTagdictWithoutAllRequired.endTag, unclusteredTagdictWithoutAllRequired.allWords, unclusteredTagdictWithoutAllRequired.allTags, unclusteredTagdictWithoutAllRequired.excludedTags)

    val tagdict = time("make tagdict", numClusters.map { numClusters =>
      val fullWordSetLowercase = fullWordSet.map(_.toLowerCase)
      val clustersById: Map[ClusterId, Set[String]] = File(f"data/brown-clusters/ccgbank-$numClusters-lc.txt").readLines.map(_.splitWhitespace).collect { case Vector(cid, word, _) if fullWordSetLowercase(word) => (ClusterId(cid), word) }.groupByKey.mapVals(_.toSet)
      val clusterIdByWord: Map[String, ClusterId] = clustersById.ungroup.map(_.swap).groupByKey.mapVals(_.head) // TODO: { case Coll(id) => id }
      val fullClusterWordset: Set[String] = clustersById.map(_._2).flatten.toSet
      (fullWordSetLowercase -- fullClusterWordset).foreach { w => println(f"Not in clusters: $w") }; assert((fullWordSetLowercase -- fullClusterWordset).isEmpty)
      println(f"Number of clusters = ${clustersById.size}")
      val unclusteredTdEntries: Map[String, Set[Cat]] = ((unclusteredTagdict.entries |+| requiredTagMappings) - unclusteredTagdict.startWord - unclusteredTagdict.endWord).toVector.mapt((w, ts) => w.toLowerCase -> ts).groupByKey.mapVals(_.flatten.toSet)
      val clusterTagsetsById: Map[ClusterId, Set[Cat]] = clustersById.mapVals(_.flatMap(unclusteredTdEntries.get).flatten)
      val m: Map[String, Set[Cat]] = fullWordSet.mapTo(w => clusterTagsetsById(clusterIdByWord(w.toLowerCase))).filter(_._2.nonEmpty).toMap
      SimpleTagDictionary(m, unclusteredTagdict.startWord, unclusteredTagdict.startTag, unclusteredTagdict.endWord, unclusteredTagdict.endTag, unclusteredTagdict.allWords, unclusteredTagdict.allTags, unclusteredTagdict.excludedTags)
    }.getOrElse {
      unclusteredTagdict
    })
    println(f"num td entries = ${tagdict.entries.values.sumBy(_.size)}")
    //        for ((w, t) <- (parseTrainData ++ testData).flatMap(_.tagged)) {
    //          assert(tagdict(w).contains(t), f"Not found in TD!  ${w}%-20s  ${t}%-20s")
    //        }

    val ata = new StandardTagDictAdditionalTagAdder[Cat]()
    val fbata = new StandardTagDictAndFwdBkdAdditionalTagAdder()
    val applicationGCB = new SimpleCfgGuideChartBuilder(rules, ata, allowTerminalDeletion = false)
    //val fbApplicationGCB = new SimpleCfgGuideChartBuilder(rules, fbata, allowTerminalDeletion = false)
    val fbApplicationGCB = new SimpleCfgGuideChartBuilder(rules, ata, allowTerminalDeletion = true)
    val fudgApplicationGCB = FudgDecisionCfgGuideChartBuilder(rules, ata, allowTerminalDeletion = false)
    //val fudgFbApplicationGCB = FudgDecisionCfgGuideChartBuilder(rules, fbata, allowTerminalDeletion = false)
    val fudgFbApplicationGCB = FudgDecisionCfgGuideChartBuilder(rules, ata, allowTerminalDeletion = true)
    def getGCB(fb: Boolean) = {
      if (punctSplit) {
        throw new RuntimeException("punctSplit not currently supported")
        //        def gcbPunctWrapper(gcb: CfgGuideChartBuilder) = new PunctBracketingCfgGuideChartBuilder(Set(",", ";", "."), gcb)
        //        val punctApplicationGCB = gcbPunctWrapper(applicationGCB)
        //        val punctFbApplicationGCB = gcbPunctWrapper(fbApplicationGCB)
        //        val trainerGuideChartBuilder = new CascadingAttemptsCfgGuideChartBuilder(Vector(punctApplicationGCB, applicationGCB))
        //        val parserGuideChartBuilder = new CascadingAttemptsCfgGuideChartBuilder(Vector(punctApplicationGCB, applicationGCB, punctFbApplicationGCB, fbApplicationGCB))
        //        (trainerGuideChartBuilder, parserGuideChartBuilder)
      }
      else {
        val gcb1 =
          if (fb) Vector( /*fudgApplicationGCB, fudgFbApplicationGCB, new BracketOnlyCfgGuideChartBuilder(fudgApplicationGCB), new BracketOnlyCfgGuideChartBuilder(fudgFbApplicationGCB),*/ applicationGCB, fudgFbApplicationGCB)
          else Vector(fudgApplicationGCB /*, new BracketOnlyCfgGuideChartBuilder(fudgApplicationGCB) , applicationGCB*/ )
        new CascadingAttemptsCfgGuideChartBuilder(gcb1)
      }
    }
    val trainerGuideChartBuilder = getGCB(trainGCB.contains("fb"))
    val parserGuideChartBuilder = getGCB(testGCB.contains("fb"))

    def deserializeGCs(dataIterator: Iterator[CcgTree], useSerializedGC: Boolean, serializedGC: Option[String], gcb: CfgGuideChartBuilder, filterOutFailedGCs: Boolean, maxNumSentences: Int, maxNumTokens: Int, annotatedSentencesProportion: Double, goldAnnoExtractor: Option[GoldAnnoExtractor]): Iterator[(Option[CfgGuideChart], CcgTree)] = {
      assert(useSerializedGC, "useSerializedGC must be true")
      val gcSerDeser = new CfgGuideChartSerDeser()
      val serializedFileName = serializedGC.getOrElse(sys.error("no serialized file name given for reading"))
      val serializedFile = File(f"data/gcser/ccgbank/${serializedFileName}.gz")
      assert(serializedFile.exists, f"serialization file not found for reading: $serializedFile")
      time(f"retrieving from serialized: $serializedFile", {
        println(f"maxNumSentences=$maxNumSentences")
        println(f"maxNumTokens=$maxNumTokens")
        println(f"filterOutFailedGCs=$filterOutFailedGCs")
        val tMap = dataIterator.map(t => t.toString -> t).toMap
        val blocks = BufferedReaderIterator(GzFileBufferedReader(serializedFile)).split("")
        blocks.zipWithIndex.map {
          case (tstr +: gcser, i) =>
            //assert(t.toString == tstr, f"sentence $i\ntrue sentence:     ${t.words.mkString(" ")}\n${t.toString}\nser file sentence: ${" ([^\\s\\]]+)\\]".r.allGroups(tstr).map(_.head).mkString(" ")}\n$tstr")
            if (i % 100 == 0 && i > 0) println(f"  deserializing $i")
            val gc =
              if (gcser == Vector("None")) None
              else Some( /*time(f"  deserialize gc $i, length=${t.length}",*/ gcSerDeser.deser(gcser)) //)
            //assert(trainerGuideChartBuilder.build(t.words, Vector.empty, tagdict).map(_.matrix) == gc.map(_.matrix)) // check the serialized charts
            (gc, tMap(tstr))
        }.filter { case (gc, tstr) => !filterOutFailedGCs || gc.isDefined }.take(maxNumSentences).takeWhileAg(_.sumBy(_._2.length) <= maxNumTokens)
        //.zipWithIndex.map { case ((gc, t), i) => println(f"$i -- ${gc.isDefined}"); (gc, t) }
      })
    }

    def makeGC(dataIterator: Iterator[CcgTree], useSerializedGC: Boolean, serializedGC: Option[String], gcb: CfgGuideChartBuilder, filterOutFailedGCs: Boolean, maxNumSentences: Int, maxNumTokens: Int, annotatedSentencesProportion: Double, goldAnnoExtractor: Option[GoldAnnoExtractor]): Vector[(Option[CfgGuideChart], CcgTree)] = {
      val gcSerDeser = new CfgGuideChartSerDeser()
      assert(dataIterator.nonEmpty, f"dataIterator is empty!")
      if (maxNumSentences == 0) {
        println("NOTHING TO DO: maxNumSentences==0")
        Vector.empty
      }
      else if (justSerialize) {
        assert(goldAnnoExtractor.isEmpty || (maxNumSentences < Int.MaxValue), "if an annotation extractor is given, the number of sentences must be constrained")
        assert(maxNumTokens == Int.MaxValue, "maxNumTokens not supported at this time")
        val serializedFileName = serializedGC.getOrElse(sys.error("no serialized file name given for writing"))
        val serializedFile = File(f"data/gcser/ccgbank/${serializedFileName}.gz")
        time(f"writing serialized sentences to $serializedFile", {
          using(new BufferedWriter(new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(serializedFile))))) { f =>
            val dataIteratorIndexed = dataIterator.zipWithIndex
            val numToAnnotate: Int = "%.0f".format(maxNumSentences * annotatedSentencesProportion).toInt
            var accumulated = Vector.empty[(Option[CfgGuideChart], CcgTree)]
            var rejectedWithAnnotation = Vector.empty[(CcgTree, Int)]
            var numParsedFromAnnotation = 0
            while (accumulated.size < numToAnnotate && dataIteratorIndexed.hasNext) {
              val group: Vector[(CcgTree, Int)] = {
                val b = new VectorBuilder[(CcgTree, Int)]
                for (_ <- 1 to 100 if dataIteratorIndexed.hasNext) { b += dataIteratorIndexed.next }
                b.result()
              }
              val gcGroup: Vector[((Option[CfgGuideChart], Boolean), CcgTree, Int)] = group.toVector.PARALLEL.map {
                case (t, i) =>
                  (time1(f"build annotated gc $i", {
                    val a = goldAnnoExtractor.map(_.apply(t))
                    val gcWithAnnotation = gcb.build(t.words, a, tagdict)
                    val gc = if (gcWithAnnotation.isDefined) gcWithAnnotation else gcb.build(t.words, None, tagdict)

                    //                    println(f"${t.words.mkString(" ")}")
                    //                    println(f"  has annotation extractor: ${goldAnnoExtractor.isDefined}")
                    //                    println(f"  has annotation:           ${a.isDefined}")
                    //                    println(f"  annotation parseable:     ${gcWithAnnotation.isDefined}")
                    //                    println(f"  at all parseable:         ${gc.isDefined}")

                    (gc, gcWithAnnotation.isDefined)
                  }), t, i)
              }.seq
              val gcGroupIterator = gcGroup.iterator
              breakable {
                while (accumulated.size < numToAnnotate && gcGroupIterator.hasNext) {
                  val ((gc, fromAnn), t, i) = gcGroupIterator.next
                  if (!filterOutFailedGCs || gc.isDefined) {
                    f.wl(t);
                    //println(f"serializing ${t.words.mkString(" ")}  (${gc.isDefined})")
                    if (gc.isDefined) {
                      gcSerDeser.ser(gc.get).foreach(f.wl)
                      if (fromAnn) numParsedFromAnnotation += 1
                    }
                    else {
                      f.wl("None")
                      //rejectedWithAnnotation :+= (t -> i)
                    };
                    f.wl;
                    accumulated :+= ((gc, t))
                  }
                }
              }
              rejectedWithAnnotation ++= gcGroupIterator.map { case (_, t, i) => (t, i) }
              //                ).filter { case (gc, t) => !filterOutFailedGCs || gc.isDefined }.take(numToAnnotate - accumulated.size) //.takeWhileAg(_.sumBy(_._2.length) <= maxNumTokens)
              //                  //.zipWithIndex.map { case ((gc, t), i) => println(f"$i -- ${gc.isDefined}"); (gc, t) }
              //                  .toVector
            }

            println(f"Finished ${accumulated.size} annotated sentences  (parses found for ${accumulated.count(_._1.isDefined)} of them, $numParsedFromAnnotation parsed from annotation)")
            println(f"  ${rejectedWithAnnotation.size} annotated sentences rejected")

            val unannotated = (rejectedWithAnnotation.iterator ++ dataIteratorIndexed).grouped(100).flatMap { group =>
              val gcGroup = group.toVector.PARALLEL.map { case (t, i) => (time1(f"build unannotated gc $i", gcb.build(t.words, None, tagdict)), t) }.seq
              for ((gc, t) <- gcGroup.iterator) yield {
                f.wl(t);
                //println(f"serializing ${t.words.mkString(" ")}  (${gc.isDefined})")
                if (gc.isDefined) { gcSerDeser.ser(gc.get).foreach(f.wl) } else { f.wl("None") }; f.wl; (gc, t)
              }
            }.filter { case (gc, t) => !filterOutFailedGCs || gc.isDefined }.take(maxNumSentences - accumulated.size) //.takeWhileAg(_.sumBy(_._2.length) <= maxNumTokens)
              //.zipWithIndex.map { case ((gc, t), i) => println(f"$i -- ${gc.isDefined}"); (gc, t) }
              .toVector

            accumulated ++ unannotated
          }
        })
      }
      else if (useSerializedGC) {
        deserializeGCs(dataIterator, useSerializedGC, serializedGC, gcb, filterOutFailedGCs, maxNumSentences, maxNumTokens, annotatedSentencesProportion, goldAnnoExtractor).toVector
      }
      else {
        throw new RuntimeException("don't try to compute and use guide charts at the same time.")
        assert(serializedGC.isEmpty, "don't try to serialize and use guide charts at the same time.")
        val dataAndAnnotationIterator: Iterator[(CcgTree, Option[FudgSentence])] = dataIterator.mapTo(t => (random.nextDouble < annotatedSentencesProportion).option(goldAnnoExtractor.map(_.apply(t))).flatten)
        val gcVector =
          time(f"guide-charting ${ /*totalCount*/ ""} sentences", {
            dataAndAnnotationIterator.zipWithIndex.map {
              case ((t, a), i) =>
                //if (i != 0 && (i % ((totalCount / 100) + 1)) == 0) print(".")
                //println(f"annotation = $a")
                //TreeViz.drawTree(a.get.fudgTree)
                val gc = time(f"build gc $i", gcb.build(t.words, a, tagdict))
                gc.foreach { gca =>
                  println(f"  GC found with ${gca.numPossibleParses} parses")
                  //    println(f"  attempting without annotations")
                  //    gcb.build(t.words, None, tagdict).foreach { gcWithoutAnnotation =>
                  //      val woAnn = gcWithoutAnnotation.numPossibleParses
                  //      val wAnn = gca.numPossibleParses
                  //      val red = BigDecimal(woAnn - wAnn) / BigDecimal(woAnn)
                  //      println(f"  Possible parses ${wAnn} vs ${woAnn}, relative reduction of ${red}")
                  //    }
                }
                if (gc.isEmpty) println("  No GC found")
                (gc, t)
            }.filter { case (gc, t) => !filterOutFailedGCs || gc.isDefined }.take(maxNumSentences).takeWhileAg(_.sumBy(_._2.length) <= maxNumTokens)
          }).toVector
        println("done")
        gcVector
      }
    }

    val rawDataGC: Vector[CfgGuideChart] = {
      val (parseTrainDataIterator, annoExtractor): (Iterator[CcgTree], Option[GoldAnnoExtractor]) = {
        val Re = """(\d+): (.+)""".r
        val annotations: Vector[(Int, AnnotationTree)] = File("human-annotated").ls(".*\\.txt".r).sortBy(_.name).flatMap { f =>
          f.readLines.map {
            case Re(UInt(n), s) => n -> UpparseTool.toAnnotationTree(s.replaceAll("\\((\\S+)\\)", "$1").replace("(", "{|{").replace(")", "}|}"))
          }
        }

        //        reader.rawCHEATING.toVector.zipWithIndex.shuffle.grouped(50).zipWithIndex.foreach {
        //          case (g, i) => writeUsing(File(f"human-annotated/raw-$i%03d.txt")) { f => for ((t, j) <- g) f.wl(f"$j: ${t.words.mkString(" ")}") }
        //        }; throw new RuntimeException("done")

        val parseTrainData: Map[Int, CcgTree] = time("read train data", reader.rawCHEATING.take(sentencesToTake)).zipWithIndex.map(_.swap).toMap
        println(f"parseTrainData.size=${parseTrainData.size}")
        val parseTrainDataIterator = annotations.iterator.flatMap { case (n, _) => parseTrainData.get(n) }

        val goldAnnoExtractor =
          if (trainAnnotator == "hum") {
            val indexedAnnotations = annotations.toMap
            val treeIndex: Map[CcgTree, Int] = parseTrainData.map(_.swap)
            val annoExtractor = new GoldAnnoExtractor() {
              def apply(ccgTree: CcgTree): FudgSentence = {
                val (tokens, tokenNodes) = ChunkerAnnoExtractor.makeTokens(ccgTree.words)
                indexedAnnotations(treeIndex(ccgTree)).toFudgSentence(tokens, tokenNodes)
              }
            }
            Some(annoExtractor)
          }
          else trainGoldAnnoExtractor
        (parseTrainDataIterator, goldAnnoExtractor)
      }
      val trainDataGC: Vector[(Option[CfgGuideChart], CcgTree)] = if (learningOpt == "sup") Vector.empty else makeGC(parseTrainDataIterator, useSerializedTrainGC, trainSerializedGC, trainerGuideChartBuilder, filterOutFailedGCs = false, numTrainSentences, maxTrainTokens, trainAnnotatedSentencesProportion, annoExtractor) /*.filter(_._1.isDefined).take(1000)*/ .toVector
      println(f"train data sentences: ${trainDataGC.size}  (parses found for ${trainDataGC.count(_._1.isDefined)} of them)")
      trainDataGC.flatMap(_._1)
    }
    println(f"raw data sentences: ${rawDataGC.size}")
    println(f"raw data tokens: ${rawDataGC.sumBy(_.words.size)}")
    //println(f"raw data annotated sentences: ${rawAnnotations.count(_.isDefined)}  (${rawAnnotations.count(_.isDefined) / parseTrainData.size.toDouble}%.2f, though trainAnnotatedSentencesProportion=$trainAnnotatedSentencesProportion%.2f)")

    if (!useSerializedTestGC) {
      val testDataIterator: Iterator[CcgTree] = time("read test data", reader.testData.take(sentencesToTake))
      val testDataGcAndTrees: Vector[(Option[CfgGuideChart], CcgTree)] = makeGC(testDataIterator, useSerializedTestGC, testSerializedGC, parserGuideChartBuilder, filterOutFailedGCs = false, numTestSentences, Int.MaxValue, testAnnotatedSentencesProportion, testGoldAnnoExtractor) /*.take(1500)*/ // TODO: Worry about pre-training tagdict?
      val testDataGC = testDataGcAndTrees.flatMap(_._1)
      println(f"test data sentences: ${testDataGC.size}")
      println(f"test data tokens: ${testDataGC.sumBy(_.words.size)}")
    }

    if (!justSerialize) {

      //      println(f"trainDataGC: ${trainDataGC.flatMap(_._1).size}/${trainDataGC.size} sentences, ${trainDataGC.flatMap(_._1).sumBy(_.length)}")
      //      println(f"testDataGC:  ${testDataGC.flatMap(_._1).size}/${testDataGC.size} sentences, ${testDataGC.flatMap(_._1).sumBy(_.length)}")

      val CHEATING_SUPERVISED_DATA = time("reading fullCorpusDONTUSE", reader.fullCorpusDONTUSE.take(sentencesToTake).toVector)

      println(f"tdCutoff=$tdCutoff")
      println(f"tdIncludesRequiredMappings=$tdIncludesRequiredMappings")
      assert(!tdIncludesRequiredMappings, f"tdIncludesRequiredMappings not allowed")
      println(f"tdIncludesTest=$tdIncludesTest")
      assert(!tdIncludesTest, f"tdIncludesTest not allowed")
      println(f"evaluator=$evaluator")

      println(f"Number of tags: ${tagdict.allTags.size}")

      //      println(f"Test data::  ambiguous tokens:  ${testDataGC.flatMap(_._2.words).map(tagdict(_).size).filter(_ > 1).size * 100.0 / testDataGC.flatMap(_._2.words).size}%.2f (${testDataGC.flatMap(_._2.words).map(tagdict(_).size).filter(_ > 1).size}/${testDataGC.flatMap(_._2.words).size}),  token ambiguity ${testDataGC.flatMap(_._2.words).map(tagdict(_).size).sum / testDataGC.flatMap(_._2.words).size.toDouble}%.2f (${testDataGC.flatMap(_._2.words).map(tagdict(_).size).sum}/${testDataGC.flatMap(_._2.words).size})")
      //      println(f"All data::  ambiguous tokens:  ${fullTdData.flatMap(_.map(_._1)).map(tagdict(_).size).filter(_ > 1).size * 100.0 / fullTdData.flatMap(_.map(_._1)).size}%.2f (${fullTdData.flatMap(_.map(_._1)).map(tagdict(_).size).filter(_ > 1).size}/${fullTdData.flatMap(_.map(_._1)).size}),  token ambiguity ${fullTdData.flatMap(_.map(_._1)).map(tagdict(_).size).sum / fullTdData.flatMap(_.map(_._1)).size.toDouble}%.2f (${fullTdData.flatMap(_.map(_._1)).map(tagdict(_).size).sum}/${fullTdData.flatMap(_.map(_._1)).size})")
      //SingleChart(HistogramDatasetBuilder(testData.flatMap(_.words.map(tagdict(_).size.toDouble))).s(0).w(1).build, BarRenderer()).draw()
      //println(f"Test data:  ${}%.2f (${}/${}),  ${}%.2f (${}/${})")

      val (rootDistInit, binyDistInit, unryDistInit, emisDistInit, lctxDistInit, rctxDistInit) = makeInits(tagdict, CHEATING_SUPERVISED_DATA)

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
      def pcfgExtracter(p: GuideChartParser): PcfgParser = p match {
        case x: PcfgParser => x
        case x: ExactScgParser => new PcfgParser(x.rootDist, x.prodDist)
        case x: NumPossibleParsesDelegatingKBestGuideChartParser => pcfgExtracter(x.exactParser)
        //case x: RerankParser => pcfgExtracter(x.delegate)
      }

      val smoothingParameter = 1.0 / rawDataGC.size
      def supHmmTrainer(delta: Double) = new AddLambdaSmoothedHmmTaggerTrainer[Cat](delta)
      def supPcfgTrainer(delta: Double) = new AlphaBetaSupPcfgTrainer(priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, alphaRoot, alphaProd, priorBinyProdMix, priorUnryProdMix, priorTermProdMix, pcfgProductionFinder, pcfgParserInstantiater)
      def supScgTrainer(delta: Double) = new AlphaBetaSupScgTrainer(priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, priorLctxDist, priorRctxDist, alphaRoot, alphaProd, alphaLctx, alphaRctx, priorBinyProdMix, priorUnryProdMix, priorTermProdMix, scgProductionFinder, scgParserInstantiater)(se)

      //val supHmmModel = supHmmTrainer(smoothingParameter * parseTrainData.size).train(parseTrainData.map(_.tagged), tagdict)
      //val supDdModel = new DualParser(supPcfgModel, supHmmModel, tagdict, parserGuideChartBuilder, maxDecompIterations, deltaConst, verbose = true)

      val testDataGC: Stream[(Option[CfgGuideChart], CcgTree)] = {
        val testDataIterator: Iterator[CcgTree] = time("read test data", reader.testData.take(sentencesToTake))
        val itr = deserializeGCs(testDataIterator, useSerializedTestGC, testSerializedGC, parserGuideChartBuilder, filterOutFailedGCs = false, numTestSentences, Int.MaxValue, testAnnotatedSentencesProportion, testGoldAnnoExtractor) /*.take(1500)*/ // TODO: Worry about pre-training tagdict?
        itr.toStream
      }
      //println(f"test data sentences: ${testDataGC.size}  (parses found for ${testDataGC.count(_._1.isDefined)} of them)")
      //println(f"test data tokens: ${testDataGC.sumBy(_._2.words.length)}")
      //println(f"test data annotated sentences: ${testAnnotations.count(_.isDefined)}  (${testAnnotations.count(_.isDefined) / testData.size.toDouble}%.2f)")

      learningOpt match {
        //        case "sup" => modelOpt match {
        //          case "pcfg" =>
        //
        //            val trainer = supPcfgTrainer(1.0)
        //            val supPcfgModel = trainer.train(parseTrainData)
        //            //println("Full PCFG Parser on train"); println(argString); evaluator.evaluate(mcmcPcfgModel, trainDataGC, tagdict, verbose = false) // doesn't use parserGCB
        //            println("Full PCFG Parser on test"); println(argString); evaluator.evaluate(supPcfgModel, testDataGC, tagdict, verbose = false)
        //
        //          case "scg" =>
        //            println(f"supTrainer = ${supScgTrainer(smoothingParameter)}")
        //            val supScgModel = time("supScgTrainer.train", supScgTrainer(smoothingParameter * parseTrainData.size).train(parseTrainData))
        //            //println("SUP SCG on train"); println(args.mkString(" ")); evaluator.evaluate(supScgModel, trainDataGC, tagdict, verbose = false) // doesn't use parserGCB
        //            println("SUP SCG on test"); println(argString); evaluator.evaluate(supScgModel, testDataGC, tagdict, verbose = false)
        //
        //            //
        //
        //            println(f"supTrainer = ${supPcfgTrainer(smoothingParameter)}")
        //            val supPcfgModel = time("supPcfgTrainer.train", supPcfgTrainer(smoothingParameter * parseTrainData.size).train(parseTrainData))
        //            //println("SUP PCFG on train"); println(argString); evaluator.evaluate(supPcfgModel, trainDataGC, tagdict, verbose = false) // doesn't use parserGCB
        //            println("SUP PCFG on test"); println(argString); evaluator.evaluate(supPcfgModel, testDataGC, tagdict, verbose = false)
        //
        //            val unsmoothedSupPcfgModel = new UnsmoothedSupPcfgTrainer(pcfgProductionFinder, pcfgParserInstantiater).train(parseTrainData)
        //            println("Unsmoothed supervised (MLE) PCFG on train"); println(argString); evaluator.evaluate(unsmoothedSupPcfgModel, trainDataGC, new SimpleTagDictionaryFactory(Some(tdCutoff)).apply(parseTrainData.map(_.tagged), "<S>", se.startTag, "<E>", se.endTag), verbose = false)
        //
        //          //            case "dd-sep" =>
        //          //              //println(f"supTrainer = $supDdTrainer")
        //          //              println(argString); evaluator.evaluate(supDdModel, testData, tagdict, verbose = false)
        //        }

        case "mcmc" =>
          val initPcfg = pcfgParserInstantiater(priorRootDist, priorProdDist)
          val initScgModel = scgParserInstantiater(priorRootDist, priorProdDist, priorLctxDist, priorRctxDist)(se)
          //val initHmm = new HmmTagger(priorTranDist, priorEmisDist, tagdict)
          //val initDdModel = new DualParser(initPcfg, initHmm, tagdict, parserGuideChartBuilder, maxDecompIterations, verbose = true)

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
              //println(f"\nInit PCFG")
              //println(argString); evaluator.evaluate(new GuideChartParserAdapter(parserGuideChartBuilder, initPcfg), testData, tagdict, verbose = false)
              println(f"\nMCMC PCFG")
              val mcmcPcfgModel: GuideChartParser = mcmcPcfgTrainer.trainGuideChartsWithSomeGold(rawDataGC, Vector.empty)

              //              // Serialize the parser model
              //              val trainedModelName = "something"
              //              val serializedFile = File(f"data/trained_models/$trainedModelName.parser")
              //              serializedFile.getParentFile.mkdirs()
              //              time(f"write pcfg parser model to $serializedFile", using(new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(serializedFile)))) { out => out.writeObject(mcmcPcfgModel) })
              //
              //              assert(serializedFile.exists, f"serialized parser file does not exist: $trainedModelName")
              //              val deserializedParser: GuideChartParser =
              //                using(new ObjectInputStream(new GZIPInputStream(new FileInputStream(serializedFile)))) { in => in.readObject().asInstanceOf[GuideChartParser] }
              //println("Full PCFG Parser on train"); println(argString); evaluator.evaluate(deserializedParser, trainDataGC, tagdict, verbose = false) // doesn't use parserGCB

              println("Full PCFG Parser on test"); println(argString); evaluator.evaluate(mcmcPcfgModel, testDataGC, tagdict, verbose = false)

            case "ipcfg" =>
              val infCatPrior = new CatgramInfCatPriorInitializer(
                new TagdictInformedAtomCatDistInitializer(atomLambda = 1000.0),
                allPunc = Set(cat".", cat",", cat";", cat":", cat"LRB", cat"RRB").map(_.asInstanceOf[PuncCat]),
                puncDist = new SimpleLogProbabilityDistribution(Map(
                  cat"," -> 0.50,
                  cat"." -> 0.45,
                  cat":" -> 0.02,
                  cat";" -> 0.01,
                  cat"LRB" -> 0.01,
                  cat"RRB" -> 0.01).mapt((c, p) => c.asInstanceOf[PuncCat] -> LogDouble(p)).normalizeValues), pPunc = 0.1,
                pTerm, pMod, pFwd).fromKnownSupertagSets(rawDataGC.map(s => s.words zipSafe s.supertagSets), tagdict)
              val mcmcPcfgTrainer = new McmcInfPcfg(
                samplingIterations,
                burninIterations,
                pcfgAlphaPriorMaker,
                pcfgProductionFinder,
                initialParserInstantiater = new ExactPcfgParserInstantiater(),
                DirSampler,
                /*priorRootDist, priorBinyDist, priorUnryDist,*/ priorTermDist,
                rootSet = Set(cat"S", cat"NP"),
                binaryRules = Set(FA, BA), unaryRules = Set(N2NP),
                alphaRoot = alphaRoot, alphaBiny = alphaBiny, alphaUnry = alphaUnry, alphaTerm = alphaTerm,
                alphaLambda = alphaLambda, priorBinyProdMix = priorBinyProdMix, priorUnryProdMix = priorUnryProdMix, priorTermProdMix = priorTermProdMix,
                qBetaA = LogDouble(0.1), qBetaB = LogDouble.one,
                trainerGuideChartBuilder,
                new SimplePcfgGuideChartProdFinder(),
                infCatPrior,
                new MersenneTwister(),
                accumulate = false,
                verbose = verbose)

              println(f"mcmcTrainer = $mcmcPcfgTrainer")
              println(f"  rootDistInit=$rootDistInit")
              println(f"  binyDistInit=$binyDistInit")
              println(f"  unryDistInit=$unryDistInit")
              println(f"  emisDistInit=$emisDistInit")
              //println(f"\nInit PCFG")
              //println(argString); evaluator.evaluate(new GuideChartParserAdapter(parserGuideChartBuilder, initPcfg), testData, tagdict, verbose = false)
              println(f"\nMCMC PCFG")
              val mcmcPcfgModel: GuideChartParser = supPcfgTrainer(1.0).train(mcmcPcfgTrainer.trainGuideChartsWithSomeGold(rawDataGC, Vector.empty, Vector.empty))
              //mcmcPcfgTrainer.trainGuideChartsWithSomeGold(rawDataGC, Vector.empty)

              //              // Serialize the parser model
              //              val trainedModelName = "something"
              //              val serializedFile = File(f"data/trained_models/$trainedModelName.parser")
              //              serializedFile.getParentFile.mkdirs()
              //              time(f"write pcfg parser model to $serializedFile", using(new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(serializedFile)))) { out => out.writeObject(mcmcPcfgModel) })
              //
              //              assert(serializedFile.exists, f"serialized parser file does not exist: $trainedModelName")
              //              val deserializedParser: GuideChartParser =
              //                using(new ObjectInputStream(new GZIPInputStream(new FileInputStream(serializedFile)))) { in => in.readObject().asInstanceOf[GuideChartParser] }
              //println("Full PCFG Parser on train"); println(argString); evaluator.evaluate(deserializedParser, trainDataGC, tagdict, verbose = false) // doesn't use parserGCB

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
                priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, priorLctxDist, priorRctxDist,
                alphaRoot = alphaRoot, alphaBiny = alphaBiny, alphaUnry = alphaUnry, alphaTerm = alphaTerm, alphaLctx = alphaLctx, alphaRctx = alphaRctx,
                alphaLambda = alphaLambda, priorBinyProdMix = priorBinyProdMix, priorUnryProdMix = priorUnryProdMix, priorTermProdMix = priorTermProdMix,
                supScgTrainer(1.0),
                Some(((parser: GuideChartParser) => {
                  println("Extracted PCFG Parser on test"); println(argString); evaluator.evaluate(pcfgExtracter(parser), testDataGC, tagdict, verbose = false)
                }, Set(-1, 0, 5, 10, 20, 50, 100, 200, 300, 400) ++ (500 until 10000 by 500))),
                mcmcOutputCountFile,
                verbose)(se)

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
              //println(argString); evaluator.evaluate(initScgModel, testDataGC, tagdict, verbose = false)
              println(f"\nMCMC SCG")
              val mcmcScgModel = mcmcScgTrainer.trainGuideChartsWithSomeGold(rawDataGC, Vector.empty)
              val extractedPcfgModel = pcfgExtracter(mcmcScgModel)
              //println("Extracted PCFG Parser on train"); println(argString); evaluator.evaluate(extractedPcfgModel, trainDataGC, tagdict, verbose = false) // doesn't use parserGCB
              println("Extracted PCFG Parser on test"); println(argString); evaluator.evaluate(extractedPcfgModel, testDataGC, tagdict, verbose = false)
            //println("Full SCG Parser on train"); println(argString); evaluator.evaluate(mcmcScgModel, trainDataGC, tagdict, verbose = false) // doesn't use parserGCB
            //println("Full SCG Parser on test"); println(argString); evaluator.evaluate(mcmcScgModel, testDataGC, tagdict, verbose = false)

            //              case "dd-sep" =>
            //                val mcmcHmmTrainer = new FromSemisupervisedTaggerTrainer(
            //                  new FfbsHmmTaggerTrainer[String, Cat](samplingIterations, burninIterations, alphaTran = alphaTran, alphaEmis = alphaEmis, new AddLambdaTransitionDistributioner(smoothingParameter), new AddLambdaEmissionDistributioner(smoothingParameter), new MersenneTwister),
            //                  trainerGuideChartBuilder)
            //                println(f"mcmcPcfgTrainer = $mcmcPcfgTrainer")
            //                println(f"mcmcHmmTrainer = $mcmcHmmTrainer")
            //                println(f"  rootDistInit=$rootDistInit")
            //                println(f"  nontDistInit=$nontDistInit")
            //                println(f"  emisDistInit=$emisDistInit")
            //                println(f"  trDist=$trDist")
            //                println(f"  emDist=$emDist")
            //                //println(f"\nInit DD-Sep")
            //                //println(argString); evaluator.evaluate(new GuideChartParserAdapter(parserGuideChartBuilder, initDdModel), testData, tagdict, verbose = false)
            //                println(f"\nMCMC DD-Sep")
            //                println(f"Training part 1: MCMC PCFG")
            //                val mcmcPcfgModel = mcmcPcfgTrainer.train(rawData, tagdict, rootDist, nontDist, termDist)
            //                println(argString); evaluator.evaluate(new GuideChartParserAdapter(parserGuideChartBuilder, mcmcPcfgModel), testData, tagdict, verbose = false)
            //                println(f"Training part 2: MCMC HMM")
            //                val mcmcHmmModel = mcmcHmmTrainer.train(rawData, tagdict, trDist, emDist)
            //                println(argString); TaggerEvaluator.apply(mcmcHmmModel, testData.map(_.tagged))
            //                val emDdModel = new DualParser(mcmcPcfgModel.asInstanceOf[WeightedParser], mcmcHmmModel.asInstanceOf[WeightedTagger[String, Cat]], tagdict, parserGuideChartBuilder, maxDecompIterations, verbose = true)
            //                println("Full SCG Parser"); println(argString); evaluator.evaluate(new GuideChartParserAdapter(parserGuideChartBuilder, emDdModel), testData, tagdict, verbose = false)
            //
            //              case "dd-joint" =>
            //                val ddTrainer = new DualParserTrainer(supPcfgTrainer(smoothingParameter * rawData.size), supHmmTrainer(smoothingParameter * rawData.size), maxEmIterations, maxDecompIterations, deltaConst, alphaRoot = alphaRoot, alphaNonts = alphaNonts, alphaTerm = alphaTerm, alphaTran = alphaTran, alphaEmis = alphaEmis, trainerGuideChartBuilder)
            //                println(f"ddTrainer = $ddTrainer")
            //                println(f"  rootDistInit=$rootDistInit")
            //                println(f"  nontDistInit=$nontDistInit")
            //                println(f"  emisDistInit=$emisDistInit")
            //                println(f"  trDist=$trDist")
            //                println(f"  emDist=$emDist")
            //                //println(f"\nInit DD-Joint")
            //                //println(argString); evaluator.evaluate(new GuideChartParserAdapter(parserGuideChartBuilder, initDdModel), testData, tagdict, verbose = false)
            //                println(f"\nMCMC DD-Joint")
            //                val emDdModel = ddTrainer.train(rawData, tagdict, rootDist, nontDist, termDist, trDist, emDist)
            //                val extractedPcfgModel = emDdModel.parser
            //                println("Extracted PCFG Parser"); println(argString); evaluator.evaluate(extractedPcfgModel, testData, tagdict, verbose = false)
            //                println("Full SCG Parser"); println(argString); evaluator.evaluate(new GuideChartParserAdapter(parserGuideChartBuilder, emDdModel), testData, tagdict, verbose = false)
          }

      }
    }
  }
}

object SimpleScgExpRunner {
  def apply(
    options: CommandLineOptions /*Map[String, String]*/ , argString: String) = {

    options.get("num")

    val additionalRules = options.get("additional-rules") /*.orElse(Some("FC,BX,FC2,BX2"))*/ .filterNot(Set(".", "x")).getOrElse("x")
    //println(f"additionalRules = $additionalRules")
    val language = options.s("lang", "en")

    val rules = CcgRules.nonComp ++ ((if (additionalRules == "x") Vector.empty else additionalRules.lsplit(",")).map { case "FC" => FC; case "BX" => BX; case "FC2" => FC2; case "BX2" => BX2 })
    //println(f"rules = $rules")

    val reader: TreeBankReader = {
      val standardReader = getReader(language)
      val dependencyConvertableTreeReader = new RuleViolatedRemovingTreeBankReader(new SimpleRuleViolationFinder(CcgRules.all, allowAllUnary = true), standardReader)
      new SeparateTrainTestTreeBankReader(
        tdReader = dependencyConvertableTreeReader,
        rawReader = dependencyConvertableTreeReader,
        testReader = dependencyConvertableTreeReader)
    }

    val runner = new SimpleScgExpRunner(
      rules,
      options.get("max-sent-len").fold(reader)(l => new MaxLengthRemovingTreeBankReader(l.toInt, reader)),
      tdTokToUse = options.get("td-tok").map(_.replaceAll("(\\d+)k", "$1000")).filterNot(Set("max", "all", "inf")).map(_.toInt).getOrElse(Int.MaxValue),
      numTrainSentences = options.get("train-sent").map(_.toLowerCase).filterNot(Set("max", "all", "inf")).map(_.toInt).getOrElse(Int.MaxValue),
      numTestSentences = options.get("test-sent").map(_.toLowerCase).filterNot(Set("max", "all", "inf")).map(_.toInt).getOrElse(Int.MaxValue),

      samplingIterations = options.i("sampling-iterations", 10000),
      burninIterations = options.i("burnin-iterations", 1000),
      //      maxEmIterations = 50,
      //      maxDecompIterations = 50,

      alphaRoot = options.d("alpha-root", 1.0),
      alphaBiny = options.d("alpha-biny", 100.0),
      alphaUnry = options.d("alpha-unry", 100.0),
      alphaTerm = options.d("alpha-term", 10000.0),
      alphaProd = options.d("alpha-prod", 100.0),
      alphaLctx = options.d("alpha-cntx", 1000.0),
      alphaRctx = options.d("alpha-cntx", 1000.0),
      alphaTran = options.d("alpha-tran", 10.0),
      alphaEmis = options.d("alpha-emis", 10.0),
      alphaLambda = 3.0,
      priorBinyProdMix = 1.0 / 3,
      priorUnryProdMix = 1.0 / 3,
      priorTermProdMix = 1.0 / 3,

      modelOpt = options.s("model"),
      learningOpt = options.s("learning"),

      rootinit = options.s("root-init"),
      ntprodInit = options.s("nt-prod-init"),
      termprodInit = options.s("term-prod-init"),
      trinit = options.s("tr-init", "x"),

      pTerm = options.d("pterm", 0.7),
      pMod = options.d("pmod", 0.1),
      pFwd = options.d("pfwd", 0.5),

      //catpriorMass = options.d("catprior-mass", 0.5),
      combinableTransitionMass = options.d("comb-tr-mass", 0.85),

      tdIncludesRequiredMappings = options.b("td-includes-required-mapping", false),
      tdCutoff = options.d("td-cutoff", 0.0),
      tdIncludesTest = options.b("td-includes-test", false),

      maxAcceptanceTries = options.i("max-accept-tries", 1),
      parseCountCutoff = options.l("parse-count-cutoff", -1L),
      //numSamples = options.i("num-samples", 10000),

      evaluator = new DepParserEvaluator(outputFile = options.get("output-file") /*.map { f => pathjoin(f, options("num") + ".txt") }*/ /*, goldSupertags = options.b("gold-test-supertags" , false*/ ),

      punctSplit = options.b("punct-split", false),
      trainAnnotator = options.s("train-annotator", "none"),
      trainAnnotatedSentencesProportion = options.d("train-anno-sent-prop", 0.0),
      trainBracketProportion = options.get("train-bracket-prop").map { case "Base" => 1.0; case n => n.toDouble }.getOrElse(0.0),
      trainBracketHighMatchBase = options.get("train-bracket-prop").contains("Base"),
      trainBracketCats = options.get("train-bracket-cats").filterNot(Set("all", "x")).map(_.split(",").map(c => NonRemovingCcgBankCatInterner(c.replace("o", "(").replace("c", ")").replace("f", "/").replace("b", "\\"))).toSet).getOrElse(UniversalSet()),
      trainBaseCats = options.b("train-base-cats", true),
      trainHighCats = options.b("train-high-cats", true),
      trainDepProportion = options.d("train-dep-prop", 0.0),
      trainGCB = options.s("train-gcb", "x"),
      testAnnotator = options.s("test-annotator", "none"),
      testAnnotatedSentencesProportion = options.d("test-anno-sent-prop", 0.0),
      testBracketProportion = options.get("test-bracket-prop").map { case "Base" => 1.0; case n => n.toDouble }.getOrElse(0.0),
      testBracketHighMatchBase = options.get("test-bracket-prop").contains("Base"),
      testBracketCats = options.get("test-bracket-cats").filterNot(Set("all", "x")).map(_.split(",").map(NonRemovingCcgBankCatInterner(_)).toSet).getOrElse(UniversalSet()),
      testBaseCats = options.b("test-base-cats", true),
      testHighCats = options.b("test-high-cats", true),
      testDepProportion = options.d("test-dep-prop", 0.0),
      testGCB = options.s("test-gcb", "x"),

      numClusters = options.get("num-clusters").filter(_ != "inf").map(_.toInt),

      useSerializedTrainGC = options.b("use-serialized-train-gc", true),
      useSerializedTestGC = options.b("use-serialized-test-gc", true),
      trainSerializedGC = options.get("train-serialized-gc"),
      testSerializedGC = options.get("test-serialized-gc"),

      sentencesToTake = options.i("takemax", Int.MaxValue),
      maxTrainTokens = options.i("max-train-tok", Int.MaxValue),

      argString = argString,

      justSerialize = options.b("just-serialize", false),

      mcmcOutputCountFile = options.get("mcmc-output-count-file"))

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

//class FromSemisupervisedTaggerTrainer(
//  delegate: SemisupervisedTaggerTrainer[Cat],
//  guideChartBuilder: OldToRemoveCfgGuideChartBuilder) {
//  import OldToRemoveCfgGuideChartBuilder.GuideTable
//  def train(
//    sentences: Vector[Vector[Word]], tagdict: TagDictionary[Cat],
//    transitions: ConditionalLogProbabilityDistribution[Cat, Cat], emissions: ConditionalLogProbabilityDistribution[Cat, Word]) = {
//    trainFromBracketed(sentences.mapToVal(Vector.empty), tagdict, transitions, emissions)
//  }
//
//  def trainFromBracketed(
//    bracketedSentences: Vector[(Vector[Word], Vector[(Int, Int)])], tagdict: TagDictionary[Cat],
//    transitions: ConditionalLogProbabilityDistribution[Cat, Cat], emissions: ConditionalLogProbabilityDistribution[Cat, Word]) = {
//    val rawSentencesWithTokenTags =
//      bracketedSentences.flatMap {
//        case (sentence, brackets) =>
//          guideChartBuilder.build(sentence, brackets, tagdict).map { guideChart =>
//            val n = guideChart.length
//            val sentenceWithTags = (0 until n).map { i =>
//              val cell = guideChart(i)(i + 1).get
//              val word = cell.values.head.head._2 match { case TermProd(word) => word }
//              val supertags = cell.keySet.toSet
//              (word, supertags)
//            }.toVector
//            sentenceWithTags
//          }
//      }
//    delegate.trainWithTagsets(rawSentencesWithTokenTags, tagdict, transitions, emissions)
//  }
//
//  override def toString = f"FromSemisupervisedTaggerTrainer(${guideChartBuilder}, $delegate)"
//}

case class Em2TermProdDist(emissionDist: ConditionalLogProbabilityDistribution[Cat, String]) extends ConditionalLogProbabilityDistribution[Cat, TermProd] {
  def apply(x: TermProd, given: Cat) = emissionDist(x.word, given)
  def sample(given: Cat) = TermProd(emissionDist.sample(given))
  override def toString = f"Em2TermProdDist($emissionDist)"
}

case class ClusterId(id: String)
