package dhg.ccg.parse.inf

import scala.annotation.tailrec
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import dhg.ccg.data._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.prob._
import dhg.ccg.tagdict._
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
import dhg.ccg.tag.learn._
import dhg.ccg.parse.scg.exp._
import dhg.ccg.parse.pcfg.typesup.BinaryCondPriorDist
import dhg.ccg.parse.pcfg.typesup.UnaryCondPriorDist

object McmcInfPcfgRunner {

  def main(args: Array[String]): Unit = {
    type Word = String
    type Tag = Cat

    val catmap = FeatRemovingCcgBankCatInterner

    val S = catmap("S")
    val NP = catmap("NP")
    val N = catmap("N")
    val PP = catmap("PP")

    val rootSet: Set[Cat] = Set(S, NP)
    val binaryRules = CcgRuleNoFeat.binaryRules.toSet
    val unaryRules = CcgRuleNoFeat.unaryRules.toSet
    val standardRsGCB = new SimpleCfgGuideChartBuilder(binaryRules.toVector ++ unaryRules, new StandardTagDictAdditionalTagAdder(), rootSet, allowTerminalDeletion = false)
    //val fwdbkdRsGCB = new SimpleCfgGuideChartBuilder(binaryRules.toVector ++ unaryRules, new StandardTagDictAndFwdBkdAdditionalTagAdder(), rootSet, allowTerminalDeletion = false)
    val fwdbkdRsGCB = new SimpleCfgGuideChartBuilder(binaryRules.toVector ++ unaryRules, new StandardTagDictAdditionalTagAdder(), rootSet, allowTerminalDeletion = true)
    val cascadeGCB = new CascadingAttemptsCfgGuideChartBuilder(Vector(standardRsGCB)) //, fwdbkdRsGCB))

    val standardReader = new MaxLengthRemovingTreeBankReader(10, EnglishCcgTreeBankReader(catmap))
    val reader = new RuleViolatedRemovingTreeBankReader(new SimpleRuleViolationFinder(CcgRules.all, allowAllUnary = false), standardReader)

    val tdData = reader.tdData() /*     */ .toVector; println(f"tdData:         #sent = ${tdData.size}%4d; #tok = ${tdData.sumBy(_.length)}%5d")
    val tagdict = new SimpleTagDictionaryFactory(tdCutoff = Some(0.1)).apply(
      tdData.map(_.tagged).toVector,
      "<S>", catmap("<S>"), "<E>", catmap("<E>"))
    val atomCats = tagdict.allTags.collect { case a: AtomCat => a }
    println(f"atomCats = {${atomCats.mkString(", ")}}")

    //TreeViz.drawTree(reader.rawCHEATING().drop(1).next)
    val rawDataGCs = reader.raw().zipWithIndex
      .flatMap { case (s, i) => time(s"gcb $i", cascadeGCB.buildFromSupertagSetSentence(s.mapToVal(Set.empty[Cat]), None, tagdict)) }
      .take(200)
      .toVector
    assert(rawDataGCs.nonEmpty, "rawDataGC is empty!")
    val numRawSentences = rawDataGCs.size
    println(f"trainSentences: #sent = ${numRawSentences}%4d; #tok = ${rawDataGCs.sumBy(_.words.length)}%5d")

    val infCatPrior = new MemoizingInfCatPrior(new CatgramInfCatPriorInitializer(
      new TagdictInformedAtomCatDistInitializer(atomLambda = 1000.0),
      allPunc = Set(cat".", cat",", cat";", cat":", cat"LRB", cat"RRB").map(_.asInstanceOf[PuncCat]),
      puncDist = new SimpleLogProbabilityDistribution(Map(
        cat"," -> 0.50,
        cat"." -> 0.45,
        cat":" -> 0.02,
        cat";" -> 0.01,
        cat"LRB" -> 0.01,
        cat"RRB" -> 0.01).mapt((c, p) => c.asInstanceOf[PuncCat] -> LogDouble(p)).normalizeValues),
      // pPunc = 0.1, pTerm = 0.8, pMod = 0.1, pFwd = 0.5 //
      pPunc = 0.1, pTerm = 0.5, pMod = 0.1, pFwd = 0.5 //
      ).fromKnownSupertagSets(rawDataGCs.map(s => s.words zipSafe s.supertagSets), tagdict))

    val pcfgProductionFinder = new SimplePcfgProductionCounter()
    val pcfgGuideChartProdFinder = new SimplePcfgGuideChartProdFinder()
    val pcfgAlphaPriorMaker = new TrainDataPcfgAlphaPriorMaker(pcfgProductionFinder, pcfgGuideChartProdFinder)
    val pcfgTreeSampler = new SimplePcfgTreeSampler(new SimplePcfgInsideChartBuilder())
    val pcfgParserInstantiater = new ExactPcfgParserInstantiater()

    val priorTermDist = {
      val emisDistInit = new EmTagDictionaryEstimateFromTagPrior[Cat](infCatPrior, lambda = 0.04, tdCountLambda = 0.26)
      def gc2supertagSets(gc: CfgGuideChart, td: TagDictionary[Cat]) = (gc.words zipSafe gc.supertagSets).mapt { (w, tags) => (w, tags & td.entries.getOrElse(w, Set.empty)) }
      val priorEmisDist = emisDistInit.fromKnownSupertagSets(rawDataGCs.map(gc2supertagSets(_, tagdict)), tagdict)
      Em2TermProdDist(priorEmisDist)
    }

    val alpha = 1e10

    val infMcmcPcfg = new McmcInfPcfg(
      samplingIterations = 50,
      burninIterations = 1,
      pcfgAlphaPriorMaker,
      pcfgProductionFinder,
      initialParserInstantiater = pcfgParserInstantiater,
      DirSampler,
      /*priorRootDist, priorBinyDist, priorUnryDist,*/ priorTermDist,
      rootSet,
      binaryRules, unaryRules,
      alphaRoot = alpha, alphaBiny = alpha, alphaUnry = alpha, alphaTerm = alpha,
      alphaLambda = 3.0, priorBinyProdMix = 1.0 / 3, priorUnryProdMix = 1.0 / 3, priorTermProdMix = 1.0 / 3,
      qBetaA = LogDouble(1e-7), qBetaB = LogDouble.one,
      initialGudeChartBuilder = cascadeGCB,
      pcfgGuideChartProdFinder,
      infCatPrior,
      new SynchronizedRandomGenerator(new MersenneTwister),
      additionalKnownCats = tagdict.allTags,
      fullAllowedCatSet = Some(tagdict.allTags),
      accumulate = false)

    val sampledTrees = infMcmcPcfg.trainGuideChartsWithSomeGold(rawDataGCs, rawDataGCs.map(_ => None), Vector.empty)

    {
      val wordTags = sampledTrees.flatMap(t => t.tagged).to[Set].groupByKey
      wordTags.foreach {
        case (word, tags) =>
          val newTags = tags -- tagdict(word)
          if (newTags.nonEmpty) {
            println(f"Newly acquired supertags: ${word}  =>  ${newTags.toVector.sorted.mkString("[", ", ", "]")};   original = ${tagdict(word).toVector.sorted.mkString("[", ", ", "]")}")
          }
      }

      val treesBySentence = sampledTrees.grouped(numRawSentences).toVector.transpose
      treesBySentence.zipWithIndex.foreach {
        case (treeGroup, i) =>
          assert(treeGroup.map(_.words).toSet.size == 1)
          val numBracketings = treeGroup.map(CcgTreeUtil.getSpans).toSet.size
          val numTrees = treeGroup.toSet.size
          if (numBracketings > 1) {
            println(f"sentence $i%3s:  num distinct bracketings = ${numBracketings}")
          }
          if (numTrees > 1) {
            println(f"               num distinct trees       = ${numTrees}")
          }
      }
    }

    val supPcfgTrainer = new AlphaBetaSupPcfgTrainer(
      priorRootDist = infCatPrior,
      priorBinyDist = new BinaryCondPriorDist(infCatPrior),
      priorUnryDist = new UnaryCondPriorDist(infCatPrior),
      priorTermDist,
      alphaRoot = alpha, alphaProd = alpha,
      priorBinyProdMix = 1.0 / 3, priorUnryProdMix = 1.0 / 3, priorTermProdMix = 1.0 / 3,
      pcfgProductionFinder,
      pcfgParserInstantiater)

    val infmcmcparser: PcfgParser = supPcfgTrainer.train(sampledTrees).asInstanceOf[PcfgParser]
    val msd = infmcmcparser.rootDist
    val mpd = infmcmcparser.prodDist

    val testData = reader.devData() /*  */ .toVector; println(f"testData:       #sent = ${testData.size}%4d; #tok = ${testData.sumBy(_.length)}%5d")

  }
}
