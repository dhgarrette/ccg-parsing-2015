package dhg.ccg.parse.pcfg.mcmc

import scala.annotation.tailrec
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import dhg.ccg.cat._
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
import dhg.ccg.math.Util._
import dhg.ccg.tagdict.StartEndTags
import dhg.ccg.tagdict.SimpleStartEndTags
import scala.collection.parallel.immutable.ParVector
import dhg.ccg.math.DirichletSampler
import dhg.ccg.parse.dep.ParserEvaluator

class McmcPcfg(
    samplingIterations: Int,
    burninIterations: Int,
    alphaPriorMaker: PcfgAlphaPriorMaker,
    productionFinder: PcfgProductionCounter,
    initialParserInstantiater: PcfgParserInstantiater,
    dirSampler: DirichletSampler,
    pcfgTreeSampler: PcfgTreeSampler,
    priorRootDist: LogProbabilityDistribution[Cat],
    priorBinyDist: ConditionalLogProbabilityDistribution[Cat, BinaryProd],
    priorUnryDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd],
    priorTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
    alphaRoot: Double, alphaBiny: Double, alphaUnry: Double, alphaTerm: Double,
    alphaLambda: Double, priorBinyProdMix: Double, priorUnryProdMix: Double, priorTermProdMix: Double,
    supPcfgTrainer: SupParserTrainer,
    itermediateEvaluatorAndEvalIters: Option[(GuideChartParser => Unit, Set[Int])] = None,
    accumulate: Boolean = false,
    verbose: Boolean = false) {

  def trainGuideChartsWithSomeGold(guideCharts: Vector[CfgGuideChart], goldTrees: Vector[CcgTree]) = {
    trainGuideChartsWithSomeGoldReturnTrees(guideCharts, goldTrees)._1
  }
  
  def trainGuideChartsWithSomeGoldReturnTrees(guideCharts: Vector[CfgGuideChart], goldTrees: Vector[CcgTree]) = {

    //println(f" >>>>> " + priorRctxDist(cat"((PP\PP)/N)", cat"(N\NP)"))

    //    val allKnownSupertags = initialTagdict.allTags | goldTrees.flatMap(_.supertags).toSet
    //    val tagdict =
    //      initialTagdict
    //        .withWords(guideCharts.flatMap(_.words).toSet | goldTrees.flatMap(_.words).toSet)
    //        .withTags(allKnownSupertags)

    for ((evaluator, evalIters) <- itermediateEvaluatorAndEvalIters if evalIters.contains(-1)) evaluator(supPcfgTrainer.train(Vector.empty))

    if (verbose) { println(f"McmcPcfg.trainGuideChartsWithSomeGold"); guideCharts.foreach(_.draw()) }

    val (alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts) =
      alphaPriorMaker.makeAll(guideCharts, goldTrees,
        priorRootDist, priorBinyDist, priorUnryDist, priorTermDist,
        alphaRoot, alphaBiny, alphaUnry, alphaTerm)

    //    // TODO: REMOVE
    //    println(f"root => ${alphaPriorRootCounts.values.sum}")
    //    println(f"nont => "); alphaPriorNontCounts.foreach{case (cat, dist) => println(f"    $cat : ${dist.values.sum}")}
    //    println(f"term => "); alphaPriorTermCounts.foreach{case (cat, dist) => println(f"    $cat : ${dist.values.sum}")}

    //    // TODO: REMOVE
    //    println(f" >>>>>>>>>" + allKnownSupertags(cat"N"))
    //    println(f" >>>>>>>>>" + allRctxSet.groupByKey.get(cat"N"))
    //    println(f" >>>>>>>>>" + allRctxSet((cat"N", cat"<E>")))
    //    println(f" >> ${rctxZ(cat"N").logValue}")
    //    println(f" >> ${LogDouble(alphaRctx).logValue}")
    //    println(f" >> ${LogDouble(alphaRctx) * priorRctxDist(cat"<E>", cat"N") / rctxZ(cat"N")}") // + LogDouble(goldRctxCounts.get(cat).flatMap(_.get(rctx)).getOrElse(0.0)))).toMap)}")
    //    println(f" >>>>> " + alphaPriorRctxCounts(cat"N")(cat"<E>") + " : " + alphaPriorRctxCounts(cat"N")(cat"<E>").logValue)
    //    println(f" >>>>> " + priorRctxDist(cat"<E>",cat"N") + " : " + priorRctxDist(cat"<E>",cat"N").logValue)

    val sampledTrees = doTrain(guideCharts, alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts)
    //    val allKnownTags = tagdict.allTags ++
    //      alphaPriorRootCounts.keys ++ alphaPriorBinyCounts.keys ++ alphaPriorUnryCounts.keys ++ alphaPriorTermCounts.keys
    //    val allWords = tagdict.allWords

    (supPcfgTrainer.train(sampledTrees), sampledTrees)
  }

  private[this] final def doTrain(
    guideCharts: Vector[CfgGuideChart],
    alphaPriorRootCounts: Map[Cat, LogDouble],
    alphaPriorBinyCounts: Map[Cat, Map[BinaryProd, LogDouble]],
    alphaPriorUnryCounts: Map[Cat, Map[UnaryProd, LogDouble]],
    alphaPriorTermCounts: Map[Cat, Map[TermProd, LogDouble]]) = {
    val allProdHeadCats = alphaPriorBinyCounts.keySet | alphaPriorUnryCounts.keySet | alphaPriorTermCounts.keySet

    val initRootDist = new SimpleLogProbabilityDistribution(alphaPriorRootCounts)
    val initProdDist = new SimpleConditionalLogProbabilityDistribution(allProdHeadCats.mapTo { cat =>
      val binyDist = new SimpleLogProbabilityDistribution(alphaPriorBinyCounts.getOrElse(cat, Map.empty))
      val unryDist = new SimpleLogProbabilityDistribution(alphaPriorUnryCounts.getOrElse(cat, Map.empty))
      val termDist = new SimpleLogProbabilityDistribution(alphaPriorTermCounts.getOrElse(cat, Map.empty))
      val prodDist = new IPDU(binyDist, unryDist, termDist, LogDouble(priorBinyProdMix), LogDouble(priorUnryProdMix), LogDouble(priorTermProdMix))
      prodDist
    }.toMap)

    val initPcfgParser = initialParserInstantiater(initRootDist, initProdDist)
    val initialTrees = guideCharts.map { gc =>
      val (t, p) = initPcfgParser.parseAndProbKBestFromGuideChart(gc, k = 1).only
      assert(p.nonZero, "Sentence parsed with zero probability")
      t
    }
    for ((evaluator, evalIters) <- itermediateEvaluatorAndEvalIters if evalIters.contains(0)) evaluator(supPcfgTrainer.train(initialTrees))

    val sampledTrees = iterate(guideCharts, allProdHeadCats,
      initRootDist, initProdDist,
      alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts,
      Vector.empty, -burninIterations)
    sampledTrees
  }

  @tailrec private[this] final def iterate(
    guideCharts: Vector[CfgGuideChart], allProdHeadCats: Set[Cat],
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    alphaPriorRootCounts: Map[Cat, LogDouble],
    alphaPriorBinyCounts: Map[Cat, Map[BinaryProd, LogDouble]],
    alphaPriorUnryCounts: Map[Cat, Map[UnaryProd, LogDouble]],
    alphaPriorTermCounts: Map[Cat, Map[TermProd, LogDouble]],
    runningTrees: Vector[CcgTree],
    iteration: Int): // 
    Vector[CcgTree] = {

    if (iteration < samplingIterations) {
      val startTime = System.currentTimeMillis()
      val resampledTrees = guideCharts.PARALLEL.zipWithIndex.map {
        case (guideChart, i) => pcfgTreeSampler.sample(guideChart, rootDist, prodDist)
      }

      println(f"iteration ${((if (iteration < 0) iteration else iteration + 1) + ":").padRight(4)} ${(System.currentTimeMillis() - startTime) / 1000.0}%.3f sec")
      for ((evaluator, evalIters) <- itermediateEvaluatorAndEvalIters if evalIters.contains(iteration + 1)) evaluator(supPcfgTrainer.train(runningTrees))

      val treesToEstimate = if (accumulate) (resampledTrees ++ runningTrees) else resampledTrees
      val estRootCounts = treesToEstimate.map(productionFinder.rootCounts).reduce(_ |+| _)
      val estBinyCounts = treesToEstimate.map(productionFinder.binyCounts).reduce(_ |+| _)
      val estUnryCounts = treesToEstimate.map(productionFinder.unryCounts).reduce(_ |+| _)
      val estTermCounts = treesToEstimate.map(productionFinder.termCounts).reduce(_ |+| _)

      val resampledTreesSeq = resampledTrees.seq
      iterate(guideCharts, allProdHeadCats,
        new SimpleLogProbabilityDistribution(dirSampler.logDir(estRootCounts.mapVals(LogDouble(_)) |+| alphaPriorRootCounts)),
        new SimpleConditionalLogProbabilityDistribution(allProdHeadCats.mapTo { cat =>
          val bc = estBinyCounts.getOrElse(cat, Map.empty).mapVals(LogDouble(_))
          val uc = estUnryCounts.getOrElse(cat, Map.empty).mapVals(LogDouble(_))
          val tc = estTermCounts.getOrElse(cat, Map.empty).mapVals(LogDouble(_))
          val bcCountsWithPrior = bc |+| alphaPriorBinyCounts.getOrElse(cat, Map.empty) // these could be empty, since `cat` might not produce binary, unary, AND term productions for the training data
          val ucCountsWithPrior = uc |+| alphaPriorUnryCounts.getOrElse(cat, Map.empty)
          val tcCountsWithPrior = tc |+| alphaPriorTermCounts.getOrElse(cat, Map.empty)
          val binyDist = new SimpleLogProbabilityDistribution(dirSampler.logDir(bcCountsWithPrior))
          val unryDist = new SimpleLogProbabilityDistribution(dirSampler.logDir(ucCountsWithPrior))
          val termDist = new SimpleLogProbabilityDistribution(dirSampler.logDir(tcCountsWithPrior))
          val Vector(binyProdMix, unryProdMix, termProdMix) = dirSampler.logDir(Vector(
            LogDouble(alphaLambda * priorBinyProdMix) + bc.values.sum,
            LogDouble(alphaLambda * priorUnryProdMix) + uc.values.sum,
            LogDouble(alphaLambda * priorTermProdMix) + tc.values.sum))
          val prodDist = new IPDU(binyDist, unryDist, termDist, binyProdMix, unryProdMix, termProdMix)
          prodDist
        }.toMap),
        alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts,
        if (iteration >= 0) runningTrees ++ resampledTreesSeq else Vector.empty, // add new trees during sampling iterations only
        iteration + 1)
    }
    else {
      println(f"MAX ITERATIONS REACHED")
      runningTrees
    }
  }

  override def toString = f"McmcPcfg(samplingIterations=${samplingIterations}, burninIterations=${burninIterations}, alphaRoot=${alphaRoot}, alphaBiny=${alphaBiny}, alphaUnry=${alphaUnry}, alphaTerm=${alphaTerm}"
}
