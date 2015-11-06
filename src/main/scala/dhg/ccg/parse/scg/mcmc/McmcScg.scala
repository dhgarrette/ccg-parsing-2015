package dhg.ccg.parse.scg.mcmc

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
import dhg.ccg.parse.scg._
import org.apache.commons.math3.random.RandomGenerator
import dhg.ccg.math.Util._
import dhg.ccg.tagdict.StartEndTags
import dhg.ccg.tagdict.SimpleStartEndTags
import scala.collection.parallel.immutable.ParVector
import dhg.ccg.math.DirichletSampler
import dhg.ccg.parse.dep.ParserEvaluator

//trait ScgTrainer {
//  final def train(
//    rawSentences: Vector[Vector[Word]],
//    initialTagdict: TagDictionary[Cat],
//    priorRootDist: LogProbabilityDistribution[Cat],
//    priorNontDist: ConditionalLogProbabilityDistribution[Cat, NontermProd],
//    priorTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
//    priorLctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
//    priorRctxDist: ConditionalLogProbabilityDistribution[Cat, Cat]): Parser = {
//    trainWithSomeGold(rawSentences, Vector.empty, initialTagdict,
//      priorRootDist, priorNontDist, priorTermDist, priorLctxDist, priorRctxDist)
//  }
//
//  final def trainWithSomeGold(
//    rawSentences: Vector[Vector[Word]], goldTrees: Vector[CcgTree],
//    initialTagdict: TagDictionary[Cat],
//    priorRootDist: LogProbabilityDistribution[Cat],
//    priorNontDist: ConditionalLogProbabilityDistribution[Cat, NontermProd],
//    priorTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
//    priorLctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
//    priorRctxDist: ConditionalLogProbabilityDistribution[Cat, Cat]): Parser = {
//    trainGuideChartsWithSomeGold(rawSentences.mapToVal(Vector.empty), goldTrees, initialTagdict,
//      priorRootDist, priorNontDist, priorTermDist, priorLctxDist, priorRctxDist)
//  }
//
//  final def trainBracketed(
//    bracketedSentences: Vector[(Vector[Word], Vector[(Int, Int)])],
//    initialTagdict: TagDictionary[Cat],
//    priorRootDist: LogProbabilityDistribution[Cat],
//    priorNontDist: ConditionalLogProbabilityDistribution[Cat, NontermProd],
//    priorTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
//    priorLctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
//    priorRctxDist: ConditionalLogProbabilityDistribution[Cat, Cat]): Parser = {
//    trainGuideChartsWithSomeGold(bracketedSentences, Vector.empty, initialTagdict,
//      priorRootDist, priorNontDist, priorTermDist, priorLctxDist, priorRctxDist)
//  }
//
//  def trainGuideChartsWithSomeGold(
//    bracketedSentences: Vector[(Vector[Word], Vector[(Int, Int)])], goldTrees: Vector[CcgTree],
//    initialTagdict: TagDictionary[Cat],
//    priorRootDist: LogProbabilityDistribution[Cat],
//    priorNontDist: ConditionalLogProbabilityDistribution[Cat, NontermProd],
//    priorTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
//    priorLctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
//    priorRctxDist: ConditionalLogProbabilityDistribution[Cat, Cat]): Parser = {
//
//    // val guideCharts = bracketedSentences.zipWithIndex.flatMap {
//    //   case ((sentence, brackets), i) =>
//    //     trainingGuideChartBuilder.build(sentence, brackets, tagdict)
//    //   //        .getOrElse {
//    //   //          println(f"failed to generate guide chart for training sentence $i:")
//    //   //          println(f"    ${sentence.mkString(" ")}")
//    //   //          for((w,j) <- sentence.zipWithIndex){
//    //   //            println(f"        ${i}%-2d: ${w}%-20s present=${tagdict.entries.contains(w)}%5s ${tagdict(w)}")
//    //   //          }
//    //   //          sys.error("    :(")
//    //   //        }
//    // }
//    //
//    // println(f"Parsable training sentences: ${guideCharts.size * 100.0 / bracketedSentences.size}%.2f  (${guideCharts.size}/${bracketedSentences.size})")
//    //
//    // trainGuideChartsWithSomeGold(guideCharts, goldTrees, initialTagdict,
//    //   priorRootDist, priorNontDist, priorTermDist, priorLctxDist, priorRctxDist)
//    ???
//  }
//
//  def trainGuideChartsWithSomeGold(
//    guideCharts: Vector[CfgGuideChart], goldTrees: Vector[CcgTree],
//    initialTagdict: TagDictionary[Cat],
//    priorRootDist: LogProbabilityDistribution[Cat],
//    priorNontDist: ConditionalLogProbabilityDistribution[Cat, NontermProd],
//    priorTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
//    priorLctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
//    priorRctxDist: ConditionalLogProbabilityDistribution[Cat, Cat]): Parser
//}

class McmcScg(
    samplingIterations: Int,
    burninIterations: Int,
    alphaPriorMaker: ScgAlphaPriorMaker,
    productionFinder: ScgProductionFinder,
    initialParserInstantiater: PcfgParserInstantiater,
    dirSampler: DirichletSampler,
    resampler: McmcScgResampler,
    priorRootDist: LogProbabilityDistribution[Cat],
    priorBinyDist: ConditionalLogProbabilityDistribution[Cat, BinaryProd],
    priorUnryDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd],
    priorTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
    priorLctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    priorRctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    alphaRoot: Double, alphaBiny: Double, alphaUnry: Double, alphaTerm: Double, alphaLctx: Double, alphaRctx: Double,
    alphaLambda: Double, priorBinyProdMix: Double, priorUnryProdMix: Double, priorTermProdMix: Double,
    supScgTrainer: SupParserTrainer,
    itermediateEvaluatorAndEvalIters: Option[(GuideChartParser => Unit, Set[Int])] = None,
    outputCountFile: Option[String] = None,
    accumulate: Boolean = false,
    verbose: Boolean = false)(se: StartEndTags[Cat]) {

  def trainGuideChartsWithSomeGold(guideCharts: Vector[CfgGuideChart], goldTrees: Vector[CcgTree]) = {

    outputCountFile.foreach { fn => assert(File(fn).getAbsoluteFile.parent.exists(_.exists), f"output file path does not exist.  outputCountFile=$fn") }

    //println(f" >>>>> " + priorRctxDist(cat"((PP\PP)/N)", cat"(N\NP)"))

    //    val allKnownSupertags = initialTagdict.allTags | goldTrees.flatMap(_.supertags).toSet
    //    val tagdict =
    //      initialTagdict
    //        .withWords(guideCharts.flatMap(_.words).toSet | goldTrees.flatMap(_.words).toSet)
    //        .withTags(allKnownSupertags)

    for ((evaluator, evalIters) <- itermediateEvaluatorAndEvalIters if evalIters.contains(-1)) evaluator(supScgTrainer.train(Vector.empty))

    if (verbose) { println(f"McmcScg.trainGuideChartsWithSomeGold"); guideCharts.foreach(_.draw()) }

    val (alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts, alphaPriorLctxCounts, alphaPriorRctxCounts) =
      alphaPriorMaker.makeAll(guideCharts, goldTrees,
        priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, priorLctxDist, priorRctxDist,
        alphaRoot, alphaBiny, alphaUnry, alphaTerm, alphaLctx, alphaRctx)(se)

    //    // TODO: REMOVE
    //    println(f"root => ${alphaPriorRootCounts.values.sum}")
    //    println(f"nont => "); alphaPriorNontCounts.foreach{case (cat, dist) => println(f"    $cat : ${dist.values.sum}")}
    //    println(f"term => "); alphaPriorTermCounts.foreach{case (cat, dist) => println(f"    $cat : ${dist.values.sum}")}
    //    println(f"lctx => "); alphaPriorLctxCounts.foreach{case (cat, dist) => println(f"    $cat : ${dist.values.sum}")}
    //    println(f"rctx => "); alphaPriorRctxCounts.foreach{case (cat, dist) => println(f"    $cat : ${dist.values.sum}")}

    //    // TODO: REMOVE
    //    println(f" >>>>>>>>>" + allKnownSupertags(cat"N"))
    //    println(f" >>>>>>>>>" + allRctxSet.groupByKey.get(cat"N"))
    //    println(f" >>>>>>>>>" + allRctxSet((cat"N", cat"<E>")))
    //    println(f" >> ${rctxZ(cat"N").logValue}")
    //    println(f" >> ${LogDouble(alphaRctx).logValue}")
    //    println(f" >> ${LogDouble(alphaRctx) * priorRctxDist(cat"<E>", cat"N") / rctxZ(cat"N")}") // + LogDouble(goldRctxCounts.get(cat).flatMap(_.get(rctx)).getOrElse(0.0)))).toMap)}")
    //    println(f" >>>>> " + alphaPriorRctxCounts(cat"N")(cat"<E>") + " : " + alphaPriorRctxCounts(cat"N")(cat"<E>").logValue)
    //    println(f" >>>>> " + priorRctxDist(cat"<E>",cat"N") + " : " + priorRctxDist(cat"<E>",cat"N").logValue)

    val sampledTrees = doTrain(guideCharts, alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts, alphaPriorLctxCounts, alphaPriorRctxCounts)(se)
    //    val allKnownTags = tagdict.allTags ++
    //      alphaPriorRootCounts.keys ++ alphaPriorBinyCounts.keys ++ alphaPriorUnryCounts.keys ++ alphaPriorTermCounts.keys ++ alphaPriorLctxCounts.keys ++ alphaPriorLctxCounts.flatMap(_._2.keys) ++ alphaPriorRctxCounts.keys ++ alphaPriorRctxCounts.flatMap(_._2.keys)
    //    val allWords = tagdict.allWords

    outputCountFile.foreach { fn =>
      val estRootCounts = sampledTrees.map(productionFinder.rootCounts).reduce(_ |+| _)
      val estBinyCounts = sampledTrees.map(productionFinder.binyCounts).reduce(_ |+| _)
      val estUnryCounts = sampledTrees.map(productionFinder.unryCounts).reduce(_ |+| _)
      val estTermCounts = sampledTrees.map(productionFinder.termCounts).reduce(_ |+| _)
      val estLctxCounts = sampledTrees.map(productionFinder.lctxCounts(_)(se)).reduce(_ |+| _)
      val estRctxCounts = sampledTrees.map(productionFinder.rctxCounts(_)(se)).reduce(_ |+| _)

      writeUsing(File(fn)) { f =>
        f.wl("Roots")
        for ((cat, c) <- estRootCounts) {
          f.wl(f"$cat\t$c%.0f")
        }
        f.wl("\nBinary Prods")
        for ((cat, cs) <- estBinyCounts) {
          f.wl(cat)
          for ((BinaryProd(x, y), c) <- cs) {
            f.wl(f"\t$x\t$y\t$c%.0f")
          }
        }
        f.wl("\nUnary Prods")
        for ((cat, cs) <- estUnryCounts) {
          f.wl(cat)
          for ((UnaryProd(x), c) <- cs) {
            f.wl(f"\t$x\t$c%.0f")
          }
        }
        f.wl("\nTerm Prods")
        for ((cat, cs) <- estTermCounts) {
          f.wl(cat)
          for ((TermProd(w), c) <- cs) {
            f.wl(f"\t$w\t$c%.0f")
          }
        }
        f.wl("\nLeft Context")
        for ((cat, cs) <- estLctxCounts) {
          f.wl(cat)
          for ((ctx, c) <- cs) {
            f.wl(f"\t$ctx\t$c%.0f")
          }
        }
        f.wl("\nRight Context")
        for ((cat, cs) <- estRctxCounts) {
          f.wl(cat)
          for ((ctx, c) <- cs) {
            f.wl(f"\t$ctx\t$c%.0f")
          }
        }
      }
    }

    supScgTrainer.train(sampledTrees)
  }

  protected[this] final def doTrain(
    guideCharts: Vector[CfgGuideChart],
    alphaPriorRootCounts: Map[Cat, LogDouble],
    alphaPriorBinyCounts: Map[Cat, Map[BinaryProd, LogDouble]],
    alphaPriorUnryCounts: Map[Cat, Map[UnaryProd, LogDouble]],
    alphaPriorTermCounts: Map[Cat, Map[TermProd, LogDouble]],
    alphaPriorLctxCounts: Map[Cat, Map[Cat, LogDouble]],
    alphaPriorRctxCounts: Map[Cat, Map[Cat, LogDouble]])(se: StartEndTags[Cat]) = {
    val allProdHeadCats = alphaPriorBinyCounts.keySet | alphaPriorUnryCounts.keySet | alphaPriorTermCounts.keySet

    val initRootDist = new SimpleLogProbabilityDistribution(alphaPriorRootCounts)
    val initProdDist = new SimpleConditionalLogProbabilityDistribution(allProdHeadCats.mapTo { cat =>
      val binyDist = new SimpleLogProbabilityDistribution(alphaPriorBinyCounts.getOrElse(cat, Map.empty))
      val unryDist = new SimpleLogProbabilityDistribution(alphaPriorUnryCounts.getOrElse(cat, Map.empty))
      val termDist = new SimpleLogProbabilityDistribution(alphaPriorTermCounts.getOrElse(cat, Map.empty))
      val prodDist = new IPDU(binyDist, unryDist, termDist, LogDouble(priorBinyProdMix), LogDouble(priorUnryProdMix), LogDouble(priorTermProdMix))
      prodDist
    }.toMap)
    val initLctxDist = new SimpleConditionalLogProbabilityDistribution(alphaPriorLctxCounts.mapVals(new SimpleLogProbabilityDistribution(_)))
    val initRctxDist = new SimpleConditionalLogProbabilityDistribution(alphaPriorRctxCounts.mapVals(new SimpleLogProbabilityDistribution(_)))

    val initPcfgParser = initialParserInstantiater(initRootDist, initProdDist)
    val initialTrees = guideCharts.map { gc =>
      val (t, p) = initPcfgParser.parseAndProbKBestFromGuideChart(gc, k = 1).only
      assert(p.nonZero, "Sentence parsed with zero probability")
      t
    }
    for ((evaluator, evalIters) <- itermediateEvaluatorAndEvalIters if evalIters.contains(0)) evaluator(supScgTrainer.train(initialTrees))

    val sampledTrees = iterate(guideCharts, allProdHeadCats, initialTrees,
      initRootDist, initProdDist, initLctxDist, initRctxDist,
      alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts, alphaPriorLctxCounts, alphaPriorRctxCounts,
      Vector.empty, -burninIterations)(se)
    sampledTrees
  }

  @tailrec private[this] final def iterate(
    guideCharts: Vector[CfgGuideChart], allProdHeadCats: Set[Cat],
    currentTrees: Vector[CcgTree],
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    alphaPriorRootCounts: Map[Cat, LogDouble],
    alphaPriorBinyCounts: Map[Cat, Map[BinaryProd, LogDouble]],
    alphaPriorUnryCounts: Map[Cat, Map[UnaryProd, LogDouble]],
    alphaPriorTermCounts: Map[Cat, Map[TermProd, LogDouble]],
    alphaPriorLctxCounts: Map[Cat, Map[Cat, LogDouble]],
    alphaPriorRctxCounts: Map[Cat, Map[Cat, LogDouble]],
    runningTrees: Vector[CcgTree],
    iteration: Int)(se: StartEndTags[Cat]): // 
    Vector[CcgTree] = {

    if (iteration < samplingIterations) {
      val startTime = System.currentTimeMillis()
      val (resampledTrees, acceptProportion, sub1AcceptProportion, avgRatio, proportionRatioGt1, sub1Ratios, agreeProportion) = resampler.resample(guideCharts, currentTrees, rootDist, prodDist, lctxDist, rctxDist)(se)

      println(f"iteration ${((if (iteration < 0) iteration else iteration + 1) + ":").padRight(4)} ${(System.currentTimeMillis() - startTime) / 1000.0}%.3f sec,  acceptProportion=$acceptProportion%.2f,  sub1AcceptProportion=${sub1AcceptProportion.fold("None")(p => f"$p%.2f")}, avgRatio=${if (avgRatio < 100000000.0) f"$avgRatio%.2f" else "big"},  proportionRatioGt1=${proportionRatioGt1}%.2f,  sub1AvgRatio=${if (sub1Ratios.nonEmpty) f"${sub1Ratios.avg}%.2f" else "None"}, agreeProportion=${agreeProportion}%.2f") // ":${sub1Ratios.map(d => (d * 10).toString.head).mkString}")
      for ((evaluator, evalIters) <- itermediateEvaluatorAndEvalIters if evalIters.contains(iteration + 1)) evaluator(supScgTrainer.train(runningTrees))

      val treesToEstimate = if (accumulate) (resampledTrees ++ runningTrees) else resampledTrees
      val estRootCounts = treesToEstimate.map(productionFinder.rootCounts).reduce(_ |+| _)
      val estBinyCounts = treesToEstimate.map(productionFinder.binyCounts).reduce(_ |+| _)
      val estUnryCounts = treesToEstimate.map(productionFinder.unryCounts).reduce(_ |+| _)
      val estTermCounts = treesToEstimate.map(productionFinder.termCounts).reduce(_ |+| _)
      val estLctxCounts = treesToEstimate.map(productionFinder.lctxCounts(_)(se)).reduce(_ |+| _)
      val estRctxCounts = treesToEstimate.map(productionFinder.rctxCounts(_)(se)).reduce(_ |+| _)

      val resampledTreesSeq = resampledTrees.seq
      iterate(guideCharts, allProdHeadCats,
        resampledTreesSeq,
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
        new SimpleConditionalLogProbabilityDistribution((estLctxCounts.mapVals(_.mapVals(LogDouble(_))) |+| alphaPriorLctxCounts).mapVals(counts => new SimpleLogProbabilityDistribution(dirSampler.logDir(counts)))),
        new SimpleConditionalLogProbabilityDistribution((estRctxCounts.mapVals(_.mapVals(LogDouble(_))) |+| alphaPriorRctxCounts).mapVals(counts => new SimpleLogProbabilityDistribution(dirSampler.logDir(counts)))),
        alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts, alphaPriorLctxCounts, alphaPriorRctxCounts,
        if (iteration >= 0) runningTrees ++ resampledTreesSeq else Vector.empty, // add new trees during sampling iterations only
        iteration + 1)(se)
    }
    else {
      println(f"MAX ITERATIONS REACHED")
      runningTrees
    }
  }

  override def toString = f"McmcScg(samplingIterations=${samplingIterations}, burninIterations=${burninIterations}, alphaRoot=${alphaRoot}, alphaBiny=${alphaBiny}, alphaUnry=${alphaUnry}, alphaTerm=${alphaTerm}, alphaLctx=${alphaLctx}, alphaRctx=${alphaRctx}"
}

trait McmcScgResampler {

  /**
   *
   * @return (resampledTrees, acceptProportion, sub1AcceptProportion, avgRatio, proportionRatioGt1, sub1Ratios)
   */
  def resample(
    guideCharts: Vector[CfgGuideChart],
    currentTrees: Vector[CcgTree],
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]): (ParVector[CcgTree], Double, Option[Double], Double, Double, ParVector[Double], Double)

}

class SimpleMcmcScgResampler(
  pcfgTreeSampler: PcfgTreeSampler,
  acceptanceSampler: ScgAcceptanceSampler,
  maxAcceptanceTries: Int,
  rand: RandomGenerator)
    extends McmcScgResampler {
  require(maxAcceptanceTries > 0, "maxAcceptanceTries must be a positive value")

  def resample(
    guideCharts: Vector[CfgGuideChart],
    currentTrees: Vector[CcgTree],
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]) = {

    val zipped =
      (currentTrees zipSafe guideCharts).PARALLEL.zipWithIndex.map {
        case ((currentTree, guideChart), i) => sampleTree(guideChart, currentTree, rootDist, prodDist, lctxDist, rctxDist)(se)
      }
    val sampledTrees = zipped.map(_._1)
    val accepted = zipped.map(_._2)
    val ratios = zipped.map(_._3)
    val sub1Ratios = zipped.map(_._4)
    val agree = zipped.map(_._5)

    //    if (verbose) {
    //      for ((cat, count) <- estRootCounts.desc) { println(f"$cat%-20s -> $count%.10f") }; println
    //      for { (cat, prods) <- estProdCounts; (NontermProd(b, c), count) <- prods.desc } { println(f"$cat%-20s -> $b%-20s$c%-20s  $count%.10f") }; println
    //      for { (cat, prods) <- estProdCounts; (TermProd(word), count) <- prods.desc } { println(f"$cat%-20s -> $word%-40s  $count%.10f") }; println
    //      for { (cat, prods) <- estLctxCounts; (prod, count) <- prods.desc } { println(f"$cat%-20s -> $prod%-40s  $count%.10f") }; println
    //      for { (cat, prods) <- estRctxCounts; (prod, count) <- prods.desc } { println(f"$cat%-20s -> $prod%-40s  $count%.10f") }; println
    //    }

    (
      sampledTrees,
      accepted.map(b => if (b) 1.0 else 0.0).avg,
      sub1Ratios.exists(_.isDefined).option((accepted zipSafe sub1Ratios.map(_.isDefined)).mapt((a, sub1) => sub1.option(if (a) 1.0 else 0.0)).flatten.avg),
      ratios.avg,
      sub1Ratios.map(b => if (b.isEmpty) 1.0 else 0.0).avg,
      sub1Ratios.flatten,
      agree.map(b => if (b) 1.0 else 0.0).avg)
  }

  private[this] def sampleTree(
    guideChart: CfgGuideChart,
    curTree: CcgTree,
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]) = {
    val n = guideChart.length
    val newTrees = pcfgTreeSampler.samples(guideChart, rootDist, prodDist, maxAcceptanceTries)
    val (newTree, accept, ratio, agree) = acceptanceRecurse(curTree, newTrees, guideChart, rootDist, prodDist, lctxDist, rctxDist)(se)
    (newTree, accept, ratio, (ratio < 1).option(ratio), agree)
  }

  @tailrec
  private[this] def acceptanceRecurse(
    curTree: CcgTree,
    newTrees: Vector[CcgTree],
    guideChart: CfgGuideChart,
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]): (CcgTree, Boolean, Double, Boolean) = newTrees match {
    case newTree +: tail =>
      val (accept, ratio, agree) = acceptanceSampler.accept(newTree, curTree, rootDist, prodDist, lctxDist, rctxDist, rand)(se)
      val nextTree = if (accept) newTree else curTree
      if (tail.isEmpty) {
        (nextTree, accept, ratio, agree)
      }
      else {
        acceptanceRecurse(nextTree, tail, guideChart, rootDist, prodDist, lctxDist, rctxDist)(se)
      }
  }

  override def toString = f"SimpleMcmcScgResampler(pcfgTreeSampler=$pcfgTreeSampler, acceptanceSampler=$acceptanceSampler, maxAcceptanceTries=$maxAcceptanceTries)"
}

class MetHastSampler() {

}
