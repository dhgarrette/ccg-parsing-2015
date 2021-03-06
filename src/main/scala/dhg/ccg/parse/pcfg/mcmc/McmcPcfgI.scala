package dhg.ccg.parse.pcfg.mcmc

import scala.annotation.tailrec
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import dhg.ccg.cat._
import dhg.ccg.prob._
import dhg.ccg.util._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.tagdict.TagDictionary
import dhg.util._
import dhg.util.FastMathUtil._
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
import scala.collection.immutable.BitSet
import scala.math._

trait McmcSamplerI {
  def trainGuideChartsWithSomeGold(guideCharts: Array[CfgGuideChartI], goldTrees: Array[CcgTreeI]): Array[CcgTreeI]
}

class McmcPcfgI(
  samplingIterations: Int,
  burninIterations: Int,
  productionCounter: PcfgProductionCounterI,
  alphaPriorMaker: PcfgAlphaPriorMakerI,
  initialParserInstantiater: PcfgParserInstantiaterI,
  pcfgTreeSampler: PcfgTreeSamplerI,
  priorRootDist: IndirectSparseVec[LogDouble], //                                 t -> p
  priorBinyDist: Array[IndirectSparseVec[IndirectSparseVec[LogDouble]]], //       t -> u -> v -> p
  priorUnryDist: IndirectSparseVec[IndirectSparseVec[LogDouble]], //              t -> u -> p
  priorTermDist: Array[Vec[LogDouble]], //                                        t -> w -> p
  priorPmixDist: Array[Array[LogDouble]], //                                      t -> p
  alphaRoot: Double, alphaBiny: Double, alphaUnry: Double, alphaTerm: Double, alphaPmix: Double,
  knownRoots: Array[Int], //                               ts
  knownBinys: Array[IndirectSparseVec[Array[Int]]], //     t -> u -> vs
  knownUnrys: IndirectSparseVec[Array[Int]], //            t -> us
  knownTerms: Array[Array[Int]], //                        t -> ws
  supPcfgTrainer: SupParserTrainerI,
  rand: RandomGenerator,
  itermediateEvaluatorAndEvalIters: Option[(PcfgParserI => Unit, Set[Int])] = None,
  numCats: Int, numWords: Int,
  accumulate: Boolean = false,
  verbose: Boolean = false) //
  (catIndexer: Indexer[Cat], wordIndexer: Indexer[String]) //
    extends McmcSamplerI {

  def trainGuideChartsWithSomeGold(guideCharts: Array[CfgGuideChartI], goldTrees: Array[CcgTreeI]) = {

    //println(f" >>>>> " + priorRctxDist(cat"((PP\PP)/N)", cat"(N\NP)"))

    //    val allKnownSupertags = initialTagdict.allTags | goldTrees.flatMap(_.supertags).toSet
    //    val tagdict =
    //      initialTagdict
    //        .withWords(guideCharts.flatMap(_.words).toSet | goldTrees.flatMap(_.words).toSet)
    //        .withTags(allKnownSupertags)

    if (verbose) { println(f"McmcPcfgI.trainGuideChartsWithSomeGold") } //; guideCharts.foreach(_.draw()) }

    val (goldRootCounts, goldBinyCounts, goldUnryCounts, goldTermCounts, goldPmixCounts) =
      productionCounter.counts(goldTrees, goldTrees.length, knownRoots, knownBinys, knownUnrys, knownTerms, numCats, numWords)

    val (alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts, alphaPriorPmixCounts) =
      alphaPriorMaker.makeAll(priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, priorPmixDist, goldRootCounts, goldBinyCounts, goldUnryCounts, goldTermCounts, goldPmixCounts, alphaRoot, alphaBiny, alphaUnry, alphaTerm, alphaPmix, numCats, numWords)

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

    val sampledTrees = doTrain(guideCharts,
      alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts, alphaPriorPmixCounts,
      knownRoots, knownBinys, knownUnrys, knownTerms,
      goldRootCounts, goldBinyCounts, goldUnryCounts, goldTermCounts, goldPmixCounts,
      numCats, numWords)
    //    val allKnownTags = tagdict.allTags ++
    //      alphaPriorRootCounts.keys ++ alphaPriorBinyCounts.keys ++ alphaPriorUnryCounts.keys ++ alphaPriorTermCounts.keys
    //    val allWords = tagdict.allWords

    //supPcfgTrainer.train(
    sampledTrees // ++ goldTrees
    //)
  }

  private[this] final def doTrain(
    guideCharts: Array[CfgGuideChartI],
    alphaPriorRootCounts: IndirectSparseVec[Double], //                            t -> c
    alphaPriorBinyCounts: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //  t -> u -> v -> c
    alphaPriorUnryCounts: IndirectSparseVec[IndirectSparseVec[Double]], //         t -> u -> c
    alphaPriorTermCounts: Array[Vec[Double]], //                                   t -> w -> c
    alphaPriorPmixCounts: Array[Array[Double]], //                                 t -> c
    knownRoots: Array[Int], //                               ts
    knownBinys: Array[IndirectSparseVec[Array[Int]]], //     t -> u -> vs
    knownUnrys: IndirectSparseVec[Array[Int]], //            t -> us
    knownTerms: Array[Array[Int]], //                        t -> ws
    goldRootCounts: IndirectSparseVec[Double], //                                   t -> c
    goldBinyCounts: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //         t -> u -> v -> c
    goldUnryCounts: IndirectSparseVec[IndirectSparseVec[Double]], //                t -> u -> c
    goldTermCounts: Array[Vec[Double]], //                                          t -> w -> c
    goldPmixCounts: Array[Array[Double]], //                                        t -> c
    numCats: Int, numWords: Int) //
    = {

    val (logInitRootDist, logInitBinyDist, logInitUnryDist, logInitTermDist, logInitPmixDist) =
      countsToDists(alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts, alphaPriorPmixCounts)

    val initPcfgParser = initialParserInstantiater.apply(logInitRootDist, logInitBinyDist, logInitUnryDist, logInitTermDist, logInitPmixDist)(catIndexer, wordIndexer)
    val initialTrees = guideCharts.map { gc =>
      val (t, p) = initPcfgParser.parseAndLogProbFromGuideChart(gc)
      assert(p != Double.NegativeInfinity, "Sentence parsed with zero probability")
      t
    }
    for ((evaluator, evalIters) <- itermediateEvaluatorAndEvalIters if evalIters.contains(0)) evaluator(supPcfgTrainer.train(initialTrees))

    //sys.error("mcmcpcfgi pre-iterate stop")
    val sampledTrees = iterate(guideCharts, guideCharts.length,
      logInitRootDist, logInitBinyDist, logInitUnryDist, logInitTermDist, logInitPmixDist,
      alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts, alphaPriorPmixCounts,
      knownRoots, knownBinys, knownUnrys, knownTerms,
      numCats, numWords,
      Array.empty, -burninIterations)
    sampledTrees
  }

  private[this] def countsToDists(
    alphaPriorRootCounts: IndirectSparseVec[Double], //                            t -> c
    alphaPriorBinyCounts: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //  t -> u -> v -> c
    alphaPriorUnryCounts: IndirectSparseVec[IndirectSparseVec[Double]], //         t -> u -> c
    alphaPriorTermCounts: Array[Vec[Double]], //                                   t -> w -> c
    alphaPriorPmixCounts: Array[Array[Double]]) //                                 t -> c
    = {

    val logRootDist: IndirectSparseVec[Double] = {
      val activeCount = alphaPriorRootCounts.activeCount
      val activeValues = alphaPriorRootCounts.activeValues
      val logSum = log(FastMathUtil.sum(activeValues, activeCount))
      val newActiveValues = new Array[Double](activeCount)
      var i = 0
      while (i < activeCount) {
        newActiveValues(i) = log(activeValues(i)) - logSum
        i += 1
      }
      new IndirectSparseVec(alphaPriorRootCounts.activeKeysSorted, newActiveValues, activeCount, alphaPriorRootCounts.length)
    }

    val logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]] = {
      alphaPriorBinyCounts.zipWithIndex.map {
        case (tCounts, t) =>
          if (tCounts != null) {
            //println(f"calculate logBinyDist::     ${catIndexer.obj(t)}  ->  $tCounts")
            val logSum = log(tCounts.activeValues.flatMap(_.activeValues).sum)
            //println(f"calculate logBinyDist::     logSum=${logSum}")

            val tActiveValues = tCounts.activeValues
            val tNewActiveValues = tActiveValues.zipWithIndex.map {
              case (uCounts, ui) =>
                val uActiveValues = uCounts.activeValues
                val uNewActiveValues = uActiveValues.zipWithIndex.map {
                  case (vCount, vi) =>
                    //println(f"calculate logBinyDist::     ${catIndexer.obj(t)}  ->  Binary(${catIndexer.obj(tCounts.activeKeysSorted(ui))}%-20s, ${catIndexer.obj(uCounts.activeKeysSorted(vi))}%-20s)      ---    log(vCount) = log($vCount) = ${log(vCount)}")
                    //if (vCount == 0.0) println(f"calculate logBinyDist::     ${catIndexer.obj(t)}  ->  Binary(${catIndexer.obj(tCounts.activeKeysSorted(ui))}%-20s, ${catIndexer.obj(uCounts.activeKeysSorted(vi))}%-20s)      ---    log(vCount) = log($vCount) = ${log(vCount)}")
                    log(vCount) - logSum
                }
                new IndirectSparseVec(uCounts.activeKeysSorted, uNewActiveValues, uCounts.activeCount, uCounts.length)
            }
            new IndirectSparseVec(tCounts.activeKeysSorted, tNewActiveValues, tCounts.activeCount, tCounts.length)
          }
          else null
      }
    }
    //    println("logBinyDist::")
    //    logBinyDist.zipWithIndex.foreach {
    //      case (uvs, t) =>
    //        if (uvs == null) {
    //          println(f"    ${catIndexer.obj(t)}%-20s -> no binary productions")
    //        }
    //        else {
    //          uvs.activePairs.foreach {
    //            case (u, vs) =>
    //              vs.activePairs.foreach {
    //                case (v, p) =>
    //                  println(f"    ${catIndexer.obj(t)}%-20s -> Binary(${catIndexer.obj(u)}%-20s, ${catIndexer.obj(v)}%-20s)  =  ${math.exp(p)}")
    //              }
    //          }
    //        }
    //    }

    val logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]] = {
      val newActiveValues = alphaPriorUnryCounts.activeValues.map { tCounts =>
        val tActiveValues = tCounts.activeValues
        val logSum = log(FastMathUtil.sum(tActiveValues, tCounts.activeCount))
        val tNewActiveValues = tActiveValues.map(uCount => log(uCount) - logSum)
        new IndirectSparseVec(tCounts.activeKeysSorted, tNewActiveValues, tCounts.activeCount, tCounts.length)
      }
      new IndirectSparseVec(alphaPriorUnryCounts.activeKeysSorted, newActiveValues, alphaPriorUnryCounts.activeCount, alphaPriorUnryCounts.length)
    }

    val logTermDist: Array[Vec[Double]] = {
      alphaPriorTermCounts.map { tCounts =>
        if (tCounts != null) {
          tCounts match {
            case tCounts: IndirectSparseVec[Double] =>
              val sum = tCounts.activeValues.sum
              val tNewActiveValues = tCounts.activeValues.map(wCount => log(wCount / sum))
              new IndirectSparseVec(tCounts.activeKeysSorted, tNewActiveValues, tCounts.activeCount, tCounts.length)

            case tCounts: DirectSparseVec[Double] =>
              val activeKeys = tCounts.activeKeysSorted
              val activeCount = tCounts.activeCount
              val countsData = tCounts.data
              val length = tCounts.length
              val sum = activeSum(countsData, activeKeys, activeCount)

              val tNewData = new Array[Double](length)
              var i = 0
              while (i < activeCount) {
                val ai = activeKeys(i)
                tNewData(ai) = log(countsData(ai) / sum)
                i += 1
              }
              new DirectSparseVec(activeKeys, tNewData, activeCount, length)
          }
        }
        else null
      }
    }

    val logPmixDist: Array[Array[Double]] = {
      alphaPriorPmixCounts.map { tCounts =>
        if (tCounts != null) {
          val logSum = log(tCounts.sum)
          tCounts.map(wCount => log(wCount) - logSum)
        }
        else null
      }
    }

    (logRootDist, logBinyDist, logUnryDist, logTermDist, logPmixDist)
  }

  @tailrec private[this] final def iterate(
    guideCharts: Array[CfgGuideChartI], numCharts: Int,
    logRootDist: IndirectSparseVec[Double], //                                 t -> p
    logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
    logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
    logTermDist: Array[Vec[Double]], //                                        t -> w -> p
    logPmixDist: Array[Array[Double]], //                                      t -> p
    alphaPriorRootCounts: IndirectSparseVec[Double], //                            t -> c
    alphaPriorBinyCounts: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //  t -> u -> v -> c
    alphaPriorUnryCounts: IndirectSparseVec[IndirectSparseVec[Double]], //         t -> u -> c
    alphaPriorTermCounts: Array[Vec[Double]], //                                   t -> w -> c
    alphaPriorPmixCounts: Array[Array[Double]], //                                 t -> c
    knownRoots: Array[Int], //                               ts
    knownBinys: Array[IndirectSparseVec[Array[Int]]], //     t -> u -> vs
    knownUnrys: IndirectSparseVec[Array[Int]], //            t -> us
    knownTerms: Array[Array[Int]], //                        t -> ws
    numCats: Int, numWords: Int,
    runningTrees: Array[CcgTreeI],
    iteration: Int): // 
    Array[CcgTreeI] = {

    if (iteration < samplingIterations) {
      val startTime = System.currentTimeMillis()

      val resampledTrees: Array[CcgTreeI] = guideCharts.par.map(pcfgTreeSampler.sample(_, logRootDist, logBinyDist, logUnryDist, logTermDist, logPmixDist, numCats)).toArray

      println(f"iteration ${((if (iteration < 0) iteration else iteration + 1) + ":").padRight(4)} ${(System.currentTimeMillis() - startTime) / 1000.0}%.3f sec")

      val runningWithNew = resampledTrees ++ runningTrees
      for ((evaluator, evalIters) <- itermediateEvaluatorAndEvalIters if iteration > 0 && iteration < samplingIterations - 1 && evalIters.contains(iteration + 1)) evaluator(supPcfgTrainer.train(runningWithNew))

      val (treesToEstimate, numTrees) = { if (accumulate) { val ts = runningWithNew; (ts, ts.length) } else (resampledTrees, numCharts) }

      val (estRootCounts, estBinyCounts, estUnryCounts, estTermCounts, estPmixCounts) =
        productionCounter.counts(treesToEstimate, numTrees, knownRoots, knownBinys, knownUnrys, knownTerms, numCats, numWords)

      val (logNewRootDist, logNewBinyDist, logNewUnryDist, logNewTermDist, logNewPmixDist) =
        resampleParams(alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts, alphaPriorPmixCounts, estRootCounts, estBinyCounts, estUnryCounts, estTermCounts, estPmixCounts)

      iterate(guideCharts, numCharts,
        logNewRootDist, logNewBinyDist, logNewUnryDist, logNewTermDist, logNewPmixDist,
        alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts, alphaPriorPmixCounts,
        knownRoots, knownBinys, knownUnrys, knownTerms,
        numCats, numWords,
        if (iteration >= 0) runningWithNew else runningTrees, // add new trees during sampling iterations only
        iteration + 1)
    }
    else {
      println(f"MAX ITERATIONS REACHED")
      runningTrees
    }
  }

  def resampleParams(
    alphaPriorRootCounts: IndirectSparseVec[Double], //                            t -> c
    alphaPriorBinyCounts: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //  t -> u -> v -> c
    alphaPriorUnryCounts: IndirectSparseVec[IndirectSparseVec[Double]], //         t -> u -> c
    alphaPriorTermCounts: Array[Vec[Double]], //                                   t -> w -> c
    alphaPriorPmixCounts: Array[Array[Double]], //                                 t -> c

    estRootCounts: IndirectSparseVec[Double], //                                   t -> c
    estBinyCounts: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //         t -> u -> v -> c
    estUnryCounts: IndirectSparseVec[IndirectSparseVec[Double]], //                t -> u -> c
    estTermCounts: Array[Vec[Double]], //                                          t -> w -> c
    estPmixCounts: Array[Array[Double]] //                                         t -> c
    ) = {

    val logNewRootDist: IndirectSparseVec[Double] = {
      val activeTCounts = estRootCounts.activeValues
      val alphaPriorActiveCounts = alphaPriorRootCounts.activeValues
      val activeTsLen = activeTCounts.length
      var activeTi = 0
      while (activeTi < activeTsLen) {
        activeTCounts(activeTi) = gammaLogDraw(activeTCounts(activeTi) + alphaPriorActiveCounts(activeTi), rand)
        activeTi += 1
      }
      logNormalize(activeTCounts, activeTsLen)
      estRootCounts
    }

    val logNewBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]] = {
      var logReusableSumArray = new Array[Double](numCats * numCats)

      val tsLen = estBinyCounts.length
      var t = 0
      while (t < tsLen) {
        val tCounts = estBinyCounts(t)
        if (tCounts != null) {
          var sumCount = 0

          {
            val tActiveValues = tCounts.activeValues
            val tActiveValuesLen = tActiveValues.length
            val tAlphaPriorActiveCounts = alphaPriorBinyCounts(t).activeValues
            var tActiveValuesi = 0
            while (tActiveValuesi < tActiveValuesLen) {
              val uActiveValues = tActiveValues(tActiveValuesi).activeValues
              val uActiveValuesLen = uActiveValues.length
              val uAlphaPriorActiveCounts = tAlphaPriorActiveCounts(tActiveValuesi).activeValues
              var uActiveValuesi = 0
              while (uActiveValuesi < uActiveValuesLen) {
                val uvCount = uActiveValues(uActiveValuesi) + uAlphaPriorActiveCounts(uActiveValuesi)
                val logSampledCount = gammaLogDraw(uvCount, rand) // sample new count
                uActiveValues(uActiveValuesi) = logSampledCount

                logReusableSumArray(sumCount) = logSampledCount
                sumCount += 1

                uActiveValuesi += 1
              }
              tActiveValuesi += 1
            }
          }

          val logSampledCountSum = logSum(logReusableSumArray, sumCount)

          {
            val tActiveValues = tCounts.activeValues
            val tActiveValuesLen = tActiveValues.length
            var tActiveValuesi = 0
            while (tActiveValuesi < tActiveValuesLen) {
              val uCounts = tActiveValues(tActiveValuesi)
              val uActiveValues = uCounts.activeValues
              val uActiveValuesLen = uActiveValues.length
              var uActiveValuesi = 0
              while (uActiveValuesi < uActiveValuesLen) {
                val uvLogSampledCount = uActiveValues(uActiveValuesi)
                uActiveValues(uActiveValuesi) = uvLogSampledCount - logSampledCountSum // normalize sampled count
                uActiveValuesi += 1
              }
              tActiveValuesi += 1
            }
          }

        }
        t += 1
      }

      estBinyCounts
    }

    val logNewUnryDist: IndirectSparseVec[IndirectSparseVec[Double]] = {
      val activeCountValues = estUnryCounts.activeValues
      val activeCountValuesLen = activeCountValues.length
      val alphaPriorUnryCountsValues = alphaPriorUnryCounts.activeValues
      var activeCountValuesi = 0
      while (activeCountValuesi < activeCountValuesLen) {
        val tActiveCountValues = activeCountValues(activeCountValuesi).activeValues
        val tAlphaPriorActiveCountValues = alphaPriorUnryCountsValues(activeCountValuesi).activeValues

        val length = tActiveCountValues.length
        var i = 0
        while (i < length) {
          tActiveCountValues(i) = gammaLogDraw(tActiveCountValues(i) + tAlphaPriorActiveCountValues(i), rand)
          i += 1
        }
        logNormalize(tActiveCountValues, length)

        activeCountValuesi += 1
      }
      estUnryCounts
    }

    val logNewTermDist: Array[Vec[Double]] = {
      val tsLen = estTermCounts.length
      var t = 0
      while (t < tsLen) {
        val tCounts = estTermCounts(t)
        if (tCounts != null) {
          tCounts match {
            case tCounts: IndirectSparseVec[Double] =>
              val tActiveCountValues = tCounts.activeValues
              val tAlphaPriorActiveCountValues = alphaPriorTermCounts(t).activeValues

              val length = tActiveCountValues.length
              var i = 0
              while (i < length) {
                tActiveCountValues(i) = gammaLogDraw(tActiveCountValues(i) + tAlphaPriorActiveCountValues(i), rand)
                i += 1
              }
              logNormalize(tActiveCountValues, length)

            case tCounts: DirectSparseVec[Double] =>
              val tActiveKeys = tCounts.activeKeysSorted
              val tCountData = tCounts.data
              val tActiveCount = tActiveKeys.length
              val tAlphaPriorActiveCountValues = alphaPriorTermCounts(t).asInstanceOf[DirectSparseVec[Double]].data

              var i = 0
              while (i < tActiveCount) {
                val ai = tActiveKeys(i)
                tCountData(i) = gammaLogDraw(tCountData(ai) + tAlphaPriorActiveCountValues(ai), rand)
                i += 1
              }
              activeLogNormalize(tCountData, tActiveKeys, tActiveCount)
          }
        }
        t += 1
      }
      estTermCounts
    }

    val logNewPmixDist: Array[Array[Double]] = {
      val tsLen = estPmixCounts.length
      var t = 0
      while (t < tsLen) {
        val tCounts = estPmixCounts(t)
        val tAlphaPriorActiveCountValues = alphaPriorPmixCounts(t)

        val length = tCounts.length
        var i = 0
        while (i < length) {
          tCounts(i) = gammaLogDraw(tCounts(i) + tAlphaPriorActiveCountValues(i), rand)
          i += 1
        }
        logNormalize(tCounts, length)
        t += 1
      }
      estPmixCounts
    }

    (logNewRootDist, logNewBinyDist, logNewUnryDist, logNewTermDist, logNewPmixDist)
  }

  override def toString = f"McmcPcfg(samplingIterations=${samplingIterations}, burninIterations=${burninIterations}, alphaRoot=${alphaRoot}, alphaBiny=${alphaBiny}, alphaUnry=${alphaUnry}, alphaTerm=${alphaTerm}"
}

