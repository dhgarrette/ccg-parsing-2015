package dhg.ccg.parse.pcfg

import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import dhg.util._
import scalaz._
import scalaz.Scalaz._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.util._
import scala.collection.immutable.BitSet

trait SupParserTrainerI {
  def train(trees: Array[CcgTreeI]): PcfgParserI
}

//trait SupKBestParserTrainerI extends SupParserTrainerI {
//  def train(trees: Vector[CcgTreeI]): KBestGuideChartParser
//}

//class UnsmoothedSupPcfgTrainerI(
//  productionFinder: PcfgProductionCounterI,
//  pcfgParserInstantiater: PcfgParserInstantiaterI,
//  rootSet: BitSet, //                ts
//  binyProds: Array[IndirectSparseVec[IndirectSparseVec[Int]]], //  t -> u -> v -> prod
//  unryProds: IndirectSparseVec[IndirectSparseVec[Int]], //         t -> u -> prods
//  termProds: Array[IndirectSparseVec[Int]], //             t -> w -> prods
//  knownBinys: Array[BitSet], //      t -> prods
//  knownUnrys: IndirectSparseVec[BitSet], //  t -> prods
//  knownTerms: Array[BitSet], //      t -> prods
//  numCats: Int)(catIndexer: Indexer[Cat], wordIndexer: Indexer[String], prodIndexer: Indexer[Prod])
//    extends SupParserTrainerI {
//
//  def train(trees: Array[CcgTreeI]): KBestGuideChartParser = {
//    val numTrees = trees.length
//    val sampledRootCounts = productionFinder.rootCounts(trees, numTrees, rootSet)
//    val sampledProdCounts = productionFinder.prodCounts(trees, numTrees, binyProds, unryProds, termProds, knownBinys, knownUnrys, knownTerms, numCats)
//
//    val rootDist = new SimpleLogProbabilityDistribution[Cat](sampledRootCounts)
//    val prodDist = new SimpleConditionalLogProbabilityDistribution[Cat, ProdI](sampledProdICounts.mapVals(new SimpleLogProbabilityDistribution(_)))
//
//    pcfgParserInstantiater(rootDist, prodDist, binyProds, unryProds, termProds)(catIndexer, wordIndexer, prodIndexer)
//  }
//
//}

class AlphaBetaSupPcfgTrainerI(
  priorRootDist: IndirectSparseVec[LogDouble], //                                 t -> p
  priorBinyDist: Array[IndirectSparseVec[IndirectSparseVec[LogDouble]]], //       t -> u -> v -> p
  priorUnryDist: IndirectSparseVec[IndirectSparseVec[LogDouble]], //              t -> u -> p
  priorTermDist: Array[Vec[LogDouble]], //                                        t -> w -> p
  priorPmixDist: Array[Array[LogDouble]], //                                      t -> p
  alphaRoot: LogDouble, alphaBiny: LogDouble, alphaUnry: LogDouble, alphaTerm: LogDouble, alphaPmix: LogDouble,
  productionCounter: PcfgProductionCounterI,
  pcfgParserInstantiater: PcfgParserInstantiaterI,
  knownRoots: Array[Int], //                               ts
  knownBinys: Array[IndirectSparseVec[Array[Int]]], //     t -> u -> vs
  knownUnrys: IndirectSparseVec[Array[Int]], //            t -> us
  knownTerms: Array[Array[Int]], //                        t -> ws
  numCats: Int, numWords: Int)(catIndexer: Indexer[Cat], wordIndexer: Indexer[String])
    extends SupParserTrainerI {

  def train(trees: Array[CcgTreeI]) = {

    val numTrees = trees.length
    val (estRootCounts, estBinyCounts, estUnryCounts, estTermCounts, estMixCounts) =
      productionCounter.counts(trees, numTrees, knownRoots, knownBinys, knownUnrys, knownTerms, numCats, numWords)

    val logRootDist: IndirectSparseVec[Double] = {
      val counts = IndirectSparseVec(
        estRootCounts.activePairs.map {
          case (t, estCount) =>
            //println(f"rootDist::  t=${catIndexer.obj(t)} -> (${alphaRoot.toDouble} * ${prior.toDouble} + (${estRootCounts(t)} + ${goldRootCounts(t)})) = ${(alphaRoot * prior + LogDouble(estRootCounts(t) + goldRootCounts(t))).toDouble}%.3f")
            t -> (alphaRoot * priorRootDist(t) + LogDouble(estCount))
        }, numCats)

      val sum = LogDouble.sum(counts.activeValues)
      //println(f"rootDist::  sum = ${sum.toDouble}")
      //println(f"rootDist::  dist:")
      //counts.activePairs.foreach { case (t, c) => println(f"rootDist::      t=${catIndexer.obj(t)} -> ${(c / sum).toDouble}") }
      IndirectSparseVec(counts.activeKeys, counts.activeValues.map(c => (c / sum).logValue), numCats)
    }

    val logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]] = {
      val counts =
        (0 until numCats).map { t =>
          val tEstCounts = estBinyCounts(t)
          if (tEstCounts != null) {
            IndirectSparseVec(tEstCounts.activePairs.map {
              case (u, tuEstCounts) => u ->
                IndirectSparseVec(tuEstCounts.activePairs.map {
                  case (v, count) =>
                    //println(f"binyDist::  t=${catIndexer.obj(t)} -> Binary(${catIndexer.obj(u)},${catIndexer.obj(v)}) (${alphaBiny.toDouble} * ${prior.toDouble} + (${estBinyCounts(t)(u)(v)} + ${goldBinyCounts(t)(u)(v)})) = ${(alphaBiny * prior + LogDouble(estBinyCounts(t)(u)(v) + goldBinyCounts(t)(u)(v))).toDouble}%.3f")
                    v -> (alphaBiny * priorBinyDist(t)(u)(v) + LogDouble(count))
                }, numCats)
            }, numCats)
          }
          else null
        }.toArray

      val sums: Array[LogDouble] = counts.map { (tCounts: IndirectSparseVec[IndirectSparseVec[LogDouble]]) =>
        if (tCounts != null) {
          LogDouble.sum(tCounts.activeValues.flatMap(_.activeValues))
        }
        else LogDouble.zero
      }

      (0 until numCats).map { t =>
        val tCounts = counts(t)
        if (tCounts != null) {
          val tSum = sums(t)
          IndirectSparseVec(tCounts.activePairs.map {
            case (u, tuCounts) => u ->
              IndirectSparseVec(tuCounts.activePairs.map {
                case (v, count) => v ->
                  (count / tSum).logValue
              }, numCats)
          }, numCats)
        }
        else null
      }.toArray
    }

    val logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]] = {
      val counts =
        IndirectSparseVec(estUnryCounts.activePairs.map {
          case (t, tEstCounts) => t ->
            IndirectSparseVec(tEstCounts.activePairs.map {
              case (u, count) =>
                //println(f"unryDist::  t=${catIndexer.obj(t)} -> Unary(${catIndexer.obj(u)}) (${alphaUnry.toDouble} * ${prior.toDouble} + (${estUnryCounts(t)(u)} + ${goldUnryCounts(t)(u)})) = ${(alphaUnry * prior + LogDouble(estUnryCounts(t)(u) + goldUnryCounts(t)(u))).toDouble}%.3f")
                u -> (alphaUnry * priorUnryDist(t)(u) + LogDouble(count))
            }, numCats)
        }, numCats)

      IndirectSparseVec(counts.activePairs.map {
        case (t, tCounts) =>
          val tSum = LogDouble.sum(tCounts.activeValues)
          //println(f"unryDist::      t=${catIndexer.obj(t)} -> ${tSum.toDouble}")
          t ->
            IndirectSparseVec(tCounts.activePairs.map {
              case (u, count) => u ->
                (count / tSum).logValue
            }, numCats)
      }, numCats)
    }

    val logTermDist: Array[Vec[Double]] = {
      val counts: Array[Vec[LogDouble]] =
        (0 until numCats).map { t =>
          val tEstCounts = estTermCounts(t)
          if (tEstCounts != null) {
            SparseVec(tEstCounts.activePairs.map {
              case (w, count) => w ->
                (alphaTerm * priorTermDist(t)(w) + LogDouble(count))
            }, numWords)
          }
          else null
        }.toArray

      val sums: Array[LogDouble] = counts.map { tCounts =>
        if (tCounts != null) {
          LogDouble.sum(tCounts.activeValues)
        }
        else LogDouble.zero
      }

      (0 until numCats).map { t =>
        val tCounts = counts(t)
        if (tCounts != null) {
          val tSum = sums(t)
          SparseVec(tCounts.activePairs.map {
            case (w, count) => w ->
              (count / tSum).logValue
          }, numWords)
        }
        else null
      }.toArray
    }

    val logPmixDist: Array[Array[Double]] = {
      (0 until numCats).map { t =>
        val binyCount = alphaPmix * priorPmixDist(t)(0) + LogDouble(estMixCounts(t)(0))
        val unryCount = alphaPmix * priorPmixDist(t)(1) + LogDouble(estMixCounts(t)(1))
        val termCount = alphaPmix * priorPmixDist(t)(2) + LogDouble(estMixCounts(t)(2))
        val tSum = binyCount + unryCount + termCount
        Array(binyCount / tSum, unryCount / tSum, termCount / tSum).map(_.logValue)
      }.toArray
    }

    pcfgParserInstantiater.apply(
      logRootDist: IndirectSparseVec[Double], //                                 t -> p
      logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
      logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
      logTermDist: Array[Vec[Double]], //                                        t -> w -> p
      logPmixDist: Array[Array[Double]])( //                                     t -> p
        catIndexer: Indexer[Cat], wordIndexer: Indexer[String])
  }

}
