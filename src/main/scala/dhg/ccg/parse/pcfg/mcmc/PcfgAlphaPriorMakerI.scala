package dhg.ccg.parse.pcfg.mcmc

import dhg.util._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.prob._
import scalaz._
import Scalaz._
import dhg.ccg.tagdict.StartEndTags
import dhg.ccg.util._
import scala.collection.immutable.BitSet

trait PcfgAlphaPriorMakerI {

  def makeAll(
    priorRootDist: IndirectSparseVec[LogDouble], //                                 t -> p
    priorBinyDist: Array[IndirectSparseVec[IndirectSparseVec[LogDouble]]], //       t -> u -> v -> p
    priorUnryDist: IndirectSparseVec[IndirectSparseVec[LogDouble]], //              t -> u -> p
    priorTermDist: Array[Vec[LogDouble]], //                                        t -> w -> p
    priorPmixDist: Array[Array[LogDouble]], //                                      t -> p
    goldRootCounts: IndirectSparseVec[Double], //                               t -> c
    goldBinyCounts: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //     t -> u -> v -> c
    goldUnryCounts: IndirectSparseVec[IndirectSparseVec[Double]], //            t -> u -> c
    goldTermCounts: Array[Vec[Double]], //                                      t -> w -> c
    goldPmixCounts: Array[Array[Double]], //                                    t -> c
    alphaRoot: Double, alphaBiny: Double, alphaUnry: Double, alphaTerm: Double, alphaPmix: Double,
    numCats: Int, numWords: Int): ( //
    IndirectSparseVec[Double], //                                 t -> c
    Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> c
    IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> c
    Array[Vec[Double]], //                                        t -> w -> c
    Array[Array[Double]]) //                                      t -> c

}

class TrainDataPcfgAlphaPriorMakerI(
  catIndexer: Indexer[Cat], wordIndexer: Indexer[String])
    extends PcfgAlphaPriorMakerI {

  def makeAll(
    priorRootDist: IndirectSparseVec[LogDouble], //                                 t -> p
    priorBinyDist: Array[IndirectSparseVec[IndirectSparseVec[LogDouble]]], //       t -> u -> v -> p
    priorUnryDist: IndirectSparseVec[IndirectSparseVec[LogDouble]], //              t -> u -> p
    priorTermDist: Array[Vec[LogDouble]], //                                        t -> w -> p
    priorPmixDist: Array[Array[LogDouble]], //                                      t -> p
    goldRootCounts: IndirectSparseVec[Double], //                               t -> c
    goldBinyCounts: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //     t -> u -> v -> c
    goldUnryCounts: IndirectSparseVec[IndirectSparseVec[Double]], //            t -> u -> c
    goldTermCounts: Array[Vec[Double]], //                                      t -> w -> c
    goldPmixCounts: Array[Array[Double]], //                                    t -> c
    alphaRoot: Double, alphaBiny: Double, alphaUnry: Double, alphaTerm: Double, alphaPmix: Double,
    numCats: Int, numWords: Int): ( //
    IndirectSparseVec[Double], //                                 t -> c
    Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> c
    IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> c
    Array[Vec[Double]], //                                        t -> w -> c
    Array[Array[Double]]) //                                      t -> c
    = {

    val alphaPriorRootCounts: IndirectSparseVec[Double] = {
      IndirectSparseVec(
        goldRootCounts.activePairs.map {
          case (t, count) =>
            val p = alphaRoot * priorRootDist(t).toDouble + count
            assert(p > 0, f"priorRootDist(${catIndexer.obj(t)}) = ${priorRootDist(t).toDouble}")
            t -> p
        }, numCats)
    }

    val alphaPriorBinyCounts: Array[IndirectSparseVec[IndirectSparseVec[Double]]] = {
      (0 until numCats).map { t =>
        val tGoldCounts = goldBinyCounts(t)
        val tPriorDist = priorBinyDist(t)
        if (tGoldCounts != null) {
          IndirectSparseVec(tGoldCounts.activePairs.map {
            case (u, tuGoldCounts) =>
              val uPriorDist = tPriorDist(u)
              u -> IndirectSparseVec(tuGoldCounts.activePairs.map {
                case (v, count) =>
                  //println(f"calculate alphaPriorBinyCounts::    ${catIndexer.obj(t)}%-20s -> Binary(${catIndexer.obj(u)}%-20s, ${catIndexer.obj(v)}%-20s)    = $alphaBiny * ${uPriorDist(v).logValue} + $count     = ${alphaBiny * uPriorDist(v).toDouble + count}")
                  val p = alphaBiny * uPriorDist(v).toDouble + count
                  assert(p > 0, f"uPriorDist(${catIndexer.obj(u)}) * uPriorDist(${catIndexer.obj(v)}) = ${uPriorDist(u).toDouble} * ${uPriorDist(v).toDouble}")
                  v -> p
              }, numCats)
          }, numCats)
        }
        else null
      }.toArray
    }

    val alphaPriorUnryCounts: IndirectSparseVec[IndirectSparseVec[Double]] = {
      IndirectSparseVec(goldUnryCounts.activePairs.map {
        case (t, tGoldCounts) =>
          val tPriorDist = priorUnryDist(t)
          t -> IndirectSparseVec(tGoldCounts.activePairs.map {
            case (u, count) =>
              val p = alphaUnry * tPriorDist(u).toDouble + count
              assert(p > 0, f"tPriorDist(${catIndexer.obj(u)}) = ${tPriorDist(u).toDouble}")
              u -> p
          }, numCats)
      }, numCats)
    }

    val alphaPriorTermCounts: Array[Vec[Double]] = {
      (0 until numCats).map { t =>
        val tGoldCounts = goldTermCounts(t)
        val tPriorDist = priorTermDist(t)
        if (tGoldCounts != null) {
          SparseVec(tGoldCounts.activePairs.map {
            case (w, count) =>
              val p = alphaTerm * tPriorDist(w).toDouble + count
              assert(p > 0, f"tPriorDist(${wordIndexer.obj(w)}) = ${tPriorDist(w).toDouble}")
              w -> p
          }, numWords)
        }
        else null
      }.toArray
    }

    val alphaPriorPmixCounts: Array[Array[Double]] = {
      (0 until numCats).map { t =>
        val tGoldCounts = goldPmixCounts(t)
        if (tGoldCounts != null) {
          val tPriorDist = priorPmixDist(t)
          (0 until 3).map { i =>
            val count = tGoldCounts(i)
            val p = alphaPmix * tPriorDist(i).toDouble + count
            assert(p > 0, f"tPriorDist($i) = ${tPriorDist(i).toDouble}")
            p
          }.toArray
        }
        else null
      }.toArray
    }

    (alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts, alphaPriorPmixCounts)
  }

}

