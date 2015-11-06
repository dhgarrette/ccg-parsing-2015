package dhg.ccg.parse.pcfg.mcmc

import dhg.util._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.tagdict.StartEndTags
import scalaz._
import Scalaz._
import dhg.ccg.util._
import scala.collection.immutable.BitSet

trait PcfgProductionCounterI {
  def counts(trees: Array[CcgTreeI], numTrees: Int,
    knownRoots: Array[Int], //                           ts
    knownBinys: Array[IndirectSparseVec[Array[Int]]], //         t -> u -> vs
    knownUnrys: IndirectSparseVec[Array[Int]], //                t -> us
    knownTerms: Array[Array[Int]], //                    t -> ws
    numCats: Int, numWords: Int): ( // 
    IndirectSparseVec[Double], //                                   t -> c
    Array[IndirectSparseVec[IndirectSparseVec[Double]]], //                 t -> u -> v -> c
    IndirectSparseVec[IndirectSparseVec[Double]], //                        t -> u -> c
    Array[Vec[Double]], //                            t -> w -> c
    Array[Array[Double]]) //                                t -> c
}

class SimplePcfgProductionCounterI(
  catIndexer: Indexer[dhg.ccg.cat.Cat], wordIndexer: Indexer[String])
    extends PcfgProductionCounterI {

  def counts(trees: Array[CcgTreeI], numTrees: Int,
    knownRoots: Array[Int], //                           ts
    knownBinys: Array[IndirectSparseVec[Array[Int]]], //         t -> u -> vs
    knownUnrys: IndirectSparseVec[Array[Int]], //                t -> us
    knownTerms: Array[Array[Int]], //                    t -> ws
    numCats: Int, numWords: Int): ( // 
    IndirectSparseVec[Double], //                                   t -> c
    Array[IndirectSparseVec[IndirectSparseVec[Double]]], //                 t -> u -> v -> c
    IndirectSparseVec[IndirectSparseVec[Double]], //                        t -> u -> c
    Array[Vec[Double]], //                            t -> w -> c
    Array[Array[Double]]) //                                t -> c
    = {

    val rootCountsVec: IndirectSparseVec[Double] = {
      val activeCount = knownRoots.length
      new IndirectSparseVec(knownRoots, new Array[Double](activeCount), activeCount, numCats)
    }

    val binyCountsVec: Array[IndirectSparseVec[IndirectSparseVec[Double]]] = {
      val tCounts = new Array[IndirectSparseVec[IndirectSparseVec[Double]]](numCats)
      var t = 0
      while (t < numCats) {
        val tKnownBinys = knownBinys(t)
        if (tKnownBinys != null) {
          val us = tKnownBinys.activeKeysSorted
          val uValues = tKnownBinys.activeValues
          val uActiveCount = tKnownBinys.activeCount
          val uCounts = new Array[IndirectSparseVec[Double]](uActiveCount)
          var ui = 0
          while (ui < uActiveCount) {
            val vs = uValues(ui)
            val vActiveCount = vs.length
            val vCounts = new Array[Double](vActiveCount)
            uCounts(ui) = new IndirectSparseVec(vs, vCounts, vActiveCount, numCats)
            ui += 1
          }
          tCounts(t) = new IndirectSparseVec(us, uCounts, uActiveCount, numCats)
        }
        t += 1
      }
      tCounts
    }

    val unryCountsVec: IndirectSparseVec[IndirectSparseVec[Double]] = {
      val ts = knownUnrys.activeKeysSorted
      val tValues = knownUnrys.activeValues
      val tActiveCount = knownUnrys.activeCount
      val tCounts = new Array[IndirectSparseVec[Double]](tActiveCount)
      var ti = 0
      while (ti < tActiveCount) {
        val t = ts(ti)
        val us = tValues(ti)
        val uActiveCount = us.length
        val uCounts = new Array[Double](uActiveCount)
        tCounts(ti) = new IndirectSparseVec(us, uCounts, uActiveCount, numCats)
        ti += 1
      }
      new IndirectSparseVec(ts, tCounts, tActiveCount, numCats)
    }

    val termCountsVec: Array[Vec[Double]] = {
      val tCounts = new Array[Vec[Double]](numCats)
      var t = 0
      while (t < numCats) {
        val ws = knownTerms(t)
        if (ws != null) {
          val wActiveCount = ws.length
          val wCounts = new Array[Double](wActiveCount)
          tCounts(t) = SparseVec(ws, wCounts, wActiveCount, numWords)
        }
        t += 1
      }
      tCounts
    }

    val pmixCountsVec: Array[Array[Double]] = {
      val tCounts = new Array[Array[Double]](numCats)
      var t = 0
      while (t < numCats) {
        tCounts(t) = new Array[Double](3)
        t += 1
      }
      tCounts
    }

    def f(tree: CcgTreeI): Unit = tree match {
      case CcgBinodeI(t, l, r) =>
        //println(f"SimplePcfgProductionCounterI.counts::   CcgBinodeI(t=${catIndexer.obj(t)}, l=${catIndexer.obj(l.cat)}, r=${catIndexer.obj(r.cat)})")
        binyCountsVec(t)(l.cat)(r.cat) += 1
        pmixCountsVec(t)(0) += 1
        f(l)
        f(r)
      case CcgUnodeI(t, s) =>
        unryCountsVec(t)(s.cat) += 1
        pmixCountsVec(t)(1) += 1
        f(s)
      case CcgLeafI(t, w) =>
        termCountsVec(t)(w) += 1
        pmixCountsVec(t)(2) += 1
    }

    var treei = 0
    while (treei < numTrees) {
      val tree = trees(treei)
      rootCountsVec(tree.cat) += 1
      f(tree)
      treei += 1
    }

    (rootCountsVec, binyCountsVec, unryCountsVec, termCountsVec, pmixCountsVec)
  }

}
