package dhg.ccg.parse.scg.mcmc

import dhg.util._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.tagdict.StartEndTags
import scalaz._
import Scalaz._
import dhg.ccg.util._
import scala.collection.immutable.BitSet

trait ScgProductionCounterI {
  type Cat = Int
  type Word = Int

  def counts(trees: Array[CcgTreeI], numTrees: Int,
    knownRoots: Array[Cat], //                           ts
    knownBinys: Array[IndirectSparseVec[Array[Cat]]], // t -> u -> vs
    knownUnrys: IndirectSparseVec[Array[Cat]], //        t -> us
    knownTerms: Array[Array[Word]], //                   t -> ws
    knownLctxs: Array[Array[Cat]], //                    t -> ls
    knownRctxs: Array[Array[Cat]], //                    t -> rs
    numCats: Int, numWords: Int,
    startCat: Cat, endCat: Cat): ( // 
    IndirectSparseVec[Double], //                                   t -> c
    Array[IndirectSparseVec[IndirectSparseVec[Double]]], //         t -> u -> v -> c
    IndirectSparseVec[IndirectSparseVec[Double]], //                t -> u -> c
    Array[Vec[Double]], //                                          t -> w -> c
    Array[Array[Double]], //                                        t -> c
    Array[IndirectSparseVec[Double]], //                            t -> l -> c
    Array[IndirectSparseVec[Double]]) //                            t -> r -> c
}

class SimpleScgProductionCounterI(
  catIndexer: Indexer[dhg.ccg.cat.Cat], wordIndexer: Indexer[String])
    extends ScgProductionCounterI {

  def counts(trees: Array[CcgTreeI], numTrees: Int,
    knownRoots: Array[Cat], //                           ts
    knownBinys: Array[IndirectSparseVec[Array[Cat]]], // t -> u -> vs
    knownUnrys: IndirectSparseVec[Array[Cat]], //        t -> us
    knownTerms: Array[Array[Word]], //                   t -> ws
    knownLctxs: Array[Array[Cat]], //                    t -> ls
    knownRctxs: Array[Array[Cat]], //                    t -> rs
    numCats: Int, numWords: Int,
    startCat: Cat, endCat: Cat): ( // 
    IndirectSparseVec[Double], //                                   t -> c
    Array[IndirectSparseVec[IndirectSparseVec[Double]]], //         t -> u -> v -> c
    IndirectSparseVec[IndirectSparseVec[Double]], //                t -> u -> c
    Array[Vec[Double]], //                                          t -> w -> c
    Array[Array[Double]], //                                        t -> c
    Array[IndirectSparseVec[Double]], //                            t -> l -> c
    Array[IndirectSparseVec[Double]]) //                            t -> r -> c
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

    val lctxCountsVec: Array[IndirectSparseVec[Double]] = {
      val tCounts = new Array[IndirectSparseVec[Double]](numCats)
      var t = 0
      while (t < numCats) {
        val ls = knownLctxs(t)
        if (ls != null) {
          val wActiveCount = ls.length
          val wCounts = new Array[Double](wActiveCount)
          tCounts(t) = new IndirectSparseVec(ls, wCounts, wActiveCount, numCats)
        }
        t += 1
      }
      tCounts
    }

    val rctxCountsVec: Array[IndirectSparseVec[Double]] = {
      val tCounts = new Array[IndirectSparseVec[Double]](numCats)
      var t = 0
      while (t < numCats) {
        val rs = knownRctxs(t)
        if (rs != null) {
          val wActiveCount = rs.length
          val wCounts = new Array[Double](wActiveCount)
          tCounts(t) = new IndirectSparseVec(rs, wCounts, wActiveCount, numCats)
        }
        t += 1
      }
      tCounts
    }

    def f(tree: CcgTreeI, i: Int, j: Int, supertags: Array[Cat]): Unit = tree match {
      case CcgBinodeI(t, l, r) =>
        binyCountsVec(t)(l.cat)(r.cat) += 1
        pmixCountsVec(t)(0) += 1
        //println(f"scg prod counter::   ($i,$j)  lctx: ${catIndexer.obj(t)} -> ${catIndexer.obj(supertags(i))}   ;     <${supertags.zipWithIndex.map { case (l, i2) => if(i2 == i) f"[${catIndexer.obj(l)}]" else catIndexer.obj(l) }.mkString(", ")}>")
        lctxCountsVec(t)(supertags(i)) += 1
        //println(f"scg prod counter::   ($i,$j)  rctx: ${catIndexer.obj(t)} -> ${catIndexer.obj(supertags(j+1))}   ;     <${supertags.zipWithIndex.map { case (l, j2) => if(j2 == j+1) f"[${catIndexer.obj(l)}]" else catIndexer.obj(l) }.mkString(", ")}>")
        //println(f"    ${rctxCountsVec(t)}")
        //println(f"    ${rctxCountsVec(t)(supertags(j + 1))}")
        rctxCountsVec(t)(supertags(j + 1)) += 1
        val k = i + l.length
        f(l, i, k, supertags)
        f(r, k, j, supertags)
      case CcgUnodeI(t, s) =>
        unryCountsVec(t)(s.cat) += 1
        pmixCountsVec(t)(1) += 1
        lctxCountsVec(t)(supertags(i)) += 1
        rctxCountsVec(t)(supertags(j + 1)) += 1
        f(s, i, j, supertags)
      case CcgLeafI(t, w) =>
        termCountsVec(t)(w) += 1
        pmixCountsVec(t)(2) += 1
        lctxCountsVec(t)(supertags(i)) += 1
        rctxCountsVec(t)(supertags(j + 1)) += 1
    }

    var treei = 0
    while (treei < numTrees) {
      val tree = trees(treei)
      rootCountsVec(tree.cat) += 1
      val supertags: Array[Cat] = startCat +: tree.supertags.toArray :+ endCat
      f(tree, 0, tree.length, supertags)
      treei += 1
    }

    (rootCountsVec, binyCountsVec, unryCountsVec, termCountsVec, pmixCountsVec, lctxCountsVec, rctxCountsVec)
  }

}
