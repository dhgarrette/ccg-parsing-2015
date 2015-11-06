package dhg.ccg.parse.scg.mcmc

import dhg.util._
import scala.annotation.tailrec
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import scala.collection.mutable.{ BitSet => MBitSet }
import scala.collection.breakOut
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.tagdict._
import dhg.ccg.util._
import scalaz._
import Scalaz._
import scala.collection.immutable.BitSet
import scala.collection.mutable.ArrayBuffer

trait ScgGuideChartProdFinderI {

  type Cat = Int
  type Word = Int

  def all(
    gcs: Array[CfgGuideChartI],
    numCats: Int, numWords: Int,
    startCat: Cat, endCat: Cat): ( //
    Array[Cat], //                           ts
    Array[IndirectSparseVec[Array[Cat]]], //         t -> u -> vs
    IndirectSparseVec[Array[Cat]], //                t -> us
    Array[Array[Word]], //                   t -> ws
    Array[Array[Cat]], //                    t -> ls
    Array[Array[Cat]]) //                    t -> rs

}

class SimpleScgGuideChartProdFinderI(catIndexer: Indexer[Cat], wordIndexer: Indexer[String]) extends ScgGuideChartProdFinderI {

  def all(
    gcs: Array[CfgGuideChartI],
    numCats: Int, numWords: Int,
    startCat: Cat, endCat: Cat): ( //
    Array[Cat], //                           ts
    Array[IndirectSparseVec[Array[Cat]]], //         t -> u -> vs
    IndirectSparseVec[Array[Cat]], //                t -> us
    Array[Array[Word]], //                   t -> ws
    Array[Array[Cat]], //                    t -> ls
    Array[Array[Cat]]) //                    t -> rs
    = {

    println(f"    SimpleScgGuideChartProdFinderI.all::    gcs.length=${gcs.length}")

    val knownRoots = time("    knownRoots", {
      val knownRootsSet = MBitSet.empty
      var gci = 0
      val gcsLen = gcs.length
      while (gci < gcsLen) {
        val roots = gcs(gci).root
        val rootsLen = roots.length
        var rooti = 0
        while (rooti < rootsLen) {
          knownRootsSet += roots(rooti)._1
          rooti += 1
        }
        gci += 1
      }
      knownRootsSet.toArray
    })

    val knownBinys = time("    knownBinys", {
      val knownBinysSets = Array.fill(numCats)(MMap.empty[Int, MBitSet])
      for {
        gc <- gcs
        //(i, j, cell) <- gc.bottomUpNodes
        span <- 1 to gc.length //        span size
        i <- 0 to (gc.length - span) //  start of span
        j = i + span //                  end of span
        (t, entries) <- gc(i, j)
        BinaryGuideChartEntryI(k, u, v) <- entries
      } {
        val tSets = knownBinysSets(t)
        val uSet = tSets.getOrElseUpdate(u, MBitSet.empty)
        uSet += v
      }
      knownBinysSets.map { case tSets if tSets.nonEmpty => IndirectSparseVec(tSets.mapVals(_.toArray), numCats); case _ => null }
    })

    val knownUnrys = time("    knownUnrys", {
      val knownUnrysSets = MMap.empty[Int, MBitSet]
      for {
        gc <- gcs
        (_, _, cell) <- gc.bottomUpNodes
        (t, entries) <- cell
        UnaryGuideChartEntryI(u) <- entries
      } {
        val tSet = knownUnrysSets.getOrElseUpdate(t, MBitSet.empty)
        tSet += u
      }
      IndirectSparseVec(knownUnrysSets.mapVals(_.toArray), numCats)
    })

    val knownTerms = time("    knownTerms", {
      val knownTermsSets = Array.fill(numCats)(MBitSet.empty)
      for {
        gc <- gcs
        //(i, j, cell) <- gc.bottomUpNodes
        span <- 1 to gc.length //        span size
        i <- 0 to (gc.length - span) //  start of span
        j = i + span //                  end of span
        (t, entries) <- gc(i, j)
        TermGuideChartEntryI(w) <- entries
      } {
        knownTermsSets(t) += w
      }
      knownTermsSets.map { case s if s.nonEmpty => s.toArray; case _ => null }
    })

    val knownLctxs = time("    knownLctxs", {
      val knownLctxsSets = Array.fill(numCats)(MBitSet.empty)
      for {
        gc <- gcs
        supertagSets = Array(startCat) +: gc.supertagSets.toArray :+ Array(endCat)
        //(i, j, cell) <- gc.bottomUpNodes
        span <- 1 to gc.length //        span size
        i <- 0 to (gc.length - span) //  start of span
        j = i + span //                  end of span
        (t, _) <- gc(i, j)
      } {
        knownLctxsSets(t) ++= supertagSets(i)
      }
      knownLctxsSets.map { case s if s.nonEmpty => s.toArray; case _ => null }
    })

    val knownRctxs = time("    knownRctxs", {
      val knownRctxsSets = Array.fill(numCats)(MBitSet.empty)
      for {
        gc <- gcs
        supertagSets = Array(startCat) +: gc.supertagSets.toArray :+ Array(endCat)
        //(i, j, cell) <- gc.bottomUpNodes
        span <- 1 to gc.length //        span size
        i <- 0 to (gc.length - span) //  start of span
        j = i + span //                  end of span
        (t, _) <- gc(i, j)
      } {
        knownRctxsSets(t) ++= supertagSets(j + 1)
      }
      knownRctxsSets.map { case s if s.nonEmpty => s.toArray; case _ => null }
    })

    //    //
    //
    //    {
    //      val btloads = ArrayBuffer[Double]()
    //      var btSingletons = 0
    //      var btOver80 = 0
    //      val buloads = ArrayBuffer[Double]()
    //      var buSingletons = 0
    //      var buOver80 = 0
    //      for (us <- knownBinys if us != null) {
    //        btloads += (us.activeCount / numCats.toDouble)
    //        if (us.activeCount == 1) btSingletons += 1
    //        if (us.activeCount > (0.8 * numCats)) btOver80 += 1
    //        for (vs <- us.activeValues) {
    //          buloads += (vs.length / numCats.toDouble)
    //          if (vs.length == 1) buSingletons += 1
    //          if (vs.length > (0.8 * numCats)) buOver80 += 1
    //        }
    //      }
    //      println(f"sirjgoirs   Binary Loads:  ")
    //      println(f"sirjgoirs       t loads:  avg load = ${btloads.avg},  # singletons = ${btSingletons / numCats.toDouble},  over 80 = ${btOver80 / numCats.toDouble}")
    //      println(f"sirjgoirs       u loads:  avg load = ${buloads.avg},  # singletons = ${buSingletons / numCats.toDouble},  over 80 = ${buOver80 / numCats.toDouble}")
    //    }
    //
    //    {
    //      val wloads = ArrayBuffer[Double]()
    //      var wSingletons = 0
    //      var wOver80 = 0
    //      for (ws <- knownTerms if ws != null) {
    //        wloads += (ws.length / numWords.toDouble)
    //        if (ws.length == 1) wSingletons += 1
    //        if (ws.length > (0.8 * numCats)) wOver80 += 1
    //      }
    //      println(f"sirjgoirs   Term Loads:  ")
    //      println(f"sirjgoirs       t loads:  avg load = ${wloads.avg},  # singletons = ${wSingletons / numCats.toDouble},  over 80 = ${wOver80 / numCats.toDouble}")
    //    }
    //
    //    {
    //      val lloads = ArrayBuffer[Double]()
    //      var lSingletons = 0
    //      var lOver80 = 0
    //      for (ls <- knownLctxs if ls != null) {
    //        lloads += (ls.length / numCats.toDouble)
    //        if (ls.length == 1) lSingletons += 1
    //        if (ls.length > (0.8 * numCats)) lOver80 += 1
    //      }
    //      println(f"sirjgoirs   L-Ctx Loads:  ")
    //      println(f"sirjgoirs       t loads:  avg load = ${lloads.avg},  # singletons = ${lSingletons / numCats.toDouble},  over 80 = ${lOver80 / numCats.toDouble}")
    //    }
    //
    //    {
    //      val rloads = ArrayBuffer[Double]()
    //      var rSingletons = 0
    //      var rOver80 = 0
    //      for (rs <- knownLctxs if rs != null) {
    //        rloads += (rs.length / numCats.toDouble)
    //        if (rs.length == 1) rSingletons += 1
    //        if (rs.length > (0.8 * numCats)) rOver80 += 1
    //      }
    //      println(f"sirjgoirs   R-Ctx Loads:  ")
    //      println(f"sirjgoirs       t loads:  avg load = ${rloads.avg},  # singletons = ${rSingletons / numCats.toDouble},  over 80 = ${rOver80 / numCats.toDouble}")
    //    }

    (knownRoots, knownBinys, knownUnrys, knownTerms, knownLctxs, knownRctxs)
  }

}
