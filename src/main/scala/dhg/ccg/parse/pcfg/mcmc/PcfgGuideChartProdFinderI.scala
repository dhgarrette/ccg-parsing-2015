package dhg.ccg.parse.pcfg.mcmc

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

trait PcfgGuideChartProdFinderI {

  type Cat = Int
  type Word = Int

  def all(
    gcs: Array[CfgGuideChartI],
    numCats: Int, numWords: Int): ( //
    Array[Cat], //                               ts
    Array[IndirectSparseVec[Array[Cat]]], //             t -> u -> vs
    IndirectSparseVec[Array[Cat]], //                    t -> us
    Array[Array[Word]]) //                       t -> ws

}

class SimplePcfgGuideChartProdFinderI() extends PcfgGuideChartProdFinderI {

  def all(
    gcs: Array[CfgGuideChartI],
    numCats: Int, numWords: Int): ( //
    Array[Cat], //                               ts
    Array[IndirectSparseVec[Array[Cat]]], //             t -> u -> vs
    IndirectSparseVec[Array[Cat]], //                    t -> us
    Array[Array[Word]]) //                       t -> ws
    = {

    println(f"    SimplePcfgGuideChartProdFinderI.all::    gcs.length=${gcs.length}")

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

    (knownRoots, knownBinys, knownUnrys, knownTerms)
  }

}
