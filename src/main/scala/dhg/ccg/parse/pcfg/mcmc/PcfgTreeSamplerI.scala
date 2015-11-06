package dhg.ccg.parse.pcfg.mcmc

import dhg.util._
import dhg.util.FastMathUtil._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.prob._
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import scalaz._
import Scalaz._
import dhg.ccg.util.DrawMatrix
import dhg.ccg.util._
import org.apache.commons.math3.random.RandomGenerator

trait PcfgTreeSamplerI {

  def sample(
    guideChart: CfgGuideChartI,
    logRootDist: IndirectSparseVec[Double], //                                 t -> p
    logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
    logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
    logTermDist: Array[Vec[Double]], //                                        t -> w -> p
    logPmixDist: Array[Array[Double]], //                                      t -> p
    numCats: Int): CcgTreeI

  def samples(
    guideChart: CfgGuideChartI,
    logRootDist: IndirectSparseVec[Double], //                                 t -> p
    logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
    logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
    logTermDist: Array[Vec[Double]], //                                        t -> w -> p
    logPmixDist: Array[Array[Double]], //                                      t -> p
    numCats: Int,
    k: Int): Array[CcgTreeI]

}

class SimplePcfgTreeSamplerI(
  pcfgInsideChartBuilder: PcfgInsideChartBuilderI,
  rand: RandomGenerator) //
  (catIndexer: Indexer[Cat], wordIndexer: Indexer[String])
    extends PcfgTreeSamplerI {

  type Cat = Int
  type Word = Int

  def sample(
    guideChart: CfgGuideChartI,
    logRootDist: IndirectSparseVec[Double], //                                 t -> p
    logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
    logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
    logTermDist: Array[Vec[Double]], //                                        t -> w -> p
    logPmixDist: Array[Array[Double]], //                                      t -> p
    numCats: Int): CcgTreeI = {

    val n = guideChart.length
    val insideChart = pcfgInsideChartBuilder.buildInsideChart(guideChart, logBinyDist, logUnryDist, logTermDist, logPmixDist, numCats)
    val ij = sampleRoot(n, logRootDist, insideChart)
    sampleRecursivelyWithoutTable(ij, 0, n, guideChart, insideChart, logBinyDist, logUnryDist, logTermDist, logPmixDist)
  }

  def samples(
    guideChart: CfgGuideChartI,
    logRootDist: IndirectSparseVec[Double], //                                 t -> p
    logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
    logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
    logTermDist: Array[Vec[Double]], //                                        t -> w -> p
    logPmixDist: Array[Array[Double]], //                                      t -> p
    numCats: Int,
    k: Int): Array[CcgTreeI] = {

    if (k == 1) {
      Array(sample(guideChart, logRootDist, logBinyDist, logUnryDist, logTermDist, logPmixDist, numCats))
    }
    else {
      val n = guideChart.length
      val insideChart = pcfgInsideChartBuilder.buildInsideChart(guideChart, logBinyDist, logUnryDist, logTermDist, logPmixDist, numCats)
      val samplingChart = buildSamplingChart(guideChart, insideChart, logBinyDist, logUnryDist, logTermDist, logPmixDist)
      val sampled = new Array[CcgTreeI](k)
      var sampledi = 0
      while (sampledi < k) {
        val ij = sampleRoot(n, logRootDist, insideChart)
        sampled(sampledi) = sampleRecursivelyFromTable(ij, 0, n, samplingChart)
        sampledi += 1
      }
      sampled
    }
  }

  def sampleRoot(
    n: Int,
    logRootDist: IndirectSparseVec[Double],
    insideChart: PcfgInsideChartI): Cat = {
    val rootCell = insideChart(0, n)
    val roots = rootCell.activeKeysSorted
    val rootLogInsideProbs = rootCell.activeValues
    val activeCount = rootCell.activeCount
    val rootLogProbs = new Array[Double](activeCount)
    var i = 0
    while (i < activeCount) {
      val ij = roots(i)
      val ijP = rootLogInsideProbs(i)
      rootLogProbs(i) = logRootDist(ij) + ijP
      i += 1
    }
    roots(FastMathUtil.logChoose(rootLogProbs, activeCount, rand))
  }

  private[this] def sampleRecursivelyWithoutTable(ij: Cat, i: Int, j: Int,
    guideChart: CfgGuideChartI,
    insideChart: PcfgInsideChartI,
    logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
    logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
    logTermDist: Array[Vec[Double]], //                                        t -> w -> p
    logPmixDist: Array[Array[Double]]) //                                      t -> p
    : CcgTreeI = {

    val entrySet = findEntrySet(guideChart(i, j), ij)
    val entriesLen = entrySet.length
    val logEntryProbs = makePD(i, j, ij, entrySet, entriesLen, insideChart, logBinyDist, logUnryDist, logTermDist, logPmixDist)
    val gce = entrySet(FastMathUtil.logChoose(logEntryProbs, entriesLen, rand))
    gce match {
      case BinaryGuideChartEntryI(k, ik, kj) =>
        CcgBinodeI(ij,
          sampleRecursivelyWithoutTable(ik, i, k, guideChart, insideChart, logBinyDist, logUnryDist, logTermDist, logPmixDist),
          sampleRecursivelyWithoutTable(kj, k, j, guideChart, insideChart, logBinyDist, logUnryDist, logTermDist, logPmixDist))
      case UnaryGuideChartEntryI(sub) =>
        CcgUnodeI(ij, sampleRecursivelyWithoutTable(sub, i, j, guideChart, insideChart, logBinyDist, logUnryDist, logTermDist, logPmixDist))
      case TermGuideChartEntryI(word) =>
        CcgLeafI(ij, word)
    }
  }

  private[this] def findEntrySet(cell: Array[(Int, Array[GuideChartEntryI])], ij: Cat): Array[GuideChartEntryI] = {
    val cellLen = cell.length
    var celli = 0
    while (celli < cellLen) {
      val a = cell(celli)
      if (a._1 == ij) return a._2
      celli += 1
    }
    sys.error("not found")
  }

  private[this] def makePD(
    i: Int, j: Int, ij: Cat,
    entries: Array[GuideChartEntryI],
    entriesLen: Int,
    insideChart: PcfgInsideChartI,
    logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
    logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
    logTermDist: Array[Vec[Double]], //                                        t -> w -> p
    logPmixDist: Array[Array[Double]]) //                                      t -> p
    = {
    var logEntryProbs = new Array[Double](entriesLen)
    var entriesi = 0
    while (entriesi < entriesLen) {
      entries(entriesi) match {
        case BinaryGuideChartEntryI(k, ik, kj) =>
          //if (logBinyDist(ij)(ik)(kj).isInfinite) println(f"oifdjiogfdj    logBinyDist(ij=${catIndexer.obj(ij)})(ik=${catIndexer.obj(ik)})(kj=${catIndexer.obj(kj)}) = ${logBinyDist(ij)(ik)(kj)}")
          //if (logPmixDist(ij)(0).isInfinite) println(f"oifdjiogfdj    logPmixDist(ij=${catIndexer.obj(ij)})(0) = ${logPmixDist(ij)(0)}")
          //if (insideChart(i, k)(ik).isInfinite) println(f"oifdjiogfdj    insideChart(i=$i, k=$k)(ik=${catIndexer.obj(ik)}) = ${insideChart(i, k)(ik)}")
          //if (insideChart(k, j)(kj).isInfinite) println(f"oifdjiogfdj    insideChart(k=$k, j=$j)(kj=${catIndexer.obj(kj)}) = ${insideChart(k, j)(kj)}")

          val logProdP = logBinyDist(ij)(ik)(kj) + logPmixDist(ij)(0)
          val ikP = insideChart(i, k)(ik)
          val kjP = insideChart(k, j)(kj)
          //println(f"sampleEntry::  logBinyDist(ij)(ik)(kj) + binyMixProb = logBinyDist(${catIndexer.obj(ij)} -> [${catIndexer.obj(ik)}, ${catIndexer.obj(kj)}]) + binyMixProb = ${logBinyDist(ij)(ik)(kj).toDouble}%.2f + ${mixDist(ij)._1.toDouble}%.2f = ${logProdP.toDouble}%.2f")
          logEntryProbs(entriesi) = logProdP + ikP + kjP

        case UnaryGuideChartEntryI(sub) =>
          //if (logUnryDist(ij)(sub).isInfinite) println(f"oifdjiogfdj    logUnryDist(ij=${catIndexer.obj(ij)})(ik=${catIndexer.obj(sub)}) = ${logUnryDist(ij)(sub)}")
          //if (logPmixDist(ij)(1).isInfinite) println(f"oifdjiogfdj    logPmixDist(ij=${catIndexer.obj(ij)})(1) = ${logPmixDist(ij)(1)}")
          //if (insideChart(i, j)(ij).isInfinite) println(f"oifdjiogfdj    insideChart(i=$i, j=$j)(ij=${catIndexer.obj(ij)}) = ${insideChart(i, j)(ij)}")

          val logProdP = logUnryDist(ij)(sub) + logPmixDist(ij)(1)
          val subP = insideChart(i, j)(sub)
          //println(f"sampleEntry::  logUnryDist(ij)(sub)    + unryMixProb = logUnryDist(${catIndexer.obj(ij)} -> [${catIndexer.obj(sub)}]) + unryMixProb = ${logUnryDist(ij)(sub).toDouble}%.2f + ${mixDist(ij)._2.toDouble}%.2f = ${logProdP.toDouble}%.2f")
          logEntryProbs(entriesi) = logProdP + subP

        case TermGuideChartEntryI(word) =>
          //if (logTermDist(ij)(word).isInfinite) println(f"oifdjiogfdj    logTermDist(ij=${catIndexer.obj(ij)})(word=${wordIndexer.obj(word)}) = ${logTermDist(ij)(word)}")
          //if (logPmixDist(ij)(2).isInfinite) println(f"oifdjiogfdj    logPmixDist(ij=${catIndexer.obj(ij)})(2) = ${logPmixDist(ij)(2)}")

          val logProdP = logTermDist(ij)(word) + logPmixDist(ij)(2)
          //println(f"sampleEntry::  logTermDist(ij)(word)   + termMixProb = logTermDist(${catIndexer.obj(ij)} -> ${wordIndexer.obj(word)}) + termMixProb = ${logTermDist(ij)(word).toDouble}%.2f + ${mixDist(ij)._3.toDouble}%.2f = ${logProdP.toDouble}%.2f")
          logEntryProbs(entriesi) = logProdP
      }
      entriesi += 1
    }
    logEntryProbs
  }

  private[this] def buildSamplingChart(
    guideChart: CfgGuideChartI,
    insideChart: PcfgInsideChartI,
    logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
    logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
    logTermDist: Array[Vec[Double]], //                                        t -> w -> p
    logPmixDist: Array[Array[Double]]) //                                      t -> p
    = {
    Chart.tabulate(guideChart.length) { (i, j) =>
      val cell = guideChart(i, j).sortBy(_._1)
      val cellLen = cell.length
      val sparseKeys = new Array[Int](cellLen)
      val sparseVals = new Array[(Array[GuideChartEntryI], Array[Double])](cellLen)
      var celli = 0
      while (celli < cellLen) {
        val (t, entries) = cell(celli)
        val entriesLen = entries.length
        val logEntryProbs = makePD(i, j, t, entries, entriesLen, insideChart, logBinyDist, logUnryDist, logTermDist, logPmixDist)
        sparseKeys(celli) = t
        sparseVals(celli) = (entries, logEntryProbs)
        celli += 1
      }
      new IndirectSparseVec(sparseKeys, sparseVals, cellLen, cellLen)
    }
  }

  private[this] def sampleRecursivelyFromTable(ij: Cat, i: Int, j: Int,
    samplingChart: Chart[IndirectSparseVec[(Array[GuideChartEntryI], Array[Double])]]): CcgTreeI = {

    val (entries, logEntryProbs) = samplingChart(i, j)(ij)
    val gce = entries(FastMathUtil.logChoose(logEntryProbs, entries.length, rand))
    gce match {
      case BinaryGuideChartEntryI(k, ik, kj) =>
        CcgBinodeI(ij,
          sampleRecursivelyFromTable(ik, i, k, samplingChart),
          sampleRecursivelyFromTable(kj, k, j, samplingChart))
      case UnaryGuideChartEntryI(subCat) =>
        CcgUnodeI(ij, sampleRecursivelyFromTable(subCat, i, j, samplingChart))
      case TermGuideChartEntryI(word) =>
        CcgLeafI(ij, word)
    }
  }
}

