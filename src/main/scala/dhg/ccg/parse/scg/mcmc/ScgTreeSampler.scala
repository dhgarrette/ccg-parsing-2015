package dhg.ccg.parse.scg.mcmc

import dhg.util._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.prob._
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }

trait ScgTreeSampler {

  final def sample(
    guideChart: CfgGuideChart,
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): CcgTree = samples(guideChart, rootDist, prodDist, k = 1).only

  def samples(
    guideChart: CfgGuideChart,
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    k: Int): Vector[CcgTree]

}

//class SimplePcfgTreeSampler(
//  scgInsideChartBuilder: ScgInsideChartBuilder)
//  extends ScgTreeSampler {
//
//  def samples(
//    guideChart: CfgGuideChart,
//    rootDist: LogProbabilityDistribution[Cat],
//    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
//    k: Int): Vector[CcgTree] = {
//
//    val n = guideChart.length
//    val insideChart = pcfgInsideChartBuilder.buildInsideChart(guideChart, prodDist)
//    val weightedRootProds = insideChart(0, n).mapt((ij, ijP) => ij -> (rootDist(ij) * ijP))
//    if (k == 1) {
//      val ij = new SimpleLogProbabilityDistribution(weightedRootProds).sample()
//      Vector(sampleRecursivelyWithoutTable(ij, 0, n, guideChart, insideChart, prodDist))
//    }
//    else {
//      val table = buildSamplingChart(guideChart, insideChart, prodDist)
//      Vector.fill(k) {
//        val ij = new SimpleLogProbabilityDistribution(weightedRootProds).sample()
//        sampleRecursivelyFromTable(ij, 0, n, table)
//      }
//    }
//  }
//
//  private[this] def sampleRecursivelyWithoutTable(ij: Cat, i: Int, j: Int,
//    guideChart: CfgGuideChart,
//    insideChart: PcfgInsideChart,
//    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): CcgTree = {
//
//    val gce = makePD(i, j, ij, guideChart(i, j)(ij), insideChart, prodDist).sample()
//    gce match {
//      case BinaryGuideChartEntry(k, BinaryProd(ik, kj)) =>
//        CcgBinode(ij,
//          sampleRecursivelyWithoutTable(ik, i, k, guideChart, insideChart, prodDist),
//          sampleRecursivelyWithoutTable(kj, k, j, guideChart, insideChart, prodDist))
//      case UnaryGuideChartEntry(UnaryProd(subCat)) =>
//        CcgUnode(ij, sampleRecursivelyWithoutTable(subCat, i, j, guideChart, insideChart, prodDist))
//      case TermGuideChartEntry(TermProd(word)) =>
//        CcgLeaf(ij, word)
//    }
//  }
//
//  private[this] def makePD(
//    i: Int, j: Int, ij: Cat,
//    entries: Set[GuideChartEntry],
//    insideChart: PcfgInsideChart,
//    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]) = {
//
//    val weightedProds: Map[GuideChartEntry, LogDouble] = entries.flatMap {
//      case gce @ BinaryGuideChartEntry(k, prod @ BinaryProd(ik, kj)) =>
//        val prodP = prodDist(prod, ij)
//        for {
//          ikP <- insideChart(i)(k).get(ik) // skip entries that weren't added to the chart
//          kjP <- insideChart(k)(j).get(kj)
//        } yield {
//          gce -> (prodP * ikP * kjP)
//        }
//
//      case gce @ UnaryGuideChartEntry(prod @ UnaryProd(subCat)) =>
//        val prodP = prodDist(prod, ij)
//        for (subP <- insideChart(i)(j).get(subCat)) yield { // skip entries that weren't added to the chart
//          gce -> (prodP * subP)
//        }
//
//      case gce @ TermGuideChartEntry(prod @ TermProd(word)) =>
//        val prodP = prodDist(prod, ij)
//        Some(gce -> prodP)
//    }.toMap
//    new SimpleLogProbabilityDistribution(weightedProds)
//  }
//
//  private[this] def buildSamplingChart(
//    guideChart: CfgGuideChart,
//    insideChart: PcfgInsideChart,
//    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): Vector[Vector[Map[Cat, LogProbabilityDistribution[GuideChartEntry]]]] = {
//    guideChart.matrix.zipWithIndex.mapt { (row, i) =>
//      row.zipWithIndex.mapt { (col, j) =>
//        col.mapt { (ij, entries) => ij -> makePD(i, j, ij, entries, insideChart, prodDist) }
//      }
//    }
//  }
//
//  private[this] def sampleRecursivelyFromTable(ij: Cat, i: Int, j: Int,
//    table: Vector[Vector[Map[Cat, LogProbabilityDistribution[GuideChartEntry]]]]): CcgTree = {
//
//    val gce = table(i)(j)(ij).sample()
//    gce match {
//      case BinaryGuideChartEntry(k, BinaryProd(ik, kj)) =>
//        CcgBinode(ij,
//          sampleRecursivelyFromTable(ik, i, k, table),
//          sampleRecursivelyFromTable(kj, k, j, table))
//      case UnaryGuideChartEntry(UnaryProd(subCat)) =>
//        CcgUnode(ij, sampleRecursivelyFromTable(subCat, i, j, table))
//      case TermGuideChartEntry(TermProd(word)) =>
//        CcgLeaf(ij, word)
//    }
//  }
//
//}
