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

trait PcfgAlphaPriorMaker {

  def makeAll(guideCharts: Vector[CfgGuideChart], goldTrees: Vector[CcgTree],
    priorRootDist: LogProbabilityDistribution[Cat],
    priorBinyDist: ConditionalLogProbabilityDistribution[Cat, BinaryProd],
    priorUnryDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd],
    priorTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
    alphaRoot: Double, alphaBiny: Double, alphaUnry: Double, alphaTerm: Double): //
    (Map[Cat, LogDouble], Map[Cat, Map[BinaryProd, LogDouble]], Map[Cat, Map[UnaryProd, LogDouble]], Map[Cat, Map[TermProd, LogDouble]])

}

class TrainDataPcfgAlphaPriorMaker(
  productionFinder: PcfgProductionCounter,
  guideChartProdFinder: PcfgGuideChartProdFinder)
    extends PcfgAlphaPriorMaker {

  def makeAll(guideCharts: Vector[CfgGuideChart], goldTrees: Vector[CcgTree],
    priorRootDist: LogProbabilityDistribution[Cat],
    priorBinyDist: ConditionalLogProbabilityDistribution[Cat, BinaryProd],
    priorUnryDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd],
    priorTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
    alphaRoot: Double, alphaBiny: Double, alphaUnry: Double, alphaTerm: Double) = {

    val goldRootCounts = goldTrees.map(productionFinder.rootCounts).fold(Map.empty[Cat, Double])(_ |+| _)
    val goldBinyCounts = goldTrees.map(productionFinder.binyCounts).fold(Map.empty[Cat, Map[BinaryProd, Double]])(_ |+| _)
    val goldUnryCounts = goldTrees.map(productionFinder.unryCounts).fold(Map.empty[Cat, Map[UnaryProd, Double]])(_ |+| _)
    val goldTermCounts = goldTrees.map(productionFinder.termCounts).fold(Map.empty[Cat, Map[TermProd, Double]])(_ |+| _)

    val allRootSet = guideCharts.map(guideChartProdFinder.roots).fold(Set.empty)(_ |+| _) |+| goldRootCounts.keySet
    val allBinySet = guideCharts.map(guideChartProdFinder.binys).fold(Map.empty)(_ |+| _) |+| goldBinyCounts.mapValues(_.keySet)
    val allUnrySet = guideCharts.map(guideChartProdFinder.unrys).fold(Map.empty)(_ |+| _) |+| goldUnryCounts.mapValues(_.keySet)
    val allTermSet = guideCharts.map(guideChartProdFinder.terms).fold(Map.empty)(_ |+| _) |+| goldTermCounts.mapValues(_.keySet)

    val alphaPriorRootCounts = allRootSet.mapTo(root => LogDouble(alphaRoot) * priorRootDist(root) + LogDouble(goldRootCounts.getOrElse(root, 0.0))).toMap
    val alphaPriorBinyCounts = allBinySet.mapt((cat, binys) => cat -> binys.mapTo(prod => (LogDouble(alphaBiny) * priorBinyDist(prod, cat) + LogDouble(goldBinyCounts.get(cat).flatMap(_.get(prod)).getOrElse(0.0)))).toMap)
    val alphaPriorUnryCounts = allUnrySet.mapt((cat, unrys) => cat -> unrys.mapTo(prod => (LogDouble(alphaUnry) * priorUnryDist(prod, cat) + LogDouble(goldUnryCounts.get(cat).flatMap(_.get(prod)).getOrElse(0.0)))).toMap)
    val alphaPriorTermCounts = allTermSet.mapt((cat, terms) => cat -> terms.mapTo(prod => (LogDouble(alphaTerm) * priorTermDist(prod, cat) + LogDouble(goldTermCounts.get(cat).flatMap(_.get(prod)).getOrElse(0.0)))).toMap)

    (alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts)
  }
}
