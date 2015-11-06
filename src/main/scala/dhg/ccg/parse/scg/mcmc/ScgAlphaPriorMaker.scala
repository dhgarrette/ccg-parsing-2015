package dhg.ccg.parse.scg.mcmc

import dhg.util._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.prob._
import scalaz._
import Scalaz._
import dhg.ccg.tagdict.StartEndTags

trait ScgAlphaPriorMaker {

  def makeAll(guideCharts: Vector[CfgGuideChart], goldLabeledSentences: Vector[CcgTree],
    priorRootDist: LogProbabilityDistribution[Cat],
    priorBinyDist: ConditionalLogProbabilityDistribution[Cat, BinaryProd],
    priorUnryDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd],
    priorTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
    priorLctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    priorRctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    alphaRoot: Double, alphaBiny: Double, alphaUnry: Double, alphaTerm: Double, alphaLctx: Double, alphaRctx: Double)(se: StartEndTags[Cat]): //
    (Map[Cat, LogDouble], Map[Cat, Map[BinaryProd, LogDouble]], Map[Cat, Map[UnaryProd, LogDouble]], Map[Cat, Map[TermProd, LogDouble]], Map[Cat, Map[Cat, LogDouble]], Map[Cat, Map[Cat, LogDouble]])

}

class TrainDataScgAlphaPriorMaker(
  productionFinder: ScgProductionFinder,
  guideChartProdFinder: ScgGuideChartProdFinder)
  extends ScgAlphaPriorMaker {

  def makeAll(guideCharts: Vector[CfgGuideChart], goldLabeledSentences: Vector[CcgTree],
    priorRootDist: LogProbabilityDistribution[Cat],
    priorBinyDist: ConditionalLogProbabilityDistribution[Cat, BinaryProd],
    priorUnryDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd],
    priorTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
    priorLctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    priorRctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    alphaRoot: Double, alphaBiny: Double, alphaUnry: Double, alphaTerm: Double, alphaLctx: Double, alphaRctx: Double)(se: StartEndTags[Cat]) = {

    val goldRootCounts = goldLabeledSentences.map(productionFinder.rootCounts /* */ ).fold(Map.empty[Cat, Double])(_ |+| _)
    val goldBinyCounts = goldLabeledSentences.map(productionFinder.binyCounts /* */ ).fold(Map.empty[Cat, Map[BinaryProd, Double]])(_ |+| _)
    val goldUnryCounts = goldLabeledSentences.map(productionFinder.unryCounts /* */ ).fold(Map.empty[Cat, Map[UnaryProd, Double]])(_ |+| _)
    val goldTermCounts = goldLabeledSentences.map(productionFinder.termCounts /* */ ).fold(Map.empty[Cat, Map[TermProd, Double]])(_ |+| _)
    val goldLctxCounts = goldLabeledSentences.map(productionFinder.lctxCounts(_)(se)).fold(Map.empty[Cat, Map[Cat, Double]])(_ |+| _)
    val goldRctxCounts = goldLabeledSentences.map(productionFinder.rctxCounts(_)(se)).fold(Map.empty[Cat, Map[Cat, Double]])(_ |+| _)

    val allRootSet = guideCharts.map(guideChartProdFinder.roots /* */ ).reduce(_ |+| _) |+| goldRootCounts.keySet
    val allBinySet = guideCharts.map(guideChartProdFinder.binys /* */ ).reduce(_ |+| _) |+| goldBinyCounts.mapValues(_.keySet)
    val allUnrySet = guideCharts.map(guideChartProdFinder.unrys /* */ ).reduce(_ |+| _) |+| goldUnryCounts.mapValues(_.keySet)
    val allTermSet = guideCharts.map(guideChartProdFinder.terms /* */ ).reduce(_ |+| _) |+| goldTermCounts.mapValues(_.keySet)
    val allLctxSet = guideCharts.map(guideChartProdFinder.lctxs(_)(se)).reduce(_ |+| _) |+| goldLctxCounts.mapValues(_.keySet)
    val allRctxSet = guideCharts.map(guideChartProdFinder.rctxs(_)(se)).reduce(_ |+| _) |+| goldRctxCounts.mapValues(_.keySet)

    val alphaPriorRootCounts = allRootSet.mapTo(root => LogDouble(alphaRoot) * priorRootDist(root) + LogDouble(goldRootCounts.getOrElse(root, 0.0))).toMap
    val alphaPriorBinyCounts = allBinySet.mapt((cat, binys) => cat -> binys.mapTo(prod => (LogDouble(alphaBiny) * priorBinyDist(prod, cat) + LogDouble(goldBinyCounts.get(cat).flatMap(_.get(prod)).getOrElse(0.0)))).toMap)
    val alphaPriorUnryCounts = allUnrySet.mapt((cat, unrys) => cat -> unrys.mapTo(prod => (LogDouble(alphaUnry) * priorUnryDist(prod, cat) + LogDouble(goldUnryCounts.get(cat).flatMap(_.get(prod)).getOrElse(0.0)))).toMap)
    val alphaPriorTermCounts = allTermSet.mapt((cat, terms) => cat -> terms.mapTo(prod => (LogDouble(alphaTerm) * priorTermDist(prod, cat) + LogDouble(goldTermCounts.get(cat).flatMap(_.get(prod)).getOrElse(0.0)))).toMap)
    val alphaPriorLctxCounts = allLctxSet.mapt((cat, lctxs) => cat -> lctxs.mapTo(lctx => (LogDouble(alphaLctx) * priorLctxDist(lctx, cat) + LogDouble(goldLctxCounts.get(cat).flatMap(_.get(lctx)).getOrElse(0.0)))).toMap)
    val alphaPriorRctxCounts = allRctxSet.mapt((cat, rctxs) => cat -> rctxs.mapTo(rctx => (LogDouble(alphaRctx) * priorRctxDist(rctx, cat) + LogDouble(goldRctxCounts.get(cat).flatMap(_.get(rctx)).getOrElse(0.0)))).toMap)

    (alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts, alphaPriorLctxCounts, alphaPriorRctxCounts)
  }
}

//class TrainDataNormalizingScgAlphaPriorMaker(
//  productionFinder: ScgProductionFinder,
//  guideChartProdFinder: ScgGuideChartProdFinder)
//  extends ScgAlphaPriorMaker {
//
//  def makeAll(guideCharts: Vector[CfgGuideChart], goldLabeledSentences: Vector[CcgTree],
//    priorRootDist: LogProbabilityDistribution[Cat],
//    priorBinyDist: ConditionalLogProbabilityDistribution[Cat, BinaryProd],
//    priorUnryDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd],
//    priorTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
//    priorLctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
//    priorRctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
//    alphaRoot: Double, alphaBiny: Double, alphaUnry: Double, alphaTerm: Double, alphaLctx: Double, alphaRctx: Double)(se: StartEndTags[Cat]) = {
//
//    val goldRootCounts = goldLabeledSentences.map(productionFinder.rootCounts /* */ ).fold(Map.empty[Cat, Double])(_ |+| _)
//    val goldBinyCounts = goldLabeledSentences.map(productionFinder.binyCounts /* */ ).fold(Map.empty[Cat, Map[BinaryProd, Double]])(_ |+| _)
//    val goldUnryCounts = goldLabeledSentences.map(productionFinder.unryCounts /* */ ).fold(Map.empty[Cat, Map[UnaryProd, Double]])(_ |+| _)
//    val goldTermCounts = goldLabeledSentences.map(productionFinder.termCounts /* */ ).fold(Map.empty[Cat, Map[TermProd, Double]])(_ |+| _)
//    val goldLctxCounts = goldLabeledSentences.map(productionFinder.lctxCounts(_)(se)).fold(Map.empty[Cat, Map[Cat, Double]])(_ |+| _)
//    val goldRctxCounts = goldLabeledSentences.map(productionFinder.rctxCounts(_)(se)).fold(Map.empty[Cat, Map[Cat, Double]])(_ |+| _)
//
//    val allRootSet = guideCharts.map(guideChartProdFinder.roots /* */ ).reduce(_ |+| _) |+| goldRootCounts.keySet
//    val allBinySet = guideCharts.map(guideChartProdFinder.binys /* */ ).reduce(_ |+| _) |+| goldBinyCounts.mapValues(_.keySet)
//    val allUnrySet = guideCharts.map(guideChartProdFinder.unrys /* */ ).reduce(_ |+| _) |+| goldUnryCounts.mapValues(_.keySet)
//    val allTermSet = guideCharts.map(guideChartProdFinder.terms /* */ ).reduce(_ |+| _) |+| goldTermCounts.mapValues(_.keySet)
//    val allLctxSet = guideCharts.map(guideChartProdFinder.lctxs(_)(se)).reduce(_ |+| _) |+| goldLctxCounts.mapValues(_.keySet)
//    val allRctxSet = guideCharts.map(guideChartProdFinder.rctxs(_)(se)).reduce(_ |+| _) |+| goldRctxCounts.mapValues(_.keySet)
//
//    val rootZ = allRootSet.sumBy(root => priorRootDist(root)); assert(rootZ.nonZero, f"rootZ=$rootZ; allRootSet=${allRootSet}") // normalization constants...
//    val binyZ = allBinySet.map { case (cat, prods) => cat -> prods.sumBy(prod => priorBinyDist(prod, cat)) }; for ((cat, z) <- binyZ) assert(z.nonZero, f"binyZ($cat)=$z ; allBinySet($cat)=${allBinySet(cat)}")
//    val unryZ = allUnrySet.map { case (cat, prods) => cat -> prods.sumBy(prod => priorUnryDist(prod, cat)) }; for ((cat, z) <- unryZ) assert(z.nonZero, f"unryZ($cat)=$z ; allUnrySet($cat)=${allUnrySet(cat)}")
//    val termZ = allTermSet.map { case (cat, prods) => cat -> prods.sumBy(prod => priorTermDist(prod, cat)) }; for ((cat, z) <- termZ) assert(z.nonZero, f"termZ($cat)=$z ; allTermSet($cat)=${allTermSet(cat)}")
//    val lctxZ = allLctxSet.map { case (cat, lctxs) => cat -> lctxs.sumBy(lctx => priorLctxDist(lctx, cat)) }; for ((cat, z) <- lctxZ) assert(z.nonZero, f"lctxZ($cat)=$z ; allLctxSet($cat)=${allLctxSet(cat)}")
//    val rctxZ = allRctxSet.map { case (cat, rctxs) => cat -> rctxs.sumBy(rctx => priorRctxDist(rctx, cat)) }; for ((cat, z) <- rctxZ) assert(z.nonZero, f"rctxZ($cat)=$z ; allRctxSet($cat)=${allRctxSet(cat)}")
//
//    val alphaPriorRootCounts = allRootSet.mapTo(root => LogDouble(alphaRoot) * priorRootDist(root) / rootZ + LogDouble(goldRootCounts.getOrElse(root, 0.0))).toMap
//    val alphaPriorBinyCounts = allBinySet.mapt((cat, binys) => cat -> binys.mapTo(prod => (LogDouble(alphaBiny) * priorBinyDist(prod, cat) / binyZ(cat) + LogDouble(goldBinyCounts.get(cat).flatMap(_.get(prod)).getOrElse(0.0)))).toMap)
//    val alphaPriorUnryCounts = allUnrySet.mapt((cat, unrys) => cat -> unrys.mapTo(prod => (LogDouble(alphaUnry) * priorUnryDist(prod, cat) / unryZ(cat) + LogDouble(goldUnryCounts.get(cat).flatMap(_.get(prod)).getOrElse(0.0)))).toMap)
//    val alphaPriorTermCounts = allTermSet.mapt((cat, terms) => cat -> terms.mapTo(prod => (LogDouble(alphaTerm) * priorTermDist(prod, cat) / termZ(cat) + LogDouble(goldTermCounts.get(cat).flatMap(_.get(prod)).getOrElse(0.0)))).toMap)
//    val alphaPriorLctxCounts = allLctxSet.mapt((cat, lctxs) => cat -> lctxs.mapTo(lctx => (LogDouble(alphaLctx) * priorLctxDist(lctx, cat) / lctxZ(cat) + LogDouble(goldLctxCounts.get(cat).flatMap(_.get(lctx)).getOrElse(0.0)))).toMap)
//    val alphaPriorRctxCounts = allRctxSet.mapt((cat, rctxs) => cat -> rctxs.mapTo(rctx => (LogDouble(alphaRctx) * priorRctxDist(rctx, cat) / rctxZ(cat) + LogDouble(goldRctxCounts.get(cat).flatMap(_.get(rctx)).getOrElse(0.0)))).toMap)
//
//    (alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts, alphaPriorLctxCounts, alphaPriorRctxCounts)
//  }
//
//}
