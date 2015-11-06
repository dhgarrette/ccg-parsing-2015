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

trait SupParserTrainer {
  def train(trees: Vector[CcgTree]): GuideChartParser
}

trait SupKBestParserTrainer extends SupParserTrainer {
  def train(trees: Vector[CcgTree]): KBestGuideChartParser
}

class UnsmoothedSupPcfgTrainer(
  productionFinder: PcfgProductionCounter,
  pcfgParserInstantiater: PcfgParserInstantiater)
  extends SupParserTrainer {

  def train(trees: Vector[CcgTree]): KBestGuideChartParser = {
    val sampledRootCounts = trees.map(productionFinder.rootCounts).fold(Map.empty[Cat, Double])(_ |+| _).mapVals(LogDouble(_))
    val sampledProdCounts = trees.map(productionFinder.prodCounts).fold(Map.empty[Cat, Map[Prod, Double]])(_ |+| _).mapVals(_.mapVals(LogDouble(_)))

    val rootDist = new SimpleLogProbabilityDistribution[Cat](sampledRootCounts)
    val prodDist = new SimpleConditionalLogProbabilityDistribution[Cat, Prod](sampledProdCounts.mapVals(new SimpleLogProbabilityDistribution(_)))

    pcfgParserInstantiater(rootDist, prodDist)
  }

}

class AlphaBetaSupPcfgTrainer(
  priorRootDist: LogProbabilityDistribution[Cat],
  priorBinyDist: ConditionalLogProbabilityDistribution[Cat, BinaryProd],
  priorUnryDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd],
  priorTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
  alphaRoot: Double, alphaProd: Double,
  priorBinyProdMix: Double, priorUnryProdMix: Double, priorTermProdMix: Double,
  productionFinder: PcfgProductionCounter,
  pcfgParserInstantiater: PcfgParserInstantiater)
  extends SupParserTrainer {

  def train(trees: Vector[CcgTree]) = {

    val priorProdDist = new ICPDU(priorBinyDist, priorUnryDist, priorTermDist,
      Map().withDefaultValue((LogDouble(priorBinyProdMix), LogDouble(priorUnryProdMix), LogDouble(priorTermProdMix))))

    val treeRootCounts = trees.map(productionFinder.rootCounts /* */ ).fold(Map.empty[Cat, Double])(_ |+| _).mapVals(LogDouble(_))
    val treeProdCounts = trees.map(productionFinder.prodCounts /* */ ).fold(Map.empty[Cat, Map[Prod, Double]])(_ |+| _).mapVals(_.mapVals(LogDouble(_)))

    val allRootSet = treeRootCounts.keySet
    val allProdSet = treeProdCounts.mapValues(_.keySet).values.flatten.toSet

    val rootDist = if (alphaRoot.isInfinite) priorRootDist else new AlphaBetaLogProbabilityDistribution[Cat /*                 */ ](treeRootCounts, LogDouble(alphaRoot), priorRootDist, Some(allRootSet))
    val prodDist = if (alphaProd.isInfinite) priorProdDist else new AlphaBetaConditionalLogProbabilityDistribution[Cat, Prod](treeProdCounts.mapt { (a, bCounts) => a -> new AlphaBetaLogProbabilityDistribution[Prod](bCounts, LogDouble(alphaProd), new ConditionalWrappingLogProbabilityDistribution(a, priorProdDist)) }, LogDouble(alphaProd), priorProdDist, Some(allProdSet))

    pcfgParserInstantiater(rootDist, prodDist)
  }

}

//trait SupervisedPcfgParserTrainer extends SupervisedParserTrainer {
//  def doTrain(rootCounts: Map[Cat, LogDouble], prodCounts: Map[Cat, Map[Prod, LogDouble]], initialTagdict: TagDictionary[Cat]): OldToRemovePcfgParser
//  def make(rootDist: LogProbabilityDistribution[Cat], prodDist: ConditionalLogProbabilityDistribution[Cat, Prod], tagdict: TagDictionary[Cat], guideChartBuilder: OldToRemoveCfgGuideChartBuilder) = new OldToRemovePcfgParser(rootDist, prodDist, tagdict, guideChartBuilder)
//}
//
//class UnsmoothedPcfgParserTrainer(
//  guideChartBuilder: OldToRemoveCfgGuideChartBuilder)
//  extends SupervisedPcfgParserTrainer {
//
//  override def train(trees: Vector[CcgTree], initialTagdict: TagDictionary[Cat]): OldToRemovePcfgParser = {
//    val rootCounts = trees.map(SupervisedPcfgParserTrainer.getRootCount).reduce(_ |+| _)
//    val prodCounts = trees.map(SupervisedPcfgParserTrainer.getProdCounts).reduce(_ |+| _)
//    doTrain(rootCounts.mapVals(LogDouble(_)), prodCounts.mapVals(_.mapVals(LogDouble(_))), initialTagdict)
//  }
//
//  override def doTrain(rootCounts: Map[Cat, LogDouble], prodCounts: Map[Cat, Map[Prod, LogDouble]], initialTagdict: TagDictionary[Cat]): OldToRemovePcfgParser = {
//    val allKnownSupertags = initialTagdict.allTags | prodCounts.ungroup.collect { case (supertag, (TermProd(_), _)) => supertag }.toSet
//    val tagdict = initialTagdict
//      .withWords(prodCounts.values.flatten.collect { case (TermProd(word), _) => word }.toSet)
//      //.withTags(nontermProdCounts.keySet | termProdCounts.keySet | rootCounts.keySet | nontermProdCounts.values.flatten.flatMap { case (NontermProd(a, b), _) => Vector(a, b) }.toSet)
//      .withTags(allKnownSupertags)
//    new OldToRemovePcfgParser(
//      new SimpleLogProbabilityDistribution(rootCounts),
//      new SimpleConditionalLogProbabilityDistribution(prodCounts.mapVals(new SimpleLogProbabilityDistribution(_))),
//      tagdict,
//      guideChartBuilder)
//  }
//}
//
///**
// * TODO: IDEA - bias non-terms toward typically-non-term cats, and terms toward typically-term cats
// */
//class AddLambdaSmoothedPcfgParserTrainer(
//  lambda: Double,
//  guideChartBuilder: OldToRemoveCfgGuideChartBuilder)
//  extends SupervisedPcfgParserTrainer {
//
//  override def train(trees: Vector[CcgTree], initialTagdict: TagDictionary[Cat]): OldToRemovePcfgParser = {
//    val rootCounts = trees.map(SupervisedPcfgParserTrainer.getRootCount).reduce(_ |+| _)
//    val prodCounts = trees.map(SupervisedPcfgParserTrainer.getProdCounts).reduce(_ |+| _)
//    doTrain(rootCounts.mapVals(LogDouble(_)), prodCounts.mapVals(_.mapVals(LogDouble(_))), initialTagdict)
//  }
//
//  override def doTrain(rootCounts: Map[Cat, LogDouble], prodCounts: Map[Cat, Map[Prod, LogDouble]], initialTagdict: TagDictionary[Cat]): OldToRemovePcfgParser = {
//    val allKnownSupertags = initialTagdict.allTags | prodCounts.ungroup.collect { case (supertag, (TermProd(_), _)) => supertag }.toSet
//    val allKnownCats = rootCounts.keySet | prodCounts.ungroup.collect { case (a, (NontermProd(b, c), _)) => Vector(a, b, c) }.flatten.toSet | allKnownSupertags
//    val tagdict = initialTagdict
//      .withWords(prodCounts.values.flatten.collect { case (TermProd(word), _) => word }.toSet)
//      //.withTags(allKnownCats)
//      .withTags(allKnownSupertags)
//
//    val allKnownProds = Some(tagdict.allWords.map(TermProd(_)) ++ prodCounts.values.flatten.map(_._1).toSet)
//
//    new OldToRemovePcfgParser(
//      rootDist = new LaplaceLogProbabilityDistribution(rootCounts, None, None, LogDouble(lambda)),
//      prodDist = new SimpleConditionalLogProbabilityDistribution(
//        prodCounts.mapt((c, cCounts) => c -> new LaplaceLogProbabilityDistribution(cCounts, allKnownProds, /*Some[Set[Prod]]((tagdict.entries.keySet -- tagdict.knownWordsForTag.getOrElse(c, Set.empty)).map(TermProd(_))) */ None, LogDouble(lambda))),
//        new LaplaceLogProbabilityDistribution(Map(), allKnownProds, /*TODO: Some[Set[Prod]](tagdict.entries.keySet.map(TermProd(_)))*/ None, LogDouble(lambda))),
//      //        nontermDist = new SimpleConditionalLogProbabilityDistribution(
//      //          nontermProdCounts.mapVals(new LaplaceLogProbabilityDistribution(_, allKnownNtProds, None, lambda)),
//      //          new LaplaceLogProbabilityDistribution(Map(), allKnownNtProds, None, lambda)),
//      //        termDist = new SimpleConditionalLogProbabilityDistribution(
//      //          termProdCounts.mapt((c, cCounts) => c -> new LaplaceLogProbabilityDistribution(cCounts, allTermProds, /*Some((tagdict.entries.keySet -- tagdict.knownWordsForTag.getOrElse(c, Set.empty)).map(TermProd(_)))*/None, lambda)),
//      //          new LaplaceLogProbabilityDistribution(Map(), allTermProds, /*Some(tagdict.entries.keySet.map(TermProd(_)))*/ None, lambda)),
//      tagdict,
//      guideChartBuilder)
//  }
//
//  override def toString() = f"AddLambdaSmoothedPcfgParserTrainer(lambda=$lambda, ${guideChartBuilder})"
//}
//
//object SupervisedPcfgParserTrainer {
//
//  final def getRootCount(tree: CcgTree): Map[Cat, Double] = {
//    Map(tree.cat -> 1.0)
//  }
//
//  final def getProductions(tree: CcgTree): Vector[(Cat, Prod)] = {
//    tree match {
//      case CcgNode(cat, left, right) =>
//        val prod = (cat, NontermProd(left.cat, right.cat))
//        prod +: (getProductions(left) ++ getProductions(right))
//      case CcgLeaf(cat, word, _) =>
//        Vector((cat, TermProd(word)))
//    }
//  }
//
//  final def getProdCounts(tree: CcgTree) = {
//    getProductions(tree).groupByKey.mapVals(_.counts.mapVals(_.toDouble))
//  }
//
//  final def getNontermProductions(tree: CcgTree): Vector[(Cat, NontermProd)] = {
//    tree match {
//      case CcgNode(cat, left, right) =>
//        val prod = (cat, NontermProd(left.cat, right.cat))
//        prod +: (getNontermProductions(left) ++ getNontermProductions(right))
//      case CcgLeaf(cat, word, _) =>
//        Vector()
//    }
//  }
//
//  final def getNontermProdCounts(tree: CcgTree) = {
//    getNontermProductions(tree).groupByKey.mapVals(_.counts.mapVals(_.toDouble))
//  }
//
//  final def getTermProductions(tree: CcgTree): Vector[(Cat, TermProd)] = {
//    tree match {
//      case CcgNode(cat, left, right) =>
//        (getTermProductions(left) ++ getTermProductions(right))
//      case CcgLeaf(cat, word, _) =>
//        Vector((cat, TermProd(word)))
//    }
//  }
//
//  final def getTermProdCounts(tree: CcgTree) = {
//    getTermProductions(tree).groupByKey.mapVals(_.counts.mapVals(_.toDouble))
//  }
//
//  //  final def getNonterminalProductions(tree: CcgTree): Vector[(Cat, NontermProd)] = {
//  //    tree match {
//  //      case CcgNode(cat, left, right) =>
//  //        val prod = (cat, NontermProd(left.cat, right.cat))
//  //        prod +: (getNonterminalProductions(left) ++ getNonterminalProductions(right))
//  //      case CcgLeaf(cat, word, index) =>
//  //        Vector()
//  //    }
//  //  }
//  //
//  //  final def getNonterminalProductionCounts(tree: CcgTree) = {
//  //    getNonterminalProductions(tree).groupByKey.mapVals(_.counts.mapVals(_.toDouble))
//  //  }
//  //
//  //  final def getTerminalProductions(tree: CcgTree): Vector[(Cat, TermProd)] = {
//  //    tree match {
//  //      case CcgNode(cat, left, right) =>
//  //        val prod = (cat, NontermProd(left.cat, right.cat))
//  //        (getTerminalProductions(left) ++ getTerminalProductions(right))
//  //      case CcgLeaf(cat, word, index) =>
//  //        Vector((cat, TermProd(word)))
//  //    }
//  //  }
//  //
//  //  final def getTerminalProductionCounts(tree: CcgTree) = {
//  //    getTerminalProductions(tree).groupByKey.mapVals(_.counts.mapVals(_.toDouble))
//  //  }
//
//}
