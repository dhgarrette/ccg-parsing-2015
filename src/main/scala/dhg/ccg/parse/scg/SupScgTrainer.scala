package dhg.ccg.parse.scg

import dhg.ccg.cat._
import dhg.ccg.prob._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.tagdict.TagDictionary
import dhg.util._
import scalaz._
import Scalaz._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.parse.scg.mcmc._
import org.apache.commons.math3.random.RandomGenerator
import dhg.ccg.math.Util._
import dhg.ccg.tagdict.StartEndTags
import dhg.ccg.tagdict.SimpleStartEndTags

class UnsmoothedSupScgTrainer(
  productionFinder: ScgProductionFinder,
  scgParserInstantiater: ScgParserInstantiater)(se: StartEndTags[Cat])
  extends SupParserTrainer {

  def train(trees: Vector[CcgTree]) = {
    val sampledRootCounts = trees.map(productionFinder.rootCounts).fold(Map.empty[Cat, Double])(_ |+| _).mapVals(LogDouble(_))
    val sampledProdCounts = trees.map(productionFinder.prodCounts).fold(Map.empty[Cat, Map[Prod, Double]])(_ |+| _).mapVals(_.mapVals(LogDouble(_)))
    val sampledLctxCounts = trees.map(productionFinder.lctxCounts(_)(se)).fold(Map.empty[Cat, Map[Cat, Double]])(_ |+| _).mapVals(_.mapVals(LogDouble(_)))
    val sampledRctxCounts = trees.map(productionFinder.rctxCounts(_)(se)).fold(Map.empty[Cat, Map[Cat, Double]])(_ |+| _).mapVals(_.mapVals(LogDouble(_)))

    val rootDist = new SimpleLogProbabilityDistribution[Cat](sampledRootCounts)
    val prodDist = new SimpleConditionalLogProbabilityDistribution[Cat, Prod](sampledProdCounts.mapVals(new SimpleLogProbabilityDistribution(_)))
    val lctxDist = new SimpleConditionalLogProbabilityDistribution[Cat, Cat /* */ ](sampledLctxCounts.mapVals(new SimpleLogProbabilityDistribution(_)))
    val rctxDist = new SimpleConditionalLogProbabilityDistribution[Cat, Cat /* */ ](sampledRctxCounts.mapVals(new SimpleLogProbabilityDistribution(_)))

    scgParserInstantiater(rootDist, prodDist, lctxDist, rctxDist)(se)
  }

}

class AlphaBetaSupScgTrainer(
  priorRootDist: LogProbabilityDistribution[Cat],
  priorBinyDist: ConditionalLogProbabilityDistribution[Cat, BinaryProd],
  priorUnryDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd],
  priorTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
  priorLctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
  priorRctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
  alphaRoot: Double, alphaProd: Double, alphaLctx: Double, alphaRctx: Double,
  priorBinyProdMix: Double, priorUnryProdMix: Double, priorTermProdMix: Double,
  productionFinder: ScgProductionFinder,
  scgParserInstantiater: ScgParserInstantiater)(se: StartEndTags[Cat])
  extends SupParserTrainer {

  def train(trees: Vector[CcgTree]) = {

    val priorProdDist = new ICPDU(priorBinyDist, priorUnryDist, priorTermDist,
      Map().withDefaultValue((LogDouble(priorBinyProdMix), LogDouble(priorUnryProdMix), LogDouble(priorTermProdMix))))

    val treeRootCounts = trees.map(productionFinder.rootCounts /* */ ).fold(Map.empty[Cat, Double])(_ |+| _).mapVals(LogDouble(_))
    val treeProdCounts = trees.map(productionFinder.prodCounts /* */ ).fold(Map.empty[Cat, Map[Prod, Double]])(_ |+| _).mapVals(_.mapVals(LogDouble(_)))
    val treeLctxCounts = trees.map(productionFinder.lctxCounts(_)(se)).fold(Map.empty[Cat, Map[Cat, Double]])(_ |+| _).mapVals(_.mapVals(LogDouble(_)))
    val treeRctxCounts = trees.map(productionFinder.rctxCounts(_)(se)).fold(Map.empty[Cat, Map[Cat, Double]])(_ |+| _).mapVals(_.mapVals(LogDouble(_)))

    val allRootSet = treeRootCounts.keySet
    val allProdSet = treeProdCounts.mapValues(_.keySet).values.flatten.toSet
    val allLctxSet = treeLctxCounts.mapValues(_.keySet).values.flatten.toSet
    val allRctxSet = treeRctxCounts.mapValues(_.keySet).values.flatten.toSet

    val rootDist = if (alphaRoot.isInfinite) priorRootDist else new AlphaBetaLogProbabilityDistribution[Cat /*                 */ ](treeRootCounts, LogDouble(alphaRoot), priorRootDist, Some(allRootSet))
    val prodDist = if (alphaProd.isInfinite) priorProdDist else new AlphaBetaConditionalLogProbabilityDistribution[Cat, Prod](treeProdCounts.mapt { (a, bCounts) => a -> new AlphaBetaLogProbabilityDistribution[Prod](bCounts, LogDouble(alphaProd), new ConditionalWrappingLogProbabilityDistribution(a, priorProdDist)) }, LogDouble(alphaProd), priorProdDist, Some(allProdSet))
    val lctxDist = if (alphaLctx.isInfinite) priorLctxDist else new AlphaBetaConditionalLogProbabilityDistribution[Cat, Cat /* */ ](treeLctxCounts.mapt { (a, bCounts) => a -> new AlphaBetaLogProbabilityDistribution[Cat /* */ ](bCounts, LogDouble(alphaLctx), new ConditionalWrappingLogProbabilityDistribution(a, priorLctxDist)) }, LogDouble(alphaLctx), priorLctxDist, Some(allLctxSet))
    val rctxDist = if (alphaRctx.isInfinite) priorRctxDist else new AlphaBetaConditionalLogProbabilityDistribution[Cat, Cat /* */ ](treeRctxCounts.mapt { (a, bCounts) => a -> new AlphaBetaLogProbabilityDistribution[Cat /* */ ](bCounts, LogDouble(alphaRctx), new ConditionalWrappingLogProbabilityDistribution(a, priorRctxDist)) }, LogDouble(alphaRctx), priorRctxDist, Some(allRctxSet))

    scgParserInstantiater(rootDist, prodDist, lctxDist, rctxDist)(se)
  }

}

//import dhg.util._
//import scalaz._
//import scalaz.Scalaz._
//import dhg.ccg.prob._
//import dhg.ccg.cat._
//import dhg.ccg.parse._
//import dhg.ccg.tagdict.TagDictionary
//import dhg.ccg.parse.pcfg._
//
//trait SupervisedScgParserTrainer extends SupervisedParserTrainer {
//  def doTrain(rootCounts: Map[Cat, LogDouble], prodCounts: Map[Cat, Map[Prod, LogDouble]], leftContextCounts: Map[Cat, Map[Cat, LogDouble]], rightContextCounts: Map[Cat, Map[Cat, LogDouble]], initialTagdict: TagDictionary[Cat]): Parser
//}
//
//class UnsmoothedSupervisedScgTrainer(
//  parseCountCutoff: Long,
//  numSamples: Int,
//  guideChartBuilder: OldToRemoveCfgGuideChartBuilder,
//  noContext: Boolean = false)
//  extends SupervisedScgParserTrainer {
//
//  override def train(trees: Vector[CcgTree], initialTagdict: TagDictionary[Cat]): Parser = {
//    val rootCounts = trees.map(SupervisedPcfgParserTrainer.getRootCount).reduce(_ |+| _)
//    val prodCounts = trees.map(SupervisedPcfgParserTrainer.getProdCounts).reduce(_ |+| _)
//    val leftContextCounts = trees.map(SupervisedScgParserTrainer.getLeftContextCounts(_, initialTagdict)).reduce(_ |+| _)
//    val rightContextCounts = trees.map(SupervisedScgParserTrainer.getRightContextCounts(_, initialTagdict)).reduce(_ |+| _)
//    doTrain(rootCounts.mapVals(LogDouble(_)), prodCounts.mapVals(_.mapVals(LogDouble(_))), leftContextCounts.mapVals(_.mapVals(LogDouble(_))), rightContextCounts.mapVals(_.mapVals(LogDouble(_))), initialTagdict)
//  }
//
//  override def doTrain(rootCounts: Map[Cat, LogDouble], prodCounts: Map[Cat, Map[Prod, LogDouble]], leftContextCounts: Map[Cat, Map[Cat, LogDouble]], rightContextCounts: Map[Cat, Map[Cat, LogDouble]], initialTagdict: TagDictionary[Cat]): Parser = {
//    val allKnownSupertags = initialTagdict.allTags | prodCounts.ungroup.collect { case (supertag, (TermProd(_), _)) => supertag }.toSet | leftContextCounts.values.flatten.map(_._1).toSet | rightContextCounts.values.flatten.map(_._1).toSet
//    val tagdict = initialTagdict
//      .withWords(prodCounts.values.flatten.collect { case (TermProd(word), _) => word }.toSet)
//      //.withTags(allKnownCats)
//      .withTags(allKnownSupertags)
//    val rootPd = new SimpleLogProbabilityDistribution(rootCounts)
//    val prodCpd = new SimpleConditionalLogProbabilityDistribution(prodCounts.mapVals(new SimpleLogProbabilityDistribution(_)))
//    val lctxCpd = new SimpleConditionalLogProbabilityDistribution(leftContextCounts.mapVals(new SimpleLogProbabilityDistribution(_)))
//    val rctxCpd = new SimpleConditionalLogProbabilityDistribution(rightContextCounts.mapVals(new SimpleLogProbabilityDistribution(_)))
//    //    new ExactScgParser(rootPd, prodCpd, lctxCpd, rctxCpd,
//    //      new OldToRemovePcfgParser(rootPd, prodCpd, tagdict, rules),
//    //      tagdict,
//    //      parseCountCutoff,
//    //      rules,
//    //      maxIterations = 50,
//    //      noContext = noContext)
//    new StupidScgParser(rootPd, prodCpd, lctxCpd, rctxCpd,
//      tagdict,
//      parseCountCutoff,
//      numSamples,
//      guideChartBuilder,
//      noContext = noContext)
//  }
//}
//
///**
// * TODO: IDEA - bias non-terms toward typically-non-term cats, and terms toward typically-term cats
// */
//class AddLambdaSmoothedSupervisedScgTrainer(
//  lambda: Double,
//  parseCountCutoff: Long,
//  numSamples: Int,
//  guideChartBuilder: OldToRemoveCfgGuideChartBuilder,
//  noContext: Boolean = false,
//  verbose: Boolean = false)
//  extends SupervisedScgParserTrainer {
//
//  override def train(trees: Vector[CcgTree], initialTagdict: TagDictionary[Cat]): Parser = {
//    val rootCounts = trees.map(SupervisedPcfgParserTrainer.getRootCount).reduce(_ |+| _)
//    val prodCounts = trees.map(SupervisedPcfgParserTrainer.getProdCounts).reduce(_ |+| _)
//    val leftContextCounts = trees.map(SupervisedScgParserTrainer.getLeftContextCounts(_, initialTagdict)).reduce(_ |+| _)
//    val rightContextCounts = trees.map(SupervisedScgParserTrainer.getRightContextCounts(_, initialTagdict)).reduce(_ |+| _)
//    doTrain(rootCounts.mapVals(LogDouble(_)), prodCounts.mapVals(_.mapVals(LogDouble(_))), leftContextCounts.mapVals(_.mapVals(LogDouble(_))), rightContextCounts.mapVals(_.mapVals(LogDouble(_))), initialTagdict)
//  }
//
//  def doTrain(rootCounts: Map[Cat, LogDouble], prodCounts: Map[Cat, Map[Prod, LogDouble]], leftContextCounts: Map[Cat, Map[Cat, LogDouble]], rightContextCounts: Map[Cat, Map[Cat, LogDouble]], initialTagdict: TagDictionary[Cat]) = {
//    val allKnownSupertags = initialTagdict.allTags | prodCounts.ungroup.collect { case (supertag, (TermProd(_), _)) => supertag }.toSet | leftContextCounts.ungroup.flatMap { case (cat, (ctx, _)) => Set(cat, ctx) }.toSet | rightContextCounts.ungroup.flatMap { case (cat, (ctx, _)) => Set(cat, ctx) }.toSet
//    val allKnownCats = rootCounts.keySet | prodCounts.ungroup.collect { case (a, (NontermProd(b, c), _)) => Vector(a, b, c) }.flatten.toSet | allKnownSupertags
//    val tagdict = initialTagdict
//      .withWords(prodCounts.values.flatten.collect { case (TermProd(word), _) => word }.toSet)
//      //.withTags(allKnownCats)
//      .withTags(allKnownSupertags)
//
//    //    val allTermProds = Some(tagdict.allWords.map(TermProd(_)))
//    //    val allKnownNtProds = Some(nontermProdCounts.values.flatten.map(_._1).toSet)
//    val allKnownProds = Some(tagdict.allWords.map(TermProd(_)) ++ prodCounts.values.flatten.map(_._1).toSet)
//
//    val rootDist = new LaplaceLogProbabilityDistribution(rootCounts, None, None, LogDouble(lambda))
//    val prodDist = new SimpleConditionalLogProbabilityDistribution(
//      prodCounts.mapt((c, cCounts) => c -> new LaplaceLogProbabilityDistribution(cCounts, allKnownProds, /*Some[Set[Prod]]((tagdict.entries.keySet -- tagdict.knownWordsForTag.getOrElse(c, Set.empty)).map(TermProd(_)))*/ None, LogDouble(lambda))),
//      new LaplaceLogProbabilityDistribution(Map(), allKnownProds, /*TODO: Some[Set[Prod]](tagdict.entries.keySet.map(TermProd(_)))*/ None, LogDouble(lambda)))
//
//    new StupidScgParser(
//      rootDist, prodDist,
//      new SimpleConditionalLogProbabilityDistribution(
//        leftContextCounts.mapVals(new LaplaceLogProbabilityDistribution(_, Some(allKnownSupertags), None, LogDouble(lambda))),
//        new LaplaceLogProbabilityDistribution(Map(), Some(allKnownSupertags), None, LogDouble(lambda))),
//      new SimpleConditionalLogProbabilityDistribution(
//        rightContextCounts.mapVals(new LaplaceLogProbabilityDistribution(_, Some(allKnownSupertags), None, LogDouble(lambda))),
//        new LaplaceLogProbabilityDistribution(Map(), Some(allKnownSupertags), None, LogDouble(lambda))),
//      //new OldToRemovePcfgParser(new Pcfg(rootDist, prodDist), tagdict, rules),
//      tagdict,
//      parseCountCutoff,
//      numSamples,
//      guideChartBuilder,
//      //maxIterations = 50,
//      noContext = noContext,
//      verbose = verbose)
//  }
//
//  override def toString() = f"AddLambdaSmoothedSupervisedScgTrainer(lambda=$lambda,  $parseCountCutoff, $numSamples, ${guideChartBuilder}, noContext=$noContext)"
//}
//
//class SupervisedPcfgParserScgFacadeTrainer(pcfgTrainer: SupervisedPcfgParserTrainer) extends SupervisedScgParserTrainer {
//  override def train(trees: Vector[CcgTree], initialTagdict: TagDictionary[Cat]): Parser =
//    pcfgTrainer.train(trees, initialTagdict)
//  override def doTrain(rootCounts: Map[Cat, LogDouble], prodCounts: Map[Cat, Map[Prod, LogDouble]], leftContextCounts: Map[Cat, Map[Cat, LogDouble]], rightContextCounts: Map[Cat, Map[Cat, LogDouble]], initialTagdict: TagDictionary[Cat]): Parser =
//    pcfgTrainer.doTrain(rootCounts, prodCounts, initialTagdict)
//  override def toString = f"SupervisedPcfgParserScgFacadeTrainer($pcfgTrainer)"
//}
//
//object SupervisedScgParserTrainer {
//  final def getContexts(tree: CcgTree, tagdict: TagDictionary[Cat]): Vector[(Cat, (Cat, Cat))] = {
//    val tags = tree.tagged.map(_._2)
//    val seTags = tagdict.startTag +: tags :+ tagdict.endTag
//    val constituentSpans = CcgTree.constituentCatSpans(tree).toVector
//    constituentSpans.map { case (cat, (i, j)) => cat -> (seTags(i), seTags(j + 1)) }
//  }
//
//  final def getContextCounts(tree: CcgTree, tagdict: TagDictionary[Cat]) = {
//    getContexts(tree, tagdict).groupByKey.mapVals(_.counts.mapVals(_.toDouble))
//  }
//
//  final def getLeftContextCounts(tree: CcgTree, tagdict: TagDictionary[Cat]): Map[Cat, Map[Cat, Double]] = {
//    getContexts(tree, tagdict).mapVals(_._1).groupByKey.mapVals(_.counts.mapVals(_.toDouble))
//  }
//
//  final def getRightContextCounts(tree: CcgTree, tagdict: TagDictionary[Cat]): Map[Cat, Map[Cat, Double]] = {
//    getContexts(tree, tagdict).mapVals(_._2).groupByKey.mapVals(_.counts.mapVals(_.toDouble))
//  }
//}
