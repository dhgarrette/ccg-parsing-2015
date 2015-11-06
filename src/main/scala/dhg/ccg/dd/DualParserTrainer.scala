//package dhg.ccg.dd
//
//import dhg.ccg.cat._
//import dhg.ccg.parse.pcfg._
//import dhg.ccg.prob._
//import dhg.ccg.tagdict.SimpleTagDictionary
//import dhg.ccg.tagdict.TagDictionary
//import dhg.util._
//import scalaz._
//import Scalaz._
//import dhg.ccg.prob._
//import dhg.ccg.cat._
//import dhg.ccg.data._
//import dhg.util.viz._
//import dhg.ccg.parse._
//import scala.collection.mutable.{ Map => MMap }
//import scala.collection.mutable.{ Set => MSet }
//import scalaz._
//import Scalaz._
//import scala.collection.breakOut
//import scala.util.Try
//import dhg.ccg.tag.learn._
//import dhg.ccg.tag._
//import dhg.ccg.parse._
//import dhg.ccg.parse.pcfg._
//import scala.annotation.tailrec
//import dhg.ccg.parse.pcfg.OldToRemoveCfgGuideChartBuilder.GuideTable
//import dhg.ccg.math.Util._
//
//class DualParserTrainer(
//  supervisedParserTrainer: SupervisedPcfgParserTrainer,
//  supervisedTaggerTrainer: SupervisedHmmTaggerTrainer[Cat],
//  maxEmIterations: Int = 50,
//  maxDecompIterations: Int = 50,
//  deltaConst: Double = 1.0,
//  alphaRoots: Double, alphaNonts: Double, alphaTerms: Double,
//  alphaTrans: Double, alphaEmiss: Double,
//  guideChartBuilder: OldToRemoveCfgGuideChartBuilder) {
//
//  private[this] val productionFinder = new OldToRemoveCfgProductionFinder(guideChartBuilder)
//
//  final def train(
//    sentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Cat],
//    baseRootDist: LogProbabilityDistribution[Cat], baseNontDist: ConditionalLogProbabilityDistribution[Cat, NontermProd], baseTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
//    baseTrDist: ConditionalLogProbabilityDistribution[Cat, Cat], baseEmDist: ConditionalLogProbabilityDistribution[Cat, Word]): DualParser = {
//    trainBracketed(sentences.mapToVal(Vector.empty), initialTagdict, baseRootDist, baseNontDist, baseTermDist, baseTrDist, baseEmDist)
//  }
//
//  final def trainBracketed(
//    bracketedSentences: Vector[(Vector[Word], Vector[(Int, Int)])], initialTagdict: TagDictionary[Cat],
//    baseRootDist: LogProbabilityDistribution[Cat], baseNontDist: ConditionalLogProbabilityDistribution[Cat, NontermProd], baseTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
//    baseTrDist: ConditionalLogProbabilityDistribution[Cat, Cat], baseEmDist: ConditionalLogProbabilityDistribution[Cat, Word]): DualParser = {
//    val tagdict = initialTagdict.withWords(bracketedSentences.map(_._1).flatten.toSet)
//
//    val goldRootCounts = Map.empty[Cat, Double]
//    val goldNontCounts = Map.empty[Cat, Map[NontermProd, Double]]
//    val goldTermCounts = Map.empty[Cat, Map[TermProd, Double]]
//    val goldTranCounts = Map.empty[Cat, Map[Cat, Double]]
//    val goldEmisCounts = Map.empty[Cat, Map[Word, Double]]
//
//    val guideCharts = bracketedSentences.flatMap { case (sentence, brackets) => guideChartBuilder.build(sentence, brackets, tagdict) }
//    val (allRootsSet, allNontsSet, allTermsSet) = guideCharts.map(productionFinder.getAllProductionsSplitFromGuideChart).reduce(_ |+| _) |+| (goldRootCounts.keySet, goldNontCounts.mapValues(_.keySet).ungroup.toSet, goldTermCounts.mapValues(_.keySet).ungroup.toSet)
//
//    val allTransSet: Set[(Cat, Cat)] = (for { sent <- guideCharts; Seq(a, b) <- (Set(tagdict.startTag) +: (0 until sent.length).map(i => sent(i)(i + 1).get.keySet) :+ Set(tagdict.endTag)).sliding(2); x <- a; y <- b } yield (x, y))(breakOut)
//    val allEmissSet: Set[(Cat, Word)] = (for { sent <- guideCharts; i <- 0 until sent.length; entry <- sent(i)(i + 1).get; (supertag, Coll((_, TermProd(word)))) = entry } yield (supertag, word)).toSet ++ Set(tagdict.startTag -> tagdict.startWord, tagdict.endTag -> tagdict.endWord)
//
//    val alphaPriorRootCounts = allRootsSet.mapTo(root => LogDouble(alphaRoots) * baseRootDist(root) + LogDouble(goldRootCounts.getOrElse(root, 0.0))).toMap
//    val alphaPriorNontCounts = allNontsSet.groupByKey.mapt((cat, nonts) => cat -> nonts.mapTo(prod => (LogDouble(alphaNonts) * baseNontDist(prod, cat) + LogDouble(goldNontCounts.get(cat).flatMap(_.get(prod)).getOrElse(0.0)))).toMap)
//    val alphaPriorTermCounts = allTermsSet.groupByKey.mapt((cat, terms) => cat -> terms.mapTo(prod => (LogDouble(alphaTerms) * baseTermDist(prod, cat) + LogDouble(goldTermCounts.get(cat).flatMap(_.get(prod)).getOrElse(0.0)))).toMap)
//    val alphaPriorTranCounts = allTransSet.groupByKey.mapt((cat, nexts) => cat -> nexts.mapTo(next => (LogDouble(alphaTrans) * baseTrDist(next, cat) + LogDouble(goldTranCounts.get(cat).flatMap(_.get(next)).getOrElse(0.0)))).toMap)
//    val alphaPriorEmisCounts = allEmissSet.groupByKey.mapt((cat, words) => cat -> words.mapTo(word => (LogDouble(alphaEmiss) * baseEmDist(word, cat) + LogDouble(goldEmisCounts.get(cat).flatMap(_.get(word)).getOrElse(0.0)))).toMap)
//    val allCats = alphaPriorNontCounts.keySet | alphaPriorTermCounts.keySet
//
//    val sentences = bracketedSentences.map(_._1)
//    val prodDist = new ICPD(baseNontDist, baseTermDist, Map().withDefaultValue(LogDouble(0.5)))
//    val initialDualParser = new DualParser(
//      new OldToRemovePcfgParser(baseRootDist, prodDist, tagdict, guideChartBuilder),
//      new HmmTagger(baseTrDist, baseEmDist, tagdict),
//      tagdict, guideChartBuilder,
//      maxDecompIterations, deltaConst,
//      verbose = true)
//
//    println(f"BEGIN EM ITERATIONS")
//    val finalParser = reestimate(1, initialDualParser,
//      guideCharts, tagdict, allCats,
//      alphaPriorRootCounts, alphaPriorNontCounts, alphaPriorTermCounts,
//      alphaPriorTranCounts, alphaPriorEmisCounts)
//    finalParser
//  }
//
//  @tailrec private[this] final def reestimate(iteration: Int, dualParser: DualParser,
//    guideCharts: Vector[GuideTable], tagdict: TagDictionary[Cat], allCats: Set[Cat],
//    alphaPriorRootCounts: Map[Cat, LogDouble], alphaPriorNontCounts: Map[Cat, Map[NontermProd, LogDouble]], alphaPriorTermCounts: Map[Cat, Map[TermProd, LogDouble]],
//    alphaPriorTranCounts: Map[Cat, Map[Cat, LogDouble]], alphaPriorEmisCounts: Map[Cat, Map[Word, LogDouble]]): DualParser = {
//    val startTime = System.currentTimeMillis()
//    val trees = guideCharts.par.zipWithIndex.flatMap {
//      case (guideChart, i) =>
//        // if ((i+1) % 100 == 0) println(f"    ${i+1}")
//        dualParser.parseAndProbFromGuideChart(guideChart).map(_._1)
//    }
//
//    val estRootCounts = trees.map(SupervisedPcfgParserTrainer.getRootCount).reduce(_ |+| _)
//    val estNontCounts = trees.map(SupervisedPcfgParserTrainer.getNontermProdCounts).reduce(_ |+| _)
//    val estTermCounts = trees.map(SupervisedPcfgParserTrainer.getTermProdCounts).reduce(_ |+| _)
//    val estTranCounts = trees.map(t => (tagdict.startTag +: t.tagged.map(_._2) :+ tagdict.endTag).sliding2.groupByKey.mapVals(_.counts.mapVals(_.toDouble))).reduce(_ |+| _)
//    val estEmisCounts = trees.map(_.tagged.map(_.swap).groupByKey.mapVals(_.counts.mapVals(_.toDouble))).reduce(_ |+| _)
//
//    println(f"iteration ${(iteration + ":").padRight(4)} ${(System.currentTimeMillis() - startTime) / 1000.0}%.3f sec")
//    if (iteration >= maxEmIterations) {
//      val seqtrees = trees.seq
//      new DualParser(
//        supervisedParserTrainer.train(seqtrees, /*         */ tagdict).asInstanceOf[WeightedParser],
//        supervisedTaggerTrainer.train(seqtrees.map(_.tagged), tagdict).asInstanceOf[WeightedTagger[Cat]],
//        tagdict, guideChartBuilder,
//        maxDecompIterations, deltaConst)
//    }
//    else {
//      val newDualParser = new DualParser(
//        new OldToRemovePcfgParser(
//          new SimpleLogProbabilityDistribution(LogDir(estRootCounts.mapVals(LogDouble(_)) |+| alphaPriorRootCounts)),
//          new SimpleConditionalLogProbabilityDistribution(allCats.mapTo { cat =>
//            val ntc = estNontCounts.getOrElse(cat, Map.empty).mapVals(LogDouble(_))
//            val tc = estTermCounts.getOrElse(cat, Map.empty).mapVals(LogDouble(_))
//            val ntcCountsWithPrior = ntc |+| alphaPriorNontCounts.getOrElse(cat, Map.empty)
//            val tcCountsWithPrior = tc |+| alphaPriorTermCounts.getOrElse(cat, Map.empty)
//            val pNonterm = LogBeta(ntc.values.sum + LogDouble(1), tc.values.sum + LogDouble(1))
//            new IPD(new SimpleLogProbabilityDistribution(LogDir(ntcCountsWithPrior)), new SimpleLogProbabilityDistribution(LogDir(tcCountsWithPrior)), pNonterm)
//          }.toMap),
//          tagdict, guideChartBuilder),
//        new HmmTagger(
//          new SimpleConditionalLogProbabilityDistribution((estTranCounts.mapVals(_.mapVals(LogDouble(_))) |+| alphaPriorTranCounts).mapVals(counts => new SimpleLogProbabilityDistribution(LogDir(counts)))),
//          new SimpleConditionalLogProbabilityDistribution((estEmisCounts.mapVals(_.mapVals(LogDouble(_))) |+| alphaPriorEmisCounts).mapVals(counts => new SimpleLogProbabilityDistribution(LogDir(counts)))),
//          tagdict),
//        tagdict, guideChartBuilder,
//        maxDecompIterations, deltaConst)
//      reestimate(iteration + 1, newDualParser,
//        guideCharts, tagdict, allCats,
//        alphaPriorRootCounts, alphaPriorNontCounts, alphaPriorTermCounts,
//        alphaPriorTranCounts, alphaPriorEmisCounts)
//    }
//  }
//
//}
