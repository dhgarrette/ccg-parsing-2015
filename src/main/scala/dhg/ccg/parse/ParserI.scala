package dhg.ccg.parse

import dhg.ccg.cat.Cat
import dhg.ccg.tagdict.TagDictionary
import dhg.util._
import dhg.ccg.prob.ConditionalLogProbabilityDistribution
import dhg.ccg.prob.LogProbabilityDistribution
import dhg.ccg.parse.pcfg.Prod
import dhg.ccg.parse.pcfg.CfgGuideChart
import dhg.ccg.parse.pcfg.CfgGuideChartBuilder
import dhg.gfl.FudgSentence

//trait Parser {
//  type Word = String
//  
//  final def parse(sentence: Vector[Word], tagdict: TagDictionary[Cat]): Option[(CcgTree, LogDouble)] = parseFromBracketed(sentence, None, tagdict)
//  def parseFromBracketed(sentence: Vector[Word], fudgAnnotation: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[(CcgTree, LogDouble)]
//}
//
//class GuideChartParserAdapter(
//  guideChartBuilder: CfgGuideChartBuilder,
//  guideChartParser: GuideChartParser)
//  extends Parser {
//  def parseFromBracketed(sentence: Vector[Word], fudgAnnotation: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[(CcgTree, LogDouble)] = {
//    //time("GuideChartParserAdapter.parseFromBracketed: guideChartBuilder.build",
//    guideChartBuilder.build(sentence, fudgAnnotation, tagdict)
//      //)
//      .flatMap(guideChartParser.parseAndProbFromGuideChart)
//  }
//}
//
////
//
//trait GuideChartParser extends Serializable {
//  def parseAndProbFromGuideChart(guideChart: CfgGuideChart): Option[(CcgTree, LogDouble)]
//}
//
//trait WeightedGuideChartParser extends GuideChartParser {
//  def parseAndProbWithWeightsFromGuideChart(guideChart: CfgGuideChart, us: Vector[Vector[Map[Cat, LogDouble]]]): Option[(CcgTree, LogDouble)]
//}
//trait AbstractWeightedGuideChartParser extends WeightedGuideChartParser {
//  private[this] def emptyUs(sentenceLength: Int) = Vector.fill(sentenceLength)(Vector.fill(sentenceLength + 1)(Map.empty[Cat, LogDouble]))
//  final override def parseAndProbFromGuideChart(guideChart: CfgGuideChart): Option[(CcgTree, LogDouble)] = parseAndProbWithWeightsFromGuideChart(guideChart, us = emptyUs(guideChart.length))
//}
//
//trait KBestGuideChartParser extends GuideChartParser {
//  def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int): Vector[(CcgTree, LogDouble)]
//}
//trait AbstractKBestGuideChartParser extends KBestGuideChartParser {
//  final override def parseAndProbFromGuideChart(guideChart: CfgGuideChart): Option[(CcgTree, LogDouble)] = parseAndProbKBestFromGuideChart(guideChart, 1).headOption
//}
//
//trait WeightedKBestGuideChartParser extends AbstractKBestGuideChartParser with WeightedGuideChartParser {
//  private[this] def emptyUs(sentenceLength: Int) = Vector.fill(sentenceLength)(Vector.fill(sentenceLength + 1)(Map.empty[Cat, LogDouble]))
//
//  final override def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int) = parseAndProbKBestWithWeightsFromGuideChart(guideChart, us = emptyUs(guideChart.length), k)
//  final override def parseAndProbWithWeightsFromGuideChart(guideChart: CfgGuideChart, us: Vector[Vector[Map[Cat, LogDouble]]]): Option[(CcgTree, LogDouble)] = parseAndProbKBestWithWeightsFromGuideChart(guideChart, us, k = 1).headOption
//  def parseAndProbKBestWithWeightsFromGuideChart(guideChart: CfgGuideChart, us: Vector[Vector[Map[Cat, LogDouble]]], k: Int): Vector[(CcgTree, LogDouble)]
//}

trait TreeWeighterI {
  def logWeight(t: CcgTreeI): Double
}

//class NumPossibleParsesDelegatingKBestGuideChartParser(
//  val exactParser: KBestGuideChartParser,
//  val approxParser: KBestGuideChartParser,
//  val maxExactPossibleParseCount: Int)
//  extends AbstractKBestGuideChartParser {
//
//  def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int): Vector[(CcgTree, LogDouble)] = {
//    println(f"NumPossibleParsesDelegatingKBestGuideChartParser.parseAndProbKBestFromGuideChart: guideChart.numPossibleParses=${guideChart.numPossibleParses}")
//    if (guideChart.numPossibleParses <= maxExactPossibleParseCount) {
//      println(f"NumPossibleParsesDelegatingKBestGuideChartParser.parseAndProbKBestFromGuideChart:   exact")
//      exactParser.parseAndProbKBestFromGuideChart(guideChart, k)
//    }
//    else {
//      println(f"NumPossibleParsesDelegatingKBestGuideChartParser.parseAndProbKBestFromGuideChart:   approx")
//      approxParser.parseAndProbKBestFromGuideChart(guideChart, k)
//    }
//  }
//
//}
