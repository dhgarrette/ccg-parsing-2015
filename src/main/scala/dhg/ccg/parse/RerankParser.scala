package dhg.ccg.parse

import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.prob._
import dhg.ccg.tagdict.TagDictionary
import dhg.util._
import scalaz._
import Scalaz._
import dhg.ccg.parse.pcfg.CfgGuideChart // tested

class RerankParser(
  reranker: TreeWeighter,
  delegateParser: KBestGuideChartParser,
  numDelegateParses: Int)
  extends AbstractKBestGuideChartParser {

  def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int): Vector[(CcgTree, LogDouble)] = {
    println(f"RerankParser.parseAndProbKBestFromGuideChart: numDelegateParses=$numDelegateParses")
    val delegateTrees = time("RerankParser.parseAndProbKBestFromGuideChart: find delegate parses", delegateParser.parseAndProbKBestFromGuideChart(guideChart, numDelegateParses))
    println(f"RerankParser.parseAndProbKBestFromGuideChart: num delegateTrees = ${delegateTrees.size}")
    val rerankedIndexedTrees = delegateTrees.map(_._1).distinct.zipWithIndex.mapTo { case (t, i) => reranker.weight(t) }.maxByN(k)(_._2)
    val rerankedTrees = rerankedIndexedTrees.map { case ((t, i), p) => (t, p) }
    println(f"RerankParser.parseAndProbKBestFromGuideChart: top changed = ${delegateTrees.head != rerankedTrees.head}; new best is #${rerankedIndexedTrees.head._1._2 + 1}/${delegateTrees.map(_._1).distinct.size}")
    rerankedTrees
  }

}
