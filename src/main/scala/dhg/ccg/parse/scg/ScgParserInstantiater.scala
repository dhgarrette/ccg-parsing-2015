package dhg.ccg.parse.scg

import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.scg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.prob._
import dhg.ccg.tagdict.StartEndTags

trait ScgParserInstantiater {

  def apply(
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]): GuideChartParser

}

trait KBestScgParserInstantiater extends ScgParserInstantiater {

  def apply(
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]): KBestGuideChartParser

}

class ExactScgParserInstantiater() extends ScgParserInstantiater {

  def apply(
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]) = {

    new ExactScgParser(rootDist, prodDist, lctxDist, rctxDist)(se)
  }

}

class RerankScgParserInstantiater(
  maxExactPossibleParseCount: Int,
  numDelegateParses: Int,
  //exactScgParserInstantiater: ScgParserInstantiater,
  delegateParserInstantiater: PcfgParserInstantiater)
  extends ScgParserInstantiater {

  def apply(
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]) = {

    val exactScgParser = new ExactScgParser(rootDist, prodDist, lctxDist, rctxDist)(se)
    new NumPossibleParsesDelegatingKBestGuideChartParser(
      exactParser = exactScgParser,
      approxParser = new RerankParser(
        reranker = exactScgParser,
        delegateParser = delegateParserInstantiater(rootDist, prodDist),
        numDelegateParses = numDelegateParses),
      maxExactPossibleParseCount = maxExactPossibleParseCount)
  }

}

class DualScgParserInstantiater(
  weightedPcfgParserInstantiater: WeightedPcfgParserInstantiater,
  maxIterations: Int,
  resultWeighter: ScgWeighter,
  verbose: Boolean = false)
  extends ScgParserInstantiater {

  def apply(
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]) = {

    new DualScgParser(
        rootDist, prodDist, lctxDist, rctxDist, 
        //weightedPcfgParserInstantiater(rootDist, prodDist), 
        maxIterations, 
        //resultWeighter, 
        verbose)(se)
  }

}
