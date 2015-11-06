package dhg.ccg.parse.pcfg

import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.scg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.prob._
import dhg.ccg.tagdict.StartEndTags

trait PcfgParserInstantiater {
  def apply(
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): KBestGuideChartParser
}

trait WeightedPcfgParserInstantiater extends PcfgParserInstantiater {
  def apply(
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): WeightedKBestGuideChartParser
}

class ExactPcfgParserInstantiater() extends WeightedPcfgParserInstantiater {
  def apply(
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]) = {

    new PcfgParser(rootDist, prodDist)
  }
}

class SamplingPcfgParserInstantiater(treeSampler: PcfgTreeSampler, weighter: PcfgWeighter) extends PcfgParserInstantiater {
  def apply(
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]) = {

    new SamplingPcfgParser(rootDist, prodDist, treeSampler, weighter)
  }
}

class CompositePcfgParserInstantiater(delegateA: PcfgParserInstantiater, kA: Int, delegateB: PcfgParserInstantiater) extends PcfgParserInstantiater {
  def apply(
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]) = {

    new CompositePcfgParser(delegateA(rootDist, prodDist), kA, delegateB(rootDist, prodDist))
  }
}
