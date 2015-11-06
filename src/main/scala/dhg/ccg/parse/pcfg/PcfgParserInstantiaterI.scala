package dhg.ccg.parse.pcfg

import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.scg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.prob._
import dhg.ccg.tagdict.StartEndTags
import dhg.ccg.util._

trait PcfgParserInstantiaterI {
  def apply(
    logRootDist: IndirectSparseVec[Double], //                                 t -> p
    logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
    logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
    logTermDist: Array[Vec[Double]], //                                        t -> w -> p
    logPmixDist: Array[Array[Double]]) //                                      t -> p
    (catIndexer: Indexer[Cat], wordIndexer: Indexer[String]): PcfgParserI
}

//trait WeightedPcfgParserInstantiaterI extends PcfgParserInstantiaterI {
//  def apply(
//    logRootDist: LogProbabilityDistribution[Cat],
//    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): WeightedKBestGuideChartParser
//}

class ExactPcfgParserInstantiaterI() extends PcfgParserInstantiaterI { // extends WeightedPcfgParserInstantiaterI {
  def apply(
    logRootDist: IndirectSparseVec[Double], //                                 t -> p
    logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
    logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
    logTermDist: Array[Vec[Double]], //                                        t -> w -> p
    logPmixDist: Array[Array[Double]]) //                                      t -> p
    (catIndexer: Indexer[Cat], wordIndexer: Indexer[String]): PcfgParserI = {

    new PcfgParserI(
      logRootDist: IndirectSparseVec[Double], //                                 t -> p
      logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
      logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
      logTermDist: Array[Vec[Double]], //                                        t -> w -> p
      logPmixDist: Array[Array[Double]])( //                                     t -> p
      catIndexer: Indexer[Cat], wordIndexer: Indexer[String])

  }
}

//class SamplingPcfgParserInstantiaterI(treeSampler: PcfgTreeSampler, weighter: PcfgWeighter) extends PcfgParserInstantiaterI {
//  def apply(
//    logRootDist: LogProbabilityDistribution[Cat],
//    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]) = {
//
//    new SamplingPcfgParser(logRootDist, prodDist, treeSampler, weighter)
//  }
//}
//
//class CompositePcfgParserInstantiaterI(delegateA: PcfgParserInstantiaterI, kA: Int, delegateB: PcfgParserInstantiaterI) extends PcfgParserInstantiaterI {
//  def apply(
//    logRootDist: LogProbabilityDistribution[Cat],
//    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]) = {
//
//    new CompositePcfgParser(delegateA(logRootDist, prodDist), kA, delegateB(logRootDist, prodDist))
//  }
//}
