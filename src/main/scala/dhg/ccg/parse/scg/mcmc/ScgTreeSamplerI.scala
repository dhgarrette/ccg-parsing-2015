package dhg.ccg.parse.scg.mcmc

import dhg.util._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.prob._
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import scalaz._
import Scalaz._
import dhg.ccg.util.DrawMatrix
import dhg.ccg.util._
import org.apache.commons.math3.random.RandomGenerator

trait ScgTreeSamplerI {

  type Cat = Int
  type Word = Int

  def resample(
    guideChart: CfgGuideChartI,
    existingTree: CcgTreeI,
    logRootDist: IndirectSparseVec[Double], //                                 t -> p
    logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
    logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
    logTermDist: Array[Vec[Double]], //                                        t -> w -> p
    logPmixDist: Array[Array[Double]], //                                      t -> p
    logLctxDist: Array[IndirectSparseVec[Double]], //                          t -> l -> p
    logRctxDist: Array[IndirectSparseVec[Double]], //                          t -> r -> p
    numCats: Int,
    startCat: Cat, endCat: Cat) //
    : (CcgTreeI, Boolean) // (tree, isAccept)

}

class MetHastScgTreeSamplerI(
  pcfgTreeSampler: PcfgTreeSamplerI,
  treesPerIteration: Int,
  rand: RandomGenerator) //
  (catIndexer: Indexer[Cat], wordIndexer: Indexer[String])
    extends ScgTreeSamplerI {

  def resample(
    guideChart: CfgGuideChartI,
    existingTree: CcgTreeI,
    logRootDist: IndirectSparseVec[Double], //                                 t -> p
    logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
    logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
    logTermDist: Array[Vec[Double]], //                                        t -> w -> p
    logPmixDist: Array[Array[Double]], //                                      t -> p
    logLctxDist: Array[IndirectSparseVec[Double]], //                          t -> l -> p
    logRctxDist: Array[IndirectSparseVec[Double]], //                          t -> r -> p
    numCats: Int,
    startCat: Cat, endCat: Cat) //
    : (CcgTreeI, Boolean) // (tree, isAccept)
    = {

    val n = guideChart.length

    val pcfgTrees = pcfgTreeSampler.samples(guideChart, logRootDist, logBinyDist, logUnryDist, logTermDist, logPmixDist, numCats, k = treesPerIteration)

    var currentTree = existingTree
    var currentTreeLogProb = logCtxProb(existingTree, logLctxDist, logRctxDist, startCat, endCat)
    var accept: Boolean = false
    var i = 0
    while (i < treesPerIteration) {
      val newTree = pcfgTrees(i)
      val newTreeLogProb = logCtxProb(newTree, logLctxDist, logRctxDist, startCat, endCat)

      val higher = newTreeLogProb >= currentTreeLogProb
      accept = (higher || rand.nextDouble < math.exp(newTreeLogProb - currentTreeLogProb))
      //if (newTreeLogProb != currentTreeLogProb) println(f"scg tree sampler::    higher=$higher%-5s  =  new=${math.exp(newTreeLogProb)}%-20s > old=${math.exp(currentTreeLogProb)}%-20s       accept=$accept%-5s")
      if (accept) {
        currentTree = newTree
        currentTreeLogProb = newTreeLogProb
      }

      i += 1
    }
    (currentTree, accept)
  }

  def logCtxProb(
    tree: CcgTreeI,
    logLctxDist: Array[IndirectSparseVec[Double]], //                  t -> l -> p
    logRctxDist: Array[IndirectSparseVec[Double]], //                  t -> r -> p
    startCat: Cat, endCat: Cat): Double = {

    val supertags: Array[Cat] = startCat +: tree.supertags.toArray :+ endCat
    logCtxProbRecurse(tree, 0, tree.length, supertags, logLctxDist, logRctxDist, startCat, endCat)
  }

  private[this] def logCtxProbRecurse(
    tree: CcgTreeI, i: Int, j: Int,
    supertags: Array[Cat],
    logLctxDist: Array[IndirectSparseVec[Double]], //                  t -> l -> p
    logRctxDist: Array[IndirectSparseVec[Double]], //                  t -> r -> p
    startCat: Cat, endCat: Cat): Double = tree match {

    case CcgBinodeI(t, l, r) =>
      val lctxLogProb = logLctxDist(t)(supertags(i))
      //println(f"sdfjksi1::   logRctxDist(${catIndexer.obj(t)})(supertags(${j + 1})) = logRctxDist(${catIndexer.obj(t)})(${catIndexer.obj(supertags(j + 1))})")
      //println(f"sdfjksi2::         ${logRctxDist(t)}")
      //println(f"sdfjksi3::         ${logRctxDist(t)(supertags(j + 1)) }")
      val rctxLogProb = logRctxDist(t)(supertags(j + 1))
      val k = i + l.length
      val lLogProb = logCtxProbRecurse(l, i, k, supertags, logLctxDist, logRctxDist, startCat, endCat)
      val rLogProb = logCtxProbRecurse(r, k, j, supertags, logLctxDist, logRctxDist, startCat, endCat)
      lctxLogProb + rctxLogProb + lLogProb + rLogProb
    case CcgUnodeI(t, s) =>
      val lctxLogProb = logLctxDist(t)(supertags(i))
      val rctxLogProb = logRctxDist(t)(supertags(j + 1))
      val sLogProb = logCtxProbRecurse(s, i, j, supertags, logLctxDist, logRctxDist, startCat, endCat)
      lctxLogProb + rctxLogProb + sLogProb
    case CcgLeafI(t, w) =>
      val lctxLogProb = logLctxDist(t)(supertags(i))
      val rctxLogProb = logRctxDist(t)(supertags(j + 1))
      lctxLogProb + rctxLogProb
  }

}

