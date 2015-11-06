package dhg.ccg.parse.scg.mcmc

import scala.annotation.tailrec
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
import dhg.ccg.parse.scg._
import org.apache.commons.math3.random.RandomGenerator
import dhg.ccg.math.Util._
import dhg.ccg.tagdict.StartEndTags
import dhg.ccg.tagdict.SimpleStartEndTags

trait ScgAcceptanceSampler {

  def accept(newTree: CcgTree, curTree: CcgTree,
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rand: RandomGenerator)(se: StartEndTags[Cat]): (Boolean, Double, Boolean)

}

class FullScgAcceptanceSampler(
  scgWeighter: ScgWeighter)
  extends ScgAcceptanceSampler {

  def accept(newTree: CcgTree, curTree: CcgTree,
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rand: RandomGenerator)(se: StartEndTags[Cat]) = {

    val newTreeWeight = scgWeighter.weight(newTree, rootDist, prodDist, lctxDist, rctxDist)(se)
    val curTreeWeight = scgWeighter.weight(curTree, rootDist, prodDist, lctxDist, rctxDist)(se)
    val ratio = (newTreeWeight / curTreeWeight).toDouble

    val newTreePcfgWeight = scgWeighter.pcfgWeight(newTree, rootDist, prodDist)
    val oldTreePcfgWeight = scgWeighter.pcfgWeight(curTree, rootDist, prodDist)
    val agree = (newTreeWeight > curTreeWeight) == (newTreePcfgWeight > oldTreePcfgWeight)

    (ratio > 1 || ratio > rand.nextDouble, ratio, agree)
  }

  override def toString() = f"FullScgAcceptanceSampler($scgWeighter)"
}

class ContextScgAcceptanceSampler(
  scgWeighter: ScgWeighter)
  extends ScgAcceptanceSampler {

  def accept(newTree: CcgTree, curTree: CcgTree,
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rand: RandomGenerator)(se: StartEndTags[Cat]) = {

    val newTreeCtxWeight = scgWeighter.ctxWeight(newTree, lctxDist, rctxDist)(se)
    val curTreeCtxWeight = scgWeighter.ctxWeight(curTree, lctxDist, rctxDist)(se)
    val ratio = (newTreeCtxWeight / curTreeCtxWeight).toDouble

    val newTreePcfgWeight = scgWeighter.pcfgWeight(newTree, rootDist, prodDist)
    val oldTreePcfgWeight = scgWeighter.pcfgWeight(curTree, rootDist, prodDist)
    val agree = (newTreeCtxWeight > curTreeCtxWeight) == (newTreePcfgWeight > oldTreePcfgWeight)

    (ratio > 1 || ratio > rand.nextDouble, ratio, agree)
  }

  override def toString() = f"ContextScgAcceptanceSampler($scgWeighter)"
}

class AlwaysAcceptScgAcceptanceSampler(
  scgWeighter: ScgWeighter)
  extends ScgAcceptanceSampler {

  def accept(newTree: CcgTree, curTree: CcgTree,
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rand: RandomGenerator)(se: StartEndTags[Cat]) = {

    val newTreeCtxWeight = scgWeighter.ctxWeight(newTree, lctxDist, rctxDist)(se)
    val curTreeCtxWeight = scgWeighter.ctxWeight(curTree, lctxDist, rctxDist)(se)
    val ratio = (newTreeCtxWeight / curTreeCtxWeight).toDouble

    val newTreePcfgWeight = scgWeighter.pcfgWeight(newTree, rootDist, prodDist)
    val oldTreePcfgWeight = scgWeighter.pcfgWeight(curTree, rootDist, prodDist)
    val agree = (newTreeCtxWeight > curTreeCtxWeight) == (newTreePcfgWeight > oldTreePcfgWeight)

    (true, ratio, agree)
  }

  override def toString() = f"ContextScgAcceptanceSampler($scgWeighter)"
}
