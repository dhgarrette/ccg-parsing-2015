package dhg.ccg.parse.scg

import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.tagdict.TagDictionary
import dhg.util._
import scalaz._
import Scalaz._
import dhg.ccg.prob._
import dhg.ccg.parse.KBestGuideChartParser
import dhg.ccg.parse.TreeWeighter
import dhg.ccg.parse.CcgTree
import dhg.ccg.parse.CcgBinode
import dhg.ccg.parse.CcgUnode
import dhg.ccg.parse.CcgLeaf
import dhg.ccg.parse.pcfg.Prod
import dhg.ccg.parse.pcfg.BinaryProd
import dhg.ccg.parse.pcfg.UnaryProd
import dhg.ccg.parse.pcfg.TermProd
import dhg.ccg.parse.pcfg.CfgGuideChart
import dhg.ccg.parse.pcfg.GuideChartEntry
import dhg.ccg.parse.pcfg.BinaryGuideChartEntry
import dhg.ccg.parse.pcfg.UnaryGuideChartEntry
import dhg.ccg.parse.pcfg.TermGuideChartEntry
import dhg.ccg.parse.pcfg.SimplePcfgWeighter
import dhg.ccg.tagdict.StartEndTags

trait ScgWeighter {

  def weight(tree: CcgTree,
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]): LogDouble

  def pcfgWeight(tree: CcgTree,
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): LogDouble

  def ctxWeight(tree: CcgTree,
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]): LogDouble

}

class SimpleScgWeighter() extends ScgWeighter {

  private[this] val pcfgWeighter = new SimplePcfgWeighter()

  def weight(tree: CcgTree,
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]): LogDouble = {
    pcfgWeight(tree, rootDist, prodDist) * ctxWeight(tree, lctxDist, rctxDist)(se)
  }

  def pcfgWeight(tree: CcgTree,
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): LogDouble = {

    pcfgWeighter.weight(tree, rootDist, prodDist)
  }

  def ctxWeight(tree: CcgTree,
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]): LogDouble = {

    val supertags = se.startTag +: tree.supertags :+ se.endTag
    def p(tree: CcgTree, i: Int, j: Int): LogDouble = {
      val ij = tree.cat
      //println(f"${supertags(i)}  <--  $i-$ij-$j  -->  ${supertags(j + 1)}")
      val lctxP = lctxDist(supertags(i), ij)
      val rctxP = rctxDist(supertags(j + 1), ij)
      assert(lctxP.nonZero && !lctxP.isNaN, f"lctxDist(supertags(${i}), ${ij}) = lctxDist(${supertags(i)}, ${ij}) = ${lctxP.logValue} ; lctxDist=$lctxDist")
      assert(rctxP.nonZero && !rctxP.isNaN, f"rctxDist(supertags(${j + 1}), ${ij}) = rctxDist(${supertags(j + 1)}, ${ij}) = $rctxP = ${rctxP.logValue} ; rctxDist=$rctxDist")
      val ctxP = lctxP * rctxP
      tree match {
        case CcgBinode(_, ik, kj) => ctxP * p(ik, i, i + ik.length) * p(kj, i + ik.length, j)
        case CcgUnode(_, sub) => ctxP * p(sub, i, j)
        case CcgLeaf(_, word, _) => ctxP
      }
    }
    val r = p(tree, 0, tree.length)
    assert(r.nonZero && !r.isNaN, f"ctxWeight($tree) = ${r.logValue}    ${r.logValue > Double.NegativeInfinity}")
    r
  }

}
