package dhg.ccg.parse.pcfg

import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.tagdict.TagDictionary
import dhg.util._
import scalaz._
import Scalaz._
import dhg.ccg.prob._
import dhg.ccg.parse._

trait PcfgWeighter extends Serializable {
  def weight(tree: CcgTree,
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): LogDouble
}

class SimplePcfgWeighter() extends PcfgWeighter {

  def weight(tree: CcgTree,
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): LogDouble = {

    def p(tree: CcgTree): LogDouble = tree match {
      case CcgBinode(ij, ik, kj) => prodDist(BinaryProd(ik.cat, kj.cat), ij) * p(ik) * p(kj)
      case CcgUnode(ij, sub) => prodDist(UnaryProd(sub.cat), ij) * p(sub)
      case CcgLeaf(ij, word, _) => prodDist(TermProd(word), ij)
    }
    val r = rootDist(tree.cat) * p(tree)
    assert(r.nonZero && !r.isNaN, f"pcfgWeight($tree) = ${r.logValue}    ${r.logValue > Double.NegativeInfinity}")
    r
  }

}
