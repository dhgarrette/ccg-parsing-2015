package dhg.ccg.parse.pcfg

import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.tagdict.TagDictionary
import dhg.util._
import scalaz._
import Scalaz._
import dhg.ccg.prob._
import dhg.ccg.parse._
import dhg.ccg.util._

trait PcfgWeighterI extends Serializable {
  def logWeight(tree: CcgTreeI,
    logRootDist: IndirectSparseVec[Double], //                                 t -> p
    logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
    logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
    logTermDist: Array[Vec[Double]], //                                        t -> w -> p
    logPmixDist: Array[Array[Double]]) //                                      t -> p
    : Double
}

class SimplePcfgWeighterI() { // extends PcfgWeighterI {

  def logWeight(tree: CcgTreeI,
    logRootDist: IndirectSparseVec[Double], //                                 t -> p
    logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
    logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
    logTermDist: Array[Vec[Double]], //                                        t -> w -> p
    logPmixDist: Array[Array[Double]]) //                                      t -> p
    : Double = {

    def p(tree: CcgTreeI): Double = tree match {
      case CcgBinodeI(ij, ik, kj) => logBinyDist(ij)(ik.cat)(kj.cat) + logPmixDist(ij)(0) + p(ik) + p(kj)
      case CcgUnodeI(ij, sub) => logUnryDist(ij)(sub.cat) + logPmixDist(ij)(1) + p(sub)
      case CcgLeafI(ij, word) => logTermDist(ij)(word) + logPmixDist(ij)(2)
    }
    logRootDist(tree.cat) + p(tree)
    //assert(r.nonZero && !r.isNaN, f"pcfgWeight($tree) = ${r.logValue}    ${r.logValue > Double.NegativeInfinity}")
  }

}

