package dhg.ccg.parse.pcfg.mcmc

import dhg.util._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.tagdict.StartEndTags
import scalaz._
import Scalaz._

trait PcfgProductionCounter {
  def rootCounts(t: CcgTree): Map[Cat, Double]
  def prodCounts(t: CcgTree): Map[Cat, Map[Prod, Double]]
  def binyCounts(t: CcgTree): Map[Cat, Map[BinaryProd, Double]]
  def unryCounts(t: CcgTree): Map[Cat, Map[UnaryProd, Double]]
  def termCounts(t: CcgTree): Map[Cat, Map[TermProd, Double]]
}

class SimplePcfgProductionCounter() extends PcfgProductionCounter {

  def rootCounts(t: CcgTree): Map[Cat, Double] = {
    Map(t.cat -> 1)
  }

  def prodCounts(t: CcgTree): Map[Cat, Map[Prod, Double]] = {
    t match {
      case CcgBinode(c, l, r) => Map(c -> Map((BinaryProd(l.cat, r.cat): Prod) -> 1.0)) |+| prodCounts(l) |+| prodCounts(r)
      case CcgUnode(c, s) => Map(c -> Map((UnaryProd(s.cat): Prod) -> 1.0)) |+| prodCounts(s)
      case CcgLeaf(c, w, _) => Map(c -> Map((TermProd(w): Prod) -> 1.0))
    }
  }

  def binyCounts(t: CcgTree): Map[Cat, Map[BinaryProd, Double]] = {
    prodCounts(t).mapVals {
      _.collect { case (p: BinaryProd, c) => p -> c }
    }.filter(_._2.nonEmpty)
  }

  def unryCounts(t: CcgTree): Map[Cat, Map[UnaryProd, Double]] = {
    prodCounts(t).mapVals {
      _.collect { case (p: UnaryProd, c) => p -> c }
    }.filter(_._2.nonEmpty)
  }

  def termCounts(t: CcgTree): Map[Cat, Map[TermProd, Double]] = {
    prodCounts(t).mapVals {
      _.collect { case (p: TermProd, c) => p -> c }
    }.filter(_._2.nonEmpty)
  }

}
