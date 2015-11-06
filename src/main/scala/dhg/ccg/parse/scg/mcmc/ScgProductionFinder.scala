package dhg.ccg.parse.scg.mcmc

import dhg.util._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.tagdict.StartEndTags
import scalaz._
import Scalaz._

trait ScgProductionFinder extends PcfgProductionCounter {
  def lctxCounts(t: CcgTree)(se: StartEndTags[Cat]): Map[Cat, Map[Cat, Double]]
  def rctxCounts(t: CcgTree)(se: StartEndTags[Cat]): Map[Cat, Map[Cat, Double]]
}

class SimpleScgProductionFinder(
  pcfgProductionFinder: PcfgProductionCounter)
  extends ScgProductionFinder {

  def rootCounts(t: CcgTree): Map[Cat, Double] = pcfgProductionFinder.rootCounts(t)
  def prodCounts(t: CcgTree): Map[Cat, Map[Prod, Double]] = pcfgProductionFinder.prodCounts(t)
  def binyCounts(t: CcgTree): Map[Cat, Map[BinaryProd, Double]] = pcfgProductionFinder.binyCounts(t)
  def unryCounts(t: CcgTree): Map[Cat, Map[UnaryProd, Double]] = pcfgProductionFinder.unryCounts(t)
  def termCounts(t: CcgTree): Map[Cat, Map[TermProd, Double]] = pcfgProductionFinder.termCounts(t)

  def lctxCounts(t: CcgTree)(se: StartEndTags[Cat]): Map[Cat, Map[Cat, Double]] = {
    val supertags = se.startTag +: t.supertags :+ se.endTag
    def r(t: CcgTree, i: Int, j: Int): Map[Cat, Map[Cat, Double]] = {
      val lctx = supertags(i)
      t match {
        case CcgBinode(c, ik, kj) => Map(c -> Map(lctx -> 1.0)) |+| r(ik, i, i + ik.length) |+| r(kj, i + ik.length, j)
        case CcgUnode(c, sub) => Map(c -> Map(lctx -> 1.0)) |+| r(sub, i, j)
        case CcgLeaf(c, word, _) => Map(c -> Map(lctx -> 1.0))
      }
    }
    r(t, 0, t.length)
  }

  def rctxCounts(t: CcgTree)(se: StartEndTags[Cat]): Map[Cat, Map[Cat, Double]] = {
    val supertags = se.startTag +: t.supertags :+ se.endTag
    def r(t: CcgTree, i: Int, j: Int): Map[Cat, Map[Cat, Double]] = {
      val rctx = supertags(j + 1)
      t match {
        case CcgBinode(c, ik, kj) => Map(c -> Map(rctx -> 1.0)) |+| r(ik, i, i + ik.length) |+| r(kj, i + ik.length, j)
        case CcgUnode(c, sub) => Map(c -> Map(rctx -> 1.0)) |+| r(sub, i, j)
        case CcgLeaf(c, word, _) => Map(c -> Map(rctx -> 1.0))
      }
    }
    r(t, 0, t.length)
  }

}
