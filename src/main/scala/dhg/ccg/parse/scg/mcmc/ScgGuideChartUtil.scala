package dhg.ccg.parse.scg.mcmc

import dhg.util._
import scala.annotation.tailrec
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.tagdict._
import scalaz._
import Scalaz._

trait ScgGuideChartProdFinder extends PcfgGuideChartProdFinder {

  def lctxs(gc: CfgGuideChart)(se: StartEndTags[Cat]): Map[Cat, Set[Cat]]
  def rctxs(gc: CfgGuideChart)(se: StartEndTags[Cat]): Map[Cat, Set[Cat]]

}

class SimpleScgGuideChartProdFinder(
  pcfgGuideChartProdFinder: PcfgGuideChartProdFinder)
  extends ScgGuideChartProdFinder {

  def roots(gc: CfgGuideChart): Set[Cat] = pcfgGuideChartProdFinder.roots(gc)
  def prods(gc: CfgGuideChart): Map[Cat, Set[Prod]] = pcfgGuideChartProdFinder.prods(gc)
  def binys(gc: CfgGuideChart): Map[Cat, Set[BinaryProd]] = pcfgGuideChartProdFinder.binys(gc)
  def unrys(gc: CfgGuideChart): Map[Cat, Set[UnaryProd]] = pcfgGuideChartProdFinder.unrys(gc)
  def terms(gc: CfgGuideChart): Map[Cat, Set[TermProd]] = pcfgGuideChartProdFinder.terms(gc)

  def lctxs(gc: CfgGuideChart)(se: StartEndTags[Cat]): Map[Cat, Set[Cat]] = {
    val tagSets = Set(se.startTag) +: gc.supertagSets :+ Set(se.endTag)
    gc.bottomUpNodes.mapt { (i, j, cell) =>
      cell.mapVals(_ => tagSets(i))
    }.fold(Map.empty[Cat, Set[Cat]])(_ |+| _).filter(_._2.nonEmpty)
  }

  def rctxs(gc: CfgGuideChart)(se: StartEndTags[Cat]): Map[Cat, Set[Cat]] = {
    val tagSets = Set(se.startTag) +: gc.supertagSets :+ Set(se.endTag)
    gc.bottomUpNodes.mapt { (i, j, cell) =>
      cell.mapVals(_ => tagSets(j + 1))
    }.fold(Map.empty[Cat, Set[Cat]])(_ |+| _).filter(_._2.nonEmpty)
  }

}
