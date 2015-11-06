package dhg.ccg.parse.pcfg.mcmc

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

trait PcfgGuideChartProdFinder {

  def roots(gc: CfgGuideChart): Set[Cat]
  def prods(gc: CfgGuideChart): Map[Cat, Set[Prod]]
  def binys(gc: CfgGuideChart): Map[Cat, Set[BinaryProd]]
  def unrys(gc: CfgGuideChart): Map[Cat, Set[UnaryProd]]
  def terms(gc: CfgGuideChart): Map[Cat, Set[TermProd]]

}

class SimplePcfgGuideChartProdFinder() extends PcfgGuideChartProdFinder {
  def roots(gc: CfgGuideChart): Set[Cat] = {
    gc(0, gc.length).keySet
  }

  final def prods(gc: CfgGuideChart): Map[Cat, Set[Prod]] = {
    gc.bottomUpNodes.map(_._3).map { (m: Map[Cat, Set[GuideChartEntry]]) =>
      m.mapVals(_.map(_.prod))
    }.fold(Map.empty[Cat, Set[Prod]])(_ |+| _)
  }

  def binys(gc: CfgGuideChart): Map[Cat, Set[BinaryProd]] = {
    prods(gc).mapVals(_.collect { case p: BinaryProd => p }).filter(_._2.nonEmpty)
  }

  def unrys(gc: CfgGuideChart): Map[Cat, Set[UnaryProd]] = {
    prods(gc).mapVals(_.collect { case p: UnaryProd => p }).filter(_._2.nonEmpty)
  }

  def terms(gc: CfgGuideChart): Map[Cat, Set[TermProd]] = {
    prods(gc).mapVals(_.collect { case p: TermProd => p }).filter(_._2.nonEmpty)
  }

}
