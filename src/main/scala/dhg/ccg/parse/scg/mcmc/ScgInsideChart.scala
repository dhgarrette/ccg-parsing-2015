package dhg.ccg.parse.pcfg.mcmc

import dhg.util._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.prob._
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import scalaz._
import Scalaz._
import dhg.ccg.tagdict.StartEndTags

class ScgInsideChart(val matrix: Vector[Vector[Map[Cat, Map[(Cat, Cat), LogDouble]]]]) {
  def apply(row: Int) = matrix(row)
  def apply(row: Int, col: Int) = matrix(row)(col)

  def length = matrix.size

  def root = this(0, length)

  /**
   * Iterate over all cells
   */
  def topDownNodes = {
    for {
      span <- length downto 1 //    span size
      i <- 0 to (length - span) //  start of span
      j = i + span //               end of span
    } yield (i, j, this(i, j))
  }
}

trait ScgInsideChartBuilder {

  def buildInsideChart(
    guideChart: CfgGuideChart,
    //rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]): ScgInsideChart

}

class SimpleScgInsideChartBuilder() extends ScgInsideChartBuilder {

  def buildInsideChart(
    guideChart: CfgGuideChart,
    //rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]): ScgInsideChart = {

    val n = guideChart.length
    val supertagSets = Set(se.startTag) +: guideChart.supertagSets :+ Set(se.endTag)
    val table: Vector[Vector[MMap[Cat, MMap[(Cat, Cat), LogDouble]]]] =
      guideChart.matrix.zipWithIndex.mapt { (row, i) =>
        row.zipWithIndex.mapt { (col, j) =>
          MMap() ++ col.mapVals { _ =>
            MMap() ++ (for {
              l <- supertagSets(i)
              r <- supertagSets(j + 1)
            } yield ((l, r) -> LogDouble.zero))
          }
        }
      }

    for {
      (i, j, cell) <- guideChart.bottomUpNodes if cell.nonEmpty // visit all relevant cells
      (ij, entries) <- cell
      entry <- entries
    } entry match {
      case BinaryGuideChartEntry(k, prod @ BinaryProd(ik, kj)) =>
        // TODO: I'm not convinced here: it might over-state things since it's possible that many
        // of those tree combinations don't produce valid trees
        val prodP = prodDist(prod, ij)
        for {
          ikCell <- table(i)(k).get(ik); ((ikLctx, ikRctx), ikP) <- ikCell; lctxP = lctxDist(ikLctx, ij)
          kjCell <- table(k)(j).get(kj); ((kjLctx, kjRctx), kjP) <- kjCell; rctxP = rctxDist(kjRctx, ij)
        } {
          val p = prodP * ikP * kjP * lctxP * rctxP
          if (shouldAdd(p)) table(i)(j).getOrElseUpdate(ij, MMap.empty).updateOrElseWith((ikLctx, kjRctx), LogDouble.zero)(_ + p)
          //println(f"v$i$j($ikLctx <- $ij -> $kjRctx): p($ij => $ik $kj) * v$i$k($ikLctx <- $ik -> $ikRctx) * v$k$j($kjLctx <- $kj -> $kjRctx) * l($ikLctx <- $ij) * r($ij -> $kjRctx) = ${prodP.toDouble} * ${ikP.toDouble} * ${kjP.toDouble} * ${lctxP.toDouble} * ${rctxP.toDouble} = ${p.toDouble} : ${CcgBinode(ij, ikTree, kjTree)}")
          //println(f"i($i,$j,$ij) += p($ij -> $prod) * i($i,$k,$ik) * i($k,$j,$kj) = ${ruleP.toDouble} * ${ikP.toDouble} ${kjP.toDouble} = ${p.toDouble}%.10f   ${p.logValue}")
          //assert(p.nonZero, f"ij=$ij, prod=${prod}, ij=($i,$j), p=${p.logValue}, prodDist=$prodDist")
          //printTable(table)
        }

      case UnaryGuideChartEntry(prod @ UnaryProd(subCat)) =>
        val prodP = prodDist(prod, ij)
        for {
          subCell <- table(i)(j).get(subCat); ((subLctx, subRctx), subP) <- subCell
        } { // skip entries that weren't added to the chart
          val lctxP = lctxDist(subLctx, ij)
          val rctxP = rctxDist(subRctx, ij)
          val p = prodP * subP * lctxP * rctxP
          if (shouldAdd(p)) table(i)(j).getOrElseUpdate(ij, MMap.empty).updateOrElseWith((subLctx, subRctx), LogDouble.zero)(_ + p)
          //println(f"v$i$j($subLctx <- $ij -> $subRctx): p($ij => $subCat) * v$i$j($subLctx <- $subCat -> $subRctx) * l($subLctx <- $ij) * r($ij -> $subRctx) = ${prodP.toDouble} * ${subP.toDouble} * ${lctxP.toDouble} * ${rctxP.toDouble} = ${p.toDouble} : ${CcgUnode(ij, subTree)}")
          //printTable(table)
        }

      case TermGuideChartEntry(prod @ TermProd(word)) =>
        val prodP = prodDist(prod, ij)
        for {
          lctx <- supertagSets(i); /*    */ lctxP = lctxDist(lctx, ij)
          rctx <- supertagSets(j + 1); /**/ rctxP = rctxDist(rctx, ij)
        } {
        	val p = prodP * lctxP * rctxP
          if (shouldAdd(p)) table(i)(j).getOrElseUpdate(ij, MMap.empty).updateOrElseWith((lctx, rctx), LogDouble.zero)(_ + p)
          //println(f"v$i${i + 1}($lctx <- $ij -> $rctx): p($word|$ij) * l($lctx <- $ij) * r($ij -> $rctx) = ${prodP.toDouble} * ${lctxP.toDouble} * ${rctxP.toDouble} = ${p.toDouble} : ${CcgLeaf(ij, word)}")
          //println(f"i($i,${i + 1},$ij) += p($ij -> $prod) = ${p.toDouble}%.10f   ${p.logValue}")
          //printTable(table)
        }
    }

    ??? // new ScgInsideChart(table.map(_.map(_.toMap)))
  }

  private[this] def shouldAdd(p: LogDouble) = {
    assert(!p.isNaN && p.nonZero)
    true
    // p.nonZero
  }

}
