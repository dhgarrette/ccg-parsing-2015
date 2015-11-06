package dhg.ccg.parse.pcfg.mcmc

import dhg.util._
import dhg.util.FastMathUtil._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.prob._
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import scala.collection.mutable.{ ListBuffer => MSeq }
import scalaz._
import Scalaz._
import dhg.ccg.util.DrawMatrix

class PcfgInsideChart(val matrix: Vector[Vector[Map[Cat, LogDouble]]]) {
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

  def draw() {
    DrawMatrix.drawMatrix(matrix.map(_.tail))(_.map {
      case (cat, p) =>
        f"$cat -> ${p.toDouble}"
    }.mkString("\n"))(println)
  }
}

trait PcfgInsideChartBuilder {

  def buildInsideChart(
    guideChart: CfgGuideChart,
    //rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): PcfgInsideChart

}

class SimplePcfgInsideChartBuilder() extends PcfgInsideChartBuilder {

  def buildInsideChart(
    guideChart: CfgGuideChart,
    //rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): PcfgInsideChart = {

    val n = guideChart.length
    val table: Vector[Vector[MMap[Cat, LogDouble]]] =
      guideChart.matrix.zipWithIndex.mapt { (row, i) =>
        row.zipWithIndex.mapt { (col, j) => MMap[Cat, LogDouble]() }
      }

    for {
      (i, j, cell) <- guideChart.bottomUpNodes if cell.nonEmpty // visit all relevant cells
      (ij, entries) <- cell
    } {
      val pValues = new Array[Double](entries.size)
      var pValuesLength = 0
      for (entry <- entries) {
        entry match {
          case BinaryGuideChartEntry(k, prod @ BinaryProd(ik, kj)) =>
            val prodP = prodDist(prod, ij)
            for {
              ikP <- table(i)(k).get(ik) // skip entries that weren't added to the chart
              kjP <- table(k)(j).get(kj)
            } {
              val p = prodP * ikP * kjP
              if (shouldAdd(p)) { pValues(pValuesLength) = p.logValue; pValuesLength += 1 }
              //println(f"v$i$j($ij) += p($ij => $prod) * v$i$k($ik) * v$k$j($kj) = ${prodP.toDouble} * ${ikP.toDouble} ${kjP.toDouble} = ${p.toDouble}")
              //printTable(table)
            }

          case UnaryGuideChartEntry(prod @ UnaryProd(sub)) =>
            val prodP = prodDist(prod, ij)
            for (subP <- table(i)(j).get(sub)) { // skip entries that weren't added to the chart
              val p = prodP * subP
              if (shouldAdd(p)) { pValues(pValuesLength) = p.logValue; pValuesLength += 1 }
              //println(f"v$i$j($ij) += p($ij => $sub) * v$i$j($sub) = ${prodP.toDouble} * ${subP.toDouble} = ${p.toDouble}")
              //printTable(table)
            }

          case TermGuideChartEntry(prod @ TermProd(word)) =>
            val prodP = prodDist(prod, ij)
            val p = prodP
            if (shouldAdd(p)) { pValues(pValuesLength) = p.logValue; pValuesLength += 1 }
          //println(f"v$i$j($ij) += p($word|$ij) = ${p.toDouble}")
          //printTable(table)
        }
      }
      val pSum = new LogDouble(FastMathUtil.logSum(pValues, pValuesLength))
      table(i)(j)(ij) = table(i)(j).get(ij) match {
        case Some(v) => v + pSum
        case None => pSum
      }
    }

    new PcfgInsideChart(table.map(_.map(_.toMap)))
  }

  private[this] def shouldAdd(p: LogDouble) = {
    assert(!p.isNaN)
    //assert(p.nonZero)
    //true
    p.nonZero
  }

  def printTable(table: Vector[Vector[MMap[Cat, LogDouble]]]) {
    DrawMatrix.drawMatrix(table.map(_.tail))(_.map {
      case (cat, p) =>
        f"$cat -> ${p.toDouble}"
    }.mkString("\n"))(println)
  }

}
