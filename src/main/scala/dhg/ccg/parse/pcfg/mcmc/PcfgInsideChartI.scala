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
import dhg.ccg.util._
import java.util.Arrays

class PcfgInsideChartI(val matrix: Chart[IndirectSparseVec[Double]]) { // chart[t -> p]
  def apply(row: Int, col: Int) = matrix(row, col)
  def root = matrix.root
  def length = matrix.length

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

  //  def draw(catIndexer: Indexer[Cat]) {
  //    DrawMatrix.drawMatrix(matrix)(_.activePairs.map {
  //      case (catI, p) =>
  //        f"${catIndexer.obj(catI)} -> ${p.toDouble}"
  //    }.mkString("\n"))
  //  }
}

trait PcfgInsideChartBuilderI {
  type Cat = Int

  def buildInsideChart(
    guideChart: CfgGuideChartI,
    //logRootDist: IndirectSparseVec[Double], //                               t -> p
    logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
    logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
    logTermDist: Array[Vec[Double]], //                                        t -> w -> p
    logPmixDist: Array[Array[Double]], //                                      t -> p
    numCats: Int): PcfgInsideChartI

}

class SimplePcfgInsideChartBuilderI(
  catIndexer: Indexer[Cat], wordIndexer: Indexer[String])
    extends PcfgInsideChartBuilderI {

  def buildInsideChart(
    guideChart: CfgGuideChartI,
    //logRootDist: IndirectSparseVec[Double], //                               t -> p
    logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
    logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
    logTermDist: Array[Vec[Double]], //                                        t -> w -> p
    logPmixDist: Array[Array[Double]], //                                      t -> p
    numCats: Int): PcfgInsideChartI = {

    val n = guideChart.length
    val logTable: Chart[IndirectSparseVec[Double]] = Chart.empty(n)

    val bottomUpNodes = guideChart.bottomUpNodes.toArray
    val bottomUpNodesLen = bottomUpNodes.length
    var bottomUpNodesi = 0
    while (bottomUpNodesi < bottomUpNodesLen) {
      val indexedCell = bottomUpNodes(bottomUpNodesi)
      val i = indexedCell._1
      val j = indexedCell._2
      val cell = indexedCell._3
      val cellLen = cell.length
      //println(f"gdhigiso  CELL ($i,$j)")

      val insideCellData = new Array[(Int, Double)](cellLen)
      val insideCellKeys = new Array[Int](cellLen)

      var celli = 0
      while (celli < cellLen) {

        val (ij, entries) = cell(celli)
        val numEntries = entries.size
        val pValues = new Array[Double](numEntries)
        var entriesi = 0
        while (entriesi < numEntries) {
          val entry = entries(entriesi)
          if (entry.isInstanceOf[BinaryGuideChartEntryI]) {
            val binaryEntry = entry.asInstanceOf[BinaryGuideChartEntryI]
            val k = binaryEntry.k
            val ik = binaryEntry.ik
            val kj = binaryEntry.kj
            //printTable(logTable)
            //println(f"      ($i,$j, ${catIndexer.obj(ij)}) --> BinaryGuideChartEntry($k, ${catIndexer.obj(ik)}, ${catIndexer.obj(kj)})  =  ${logBinyDist(ij)(ik)(kj)}")
            //println(f"($i,$k,$j)   logBinyDist(ij)(ik)(kj) = logBinyDist($ij)(${catIndexer.obj(ik)})(${catIndexer.obj(kj)}) = ${logBinyDist(ij)(ik)(kj)}")
            //println(f"($i,$k,$j)   logPmixDist(ij)(0) = logPmixDist($ij)(0) = ${logPmixDist(ij)(0)}")
            val logProdP = logBinyDist(ij)(ik)(kj) + logPmixDist(ij)(0)
            val ikP = logTable(i, k)(ik)
            //println(f"gdhigiso          need!  at kj=$kj  in  ($k,$j),  found IndirectSparseVec(${logTable(k, j).activeKeys.mkString("[", ", ", "]")}, ${logTable(k, j).activeCount}, ${logTable(k, j).length})")
            val kjP = logTable(k, j)(kj)
            val logP = logProdP + ikP + kjP
            //println(f"NaN found: logProdP=$logProdP=${math.exp(logProdP)} + ikP=$ikP=${math.exp(ikP)} + kjP=$kjP=${math.exp(kjP)}")
            //assert(!logP.isNaN, f"NaN found: logProdP=$logProdP=${math.exp(logProdP)} + ikP=$ikP=${math.exp(ikP)} + kjP=$kjP=${math.exp(kjP)}")
            pValues(entriesi) = logP
          }
          else if (entry.isInstanceOf[UnaryGuideChartEntryI]) {
            val unaryEntry = entry.asInstanceOf[UnaryGuideChartEntryI]
            val sub = unaryEntry.sub
            //printTable(logTable)
            //println(f"      ($i,$j, ${catIndexer.obj(ij)}) --> UnaryGuideChartEntry(${catIndexer.obj(sub)})  ")
            val logProdP = logUnryDist(ij)(sub) + logPmixDist(ij)(1)
            //println(f"gdhigiso      looking for sub=$sub  in  celli=$celli;    insideCellKeys=${insideCellKeys.mkString("[", ", ", "]")}")
            val subIdx = getIndexOf(sub, insideCellKeys, 0, celli)
            //println(f"gdhigiso          found!  at $subIdx")
            val subP = insideCellData(subIdx)._2 // insideCell(sub) // logTable(i, j)(sub)
            val logP = logProdP + subP
            pValues(entriesi) = logP
          }
          else {
            val termEntry = entry.asInstanceOf[TermGuideChartEntryI]
            val word = termEntry.w
            //printTable(logTable)
            //println(f"      ($i,$j, ${catIndexer.obj(ij)}) --> TermGuideChartEntry(${wordIndexer.obj(word)})  ")
            // TODO: ij not found in termProds
            //println(f"        looking for ${catIndexer.obj(ij)} in [${termProds.activePairs.map { case (t, p) => f"${catIndexer.obj(t)} -> $p" }.mkString(", ")}]")
            //println(f"SimplePcfgInsideChartBuilderI.buildInsideChart::   TermGuideChartEntryI(${wordIndexer.obj(word)}%-10s)     prodDist(${catIndexer.obj(ij)}%-20s) = ${prodDist(ij)}     termProds(ij)")
            //println(f"                                                                                                                                 termProds(${catIndexer.obj(ij)}%-20s) = ${prodDist(ij)}")
            val logProdP = logTermDist(ij)(word) + logPmixDist(ij)(2)
            val logP = logProdP
            pValues(entriesi) = logP
          }

          entriesi += 1
        }

        insideCellKeys(celli) = ij
        insideCellData(celli) = (ij, FastMathUtil.logSum(pValues, numEntries))
        //println(f"gdhigiso      inserted at celli=$celli  =>  ij=$ij;    insideCellKeys=${insideCellKeys.mkString("[", ", ", "]")}")
        celli += 1
      }

      val (insideCellActiveKeysSorted, insideCellActiveValues) = insideCellData.sortBy(_._1).unzip
      val insideCell = new IndirectSparseVec(insideCellActiveKeysSorted, insideCellActiveValues, cellLen, numCats)
      logTable(i, j) = insideCell

      bottomUpNodesi += 1
    }

    //printTable(logTable)
    new PcfgInsideChartI(logTable)
  }

  def getIndexOf(v: Int, a: Array[Int], start: Int, end: Int): Int = {
    var i = start
    while (i < end) {
      if (a(i) == v) return i
      i += 1
    }
    sys.error(f"not found: $v in ${a.mkString("[", ", ", "]")} between ($start,$end)")
  }

  def printTable(logTable: Chart[IndirectSparseVec[Double]]) {
    DrawMatrix.drawMatrix(logTable) { xs => if (xs == null) "null" else xs.activePairs.map { case (cat, p) => f"${catIndexer.obj(cat)} -> ${math.exp(p)}" }.mkString("\n") }(println)
  }

}
