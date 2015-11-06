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
import dhg.ccg.tagdict.StartEndTags
import dhg.ccg.parse.pcfg.FastKMaxPriorityQueue
import dhg.ccg.parse.AbstractKBestGuideChartParser

class ExactScgParser(
  val rootDist: LogProbabilityDistribution[Cat],
  val prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
  val lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
  val rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat])
    extends AbstractKBestGuideChartParser
    with TreeWeighter {

  private[this] val weighter = new SimpleScgWeighter()

  def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int): Vector[(CcgTree, LogDouble)] = {
    val n = guideChart.length
    val supertagSets = Set(se.startTag) +: guideChart.supertagSets :+ Set(se.endTag)
    val table: Vector[Vector[Map[Cat, Map[(Cat, Cat), FastKMaxPriorityQueue[CcgTree]]]]] =
      guideChart.matrix.zipWithIndex.mapt { (row, i) =>
        if (row != null)
          row.zipWithIndex.mapt { (col, j) =>
            if (col != null)
              col.mapVals { _ =>
                (for {
                  l <- supertagSets(i)
                  r <- supertagSets(j + 1)
                } yield ((l, r) -> FastKMaxPriorityQueue.empty[CcgTree](k))).toMap
              }
            else null
          }
        else null
      }

    for {
      (i, j, cell) <- guideChart.bottomUpNodes if cell.nonEmpty // visit all relevant cells
      (ij, entries) <- cell // cell is sorted according to unary rule dependency relationships
      entry <- entries
    } entry match {
      case BinaryGuideChartEntry(k, prod @ BinaryProd(ik, kj)) =>
        val prodP = prodDist(prod, ij)
        for {
          ((ikLctx, ikRctx), ikCell) <- table(i)(k)(ik); lctxP = lctxDist(ikLctx, ij); (ikTree, ikP) <- ikCell.iterator
          ((kjLctx, kjRctx), kjCell) <- table(k)(j)(kj); rctxP = rctxDist(kjRctx, ij); (kjTree, kjP) <- kjCell.iterator
          if ikRctx == kjTree.supertags.head // contexts must line up with subtrees!
          if kjLctx == ikTree.supertags.last
        } {
          val p = prodP * ikP * kjP * lctxP * rctxP
          if (shouldAdd(p)) table(i)(j)(ij)((ikLctx, kjRctx)).add(CcgBinode(ij, ikTree, kjTree), p)
          //println(f"v$i$j($ikLctx <- $ij -> $kjRctx): p($ij => $ik $kj) * v$i$k($ikLctx <- $ik -> $ikRctx) * v$k$j($kjLctx <- $kj -> $kjRctx) * l($ikLctx <- $ij) * r($ij -> $kjRctx) = ${prodP.toDouble} * ${ikP.toDouble} * ${kjP.toDouble} * ${lctxP.toDouble} * ${rctxP.toDouble} = ${p.toDouble} : ${CcgBinode(ij, ikTree, kjTree)}")
          //printTable(table)
        }

      case UnaryGuideChartEntry(prod @ UnaryProd(subCat)) =>
        val prodP = prodDist(prod, ij)
        for {
          ((subLctx, subRctx), subCell) <- table(i)(j)(subCat)
          lctxP = lctxDist(subLctx, ij)
          rctxP = rctxDist(subRctx, ij)
          (subTree, subP) <- subCell.iterator
        } {
          val p = prodP * subP * lctxP * rctxP
          if (shouldAdd(p)) table(i)(j)(ij)((subLctx, subRctx)).add(CcgUnode(ij, subTree), p)
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
          if (shouldAdd(p)) table(i)(j)(ij)((lctx, rctx)).add(CcgLeaf(ij, word, "FAKEPOS"), p)
          //println(f"v$i${i + 1}($lctx <- $ij -> $rctx): p($word|$ij) * l($lctx <- $ij) * r($ij -> $rctx) = ${prodP.toDouble} * ${lctxP.toDouble} * ${rctxP.toDouble} = ${p.toDouble} : ${CcgLeaf(ij, word)}")
          //printTable(table)
        }
    }

    (for {
      (ij, cell) <- table(0)(n).toVector
      if guideChart.rootSet.contains(ij)
      rootP = rootDist(ij)
      (tree, treeP) <- cell.values.flatMap(_.iterator)
      p = rootP * treeP
      //_ = println(f"p($ij) * v0$n(${se.startTag} <- $ij -> ${se.endTag}) = ${rootP.toDouble} * ${treeP.toDouble} = ${p.toDouble} : ${tree}")
      if shouldAdd(p)
    } yield (tree, p)).maxByN(k)(_._2)
  }

  def weight(tree: CcgTree): LogDouble = weighter.weight(tree, rootDist, prodDist, lctxDist, rctxDist)(se)
  def pcfgWeight(tree: CcgTree): LogDouble = weighter.pcfgWeight(tree, rootDist, prodDist)
  def ctxWeight(tree: CcgTree): LogDouble = weighter.ctxWeight(tree, lctxDist, rctxDist)(se)

  private[this] def shouldAdd(p: LogDouble) = {
    assert(!p.isNaN)
    //assert(p.nonZero)
    p.nonZero
  }
}
