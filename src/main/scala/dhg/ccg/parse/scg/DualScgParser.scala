package dhg.ccg.parse.scg

import scala.collection.mutable.{ Set => MSet }
import scala.collection.mutable.{ Map => MMap }
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.parse._
import dhg.ccg.tagdict.TagDictionary
import dhg.util._
import scalaz._
import Scalaz._
import dhg.ccg.parse.pcfg._
import dhg.ccg.prob._
import java.util.concurrent.atomic.AtomicInteger
import dhg.ccg.tagdict.StartEndTags
import dhg.ccg.util.DrawMatrix
import scala.annotation.tailrec

class DualScgParser(
  val rootDist: LogProbabilityDistribution[Cat],
  val prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
  val lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
  val rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
  //weightedParser: WeightedGuideChartParser,
  maxIterations: Int,
  //resultWeighter: ScgWeighter,
  verbose: Boolean = false)(se: StartEndTags[Cat])
  extends GuideChartParser {

  val ddConverge = new AtomicInteger
  val ddAttempts = new AtomicInteger

  def parseAndProbFromGuideChart(guideChart: CfgGuideChart): Option[(CcgTree, LogDouble)] = {
    val n = guideChart.length

    if (verbose) guideChart.draw()

    //if (verbose) println(f"    begin dual decomp iterations")
    ddAttempts.incrementAndGet()
    val umat = Vector.fill(n, n + 1)(Map.empty[Cat, LogDouble])
    val tree = iterate2(guideChart, 1, umat, LogDouble.one, 1)
    //    tree.foreach { case (t,p) => if (verbose) println(t.tagged) }
    tree.map(t => (t, new SimpleScgWeighter().weight(t, rootDist, prodDist, lctxDist, rctxDist)(se)))
  }

  def iterate(guideChart: CfgGuideChart, k: Int, umat: Vector[Vector[Map[Cat, LogDouble]]], prevL: LogDouble, lagrangianIncreases: Int): Option[CcgTree] = {
    val n = guideChart.length

    if (verbose) println(f"SCG Parser DD  iteration=$k/$maxIterations")
    printUTable(umat)

    val delta = new LogDouble(1.0 / lagrangianIncreases) // NOTE: Based on Rush&Collins advice 
    new PcfgParser(rootDist, prodDist).parseAndProbWithWeightsFromGuideChart(guideChart, umat).flatMap {
      case (maxTree, pMaxTree) =>

        if (verbose) println(f"  maxTree:\n${maxTree.pretty.indent(4)}")

        def getAllTreeCats(t: CcgTree, i: Int, j: Int): Vector[(Cat, Int, Int)] = t match {
          case CcgBinode(cat, ik, kj) => ((cat, i, j)) +: (getAllTreeCats(ik, i, i + ik.length) ++ getAllTreeCats(kj, i + ik.length, j))
          case CcgUnode(cat, sub) => ((cat, i, j)) +: getAllTreeCats(sub, i, j)
          case CcgLeaf(cat, word, _) => Vector((cat, i, i + 1))
        }
        val maxTreeCats = getAllTreeCats(maxTree, 0, n)
        val treeNodes: Map[Int, Map[Int, Cat]] =
          maxTreeCats.groupBy(_._2).mapVals { is =>
            is.groupBy(_._3).mapVals { js =>
              js.only._1
            }
          }

        var matches = true
        var pCtxCatProduct = LogDouble.one
        val newUMat =
          umat.zipWithIndex.mapt { (row, i) =>
            row.zipWithIndex.mapt { (u, j) =>
              val gc = guideChart(i)(j)
              if (gc.isEmpty) u
              else {
                if (verbose) println(f"  Handle ($i,$j)")
                val (maxCntxCat, pMaxCntxCat) = gc.keys match {
                  case Coll(cat) =>
                    if (verbose) println(f"    only choice: ${gc.keys.head}")
                    (cat, LogDouble.one)
                  case cats =>
                    val contextProbs =
                      {
                        for {
                          cat <- cats
                          lctx <- if (i == 0) Set(se.startTag) else guideChart(i - 1)(i).keys
                          rctx <- if (j == n) Set(se.endTag) else guideChart(j)(j + 1).keys
                        } yield {
                          val pLctx = lctxDist(lctx, cat)
                          val pRctx = rctxDist(rctx, cat)
                          val p = pLctx * pRctx / umat(i)(j).getOrElse(cat, LogDouble.one)
                          if (verbose) println(f"    $lctx  <--  ${cat}  -->  $rctx  ::  ${p.logValue}")
                          cat -> p
                        }
                      }
                    //                        {
                    //                          val maxTreeSupertags = maxTree.supertags
                    //                          for {
                    //                            cat <- cats
                    //                          } yield {
                    //                            val lctx = if (i == 0) se.startTag else maxTreeSupertags(i - 1)
                    //                            val rctx = if (j == n) se.endTag else maxTreeSupertags(j)
                    //                            val pLctx = lctxDist(lctx, cat)
                    //                            val pRctx = rctxDist(rctx, cat)
                    //                            val p = pLctx * pRctx / umat(i)(j).getOrElse(cat, LogDouble.one)
                    //                            if (verbose) println(f"    $lctx  <--  ${cat}  -->  $rctx  ::  ${p.logValue}")
                    //                            cat -> p
                    //                          }
                    //                        }
                    contextProbs.maxBy(_._2)
                }
                if (verbose) println(f"    maxCntxCat: ${maxCntxCat}")
                val maxTreeCat = treeNodes.get(i).flatMap(_.get(j))
                if (verbose) println(f"    maxTreeCat: ${maxTreeCat.fold("None")(_.toString)}")
                pCtxCatProduct *= pMaxCntxCat
                if (maxTreeCat.exists(_ == maxCntxCat)) {
                  u
                }
                else {
                  val u1 = u.updated(maxCntxCat, u.getOrElse(maxCntxCat, LogDouble.one) * delta)
                  maxTreeCat.fold(u1) { mtc =>
                    matches = false
                    u1.updated(mtc, u.getOrElse(mtc, LogDouble.one) / delta)
                  }
                }
              }
            }
          }

        //if (verbose) println(f"    k=${(k + ":").padRight(4)} ${(System.currentTimeMillis() - startTime) / 1000.0}%.3f sec")
        if (matches || k >= maxIterations) { // matches. stop.
          ddConverge.incrementAndGet()
          Some(maxTree)
        }
        else {
          val l = pMaxTree * pCtxCatProduct // Lagrangian object L(u)
          iterate(guideChart, k + 1, newUMat, l, lagrangianIncreases + (if (l > prevL) 1 else 0)) // next iteration
        }
    }
  }

  final def iterate2(guideChart: CfgGuideChart, k: Int, umat: Vector[Vector[Map[Cat, LogDouble]]], prevL: LogDouble, lagrangianIncreases: Int): Option[CcgTree] = {
    if (verbose) println(f"SCG Parser DD  iteration=$k/$maxIterations")
    printUTable(umat)

    val delta = new LogDouble(1.0 / lagrangianIncreases) // NOTE: Based on Rush&Collins advice 
    new PcfgParser(rootDist, prodDist).parseAndProbWithWeightsFromGuideChart(guideChart, umat).flatMap {
      case (maxFullTree, pMaxFullTree) =>
        val (newUMat, matches, lagrangian) = computeNewU2(guideChart, umat, delta, maxFullTree.cat, pMaxFullTree)

        //if (verbose) println(f"    k=${(k + ":").padRight(4)} ${(System.currentTimeMillis() - startTime) / 1000.0}%.3f sec")
        if (matches || k >= maxIterations) { // matches. stop.
          ddConverge.incrementAndGet()
          Some(maxFullTree)
        }
        else {
          iterate2(guideChart, k + 1, newUMat, lagrangian, lagrangianIncreases + (if (lagrangian > prevL) 1 else 0)) // next iteration
        }
    }
  }

  /**
   * @return (newUMat: Vector[Vector[Map[Cat, LogDouble]]], matches: Boolean, lagrangian: LogDouble)
   */
  def computeNewU2(guideChart: CfgGuideChart, umat: Vector[Vector[Map[Cat, LogDouble]]], delta: LogDouble, maxTreeCat: Cat, pMaxTreeCat: LogDouble): (Vector[Vector[Map[Cat, LogDouble]]], Boolean, LogDouble) = {
    val n = guideChart.length

    var matches = true
    var lagrangian = LogDouble.one // Lagrangian object L(u)

    // Traverse all cells, updating u in each cell as appropriate
    val newUMat =
      umat.zipWithIndex.mapt { (row, i) =>
        row.zipWithIndex.mapt { (u, j) =>
          val gcCell = guideChart(i)(j)
          gcCell.keys match {
            case Coll() => // cell is empty.  u doesn't change.
              if (verbose) println(f"    empty cell"); u
            case Coll(cat) => // cell only has one category option.  u doesn't change.
              if (verbose) println(f"    only choice: ${gcCell.keys.only}"); u
            case Coll(cats @ _*) => // cell has multiple category options.  find the best under each model and update u if there is a conflict.
              if (verbose) println(f"  Handle ($i,$j)")

              val (maxSubtreeCat, pMaxSubtreeCat) = getMaxSubtreeCat(i, j, guideChart, umat, maxTreeCat, pMaxTreeCat)
              val (maxContextCat, pMaxContextCat) = getMaxContextCat(cats, i, j, guideChart, umat, n)

              if (verbose) println(f"    maxSubtreeCat: ${maxSubtreeCat}")
              if (verbose) println(f"    maxCntxCat:    ${maxContextCat}")

              lagrangian *= (pMaxSubtreeCat * pMaxContextCat)
              if (maxSubtreeCat == maxContextCat) {
                u
              }
              else {
                matches = false
                u
                  .updated(maxSubtreeCat, u.getOrElse(maxSubtreeCat, LogDouble.one) / delta)
                  .updated(maxContextCat, u.getOrElse(maxContextCat, LogDouble.one) * delta)
              }
          }
        }
      }

    (newUMat, matches, lagrangian)
  }

  def sliceMatrix[T](m: Vector[Vector[T]], i: Int, j: Int) = m.slice(i, j).map(row => row.slice(i, j + 1))

  def getMaxSubtreeCat(i: Int, j: Int, guideChart: CfgGuideChart, umat: Vector[Vector[Map[Cat, LogDouble]]], maxTreeCat: Cat, pMaxTreeCat: LogDouble): (Cat, LogDouble) = {
    if (i == 0 && j == guideChart.length)
      (maxTreeCat, pMaxTreeCat)
    else {
      val parser = new PcfgParser(new UniformDefaultLogProbabilityDistribution(LogDouble.one), prodDist)
      val guideChartSubset = CfgGuideChart(guideChart.words, sliceMatrix(guideChart.matrix, i, j), UniversalSet())
      val umatSubset = sliceMatrix(umat, i, j)
      val (t, p) = parser.parseAndProbWithWeightsFromGuideChart(guideChartSubset, umatSubset).get
      if (verbose) println(f"    ${p.logValue}\n${t.pretty.indent(4)}")
      (t.cat, p)
    }
  }

  def getMaxContextCat(cats: Seq[Cat], i: Int, j: Int, guideChart: CfgGuideChart, umat: Vector[Vector[Map[Cat, LogDouble]]], n: Int): (Cat, LogDouble) = {
    val contextProbs =
      for {
        cat <- cats
        lctx <- if (i == 0) Set(se.startTag) else guideChart(i - 1)(i).keys
        rctx <- if (j == n) Set(se.endTag) else guideChart(j)(j + 1).keys
      } yield {
        val pLctx = lctxDist(lctx, cat)
        val pRctx = rctxDist(rctx, cat)
        val p = pLctx * pRctx / umat(i)(j).getOrElse(cat, LogDouble.one)
        if (verbose) println(f"    $lctx  <--  ${cat}  -->  $rctx  ::  ${p.logValue}")
        cat -> p
      }
    contextProbs.maxBy(_._2)
  }

  def printUTable(table: Vector[Vector[Map[Cat, LogDouble]]]) {
    DrawMatrix.drawMatrix(table.map(_.tail))(_.map {
      case (cat, u) => f"$cat -> ${u.toDouble}"
    }.mkString("\n"))(println)
  }
  def printTreeTable(tree: CcgTree) {
    val a = Array.fill(tree.length)(Array.fill[Option[Cat]](tree.length + 1)(None))
    def r(t: CcgTree, i: Int, j: Int): Unit = t match {
      case CcgBinode(cat, ik, kj) =>
        a(i)(j) = Some(cat)
        r(ik, i, i + ik.length)
        r(kj, i + ik.length, j)
      case CcgUnode(cat, sub) =>
        assert(i + 1 < j)
        a(i)(j) = Some(cat)
        r(sub, i, j)
      case CcgLeaf(cat, word, _) =>
    }
    r(tree, 0, tree.length)
    DrawMatrix.drawMatrix(a.map(_.toVector.tail).toVector)(_.fold("")(_.toString))(println)
  }
  def printCtxTreeTable(table: Vector[Vector[Map[Cat, LogDouble]]]) {
    DrawMatrix.drawMatrix(table.map(_.tail))(_.map {
      case (cat, u) => f"$cat -> ${u.toDouble}"
    }.mkString("\n"))(println)
  }
}
