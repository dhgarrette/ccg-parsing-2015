package dhg.ccg.parse.pcfg

import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import scala.collection.mutable
import scala.math.{ log, exp }
import scalaz.{ Ordering => _, _ }
import Scalaz._
import dhg.util._
import dhg.ccg.prob._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.parse._
import dhg.ccg.util.DrawMatrix
import dhg.ccg.parse.pcfg.mcmc.PcfgTreeSampler
import dhg.ccg.data.EnglishCcgTreeBankReader
import dhg.ccg.tagdict.SimpleTagDictionaryFactory
import dhg.ccg.parse.dep.DepParserEvaluator
import dhg.ccg.parse.pcfg.mcmc.PcfgProductionCounter
import dhg.ccg.parse.pcfg.mcmc.SimplePcfgProductionCounter
import dhg.ccg.parse.pcfg.typesup._
import dhg.ccg.tag.learn.CatgramCatPriorInitializer
import dhg.ccg.tag.learn.TagdictInformedAtomCatDistInitializer
import dhg.ccg.tag.learn.TagDictionaryEstimateTagPriorInitializer
import dhg.ccg.tag.learn.EmTagDictionaryEstimate
import dhg.ccg.parse.scg.exp.Em2TermProdDist
import dhg.ccg.data.RuleViolatedRemovingTreeBankReader

class PcfgParser(
  val rootDist: LogProbabilityDistribution[Cat],
  val prodDist: ConditionalLogProbabilityDistribution[Cat, Prod])
    extends WeightedKBestGuideChartParser
    with TreeWeighter {

  private[this] val weighter = new SimplePcfgWeighter()

  def parseAndProbKBestWithWeightsFromGuideChart(guideChart: CfgGuideChart, us: Vector[Vector[Map[Cat, LogDouble]]], k: Int): Vector[(CcgTree, LogDouble)] = {
    val n = guideChart.length
    val table: Vector[Vector[Map[Cat, FastKMaxPriorityQueue[CcgTree]]]] =
      guideChart.matrix.map { row =>
        if (row != null)
          row.map { col =>
            if (col != null)
              col.mapVals { _ =>
                FastKMaxPriorityQueue.empty[CcgTree](k)
              }
            else null
          }
        else null
      }

    for {
      (i, j, cell) <- guideChart.bottomUpNodes if cell.nonEmpty // visit all relevant cells
      u = us(i)(j)
      (ij, entries) <- cell // cell is sorted according to unary rule dependency relationships
      entry <- entries
    } entry match {
      case BinaryGuideChartEntry(k, prod @ BinaryProd(ik, kj)) =>
        val prodP = prodDist(prod, ij)

        //        // TODO: DEBUG START
        //        if (!table(i)(k).contains(ik)) {
        //          println(f"  ij=$ij  =>  ik=$ik  kj=$kj")
        //          println(f"    guideChart(i,k) = ${guideChart(i, k).keySet.toVector.map(_.toString).sorted}")
        //          println(f"    table(i)(k) =     ${table(i)(k).keySet.toVector.map(_.toString).sorted}")
        //        }
        //        if (!FA(ik, kj).contains(ij) && !BA(ik, kj).contains(ij)) {
        //          println(f"Invalid rule!  $ij  ->  $ik  $kj")
        //        }
        //        // TODO: DEBUG END

        for {
          (ikTree, ikP) <- table(i)(k)(ik).iterator
          (kjTree, kjP) <- table(k)(j)(kj).iterator
        } {
          val p = prodP * ikP * kjP * u.getOrElse(ij, LogDouble.one)
          if (shouldAdd(p)) table(i)(j)(ij).add(CcgBinode(ij, ikTree, kjTree), p)
          //println(f"""v$i$j(${ij.toString.toLowerCase.replace(" ", "")}):  p(${ij.toString.toLowerCase.replace(" ", "")} -> ${ik.toString.toLowerCase.replace(" ", "")} ${kj.toString.toLowerCase.replace(" ", "")}) = ${prodP.toDouble} * ${ikP.toDouble} * ${kjP.toDouble} = ${p.toDouble} : CcgBinode($ij, $ikTree, $kjTree)""")
          //printTable(table)
        }

      case UnaryGuideChartEntry(prod @ UnaryProd(subCat)) =>
        val prodP = prodDist(prod, ij)
        for ((subTree, subP) <- table(i)(j)(subCat).iterator) {
          val p = prodP * subP * u.getOrElse(ij, LogDouble.one)
          if (shouldAdd(p)) table(i)(j)(ij).add(CcgUnode(ij, subTree), p)
          //println(f"""v$i$j(${ij.toString.toLowerCase.replace(" ", "")}):  p(${ij.toString.toLowerCase.replace(" ", "")} -> ${subCat.toString.toLowerCase.replace(" ", "")}) = ${prodP.toDouble} * ${subP.toDouble} = ${p.toDouble} : CcgUnode($ij, $subTree)""")
          //printTable(table)
        }

      case TermGuideChartEntry(prod @ TermProd(word)) =>
        val prodP = prodDist(prod, ij)
        val p = prodP * u.getOrElse(ij, LogDouble.one)
        if (shouldAdd(p)) table(i)(j)(ij).add(CcgLeaf(ij, word, "FAKEPOS"), p)
      //println(f"""v$i$j(${ij.toString.toLowerCase.replace(" ", "")}):  p($word|${ij.toString.toLowerCase.replace(" ", "")}) = ${prodP.toDouble} = ${p.toDouble} : CcgLeaf($ij, "$word")""")
      //printTable(table)
    }

    (for {
      (ij, cell) <- table(0)(n).toVector
      if guideChart.rootSet.contains(ij)
      rootP = rootDist(ij)
      (tree, treeP) <- cell.iterator
      p = rootP * treeP
      //_ = println(f"""p(${ij.toString.toLowerCase.replace(" ", "")}) * v0$n(${ij.toString.toLowerCase.replace(" ", "")}) = ${rootP.toDouble} * ${treeP.toDouble} = ${p.toDouble} : $t""")
      if shouldAdd(p)
    } yield (tree, p)).maxByN(k)(_._2)
  }

  private[this] def shouldAdd(p: LogDouble) = {
    assert(!p.isNaN)
    //assert(p.nonZero)
    p.nonZero
  }

  def printTable(table: Vector[Vector[Map[Cat, FastKMaxPriorityQueue[CcgTree]]]]) {
    DrawMatrix.drawMatrix(table.map(_.tail))(_.map {
      case (cat, q) =>
        val qvals = q.toVector.map { case (t, p) => f"(${p.toDouble}%.6f, $t)" }
        val left = f"$cat -> {"
        f"$left${qvals.mkString(",\n" + " " * left.length)}}"
    }.mkString("\n"))(println)
  }

  def weight(t: CcgTree): LogDouble = weighter.weight(t, rootDist, prodDist)

}

class SamplingPcfgParser(
  rootDist: LogProbabilityDistribution[Cat],
  prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
  treeSampler: PcfgTreeSampler,
  weighter: PcfgWeighter)
    extends AbstractKBestGuideChartParser {

  def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int): Vector[(CcgTree, LogDouble)] = {
    val trees = treeSampler.samples(guideChart, rootDist, prodDist, k).distinct.mapTo(weighter.weight(_, rootDist, prodDist))
    trees.sorted(Ordering.by[(CcgTree, LogDouble), LogDouble](_._2).reverse)
  }
}

class CompositePcfgParser(
  delegateA: KBestGuideChartParser, kA: Int,
  delegateB: KBestGuideChartParser)
    extends AbstractKBestGuideChartParser {

  def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int): Vector[(CcgTree, LogDouble)] = {
    val kB = (k - kA) max 0
    val aTrees = time1("CompositePcfgParser aTrees", delegateA.parseAndProbKBestFromGuideChart(guideChart, k min kA))
    val bTrees = time1("CompositePcfgParser bTrees", (if (kB > 0) delegateB.parseAndProbKBestFromGuideChart(guideChart, kB) else Vector.empty))
    println(f"CompositePcfgParser: num aTrees = ${aTrees.size}; num bTrees = ${bTrees.size};  (${(aTrees ++ bTrees).map(_._1).toSet.size} unique)")
    (aTrees ++ bTrees).distinctBy(_._1).sorted(Ordering.by[(CcgTree, LogDouble), LogDouble](_._2).reverse)
  }
}

//
//

object PcfgParser {

  def main(args: Array[String]): Unit = {

    val rules = CcgRules.nonComp

    val reader = new RuleViolatedRemovingTreeBankReader(new SimpleRuleViolationFinder(CcgRules.all, allowAllUnary = true), EnglishCcgTreeBankReader())

    val trainingData = time("read raw", reader.rawDataDONTUSE.toVector)
    val rawData = trainingData.map(_.words)
    val tdData = time("read td", reader.tdData.toVector)
    val testingData = time("read test", reader.testData.toVector)

    val tagdict = time("make td", new SimpleTagDictionaryFactory().apply(tdData.map(_.tagged), "<S>", cat"<S>", "<E>", cat"<E>", Set.empty, Set.empty))

    val catPriorInit = new CatgramCatPriorInitializer(new TagdictInformedAtomCatDistInitializer(1.0), pTerm = 0.8, pMod = 0.6, pFwd = 0.5)
    val catPrior = time("make catprior", catPriorInit.fromRaw(rawData, tagdict))

    val priorRootDist = catPrior
    val priorBinyProd = new BinaryPriorDist(catPrior)
    val priorBinyDist = new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, BinaryProd](priorBinyProd)
    val priorUnryProd = new UnaryPriorDist(catPrior)
    val priorUnryDist = new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, UnaryProd](priorUnryProd)
    val priorTermDist = time("make term prior", Em2TermProdDist(new EmTagDictionaryEstimate(catPriorInit).fromRaw(rawData, tagdict)))
    val priorProdProd = new IPDU(priorBinyProd, priorUnryProd, new ConditionalWrappingLogProbabilityDistribution[Cat, TermProd](cat"fake !!", priorTermDist), LogDouble(1.0 / 3), LogDouble(1.0 / 3), LogDouble(1.0 / 3))
    val priorProdDist = new ICPDU(priorBinyDist, priorUnryDist, priorTermDist, Map().withDefaultValue(LogDouble(1.0 / 3), LogDouble(1.0 / 3), LogDouble(1.0 / 3)))

    val prodFinder = new SimplePcfgProductionCounter()
    val rootCounts = time("make root counts", trainingData.map(prodFinder.rootCounts).reduce(_ |+| _).mapVals(LogDouble(_)))
    val prodCounts = time("make prod counts", trainingData.map(prodFinder.prodCounts).reduce(_ |+| _).mapVals(_.mapVals(LogDouble(_))))

    val rootDist = time("make root dist", new AlphaBetaLogProbabilityDistribution(rootCounts, LogDouble(1.0), priorRootDist))
    val prodDist = time("make prod dist", new AlphaBetaConditionalLogProbabilityDistribution(prodCounts.mapVals { counts => new AlphaBetaLogProbabilityDistribution(counts, LogDouble(1.0), priorProdProd) }, LogDouble(1.0), priorProdDist))

    val parser = new PcfgParser(rootDist, prodDist)

    val guideChartBuilder = new SimpleCfgGuideChartBuilder(rules, allowTerminalDeletion = false)
    val testingGCs = time("make test guidecharts", testingData.iterator.map(t => (guideChartBuilder.build(t.words, None, tagdict), t)))
    val evaluator = new DepParserEvaluator(None, true)
    time("evaluate", evaluator.evaluate(parser, testingGCs, tagdict))

  }

}
