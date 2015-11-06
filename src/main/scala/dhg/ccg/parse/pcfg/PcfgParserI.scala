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
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.tag.learn._
import dhg.ccg.parse.scg.exp.Em2TermProdDist
import dhg.ccg.data.RuleViolatedRemovingTreeBankReader
import dhg.ccg.util._
import scala.collection.immutable.BitSet
import dhg.ccg.parse.pcfg.typesup._

class PcfgParserI(
  val logRootDist: IndirectSparseVec[Double], //                                 t -> p
  val logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
  val logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
  val logTermDist: Array[Vec[Double]], //                                        t -> w -> p
  val logPmixDist: Array[Array[Double]]) //                                      t -> p
  (val catIndexer: Indexer[Cat], val wordIndexer: Indexer[String]) //
    //    extends WeightedKBestGuideChartParserI
    extends TreeWeighterI {

  private[this]type Cat = Int

  private[this] val weighter = new SimplePcfgWeighterI()

  def parseAndLogProbFromGuideChart(guideChart: CfgGuideChartI): (CcgTreeI, Double) = {
    val n = guideChart.length
    val table: Chart[MMap[Cat, (CcgTreeI, Double)]] = Chart.fill(n)(MMap.empty)
    //      guideChart.matrix.map(_.map(_.mapVals(_ => FastKMaxPriorityQueue.empty[CcgTree](k))))

    //println(f"parseAndProbKBestWithWeightsFromGuideChart:")
    for {
      (i, j, cell) <- guideChart.bottomUpNodes if cell.nonEmpty // visit all relevant cells
      (ij, entries) <- cell // cell is sorted according to unary rule dependency relationships
      entry <- entries
    } {
      //printTable(table)

      entry match {
        case BinaryGuideChartEntryI(k, ik, kj) =>
          //println(f"dkjldksjfs1::      ($i,$j, ${catIndexer.obj(ij)}) --> entry = BinaryGuideChartEntry($k, ${catIndexer.obj(ik)}, ${catIndexer.obj(kj)})  ")
          val logProdP = logBinyDist(ij)(ik)(kj) + logPmixDist(ij)(0)
          val (ikTree, ikP) = table(i, k)(ik)
          val (kjTree, kjP) = table(k, j)(kj)
          val logP = logProdP + ikP + kjP

          val existing = table(i, j).get(ij)
          if (existing.isEmpty || logP >= existing.get._2) {
            table(i, j)(ij) = CcgBinodeI(ij, ikTree, kjTree) -> logP
          }

        case UnaryGuideChartEntryI(sub) =>
          //println(f"dkjldksjfs2::      ($i,$j, ${catIndexer.obj(ij)}) --> entry = UnaryGuideChartEntry(${catIndexer.obj(sub)})  ")
          val logProdP = logUnryDist(ij)(sub) + logPmixDist(ij)(1)
          //println(f"weiaosfjoeiaj5::    ${catIndexer.obj(sub)} : ${table(i, j).map { case (cat, (tree, p)) => (catIndexer.obj(cat), (CcgTreeI.from(tree, catIndexer, wordIndexer), p)) }}")
          val (subTree, subP) = table(i, j).getOrElse(sub, {
            //writeUsing(File("gc.txt")) { f => CfgGuideChartI.from(guideChart, catIndexer, wordIndexer).draw(f.wl) }
            Console.err.println(f"cell:")
            for ((cij, centries) <- cell) { Console.err.println(f"    ${catIndexer.obj(cij)}, ${centries.toVector.map(GuideChartEntryI.from(_, catIndexer, wordIndexer))}") }
            Console.err.println(f"ij=${catIndexer.obj(ij)}, entries=${entries.toVector.map(GuideChartEntryI.from(_, catIndexer, wordIndexer))}")
            sys.error(f"xkhjvxdbkhkfc1::    ($i,$j)  ${catIndexer.obj(sub)} : ${table(i, j).map { case (cat, (tree, p)) => (catIndexer.obj(cat), (CcgTreeI.from(tree, catIndexer, wordIndexer), p)) }}")
          })
          val logP = logProdP + subP

          val existing = table(i, j).get(ij)
          if (existing.isEmpty || logP >= existing.get._2) {
            table(i, j)(ij) = CcgUnodeI(ij, subTree) -> logP
          }

        case TermGuideChartEntryI(word) =>
          //println(f"dkjldksjfs3::      ($i,$j, ${catIndexer.obj(ij)}) --> entry = TermGuideChartEntry(${wordIndexer.obj(word)})  ")
          val logProdP = logTermDist(ij)(word) + logPmixDist(ij)(2)
          val logP = logProdP

          table(i, j)(ij) = CcgLeafI(ij, word) -> logP
      }
    }

    (for {
      (ij, (tree, logTreeP)) <- table(0, n)
      logRootP = logRootDist(ij)
      logP = logRootP + logTreeP
    } yield (tree, logP)).maxBy(_._2)
  }

  //  def printTable(table: Chart[MMap[Cat, (CcgTreeI, Double)]]) {
  //    DrawMatrix.drawMatrix(table)(_.map {
  //      case (cat, (t, p)) => f"$cat -> (${p.toDouble}%.6f, $t)"
  //    }.mkString("\n"))
  //  }

  def printTable(table: Chart[MMap[Cat, (CcgTreeI, Double)]]) {
    DrawMatrix.drawMatrix(table)(_.map {
      case (cat, (t, p)) => f"${catIndexer.obj(cat)} -> (${math.exp(p)}%.6f, ${
        t match {
          case CcgBinodeI(`cat`, l, r) => f"${catIndexer.obj(cat)}:[${catIndexer.obj(l.cat)} ${catIndexer.obj(r.cat)}]"
          case CcgUnodeI(`cat`, s) => f"${catIndexer.obj(cat)}:[${catIndexer.obj(s.cat)}]"
          case CcgLeafI(`cat`, w) => f"${catIndexer.obj(cat)}:[${wordIndexer.obj(w)}]"
        }
      })"
    }.mkString("\n"))(println)
  }

  def logWeight(tree: CcgTreeI): Double = weighter.logWeight(tree, logRootDist, logBinyDist, logUnryDist, logTermDist, logPmixDist)

}

//
//

//object PcfgParserI {
//
//  def main(args: Array[String]): Unit = {
//
//    val rules = CcgRules.nonComp
//
//    val reader = new RuleViolatedRemovingTreeBankReader(new SimpleRuleViolationFinder(CcgRules.all, allowAllUnary = true), EnglishCcgTreeBankReader())
//
//    val trainingData = time("read raw", reader.rawDataDONTUSE.toVector)
//    val rawData = trainingData.map(_.words)
//    val tdData = time("read td", reader.tdData.toVector)
//    val testingData = time("read test", reader.testData.toVector)
//
//    val tagdict = time("make td", new SimpleTagDictionaryFactory().apply(tdData.map(_.tagged), "<S>", cat"<S>", "<E>", cat"<E>", Set.empty, Set.empty))
//
//    val catPriorInit = new CatgramCatPriorInitializer(new TagdictInformedAtomCatDistInitializer(1.0), pTerm = 0.8, pMod = 0.6, pFwd = 0.5)
//    val catPrior = time("make catprior", catPriorInit.fromRaw(rawData, tagdict))
//
//    val priorlogRootDist = catPrior
//    val priorBinyProd = new BinaryPriorDist(catPrior)
//    val priorlogBinyDist = new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, BinaryProd](priorBinyProd)
//    val priorUnryProd = new UnaryPriorDist(catPrior)
//    val priorlogUnryDist = new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, UnaryProd](priorUnryProd)
//    val priorlogTermDist = time("make term prior", Em2TermProdDist(new EmTagDictionaryEstimate(catPriorInit).fromRaw(rawData, tagdict)))
//    val priorProdProd = new IPDU(priorBinyProd, priorUnryProd, new ConditionalWrappingLogProbabilityDistribution[Cat, TermProd](cat"fake !!", priorlogTermDist), Double(1.0 / 3), Double(1.0 / 3), Double(1.0 / 3))
//    val priorProdDist = new ICPDU(priorlogBinyDist, priorlogUnryDist, priorlogTermDist, Map().withDefaultValue(Double(1.0 / 3), Double(1.0 / 3), Double(1.0 / 3)))
//
//    val prodFinder = new SimplePcfgProductionCounter()
//    val rootCounts = time("make root counts", trainingData.map(prodFinder.rootCounts).reduce(_ |+| _).mapVals(Double(_)))
//    val prodCounts = time("make prod counts", trainingData.map(prodFinder.prodCounts).reduce(_ |+| _).mapVals(_.mapVals(Double(_))))
//
//    val logRootDist = time("make root dist", new AlphaBetaLogProbabilityDistribution(rootCounts, Double(1.0), priorlogRootDist))
//    val prodDist = time("make prod dist", new AlphaBetaConditionalLogProbabilityDistribution(prodCounts.mapVals { counts => new AlphaBetaLogProbabilityDistribution(counts, Double(1.0), priorProdProd) }, Double(1.0), priorProdDist))
//
//    val parser = new PcfgParser(logRootDist, prodDist)
//
//    val guideChartBuilder = new SimpleCfgGuideChartBuilder(rules)
//    val testingGCs = time("make test guidecharts", testingData.iterator.map(t => (guideChartBuilder.build(t.words, None, tagdict), t)))
//    val evaluator = new DepParserEvaluator(None, true)
//    time("evaluate", evaluator.evaluate(parser, testingGCs, tagdict))
//
//  }
//
//}
