package dhg.ccg.parse

import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import scala.collection.mutable
import scalaz.{ Ordering => _, _ }
import Scalaz._
import dhg.util._
import dhg.util.viz._
import dhg.ccg.prob._
import dhg.ccg.tagdict._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.parse.pcfg._
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
import scala.collection.mutable.Buffer
import dhg.ccg.parse.dep._

class CfgParser(
    guideChartBuilder: CfgGuideChartBuilder = new SimpleCfgGuideChartBuilder(
      rules = Vector[CcgRule](
        FA, BA,

        N2NP,

        LeftBracketPunctRight,
        RightBracketPunctRight,
        CommaPunctRight,
        SemicolonPunctRight,
        ColonPunctRight,
        FullstopPunctRight,
        LeftBracketPunctLeft //,Merge
        ))) {

  def parse(sentence: Vector[String], tagdict: TagDictionary[Cat]) = {
    guideChartBuilder.build(sentence, None, tagdict).fold(Vector.empty[CcgTree]) { gc =>
      parseFromGuideChart(gc)
    }
  }

  def parseFromTagged(sentence: Vector[(String, Cat)]) = {
    parseFromSupertagSetSentence(sentence.mapVals(Set(_)))
  }

  def parseFromSupertagSetSentence(sentence: Vector[(String, Set[Cat])], tagdict: TagDictionary[Cat] = SimpleTagDictionary.empty("<S>", StartCat, "<E>", EndCat)) = {
    guideChartBuilder.buildFromSupertagSetSentence(sentence, None, tagdict).fold(Vector.empty[CcgTree]) { gc =>
      parseFromGuideChart(gc)
    }
  }

  def parseFromGuideChart(guideChart: CfgGuideChart): Vector[CcgTree] = {
    val n = guideChart.length
    val table: Vector[Vector[Map[Cat, Buffer[CcgTree]]]] =
      guideChart.matrix.map { row =>
        if (row != null)
          row.map { col =>
            if (col != null)
              col.mapVals { _ =>
                Buffer.empty[CcgTree]
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
      case BinaryGuideChartEntry(k, BinaryProd(ik, kj)) =>
        for {
          ikTree <- table(i)(k)(ik).iterator
          kjTree <- table(k)(j)(kj).iterator
        } {
          table(i)(j)(ij) += CcgBinode(ij, ikTree, kjTree)
        }

      case UnaryGuideChartEntry(UnaryProd(subCat)) =>
        for (subTree <- table(i)(j)(subCat).iterator) {
          table(i)(j)(ij) += CcgUnode(ij, subTree)
        }

      case TermGuideChartEntry(TermProd(word)) =>
        table(i)(j)(ij) += CcgLeaf(ij, word, "FAKEPOS")
    }

    for {
      (ij, cell) <- table(0)(n).toVector
      if guideChart.rootSet.contains(ij)
      tree <- cell
    } yield tree
  }

}

//
//

object CfgParser {
	
  def main(args: Array[String]): Unit = {

    //    val tagdict = time("make td", new SimpleTagDictionaryFactory().apply(tdData.map(_.tagged), "<S>", cat"<S>", "<E>", cat"<E>", Set.empty, Set.empty))
    //
    //    val catPriorInit = new CatgramCatPriorInitializer(new TagdictInformedAtomCatDistInitializer(1.0), pTerm = 0.8, pMod = 0.6, pFwd = 0.5)
    //    val catPrior = time("make catprior", catPriorInit.fromRaw(rawData, tagdict))
    //
    //    val priorRootDist = catPrior
    //    val priorBinyProd = new BinaryPriorDist(catPrior)
    //    val priorBinyDist = new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, BinaryProd](priorBinyProd)
    //    val priorUnryProd = new UnaryPriorDist(catPrior)
    //    val priorUnryDist = new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, UnaryProd](priorUnryProd)
    //    val priorTermDist = time("make term prior", Em2TermProdDist(new EmTagDictionaryEstimate(catPriorInit).fromRaw(rawData, tagdict)))
    //    val priorProdProd = new IPDU(priorBinyProd, priorUnryProd, new ConditionalWrappingLogProbabilityDistribution[Cat, TermProd](cat"fake !!", priorTermDist), LogDouble(1.0 / 3), LogDouble(1.0 / 3), LogDouble(1.0 / 3))
    //    val priorProdDist = new ICPDU(priorBinyDist, priorUnryDist, priorTermDist, Map().withDefaultValue(LogDouble(1.0 / 3), LogDouble(1.0 / 3), LogDouble(1.0 / 3)))
    //
    //    val prodFinder = new SimplePcfgProductionCounter()
    //    val rootCounts = time("make root counts", trainingData.map(prodFinder.rootCounts).reduce(_ |+| _).mapVals(LogDouble(_)))
    //    val prodCounts = time("make prod counts", trainingData.map(prodFinder.prodCounts).reduce(_ |+| _).mapVals(_.mapVals(LogDouble(_))))
    //
    //    val rootDist = time("make root dist", new AlphaBetaLogProbabilityDistribution(rootCounts, LogDouble(1.0), priorRootDist))
    //    val prodDist = time("make prod dist", new AlphaBetaConditionalLogProbabilityDistribution(prodCounts.mapVals { counts => new AlphaBetaLogProbabilityDistribution(counts, LogDouble(1.0), priorProdProd) }, LogDouble(1.0), priorProdDist))
    //
    //    val parser = new PcfgParser(rootDist, prodDist)
    //
    //    val guideChartBuilder = new SimpleCfgGuideChartBuilder(rules, allowTerminalDeletion = false)
    //    val testingGCs = time("make test guidecharts", testingData.iterator.map(t => (guideChartBuilder.build(t.words, None, tagdict), t)))
    //    val evaluator = new DepParserEvaluator(None, true)
    //    time("evaluate", evaluator.evaluate(parser, testingGCs, tagdict))

//    val parser = new CfgParser()
//    def parse(s: String) = parser.parseFromTagged(s.splitWhitespace.map(_.rsplit("\\|")).map { case Vector(w, t) => (w, NonRemovingCcgBankCatInterner(t)) })
//    val parses = parse(raw"John|NP asked|(((S\NP_1)/(S_2\NP_3)_2)/NP_3) a|(NP_4/N_4) girl|N to|((S\NP_5)/(S_6\NP_5)_6) dance|(S\NP)")
//    parses.foreach(TreeViz.drawTree)
//    val depparses = parses.map(DepTree.fromCcgTree)
//    //depparses.foreach(TreeViz.drawTree)
//    val depgraphs = parses.map(DepGraph.fromCcgTree)
//    depgraphs.foreach(g => println(g.pretty))
//    depgraphs.foreach(VizGraph.drawGraph)

  }

  
}