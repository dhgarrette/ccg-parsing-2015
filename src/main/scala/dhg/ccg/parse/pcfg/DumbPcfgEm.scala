package dhg.ccg.parse.pcfg

import scala.annotation.tailrec
import scala.collection.mutable.{ Map => MMap }
import dhg.ccg.cat._
import dhg.ccg.prob._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.tagdict.TagDictionary
import dhg.util._
import scalaz._
import Scalaz._
import dhg.ccg.tagdict.StartEndTags
import dhg.util.viz.TreeViz

class DumbPcfgEm(
    maxIterations: Int,
    pcfgOnly: Boolean = false) {

  private[this] val counter = new SimplePcfgProductionCounter()

  def train(guideCharts: Vector[CfgGuideChart]) = {
    val initRootDist = new UniformDefaultLogProbabilityDistribution[Cat](LogDouble.one)
    val initProdDist = new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, Prod](new UniformDefaultLogProbabilityDistribution(LogDouble.one))
    trainFromInit(guideCharts, initRootDist, initProdDist)
  }

  def trainFromInit(guideCharts: Vector[CfgGuideChart],
    initRootDist: LogProbabilityDistribution[Cat],
    initProdDist: ConditionalLogProbabilityDistribution[Cat, Prod]) = {
    iterate(1, guideCharts, initRootDist, initProdDist)
  }

  private[this] def iterate(iteration: Int, guideCharts: Vector[CfgGuideChart],
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): ( //
    Map[Cat, LogDouble], //
    Map[Cat, Map[Prod, LogDouble]]) = {

    val pcfgParser = new PcfgParser(rootDist, prodDist)

    println(f"Iteration $iteration")
    val (sc, pc) =
      (for ((gc, i) <- guideCharts.zipWithIndex) yield {
        val treesAndProbs = pcfgParser.parseAndProbKBestFromGuideChart(gc, gc.numPossibleParses.toInt * 2)
        val totalProb = treesAndProbs.sumBy(_._2)
        for (((t, p), j) <- treesAndProbs.zipWithIndex) yield {
          //      if (iteration == 1) {
          //        TreeViz.drawTree(t)
          //      }

          //println(f"  ${i + 1}-${j + 1}  ${(p / totalProb).toDouble}%.4f  ${t}")

          val sc = counter.rootCounts(t).mapValues(LogDouble(_) * (p / totalProb))
          val pc = counter.prodCounts(t).mapValues(_.mapValues(LogDouble(_) * (p / totalProb)))
          (sc, pc)
        }
      }).flatten.reduce(_ |+| _)
    //println

    if (iteration >= maxIterations) {
      (sc, pc)
    }
    else {
      val sd = new SimpleLogProbabilityDistribution(sc)
      val pd = new SimpleConditionalLogProbabilityDistribution(pc.mapVals(new SimpleLogProbabilityDistribution(_)))
      iterate(iteration + 1, guideCharts, sd, pd)
    }
  }

}
