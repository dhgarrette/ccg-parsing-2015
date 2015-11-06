package dhg.ccg.parse.scg

import scala.annotation.tailrec
import scala.collection.mutable.{ Map => MMap }
import dhg.ccg.cat._
import dhg.ccg.prob._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.parse.scg.mcmc._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.tagdict.TagDictionary
import dhg.util._
import dhg.util.viz._
import scalaz._
import Scalaz._
import dhg.ccg.tagdict.StartEndTags

class DumbScgEm(
  maxIterations: Int,
  pcfgOnly: Boolean = false) {

  private[this] val counter = new SimpleScgProductionFinder(new SimplePcfgProductionCounter())

  def train(guideCharts: Vector[CfgGuideChart])(se: StartEndTags[Cat]) = {
    val initRootDist = new UniformDefaultLogProbabilityDistribution[Cat](LogDouble.one)
    val initProdDist = new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, Prod](new UniformDefaultLogProbabilityDistribution(LogDouble.one))
    val initLctxDist = new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, Cat](new UniformDefaultLogProbabilityDistribution(LogDouble.one))
    val initRctxDist = new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, Cat](new UniformDefaultLogProbabilityDistribution(LogDouble.one))
    trainFromInit(guideCharts, initRootDist, initProdDist, initLctxDist, initRctxDist)(se)
  }

  def trainFromInit(guideCharts: Vector[CfgGuideChart],
    initRootDist: LogProbabilityDistribution[Cat],
    initProdDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    initLctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    initRctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]) = {
    iterate(1, guideCharts, initRootDist, initProdDist, initLctxDist, initRctxDist)(se)
  }

  private[this] def iterate(iteration: Int, guideCharts: Vector[CfgGuideChart],
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
    rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]): ( //
    Map[Cat, LogDouble], //
    Map[Cat, Map[Prod, LogDouble]], //
    Map[Cat, Map[Cat, LogDouble]], //
    Map[Cat, Map[Cat, LogDouble]]) = {

    val scgParser = new ExactScgParser(rootDist, prodDist, lctxDist, rctxDist)(se)

    println(f"Iteration $iteration")
    val (sc, pc, lc, rc) =
      (for ((gc, i) <- guideCharts.zipWithIndex) yield {
        val trees = scgParser.parseAndProbKBestFromGuideChart(gc, gc.numPossibleParses.toInt * 2)
        val treesAndProbs = trees.mapt((t, p) => t -> (if (!pcfgOnly) p else scgParser.pcfgWeight(t)))
        val totalProb = treesAndProbs.sumBy(_._2)
        for (((t, p), j) <- treesAndProbs.zipWithIndex) yield {
          //      if (iteration == 1) {
          //        TreeViz.drawTree(t)
          //      }

          // println(f"  ${i + 1}-${j + 1}  ${(p / totalProb).toDouble}%.4f  ${t}")

          val sc = counter.rootCounts(t).mapVals(LogDouble(_) * (p / totalProb))
          val pc = counter.prodCounts(t).mapVals(_.mapVals(LogDouble(_) * (p / totalProb)))
          val lc = counter.lctxCounts(t)(se).mapVals(_.mapVals(LogDouble(_) * (p / totalProb)))
          val rc = counter.rctxCounts(t)(se).mapVals(_.mapVals(LogDouble(_) * (p / totalProb)))
          (sc, pc, lc, rc)
        }
      }).flatten.reduce(_ |+| _)
    //println

    if (iteration >= maxIterations) {
      (sc, pc, lc, rc)
    }
    else {
      val sd = new SimpleLogProbabilityDistribution(sc)
      val pd = new SimpleConditionalLogProbabilityDistribution(pc.mapVals(new SimpleLogProbabilityDistribution(_)))
      val ld = new SimpleConditionalLogProbabilityDistribution(lc.mapVals(new SimpleLogProbabilityDistribution(_)))
      val rd = new SimpleConditionalLogProbabilityDistribution(rc.mapVals(new SimpleLogProbabilityDistribution(_)))
      iterate(iteration + 1, guideCharts, sd, pd, ld, rd)(se)
    }
  }

}
