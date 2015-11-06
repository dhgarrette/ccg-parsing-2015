package dhg.ccg.parse.pcfg.mcmc

import scala.annotation.tailrec
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import dhg.ccg.cat._
import dhg.ccg.prob._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.tagdict.TagDictionary
import dhg.util._
import scalaz._
import Scalaz._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import org.apache.commons.math3.random.RandomGenerator
import dhg.ccg.math.Util._
import dhg.ccg.tagdict.StartEndTags
import dhg.ccg.tagdict.SimpleStartEndTags
import scala.collection.parallel.immutable.ParVector
import dhg.ccg.math._
import dhg.ccg.parse.dep.ParserEvaluator

class DumbPcfgMcmc(
  samplingIterations: Int,
  burninIterations: Int,
  pcfgTreeSampler: PcfgTreeSampler,
  alphaRoot: Double, alphaProd: Double,
  accumulate: Boolean = false) {

  val guideChartProdFinder = new SimplePcfgGuideChartProdFinder()
  val productionFinder = new SimplePcfgProductionCounter()

  val dirSampler = DirSampler

  def train(
    guideCharts: Vector[CfgGuideChart],
    priorRootDist: LogProbabilityDistribution[Cat],
    priorProdDist: ConditionalLogProbabilityDistribution[Cat, Prod]) = {

    val (alphaPriorRootCounts, alphaPriorProdCounts) = makeAlphaPrior(guideCharts, priorRootDist, priorProdDist)

    val sampledTrees = doTrain(guideCharts, alphaPriorRootCounts, alphaPriorProdCounts)

    val sampledRootCounts = sampledTrees.map(productionFinder.rootCounts).fold(Map.empty[Cat, /*            */ Double])(_ |+| _).mapVals(LogDouble(_))
    val sampledProdCounts = sampledTrees.map(productionFinder.prodCounts).fold(Map.empty[Cat, Map[Prod, Double]])(_ |+| _).mapVals(_.mapVals(LogDouble(_)))
    val rootDist = new SimpleLogProbabilityDistribution[Cat](sampledRootCounts)
    val prodDist = new SimpleConditionalLogProbabilityDistribution[Cat, Prod](sampledProdCounts.mapVals(new SimpleLogProbabilityDistribution(_)))
    new PcfgParser(rootDist, prodDist)
  }

  private[this] final def doTrain(
    guideCharts: Vector[CfgGuideChart],
    alphaPriorRootCounts: Map[Cat, LogDouble],
    alphaPriorProdCounts: Map[Cat, Map[Prod, LogDouble]]) = {

    val initRootDist = new SimpleLogProbabilityDistribution(alphaPriorRootCounts)
    val initProdDist = new SimpleConditionalLogProbabilityDistribution(alphaPriorProdCounts.mapVals(new SimpleLogProbabilityDistribution(_)))

    val sampledTrees = iterate(guideCharts,
      initRootDist, initProdDist,
      alphaPriorRootCounts, alphaPriorProdCounts,
      Vector.empty, -burninIterations)
    sampledTrees
  }

  @tailrec private[this] final def iterate(
    guideCharts: Vector[CfgGuideChart],
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    alphaPriorRootCounts: Map[Cat, LogDouble],
    alphaPriorProdCounts: Map[Cat, Map[Prod, LogDouble]],
    runningTrees: Vector[CcgTree],
    iteration: Int): // 
    Vector[CcgTree] = {

    if (iteration < samplingIterations) {
      val startTime = System.currentTimeMillis()
      val resampledTrees = guideCharts.map(pcfgTreeSampler.sample(_, rootDist, prodDist))

      println(f"iteration ${((if (iteration < 0) iteration else iteration + 1) + ":").padRight(4)} ${(System.currentTimeMillis() - startTime) / 1000.0}%.3f sec")

      val treesToEstimate = if(accumulate) (resampledTrees ++ runningTrees) else resampledTrees
      val estRootCounts = treesToEstimate.map(productionFinder.rootCounts).reduce(_ |+| _)
      val estProdCounts = treesToEstimate.map(productionFinder.prodCounts).reduce(_ |+| _)

      iterate(guideCharts,
        new SimpleLogProbabilityDistribution(dirSampler.logDir(estRootCounts.mapVals(LogDouble(_)) |+| alphaPriorRootCounts)),
        new SimpleConditionalLogProbabilityDistribution(estProdCounts.mapt { (cat, estCounts) =>
          cat -> new SimpleLogProbabilityDistribution(dirSampler.logDir(estCounts.mapVals(LogDouble(_)) |+| alphaPriorProdCounts(cat)))
        }),
        alphaPriorRootCounts, alphaPriorProdCounts,
        if (iteration >= 0) runningTrees ++ resampledTrees else Vector.empty, // add new trees during sampling iterations only
        iteration + 1)
    }
    else {
      println(f"MAX ITERATIONS REACHED")
      runningTrees
    }
  }

  def makeAlphaPrior(guideCharts: Vector[CfgGuideChart],
    priorRootDist: LogProbabilityDistribution[Cat],
    priorProdDist: ConditionalLogProbabilityDistribution[Cat, Prod]) = {

    val allRootSet = guideCharts.map(guideChartProdFinder.roots).fold(Set.empty)(_ |+| _)
    val allProdSet = guideCharts.map(guideChartProdFinder.prods).fold(Map.empty)(_ |+| _)

    val alphaPriorRootCounts = allRootSet.mapTo( /*                            */ root => LogDouble(alphaRoot) * priorRootDist(root)).toMap
    val alphaPriorProdCounts = allProdSet.mapt((cat, terms) => cat -> terms.mapTo(prod => LogDouble(alphaProd) * priorProdDist(prod, cat)).toMap)

    (alphaPriorRootCounts, alphaPriorProdCounts)
  }

  override def toString = f"McmcPcfg(samplingIterations=${samplingIterations}, burninIterations=${burninIterations}, alphaRoot=${alphaRoot}, alphaBiny=${alphaProd}"
}
