package dhg.ccg.parse.inf

import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.mutable.{ BitSet => MBitSet }
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import dhg.ccg.cat._
import dhg.ccg.prob._
import dhg.ccg.util._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.tagdict.TagDictionary
import dhg.util._
import scalaz._
import Scalaz._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import org.apache.commons.math3.random.RandomGenerator
import dhg.ccg.math._
import dhg.ccg.math.Util._
import dhg.ccg.tagdict.StartEndTags
import dhg.ccg.tagdict.SimpleStartEndTags
import scala.collection.parallel.immutable.ParVector
import dhg.ccg.math.DirichletSampler
import dhg.ccg.parse.dep.ParserEvaluator
import dhg.gfl.FudgSentence
import dhg.ccg.math.SimpleDirichletSampler
import breeze.stats.distributions.Rand
import scala.util.control.Breaks._
import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.random.SynchronizedRandomGenerator
import dhg.util.viz._
import scala.collection.mutable.ArrayBuffer

trait ParamResampler {
  type Word = String

  def resampleParameters(
    treesToEstimate: Vector[CcgTree],
    goldRootCounts: Map[Cat, Double],
    goldBinyCounts: Map[Cat, Map[BinaryProd, Double]],
    goldUnryCounts: Map[Cat, Map[UnaryProd, Double]],
    goldTermCounts: Map[Cat, Map[TermProd, Double]],
    allWordSet: Set[TermProd]): ( //
    LogProbabilityDistribution[Cat], //                        newRootDist
    Map[Cat, (Map[BinaryProd, LogDouble], LogDouble)], //      newKnownBinyProds
    ConditionalLogProbabilityDistribution[Cat, UnaryProd], //  newUnryDist
    ConditionalLogProbabilityDistribution[Cat, TermProd], //   newTermDist
    Map[Cat, (LogDouble, LogDouble, LogDouble)]) //            newProdMixes
}

class SimpleParamResampler(
  productionFinder: PcfgProductionCounter,
  catPrior: InfCatPrior,
  termPrior: ConditionalLogProbabilityDistribution[Cat, TermProd],
  alphaRoot: Double, alphaBiny: Double, alphaUnry: Double, alphaTerm: Double,
  alphaLambda: Double, priorBinyProdMix: Double, priorUnryProdMix: Double, priorTermProdMix: Double,
  rootSet: Set[Cat],
  allUnrySet: Map[Cat, Set[UnaryProd]],
  dirSampler: DirichletSampler)
    extends ParamResampler {

  private[this] val priorProdMixSum = priorBinyProdMix + priorUnryProdMix + priorTermProdMix

  def resampleParameters(
    treesToEstimate: Vector[CcgTree],
    goldRootCounts: Map[Cat, Double],
    goldBinyCounts: Map[Cat, Map[BinaryProd, Double]],
    goldUnryCounts: Map[Cat, Map[UnaryProd, Double]],
    goldTermCounts: Map[Cat, Map[TermProd, Double]],
    allWordSet: Set[TermProd]): ( //
    LogProbabilityDistribution[Cat], //                        newRootDist
    Map[Cat, (Map[BinaryProd, LogDouble], LogDouble)], //      newKnownBinyProds
    ConditionalLogProbabilityDistribution[Cat, UnaryProd], //  newUnryDist
    ConditionalLogProbabilityDistribution[Cat, TermProd], //   newTermDist
    Map[Cat, (LogDouble, LogDouble, LogDouble)]) //            newProdMixes
    = {

    val estRootCounts = (treesToEstimate.map(productionFinder.rootCounts).reduce(_ |+| _) |+| goldRootCounts).withDefaultValue(0.0)
    val estBinyCounts = (treesToEstimate.map(productionFinder.binyCounts).reduce(_ |+| _) |+| goldBinyCounts).mapVals(_.withDefaultValue(0.0)).withDefaultValue(Map.empty.withDefaultValue(0.0))
    val estUnryCounts = (treesToEstimate.map(productionFinder.unryCounts).reduce(_ |+| _) |+| goldUnryCounts).mapVals(_.withDefaultValue(0.0)).withDefaultValue(Map.empty.withDefaultValue(0.0))
    val estTermCounts = (treesToEstimate.map(productionFinder.termCounts).reduce(_ |+| _) |+| goldTermCounts).mapVals(_.withDefaultValue(0.0)).withDefaultValue(Map.empty.withDefaultValue(0.0))

    val rootDist = new SimpleLogProbabilityDistribution(dirSampler.logDir(rootSet.mapTo { t => LogDouble(alphaRoot) * catPrior(t) + LogDouble(estRootCounts(t)) }.toMap))
    val unryDist = new SimpleConditionalLogProbabilityDistribution(
      allUnrySet.mapt { (t, prods) =>
        t -> new SimpleLogProbabilityDistribution(dirSampler.logDir(prods.mapTo {
          case prod @ UnaryProd(u) => LogDouble(alphaUnry) * catPrior(u) + LogDouble(estUnryCounts(t)(prod))
        }.toMap))
      }.toMap)
    val termDist = new SimpleConditionalLogProbabilityDistribution(
      estTermCounts.mapt { (t, counts) =>
        t -> new SimpleLogProbabilityDistribution(dirSampler.logDir(allWordSet.mapTo { prod =>
          LogDouble(alphaTerm) * termPrior(prod, t) + LogDouble(counts(prod))
        }.toMap))
      }.toMap, default = termPrior)

    val knownBinyProbs =
      estBinyCounts.mapVals { counts =>
        val totalCount = counts.values.sum
        val (knownProds, knownCounts) = counts.toVector.map { case (prod @ BinaryProd(u, v), count) => prod -> (LogDouble(alphaBiny) * catPrior(u) * catPrior(v) + LogDouble(count)) }.unzip
        val unknownPriorsSum = LogDouble.one - knownProds.sumBy { case BinaryProd(u, v) => catPrior(u) * catPrior(v) }
        val sampledProbs :+ sampledUnknownBucket = dirSampler.logDir(knownCounts :+ (LogDouble(alphaBiny) * unknownPriorsSum))
        val knownProbs = (knownProds zipSafe sampledProbs).toMap
        val scaleFactor = sampledUnknownBucket / unknownPriorsSum
        (knownProbs, scaleFactor)
      }

    val prodMixes = (estRootCounts.keys ++ estBinyCounts.keys ++ estUnryCounts.keys ++ estTermCounts.keys).toSet.mapTo(t => dirSampler.logDir(Vector(
      LogDouble(alphaLambda * priorBinyProdMix / priorProdMixSum + estBinyCounts(t).values.sum),
      LogDouble(alphaLambda * priorUnryProdMix / priorProdMixSum + estUnryCounts(t).values.sum),
      LogDouble(alphaLambda * priorTermProdMix / priorProdMixSum + estTermCounts(t).values.sum)))
      .toTuple3).toMap
      .withDefaultValue((
        LogDouble(priorBinyProdMix / priorProdMixSum),
        LogDouble(priorUnryProdMix / priorProdMixSum),
        LogDouble(priorTermProdMix / priorProdMixSum)))

    (rootDist, knownBinyProbs, unryDist, termDist, prodMixes)
  }

  override def toString = f"SimpleParamResamplerI(alphaRoot=${alphaRoot}, alphaBiny=${alphaBiny}, alphaUnry=${alphaUnry}, alphaTerm=${alphaTerm}"
}
