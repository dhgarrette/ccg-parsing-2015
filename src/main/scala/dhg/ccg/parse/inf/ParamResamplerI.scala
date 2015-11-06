//package dhg.ccg.parse.inf
//
//import scala.annotation.tailrec
//import scala.collection.immutable.BitSet
//import scala.collection.mutable.{ BitSet => MBitSet }
//import scala.collection.mutable.{ Map => MMap }
//import scala.collection.mutable.{ Set => MSet }
//import dhg.ccg.cat._
//import dhg.ccg.prob._
//import dhg.ccg.util._
//import dhg.ccg.tagdict.SimpleTagDictionary
//import dhg.ccg.tagdict.TagDictionary
//import dhg.util._
//import scalaz._
//import Scalaz._
//import dhg.ccg.parse._
//import dhg.ccg.parse.pcfg._
//import dhg.ccg.parse.pcfg.mcmc._
//import org.apache.commons.math3.random.RandomGenerator
//import dhg.ccg.math._
//import dhg.ccg.math.Util._
//import dhg.ccg.tagdict.StartEndTags
//import dhg.ccg.tagdict.SimpleStartEndTags
//import scala.collection.parallel.immutable.ParVector
//import dhg.ccg.math.DirichletSampler
//import dhg.ccg.parse.dep.ParserEvaluator
//import dhg.gfl.FudgSentence
//import dhg.ccg.math.SimpleDirichletSampler
//import breeze.stats.distributions.Rand
//import scala.util.control.Breaks._
//import org.apache.commons.math3.random.MersenneTwister
//import org.apache.commons.math3.random.SynchronizedRandomGenerator
//import dhg.util.viz._
//import scala.collection.mutable.ArrayBuffer
//
//trait ParamResamplerI {
//  type Word = String
//  type WordI = Int
//  type CatI = Int
//  type CatSet = BitSet
//  type MCatSet = MBitSet
//  type CatMap[A] = Vec[A]
//  type SortedCatMap[A] = OrderedIndirectSparseVec[A]
//
//  def resampleParameters(
//    rootCounts: Map[CatI, Double],
//    binyCounts: Map[CatI, Map[BinaryProdI, Double]],
//    unryCounts: Map[CatI, Map[UnaryProdI, Double]],
//    termCounts: Map[CatI, Map[TermProdI, Double]],
//    catPrior: Array[LogDouble],
//    termPrior: Array[LogDouble],
//    unaryRules: Vec[BitSet],
//    catIndexer: Indexer[Cat]): ( //
//    CatMap[LogDouble], //                             newRootDist
//    CatMap[CatMap[CatMap[LogDouble]]], //             newKnownBinyProds
//    CatMap[CatMap[LogDouble]], //                     newUnryDist
//    CatMap[Array[LogDouble]], //                      newTermDist
//    IndirectSparseVec[(LogDouble, LogDouble, LogDouble)]) //  newProdMixes
//}
//
//class SimpleParamResamplerI(
//  dirSampler: DirichletSampler,
//  alphaRoot: Double, alphaBiny: Double, alphaUnry: Double, alphaTerm: Double,
//  alphaLambda: Double, priorBinyProdMix: Double, priorUnryProdMix: Double, priorTermProdMix: Double,
//  rootSet: Set[Cat])
//    extends ParamResamplerI {
//
//  private[this] val priorProdMixSum = priorBinyProdMix + priorUnryProdMix + priorTermProdMix
//
//  def resampleParameters(
//    rootCounts: Map[CatI, Double],
//    binyCounts: Map[CatI, Map[BinaryProdI, Double]],
//    unryCounts: Map[CatI, Map[UnaryProdI, Double]],
//    termCounts: Map[CatI, Map[TermProdI, Double]],
//    catPrior: Array[LogDouble],
//    termPrior: Array[LogDouble],
//    unaryRules: Vec[BitSet],
//    catIndexer: Indexer[Cat]): ( //
//    CatMap[LogDouble], //                             newRootDist
//    CatMap[CatMap[CatMap[LogDouble]]], //             newKnownBinyProds
//    CatMap[CatMap[LogDouble]], //                     newUnryDist
//    CatMap[Array[LogDouble]], //                      newTermDist
//    IndirectSparseVec[(LogDouble, LogDouble, LogDouble)]) //  newProdMixes
//    = {
//
//    val rootSetI = BitSet.empty ++ rootSet.map(catIndexer)
//    
//    val rootDist = IndirectSparseVec(dirSampler.logDir(rootSetI.mapTo { t =>
//      LogDouble(alphaRoot) * catPrior(t) + LogDouble(rootCounts(t))
//    }.toMap))
//
//    val knownBinyProbs = IndirectSparseVec(
//      binyCounts.mapt { (t, prods) =>
//        val totalCount = binyCounts(t).values.sum
//        val (knownProds, knownCounts) = prods.toVector.map { case (prod @ BinaryProdI(u, v), count) => prod -> (LogDouble(alphaBiny) * catPrior(u) * catPrior(v) + LogDouble(count)) }.unzip
//        val unknownSum = LogDouble.one - knownProds.sumBy { case BinaryProdI(u, v) => catPrior(u) * catPrior(v) }
//        val sampledProbs = dirSampler.logDir(knownCounts :+ unknownSum).dropRight(1)
//        t -> (IndirectSparseVec(
//          (knownProds zipSafe sampledProbs)
//            .map { case (BinaryProdI(u, v), sampledProb) => u -> (v -> sampledProb) }
//            .groupByKey
//            .mapVals(x => IndirectSparseVec(x): CatMap[LogDouble])): CatMap[CatMap[LogDouble]])
//      })
//
//    val unryDist: CatMap[CatMap[LogDouble]] = IndirectSparseVec(unaryRules.activePairs.map {
//      case (t, prods) => t ->
//        IndirectSparseVec(dirSampler.logDir(prods.mapTo { prod =>
//          LogDouble(alphaUnry) * catPrior(prod) + LogDouble(unryCounts(t)(UnaryProdI(prod)))
//        }.toMap))
//    }.toMap)
//
//    val termDist: CatMap[Array[LogDouble]] = IndirectSparseVec(termCounts.keys.mapTo { t =>
//      dirSampler.logDir((0 until termPrior.length).map { word =>
//        LogDouble(alphaTerm) * termPrior(word) + LogDouble(termCounts(t)(TermProdI(word)))
//      }.toVector).toArray
//    }.toMap).withDefaultValue(termPrior)
//
//    val prodMixes = IndirectSparseVec((rootDist.activeKeysSorted ++ knownBinyProbs.activeKeys ++ unryDist.activeKeys ++ termDist.activeKeys).distinct.map(t => t -> dirSampler.logDir(Vector(
//      LogDouble(alphaLambda * priorBinyProdMix / priorProdMixSum + binyCounts(t).values.sum),
//      LogDouble(alphaLambda * priorUnryProdMix / priorProdMixSum + unryCounts(t).values.sum),
//      LogDouble(alphaLambda * priorTermProdMix / priorProdMixSum + termCounts(t).values.sum)))
//      .toTuple3).toMap.withDefaultValue((LogDouble(priorBinyProdMix / priorProdMixSum), LogDouble(priorUnryProdMix / priorProdMixSum), LogDouble(priorTermProdMix / priorProdMixSum))))
//
//    (rootDist, knownBinyProbs, unryDist, termDist, prodMixes)
//  }
//
//  override def toString = f"SimpleParamResamplerI(alphaRoot=${alphaRoot}, alphaBiny=${alphaBiny}, alphaUnry=${alphaUnry}, alphaTerm=${alphaTerm}"
//}
