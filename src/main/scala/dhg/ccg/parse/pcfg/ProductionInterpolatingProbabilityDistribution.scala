package dhg.ccg.parse.pcfg

import dhg.util._
import dhg.ccg.prob.LogProbabilityDistribution
import dhg.ccg.prob.ConditionalLogProbabilityDistribution
import dhg.ccg.cat.Cat

class ICPDU(
  val binyProdDist: ConditionalLogProbabilityDistribution[Cat, BinaryProd],
  val unryProdDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd],
  val termProdDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
  val prodMixes: Map[Cat, (LogDouble, LogDouble, LogDouble)])
    extends ConditionalLogProbabilityDistribution[Cat, Prod] {
  def apply(x: Prod, given: Cat) = {
    val (binyProdMix, unryProdMix, termProdMix) = prodMixes(given)
    val prodMixSum = binyProdMix + unryProdMix + termProdMix
    x match {
      case binyProd: BinaryProd => binyProdDist(binyProd, given) * binyProdMix / prodMixSum
      case unryProd: UnaryProd => unryProdDist(unryProd, given) * unryProdMix / prodMixSum
      case termProd: TermProd => termProdDist(termProd, given) * termProdMix / prodMixSum
    }
  }
  def sample(given: Cat) = ???
}

class IPDU(
  val binyProdDist: LogProbabilityDistribution[BinaryProd],
  val unryProdDist: LogProbabilityDistribution[UnaryProd],
  val termProdDist: LogProbabilityDistribution[TermProd],
  val binyProdMix: LogDouble,
  val unryProdMix: LogDouble,
  val termProdMix: LogDouble)
    extends LogProbabilityDistribution[Prod] {
  private[this] val prodMixSum = binyProdMix + unryProdMix + termProdMix
  def apply(x: Prod) = x match {
    case binyProd: BinaryProd => binyProdDist(binyProd) * binyProdMix / prodMixSum
    case unryProd: UnaryProd => unryProdDist(unryProd) * unryProdMix / prodMixSum
    case termProd: TermProd => termProdDist(termProd) * termProdMix / prodMixSum
  }
  def sample() = ???
  def defaultProb = ???
}
