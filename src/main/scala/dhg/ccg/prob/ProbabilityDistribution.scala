package dhg.ccg.prob

import scala.util.Random
import dhg.util._

trait ProbabilityDistributionSampler[B] extends Serializable {
  def sample(): B
}

trait ExpProbabilityDistribution[B] extends ProbabilityDistributionSampler[B] {
  def apply(b: B): Double
  def sample(): B
  def defaultProb: Double
}

abstract class AbstractExpProbabilityDistribution[B](totalAddition: Double = 0.0) extends ExpProbabilityDistribution[B] {
  protected[this] def allKnownBs: Set[B]
  protected[this] def counts(b: B): Double
  protected[this] def defaultCount: Double

  final private[this] def allKnownProbs = allKnownBs.iterator.mapTo(apply)
  final def defaultProb = defaultCount / smoothedCountSum

  final protected[this] lazy val smoothedCountSum = allKnownBs.sumBy(counts) + totalAddition

  final def apply(b: B): Double = {
    if (smoothedCountSum == 0.0)
      0.0
    else
      counts(b) / smoothedCountSum
  }

  final def sample(): B = {
    assert(allKnownBs.nonEmpty, f"allKnownBs is empty in sample()!")
    //val orderedBs = allKnownBs.toVector.mapTo { b => val c = counts(b); assert(c >= 0.0 && !c.isInfinite, f"Invalid count in sample(): $b -> $c"); c }//.desc
    var accum = Random.nextDouble * (smoothedCountSum - totalAddition)
    val it = allKnownBs.iterator
    while (it.hasNext) {
      val item = it.next
      val count = counts(item)
      assert(count >= 0.0 && !count.isInfinite, f"Invalid count in sample(): $item -> $count")
      accum -= count
      if (accum <= 0)
        return item
    }
    sys.error(f"nothing sampled!  orderedBs=${allKnownBs.map(counts)}; smoothedCountSum=$smoothedCountSum")
  }

  final override def toString = f"PD(${allKnownBs.toVector.mapTo(counts).sortBy(-_._2).map(_._1).mapTo(apply).map { case (k, v) => f"$k -> $v" }.mkString(", ")})"
}

object ExpProbabilityDistribution {
  def empty[B] = new SimpleExpProbabilityDistribution[B](Map())
}

class SimpleExpProbabilityDistribution[B](unsmoothedCounts: Map[B, Double])
  extends LaplaceExpProbabilityDistribution[B](unsmoothedCounts, None, None, 0.0)

class LaplaceExpProbabilityDistribution[B](
  unsmoothedCounts: Map[B, Double],
  knownBs: Option[Set[B]],
  excludedBs: Option[B => Boolean],
  lambda: Double,
  totalAddition: Double = 0.0)
  extends AbstractExpProbabilityDistribution[B](totalAddition = totalAddition) {

  for ((key, count) <- unsmoothedCounts) assert(count >= 0.0 && !count.isInfinite, f"Trying to create LaplaceExpProbabilityDistribution with bad key=$key count=$count: unsmoothedCounts=$unsmoothedCounts")
  assert(totalAddition >= 0.0 && !totalAddition.isInfinite, f"Trying to create LaplaceExpProbabilityDistribution with bad totalAddition=$totalAddition")

  val allKnownBs = (knownBs.getOrElse(Set()) | unsmoothedCounts.keySet).filterNot(excludedBs.getOrElse(Set()))
  def counts(b: B) = if (excludedBs.isDefined && excludedBs.get(b)) 0.0 else (unsmoothedCounts.getOrElse(b, 0.0) + lambda)
  def defaultCount: Double = lambda
}

class DefaultedExpProbabilityDistribution[B](
  smoothedCounts: Map[B, Double],
  knownBs: Option[Set[B]],
  excludedBs: Option[B => Boolean],
  val defaultCount: Double,
  totalAddition: Double = 0.0)
  extends AbstractExpProbabilityDistribution[B](totalAddition = totalAddition) {

  def this(smoothedCounts: Map[B, Double]) = this(smoothedCounts, None, None, 0.0)

  for ((key, count) <- smoothedCounts) assert(count >= 0.0 && !count.isInfinite, f"Trying to create DefaultedExpProbabilityDistribution with bad key=$key count=$count: smoothedCounts=$smoothedCounts")
  assert(totalAddition >= 0.0 && !totalAddition.isInfinite, f"Trying to create DefaultedExpProbabilityDistribution with bad totalAddition=$totalAddition")

  val allKnownBs = (knownBs.getOrElse(Set()) | smoothedCounts.keySet).filterNot(excludedBs.getOrElse(Set()))
  def counts(b: B) = if (excludedBs.isDefined && excludedBs.get(b)) 0.0 else smoothedCounts.getOrElse(b, defaultCount)
}

//
//
//

trait ConditionalExpProbabilityDistribution[A, B] {
  def apply(x: B, given: A): Double
  def sample(given: A): B
}

class SimpleConditionalExpProbabilityDistribution[A, B](
  conditionedDistributions: Map[A, ExpProbabilityDistribution[B]],
  knownAs: Option[Set[A]],
  excludedAs: Option[A => Boolean],
  default: ExpProbabilityDistribution[B])
  extends ConditionalExpProbabilityDistribution[A, B] {

  def this(conditionedDistributions: Map[A, ExpProbabilityDistribution[B]]) = this(conditionedDistributions, None, None, ExpProbabilityDistribution.empty[B])

  private[this] val allKnownAs = (knownAs.getOrElse(Set()) | conditionedDistributions.keySet).filterNot(excludedAs.getOrElse(Set()))
  private[this] def allKnownConditionedDistributions = allKnownAs.iterator.mapTo(given => conditionedDistributions.getOrElse(given, default))

  def apply(x: B, given: A): Double = {
    if (excludedAs.isDefined && excludedAs.get(given))
      0.0
    else
      conditionedDistributions.getOrElse(given, default)(x)
  }

  def sample(given: A): B = {
    if (excludedAs.isDefined && excludedAs.get(given))
      sys.error(f"cannot sample from $given")
    else
      conditionedDistributions.getOrElse(given, default).sample
  }
}

class InterpolatingConditionalExpProbabilityDistribution[Tag](
  delegates: Vector[(ConditionalExpProbabilityDistribution[Tag, Tag], Double)])
  extends ConditionalExpProbabilityDistribution[Tag, Tag] {

  def apply(x: Tag, given: Tag): Double = {
    delegates.sumBy { case (d, w) => d(x, given) * w }
  }

  def sample(given: Tag): Tag = ???
}

class ReversingConditionalExpProbabilityDistribution[A, B](delegate: ConditionalExpProbabilityDistribution[B, A]) extends ConditionalExpProbabilityDistribution[A, B] {
  def apply(x: B, given: A): Double = delegate(given, x)
  def sample(given: A): B = ???
}
