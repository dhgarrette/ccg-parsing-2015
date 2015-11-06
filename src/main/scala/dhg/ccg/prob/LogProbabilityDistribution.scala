package dhg.ccg.prob

import scala.util.Random
import dhg.util._
import com.sun.org.apache.bcel.internal.generic.UnconditionalBranch

trait LogProbabilityDistribution[B] extends ProbabilityDistributionSampler[B] with (B => LogDouble) {
  def apply(b: B): LogDouble
  def sample(): B
  def defaultProb: LogDouble
}

abstract class AbstractLogProbabilityDistribution[B](totalAddition: LogDouble = LogDouble.zero) extends LogProbabilityDistribution[B] {
  protected[this] def allKnownBs: Set[B]
  protected[this] def counts(b: B): LogDouble
  protected[this] def defaultCount: LogDouble

  final private[this] lazy val knownCountSum = allKnownBs.sumBy(b => counts(b))
  final private[this] lazy val knownSumAndAddition = knownCountSum + totalAddition
  final private[this] lazy val probCache = allKnownBs.mapTo(b => counts(b) / knownSumAndAddition).toMap

  final def apply(b: B): LogDouble = {
    if (knownSumAndAddition.isZero) // prevent division by zero
      LogDouble.zero
    else
      probCache.getOrElse(b, counts(b) / knownSumAndAddition)
  }

  final def defaultProb =
    if (knownSumAndAddition.isZero) // prevent division by zero
      LogDouble.zero
    else
      defaultCount / knownSumAndAddition

  private[this] lazy val sampleScale = knownCountSum / knownSumAndAddition
  final def sample(): B = {
    assert(allKnownBs.nonEmpty, "cannot sample from an empty distribution")
    assert(sampleScale > LogDouble.zero, "cannot sample from a distribution without mass")
    //val orderedBs = allKnownBs.toVector.mapTo { b => val c = counts(b); assert(c >= 0.0 && !c.isInfinite, f"Invalid count in sample(): $b -> $c"); c }//.desc
    var accum = LogDouble(Random.nextDouble) * sampleScale
    val it = allKnownBs.iterator
    while (it.hasNext) {
      val item = it.next
      val pItem = probCache(item)
      assert(!pItem.isNaN && !pItem.isInfinite, f"Invalid pItem in sample(): $item -> $pItem")
      if (pItem > accum)
        return item
      accum -= pItem
    }
    sys.error(f"nothing sampled!  orderedBs=${allKnownBs.map(counts)}; knownCountSum=$knownCountSum")
  }

  final override def toString = f"PD(${allKnownBs.toVector.mapTo(counts).desc.map(_._1).mapTo(apply).map { case (k, v) => f"$k -> $v" }.mkString(", ")})"
}

object LogProbabilityDistribution {
  def empty[B] = new SimpleLogProbabilityDistribution[B](Map.empty)
}

class SimpleLogProbabilityDistribution[B](unsmoothedCounts: Map[B, LogDouble])
  extends LaplaceLogProbabilityDistribution[B](unsmoothedCounts, None, None, LogDouble.zero)

class LaplaceLogProbabilityDistribution[B](
  unsmoothedCounts: Map[B, LogDouble],
  knownBs: Option[Set[B]],
  excludedBs: Option[B => Boolean],
  lambda: LogDouble,
  totalAddition: LogDouble = LogDouble.zero)
  extends AbstractLogProbabilityDistribution[B](totalAddition = totalAddition) {

  for ((key, count) <- unsmoothedCounts) assert(!count.isNaN && !count.isInfinite, f"Trying to create LaplaceLogProbabilityDistribution with bad key=$key count=$count: unsmoothedCounts=$unsmoothedCounts")
  assert(!totalAddition.isNaN && !totalAddition.isInfinite, f"Trying to create LaplaceLogProbabilityDistribution with bad totalAddition=$totalAddition")

  val allKnownBs = (knownBs.getOrElse(Set()) | unsmoothedCounts.keySet).filterNot(excludedBs.getOrElse(Set()))
  def counts(b: B) = if (excludedBs.isDefined && excludedBs.get(b)) LogDouble.zero else (unsmoothedCounts.getOrElse(b, LogDouble.zero) + lambda)
  def defaultCount: LogDouble = lambda
}

//class AlphaBetaLogProbabilityDistribution[B](
//  unsmoothedCounts: Map[B, LogDouble],
//  alpha: LogDouble,
//  prior: LogProbabilityDistribution[B],
//  knownBs: Option[Set[B]] = None,
//  excludedBs: Option[B => Boolean] = None,
//  totalAddition: LogDouble = LogDouble.zero)
//  extends AbstractLogProbabilityDistribution[B](totalAddition = totalAddition) {
//
//  for ((key, count) <- unsmoothedCounts) assert(!count.isNaN && !count.isInfinite, f"Trying to create AlphaBetaLogProbabilityDistribution with bad key=$key count=$count: unsmoothedCounts=$unsmoothedCounts")
//  assert(!totalAddition.isNaN && !totalAddition.isInfinite, f"Trying to create AlphaBetaLogProbabilityDistribution with bad totalAddition=$totalAddition")
//
//  protected[this] val allKnownBs = (knownBs.getOrElse(Set.empty) | unsmoothedCounts.keySet).filterNot(excludedBs.getOrElse(Set.empty))
//  def counts(b: B) = if (excludedBs.isDefined && excludedBs.get(b)) LogDouble.zero else (unsmoothedCounts.getOrElse(b, LogDouble.zero) + alpha * prior(b))
//  def defaultCount: LogDouble = sys.error("`defaultCount` not available on AlphaBetaLogProbabilityDistribution")
//}

class AlphaBetaLogProbabilityDistribution[B](
  unsmoothedCounts: Map[B, LogDouble],
  alpha: LogDouble,
  prior: LogProbabilityDistribution[B],
  knownBs: Option[Set[B]] = None,
  excludedBs: Option[B => Boolean] = None,
  totalAddition: LogDouble = LogDouble.zero)
  extends LogProbabilityDistribution[B] {

  private[this] val allKnownBs = (knownBs.getOrElse(Set()) | unsmoothedCounts.keySet).filterNot(excludedBs.getOrElse(Set()))

  private[this] lazy val totalCounts = unsmoothedCounts.filterKeys(b => !excludedBs.exists(_.apply(b))).values.sum
  private[this] lazy val pCache = allKnownBs.mapTo(p).toMap

  def apply(x: B): LogDouble = {
    if (excludedBs.exists(_.apply(x))) LogDouble.zero
    else pCache.getOrElse(x, p(x))
  }

  private[this] def p(x: B): LogDouble = {
    val count = unsmoothedCounts.getOrElse(x, LogDouble.zero)
    (alpha * prior(x) + count) / (alpha + totalCounts + totalAddition)
  }

  private[this] lazy val sampler = new SimpleLogProbabilityDistribution(pCache)
  def sample(): B = sampler.sample()

  def defaultProb: LogDouble = sys.error("`defaultProb` not available for AlphaBetaLogProbabilityDistribution")
}

class DefaultedLogProbabilityDistribution[B](
  smoothedCounts: Map[B, LogDouble],
  knownBs: Option[Set[B]],
  excludedBs: Option[B => Boolean],
  val defaultCount: LogDouble,
  totalAddition: LogDouble = LogDouble.zero)
  extends AbstractLogProbabilityDistribution[B](totalAddition = totalAddition) {

  def this(smoothedCounts: Map[B, LogDouble]) = this(smoothedCounts, None, None, LogDouble.zero)

  for ((key, count) <- smoothedCounts) assert(!count.isNaN && !count.isInfinite, f"Trying to create DefaultedLogProbabilityDistribution with bad key=$key count=$count: smoothedCounts=$smoothedCounts")
  assert(!totalAddition.isNaN && !totalAddition.isInfinite, f"Trying to create DefaultedLogProbabilityDistribution with bad totalAddition=$totalAddition")

  val allKnownBs = (knownBs.getOrElse(Set()) | smoothedCounts.keySet).filterNot(excludedBs.getOrElse(Set()))
  def counts(b: B) = if (excludedBs.isDefined && excludedBs.get(b)) LogDouble.zero else smoothedCounts.getOrElse(b, defaultCount)
}

class UniformDefaultLogProbabilityDistribution[B](val defaultProb: LogDouble) extends LogProbabilityDistribution[B] {
  def apply(b: B) = defaultProb
  def sample(): B = sys.error("Cannot sample from a UniformDefaultLogProbabilityDistribution")
}

class ConditionalWrappingLogProbabilityDistribution[A, B](a: A, cpd: ConditionalLogProbabilityDistribution[A, B]) extends LogProbabilityDistribution[B] {
  def apply(b: B): LogDouble = cpd(b, a)
  def sample(): B = cpd.sample(a)
  def defaultProb = sys.error("defaultProb not available for ConditionalWrappingLogProbabilityDistribution")
}

class UnconditionalWrappingConditionalLogProbabilityDistribution[A, B](pd: LogProbabilityDistribution[B]) extends ConditionalLogProbabilityDistribution[A, B] {
  def apply(b: B, given: A): LogDouble = pd(b)
  def sample(given: A): B = pd.sample
}

//
//
//

trait ConditionalLogProbabilityDistribution[A, B] extends Serializable {
  def apply(x: B, given: A): LogDouble
  def sample(given: A): B
}

class SimpleConditionalLogProbabilityDistribution[A, B](
  knownDistributions: Map[A, LogProbabilityDistribution[B]],
  default: ConditionalLogProbabilityDistribution[A, B],
  excludedAs: Option[A => Boolean] = None)
  extends ConditionalLogProbabilityDistribution[A, B] {

  def this(conditionedDistributions: Map[A, LogProbabilityDistribution[B]]) =
    this(conditionedDistributions, new UnconditionalWrappingConditionalLogProbabilityDistribution[A, B](LogProbabilityDistribution.empty[B]), None)
  def this(conditionedDistributions: Map[A, LogProbabilityDistribution[B]], defaultPD: LogProbabilityDistribution[B]) =
    this(conditionedDistributions, new UnconditionalWrappingConditionalLogProbabilityDistribution[A, B](defaultPD), None)
  def this(conditionedDistributions: Map[A, LogProbabilityDistribution[B]], defaultPD: LogProbabilityDistribution[B], excludedAs: Option[A => Boolean]) =
    this(conditionedDistributions, new UnconditionalWrappingConditionalLogProbabilityDistribution[A, B](defaultPD), excludedAs)

  def apply(x: B, given: A): LogDouble = {
    if (excludedAs.isDefined && excludedAs.get(given))
      LogDouble.zero
    else
      knownDistributions.get(given).map(_.apply(x)).getOrElse(default(x, given))
  }

  def sample(given: A): B = {
    if (excludedAs.isDefined && excludedAs.get(given))
      sys.error(f"cannot sample from $given")
    else
      knownDistributions.get(given).map(_.sample).getOrElse(default.sample(given))
  }
}

class NormalizingConditionalLogProbabilityDistribution[A, B](
  delegate: ConditionalLogProbabilityDistribution[A, B],
  normalizingConstants: Map[A, LogDouble]) // should have a default
  extends ConditionalLogProbabilityDistribution[A, B] {

  def apply(x: B, given: A): LogDouble = delegate(x, given) / normalizingConstants.getOrElse(given, LogDouble.one)

  def sample(given: A): B = delegate.sample(given)
}

/**
 * Assumes no counts were seen for any As other than those in knownBDistributions!
 *
 * Parameters knownAs and knownBs are used for caching.  They do *not* affect probabilities. They *may*, however, affect sampler choices.
 */
class AlphaBetaConditionalLogProbabilityDistribution[A, B](
  knownBDistributions: Map[A, LogProbabilityDistribution[B]],
  alpha: LogDouble,
  prior: ConditionalLogProbabilityDistribution[A, B],
  knownBs: Option[Set[B]] = None,
  excludedBs: Option[B => Boolean] = None,
  totalAddition: LogDouble = LogDouble.zero)
  extends ConditionalLogProbabilityDistribution[A, B] {

  private[this] val allKnownBs: Set[B] = knownBs.getOrElse(Set.empty).filterNot(excludedBs.getOrElse(Set.empty))

  def apply(x: B, given: A): LogDouble = {
    if (excludedBs.exists(_.apply(x))) LogDouble.zero
    else knownBDistributions.get(given).map(_.apply(x)).getOrElse {
      (alpha * prior(x, given)) / (alpha + totalAddition) // when there are no counts for given
    }
  }

  def sample(given: A): B = knownBDistributions.getOrElse(given, new SimpleLogProbabilityDistribution(allKnownBs.mapTo(b => apply(b, given)).toMap)).sample()
}

object AlphaBetaConditionalLogProbabilityDistribution {
  def apply[A, B](
    unsmoothedCounts: Map[A, Map[B, LogDouble]],
    alpha: LogDouble,
    prior: ConditionalLogProbabilityDistribution[A, B],
    knownAs: Option[Set[A]] = None,
    knownBs: Option[Set[B]] = None,
    excludedBs: Option[B => Boolean] = None,
    totalAddition: LogDouble = LogDouble.zero) = {

    val allKnownAs: Set[A] = (knownAs.getOrElse(Set.empty) ++ unsmoothedCounts.keys)
    val allKnownBs: Set[B] = (knownBs.getOrElse(Set.empty) ++ unsmoothedCounts.values.flatMap(_.keys)).filterNot(excludedBs.getOrElse(Set.empty))
    val knownBDistributions: Map[A, LogProbabilityDistribution[B]] =
      unsmoothedCounts.mapt { (a, bCounts) =>
        a -> new AlphaBetaLogProbabilityDistribution(bCounts, alpha, new ConditionalWrappingLogProbabilityDistribution(a, prior), Some(allKnownBs), excludedBs, totalAddition)
      }

    new AlphaBetaConditionalLogProbabilityDistribution[A, B](
      knownBDistributions,
      alpha,
      prior,
      Some(allKnownBs),
      excludedBs,
      totalAddition)
  }
}

class InterpolatingConditionalLogProbabilityDistribution[A, B](
  delegates: Vector[(ConditionalLogProbabilityDistribution[A, B], LogDouble)])
  extends ConditionalLogProbabilityDistribution[A, B] {

  def apply(x: B, given: A): LogDouble = {
    delegates.sumBy { case (d, w) => d(x, given) * w }
  }

  private[this] val pd = new SimpleLogProbabilityDistribution(delegates.toMap)
  def sample(given: A): B = pd.sample().sample(given)
}

class ReversingConditionalLogProbabilityDistribution[A, B](delegate: ConditionalLogProbabilityDistribution[B, A]) extends ConditionalLogProbabilityDistribution[A, B] {
  def apply(x: B, given: A): LogDouble = delegate(given, x)
  def sample(given: A): B = sys.error("sample not available on ReversingConditionalLogProbabilityDistribution")
}

object ConditionalLogProbabilityDistribution {
  def empty[A, B] = new SimpleConditionalLogProbabilityDistribution[A, B](Map.empty)
}

//
//
//

@deprecated("Convert to LogProbabilityDistribution", "")
class Double2LogProbabilityDistributionAdapter[B](dpd: ExpProbabilityDistribution[B]) extends LogProbabilityDistribution[B] {
  override def apply(b: B): LogDouble = LogDouble(dpd(b))
  override def sample(): B = dpd.sample()
  override def defaultProb: LogDouble = LogDouble(dpd.defaultProb)
}

@deprecated("Convert to ConditionalLogProbabilityDistribution", "")
class Double2LogConditionalProbabilityDistributionAdapter[A, B](dcpd: ConditionalExpProbabilityDistribution[A, B]) extends ConditionalLogProbabilityDistribution[A, B] {
  override def apply(x: B, given: A): LogDouble = LogDouble(dcpd(x, given))
  override def sample(given: A): B = dcpd.sample(given)
}
