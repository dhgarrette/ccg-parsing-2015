package dhg.ccg.prob

import dhg.util._
import scalaz._
import Scalaz._

class AddLambdaSmoother[A, B](
  lambda: Double = 0.1,
  additionalKnownAs: Set[A] = Set.empty[A],
  excludedAs: Option[Set[A]] = none[Set[A]],
  additionalKnownBs: Set[B] = Set.empty[B],
  excludedBs: Option[Set[B]] = none[Set[B]]) {

  def apply(counts: Map[A, Map[B, Double]]) = {
    val allKnownAs = counts.keySet | additionalKnownAs
    val allKnownBs = counts.flatMap(_._2.keySet).toSet | additionalKnownBs

    new SimpleConditionalExpProbabilityDistribution[A, B](
      counts.mapVals(new LaplaceExpProbabilityDistribution(_, Some(allKnownBs), excludedBs, lambda)),
      Some(allKnownAs),
      excludedAs,
      new LaplaceExpProbabilityDistribution(Map(), Some(allKnownBs), excludedBs, lambda))
  }
  override def toString = f"AddLambdaSmoother($lambda)"

}
