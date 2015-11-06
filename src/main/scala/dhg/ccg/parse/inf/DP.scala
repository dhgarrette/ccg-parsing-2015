package dhg.ccg.parse.inf

import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import org.apache.commons.math3.random.RandomGenerator
import breeze.stats.distributions.Rand
import dhg.ccg.math.SimpleDirichletSampler
import dhg.util._
import dhg.ccg.prob.LogProbabilityDistribution
import dhg.ccg.prob.SimpleLogProbabilityDistribution

trait DP {

}

class MemoizingDP[A](observedCounts: Map[A, LogDouble], prior: LogProbabilityDistribution[A], alpha: LogDouble, randomGenerator: RandomGenerator = Rand.generator) {
  private[this] val probs = MMap[A, LogDouble]()
  private[this] var remainingStick: LogDouble = LogDouble.one
  private[this] var remainingCount: LogDouble = observedCounts.values.sum + alpha
  private[this] val dirSampler = new SimpleDirichletSampler(randomGenerator)
  import dirSampler._

  /**
   * Get the probability of the given A according to the probability
   * distribution sampled by this DP.  If the probability has previously
   * been sampled, return it.  Otherwise, sample a new one and store it.
   */
  def apply(a: A): LogDouble = {
    probs.getOrElseUpdate(a, {
      val count = observedCounts.getOrElse(a, LogDouble.zero) + alpha * prior(a)
      remainingCount -= count
      assert(remainingCount > LogDouble.zero, "count sum exceeded total count")
      val pPrime = logBeta(count, remainingCount)
      val p = pPrime * remainingStick
      //println(f"$a:  Beta($count, $remainingCount) * $remainingStick = $p")
      remainingStick *= (LogDouble.one - pPrime)
      p
    })
  }
  
  def unassignedProb = remainingStick
  def asSimplePD = new SimpleLogProbabilityDistribution(Map() ++ probs)
}
