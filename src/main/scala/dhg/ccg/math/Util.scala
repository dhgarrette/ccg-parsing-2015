package dhg.ccg.math

import dhg.util.{Counter=>_, _}
import dhg.util.FastMathUtil._
import breeze.linalg.{ Vector => _, _ }
import breeze.stats.distributions.Rand
import org.apache.commons.math3.random.RandomGenerator

trait DirichletSampler {
  //  def dir[A](counts: Map[A, Double]): Map[A, Double]
  //  def dir(counts: Vector[Double]): Vector[Double]
  def logDir[A](counts: Map[A, LogDouble]): Map[A, LogDouble]
  def logDir(counts: Vector[LogDouble]): Vector[LogDouble]

  def logBeta(a: LogDouble, b: LogDouble): LogDouble
}

class SimpleDirichletSampler(randomGenerator: RandomGenerator = Rand.generator) extends DirichletSampler {
  //  def dir[A](counts: Map[A, Double]): Map[A, Double] = breeze.stats.distributions.Dirichlet(Counter(counts)).draw().toMap
  //  def dir(counts: Vector[Double]): Vector[Double] = Vector() ++ breeze.stats.distributions.Dirichlet(DenseVector(counts: _*)).draw().valuesIterator
  def logDir[A](counts: Map[A, LogDouble]): Map[A, LogDouble] = {
    val (keys, values) = counts.toVector.unzip
    (keys zipSafe logDir(values)).toMap
  }
  def logDir(counts: Vector[LogDouble]): Vector[LogDouble] = {
    if (counts.isEmpty) Vector.empty
    else {
      val valueArray = counts.map(_.toDouble).toArray
      convertToLogDirichletDraw(valueArray, valueArray.length, randomGenerator)
      valueArray.toVector.map(new LogDouble(_))
    }
  }

  def logBeta(a: LogDouble, b: LogDouble): LogDouble = {
    logDir(Vector(a, b))(0)
  }
}
object DirSampler extends SimpleDirichletSampler()

class MutableDirichletProcess(totalCount: LogDouble, randomGenerator: RandomGenerator = Rand.generator) {
  private[this] var remainingStick: LogDouble = LogDouble.one
  private[this] var remainingCount: LogDouble = totalCount
  private[this] val dirSampler = new SimpleDirichletSampler(randomGenerator)
  import dirSampler._

  def next(count: LogDouble) = {
    remainingCount -= count
    assert(remainingCount > LogDouble.zero, "count sum exceeded total count")
    val pPrime = logBeta(count, remainingCount)
    val p = pPrime * remainingStick
    //println(f"Beta($count, $remainingCount) * $remainingStick = $p")
    remainingStick *= (LogDouble.one - pPrime)
    p
  }
}

object Util {

  def Dir[A](counts: Map[A, Double]): Map[A, Double] = breeze.stats.distributions.Dirichlet(Counter(counts)).draw().toMap
  def Dir(counts: Vector[Double]): Vector[Double] = Vector() ++ breeze.stats.distributions.Dirichlet(DenseVector(counts: _*)).draw().valuesIterator
  def LogDir[A](counts: Map[A, LogDouble]): Map[A, LogDouble] = DirSampler.logDir(counts)
  def LogDir(counts: Vector[LogDouble]): Vector[LogDouble] = DirSampler.logDir(counts)

  def Beta(a: Double, b: Double): Double = new breeze.stats.distributions.Beta(a, b).draw()
  def LogBeta(a: LogDouble, b: LogDouble): LogDouble = DirSampler.logBeta(a, b)

}
