package dhg.ccg.tag.learn

import dhg.util._
import dhg.util.FastMathUtil._
import math.{ log, exp, abs }
import com.typesafe.scalalogging.slf4j.{ StrictLogging => Logging }
import scala.collection.immutable.BitSet
import scala.collection.breakOut
import scala.util.Random
import annotation.tailrec
import java.util.Arrays
import dhg.ccg.prob._
import org.apache.commons.math3.random.{ MersenneTwister, RandomGenerator }

final class FfbsHmmTaggerTrainer[Tag](
  samplingIterations: Int, burninIterations: Int,
  alphaTrans: Double, alphaEmiss: Double,
  transitionDistributioner: TransitionDistributioner[Tag],
  emissionDistributioner: EmissionDistributioner[Tag],
  rand: RandomGenerator)
  extends SemisupervisedHmmTaggerTrainer[Tag](transitionDistributioner, emissionDistributioner, alphaTrans, alphaEmiss) {

  /**
   * @return: Transition and Emission expected counts
   */
  final override def doTrain(
    sentsWithTokenTags: Vector[(Array[Int], Array[Array[Int]])],
    numWords: Int, numTags: Int,
    rtd: Array[Array[Int]],
    alphaPriorTr: Array[Array[Double]], alphaPriorEm: Array[Array[Double]],
    logTr: Array[Array[Double]], logEm: Array[Array[Double]]) = {

    val trRunningTotalCounts: Array[Array[Double]] = {
      val data = new Array[Array[Double]](numTags)
      var i = 0; while (i < numTags) { data(i) = new Array[Double](numTags); i += 1 }
      data
    }
    val emRunningTotalCounts: Array[Array[Double]] = {
      val data = new Array[Array[Double]](numTags)
      var i = 0; while (i < numTags) { data(i) = new Array[Double](numWords); i += 1 }
      data
    }

    iterate(sentsWithTokenTags, numWords, numTags, rtd, logTr, logEm, alphaPriorTr, alphaPriorEm, trRunningTotalCounts, emRunningTotalCounts, -burninIterations)

    (trRunningTotalCounts.map(_.map(_ / samplingIterations)), emRunningTotalCounts.map(_.map(_ / samplingIterations)))
  }

  /**
   * @return: learned Transition and Emission probability distributions
   */
  @tailrec private[this] final def iterate(
    sentsWithTokenTags: Vector[(Array[Int], Array[Array[Int]])],
    numWords: Int, numTags: Int,
    rtd: Array[Array[Int]],
    logTr: Array[Array[Double]], logEm: Array[Array[Double]],
    alphaPriorTr: Array[Array[Double]], alphaPriorEm: Array[Array[Double]],
    trRunningTotalCounts: Array[Array[Double]], emRunningTotalCounts: Array[Array[Double]],
    iteration: Int): Unit = {

    if (iteration < samplingIterations) {
      val startTime = System.currentTimeMillis()
      val (newLogTr, newLogEm) = reestimate(sentsWithTokenTags, numWords, numTags, rtd, logTr, logEm, alphaPriorTr, alphaPriorEm, trRunningTotalCounts, emRunningTotalCounts, samplingIteration = iteration >= 0)
      println(f"iteration ${((if (iteration < 0) iteration else iteration + 1) + ":").padRight(4)} ${(System.currentTimeMillis() - startTime) / 1000.0}%.3f sec")
      iterate(sentsWithTokenTags, numWords, numTags, rtd, newLogTr, newLogEm, alphaPriorTr, alphaPriorEm, trRunningTotalCounts, emRunningTotalCounts, iteration + 1)
    }
    else {
      println(f"MAX ITERATIONS REACHED")
    }
  }

  private[this] final def reestimate(
    sentsWithTokenTags: Vector[(Array[Int], Array[Array[Int]])],
    numWords: Int, numTags: Int,
    rtd: Array[Array[Int]],
    logTr: Array[Array[Double]], logEm: Array[Array[Double]],
    alphaPriorTr: Array[Array[Double]], alphaPriorEm: Array[Array[Double]],
    trRunningTotalCounts: Array[Array[Double]], emRunningTotalCounts: Array[Array[Double]],
    samplingIteration: Boolean //
    ): (Array[Array[Double]], Array[Array[Double]]) = {

    // DEBUG
    //println("\nTRANSITIONS"); for (t1 <- 0 until numTags) println((0 until numTags).map(t2 => if (t1 != 1 && t2 != 0 && !(t1 == 0 && t2 <= 1)) f"${exp(logTr(t1)(t2))}%.4f" else "").mkString("\t"))
    //println("\nEMISSIONS"); for (t <- 0 until numTags) println((0 until numWords).map(w => if (td(w).contains(t)) f"${exp(logEm(t)(w))}%.4f" else "").mkString("\t")); println

    val trCounts: Array[Array[Double]] = {
      val data = new Array[Array[Double]](numTags)
      var i = 0; while (i < numTags) { val a = new Array[Double](numTags); System.arraycopy(alphaPriorTr(i), 0, a, 0, numTags); data(i) = a; i += 1 }
      data
    }
    val emCounts: Array[Array[Double]] = {
      val data = new Array[Array[Double]](numTags)
      var i = 0; while (i < numTags) { val a = new Array[Double](numWords); System.arraycopy(alphaPriorEm(i), 0, a, 0, numWords); data(i) = a; i += 1 }
      data
    }

    for ((s, stags) <- sentsWithTokenTags.seq) {
      contributeCounts(s, stags, numWords, numTags, logTr, logEm, trCounts, emCounts, trRunningTotalCounts, emRunningTotalCounts, samplingIteration)
    }

    //println("\nTRANSITION COUNTS"); for (t1 <- 0 until numTags) println((0 until numTags).map(t2 => if (t1 != 1 && t2 != 0 && !(t1 == 0 && t2 <= 1)) f"${expectedTrCounts(t1)(t2)}%.4f" else "").mkString("\t"))
    //println("\nEMISSION COUNTS"); for (t <- 0 until numTags) println((0 until numWords).map(w => if (td(w).contains(t)) f"${expectedEmCounts(t)(w)}%.4f" else "").mkString("\t")); println

    convertCountsToProbabilitiesWithDirichlet(trCounts, emCounts, numWords, numTags, rtd)

    // At this point the "count" are actually log probabilities!!
    (trCounts, emCounts)
  }

  /**
   * Convert, IN-PLACE, counts matrices into conditional
   * probability distribution matrices.
   */
  private[this] final def convertCountsToProbabilitiesWithDirichlet(
    trCounts: Array[Array[Double]], emCounts: Array[Array[Double]],
    numWords: Int, numTags: Int,
    rtd: Array[Array[Int]]): Unit = {
    // newLogTr
    //   1. Divide by sum (to get probability)
    //   2. Log
    var k1 = 0
    while (k1 < numTags) {
      convertToLogDirichletDraw(trCounts(k1), numTags, rand)
      //normalizeAndLog(trCounts(k1), numTags)
      k1 += 1
    }

    // newLogEm
    //   1. Divide by sum (to get probability) 
    //   2. Log
    //    expectedEmCounts(0)(0) = 0.0
    //    expectedEmCounts(1)(1) = 0.0
    var k = 2
    while (k < numTags) {
      val rtdK = rtd(k)
      val rtdKLen = rtdK.length
      convertActiveToLogDirichletDraw(emCounts(k), rtdK, rtdKLen, rand)
      //activeNormalizeAndLog(emCounts(k), rtdK, rtdKLen)
      k += 1
    }

    // At this point the "counts" are actually log probabilities!!
  }

  private[this] final def contributeCounts(
    sent: Array[Int], tokenTags: Array[Array[Int]],
    numWords: Int, numTags: Int,
    logTr: Array[Array[Double]], logEm: Array[Array[Double]],
    expectedTrCounts: Array[Array[Double]], expectedEmCounts: Array[Array[Double]],
    trRunningTotalCounts: Array[Array[Double]], emRunningTotalCounts: Array[Array[Double]],
    samplingIteration: Boolean): Unit = {

    val logSumHolder = new Array[Double](numTags)

    val sentLen = sent.length

    val logTrellis = Array.fill(sentLen)(new Array[Double](numTags))
    //logTrellis(0)(0) = 0.0

    // FORWARD FILTER
    var t = 1
    while (t < sentLen) {

      val curw = sent(t)
      val curwK = tokenTags(t)
      val curwKlen = curwK.length

      val prevw = sent(t - 1)
      val prevwK = tokenTags(t - 1)
      val prevwKlen = prevwK.length

      if (curwKlen > 1) { // don't bother calculating a real score if there's only one choice (call it p=1.0)
        var curwKi = 0
        while (curwKi < curwKlen) {
          val k = curwK(curwKi)

          if (prevwKlen == 1) { // if the was only one previous choice, don't bother multiplying and summing
            val kPrime = prevwK(0)
            logTrellis(t)(k) = logEm(k)(curw) + logTr(kPrime)(k)
          }
          else {
            var prevwKi = 0
            while (prevwKi < prevwKlen) {
              val kPrime = prevwK(prevwKi)
              logSumHolder(prevwKi) = logTr(kPrime)(k) + logTrellis(t - 1)(kPrime)
              prevwKi += 1
            }
            logTrellis(t)(k) = logEm(k)(curw) + logSum(logSumHolder, prevwKlen)
          }

          curwKi += 1
        }
      }

      t += 1
    }

    // DEBUG
    //println("\nTRELLIS"); println((0 until sentLen).mkString("\t", "\t", "")); for (t <- 0 to numTags) { println(t + "\t" + sent.zipWithIndex.map { case (w, i) => if (td(w).contains(t)) f"${exp(logTrellis(i)(t))}%.4f" else "" }.mkString("\t")) }

    // DEBUG
    //println("Backward Sample :")

    //BACKWARD SAMPLE
    var nextTag = 1 // end tag
    t = sentLen - 2 // ignore end symbol
    while (t > 0) { // ignore start symbol

      // DEBUG
      //print(f" $t ")

      val curw = sent(t)
      val curwK = tokenTags(t)
      val curwKlen = curwK.length

      val curTag =
        if (curwKlen == 1) {
          curwK(0)
        }
        else {
          val curkLogDist = logTrellis(t)
          var curwKi = 0
          while (curwKi < curwKlen) {
            val k = curwK(curwKi)
            curkLogDist(k) += logTr(k)(nextTag)
            curwKi += 1
          }

          // DEBUG
          //println(f"activeLogChoose(${curkLogDist.map(exp).mkString("[", ", ", "]")}, ${curwK.mkString("[", ", ", "]")}, $curwKlen, $rand)")
          //val tempSum = activeLogSum(curkLogDist, curwK, curwKlen); print(f"activeLogChoose(${curwK.take(curwKlen).map(k => f"$k: ${exp(curkLogDist(k) - tempSum)}%.4f").mkString("[", ", ", "]")}, $rand)")

          activeLogChoose(curkLogDist, curwK, curwKlen, rand)
        }

      // DEBUG
      //println(" -> " + curTag)

      expectedTrCounts(curTag)(nextTag) += 1
      expectedEmCounts(curTag)(curw) += 1
      if (samplingIteration) {
        trRunningTotalCounts(curTag)(nextTag) += 1
        emRunningTotalCounts(curTag)(curw) += 1
      }

      nextTag = curTag
      t -= 1
    }

    // DEBUG 
    //println

    expectedTrCounts(0)(nextTag) += 1 //     start transition
    if (samplingIteration) {
      trRunningTotalCounts(0)(nextTag) += 1 // start transition
    }
  }

  override final def toString = f"FfbsHmmTaggerTrainer(sampling=$samplingIterations, burnin=$burninIterations, alphaTrans=$alphaTrans, alphaEmiss=$alphaEmiss, $transitionDistributioner, $emissionDistributioner)"
}
