package dhg.ccg.tag.learn

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.math.abs
import scala.math.exp
import scala.math.log
import dhg.util._
import dhg.util.FastMathUtil._
import dhg.ccg.prob.ConditionalLogProbabilityDistribution
import dhg.ccg.tag._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tagdict.SimpleTagDictionary

class SimpleTypeSupervisedTaggerTrainer[Tag](
  learner: SemisupervisedTaggerTrainer[Tag],
  trInitializer: TransitionInitializer[Tag], emInitializer: EmissionInitializer[Tag])
    extends TypeSupervisedTaggerTrainer[Tag] {
  override def typesupTrain(rawSentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Tag]): Tagger[Tag] = {
    val tagdict = initialTagdict.withWords(rawSentences.flatten.toSet)
    println("Make Transition Distributions")
    val transitions = trInitializer.fromRaw(rawSentences, tagdict)
    println("Make Emission Distributions")
    val emissions = emInitializer.fromRaw(rawSentences, tagdict)
    learner.train(rawSentences, tagdict, transitions, emissions)
  }

  override final def toString = f"SimpleTypeSupervisedTaggerTrainer($learner, $trInitializer, $emInitializer)"
}

class SimpleNoisilySupervisedTaggerTrainer[Tag](
  learner: SemisupervisedTaggerTrainer[Tag],
  transitionDistributioner: TransitionDistributioner[Tag], emissionDistributioner: EmissionDistributioner[Tag])
    extends NoisilySupervisedTaggerTrainer[Tag] {
  override def noisySupTrainWithSomeGold(noisilyLabeledSentences: Vector[Vector[(Word, Tag)]], goldLabeledSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Tag]): Tagger[Tag] = {
    val tagdict = initialTagdict
      .withWords(noisilyLabeledSentences.flatten.map(_._1).toSet ++ goldLabeledSentences.flatten.map(_._1))
      .withTags(noisilyLabeledSentences.flatten.map(_._2).toSet ++ goldLabeledSentences.flatten.map(_._2))
    val transitions = transitionDistributioner(noisilyLabeledSentences ++ goldLabeledSentences, tagdict)
    val emissions = emissionDistributioner(noisilyLabeledSentences ++ goldLabeledSentences, tagdict)
    learner.trainWithSomeGold(noisilyLabeledSentences.map(_.map(_._1)), goldLabeledSentences, tagdict, transitions, emissions)
  }

  override final def toString = f"SimpleNoisilySupervisedTaggerTrainer($learner, $transitionDistributioner, $emissionDistributioner)"
}

abstract class SemisupervisedHmmTaggerTrainer[Tag](
  transitionDistributioner: TransitionDistributioner[Tag],
  emissionDistributioner: EmissionDistributioner[Tag],
  alphaT: Double, alphaE: Double)
    extends SemisupervisedTaggerTrainer[Tag] {

  final override def trainWithTagsetsAndSomeGold(
    rawSentencesWithTokenTags: Vector[Vector[(Word, Set[Tag])]], goldLabeledSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Tag],
    transitions: ConditionalLogProbabilityDistribution[Tag, Tag], emissions: ConditionalLogProbabilityDistribution[Tag, Word]) = {
    val rawSentences = rawSentencesWithTokenTags.map(_.map(_._1))
    val tagdict = initialTagdict.withWords(rawSentences.flatten.toSet)

    //
    //
    //

    val allActualWords = rawSentences.flatten.toSet.toVector
    val allWords = tagdict.startWord +: tagdict.endWord +: allActualWords
    val allTdTags = allActualWords.flatMap(tagdict).distinct // filter out tags that are not used by any word
    val allTags = tagdict.startTag +: tagdict.endTag +: allTdTags

    val numWords = allWords.size
    val numTags = allTags.size

    println(f"raw tokens = ${rawSentences.flatten.size}  (${rawSentences.size} sentences)")
    println("numWords = " + numWords)
    println("numTags  = " + numTags)
    println

    val wordIndex = allWords.zipWithIndex.toMap
    val tagIndex = allTags.zipWithIndex.toMap

    val td: Array[Array[Int]] = Array(0) +: Array(1) +: {
      val fullTagsetSet = (2 until numTags).toArray
      allWords.drop(2).map { w =>
        val tdws = tagdict(w)
        if (tdws.size < numTags - 2) // incomplete set of tags
          tdws.map(tagIndex)(breakOut): Array[Int]
        else // complete set of tags
          fullTagsetSet // re-use the same array
      }(breakOut): Array[Array[Int]]
    }

    val rtd: Array[Array[Int]] = Array(0) +: Array(1) +: {
      allTags.drop(2).map { t =>
        allWords.zipWithIndex.drop(2).collect { case (w, wi) if tagdict(w)(t) => wi }(breakOut): Array[Int]
      }(breakOut): Array[Array[Int]]
    }

    // DEBUG
    //rawSentences.foreach(s => println(s.mkString(" "))); println

    // DEBUG
    //println("TAG DICT"); for (w <- allWords.sortBy(_.toString)) println(f"  $w%-6s -> [${td(wordIndex(w)).map(allTags).sortBy(_.toString).mkString(", ")}]"); println
    //println("REVERSE TAG DICT"); for (t <- allTags.sortBy(_.toString)) println(f"  $t -> [${rtd(tagIndex(t)).map(allWords).sortBy(_.toString).mkString(", ")}]"); println

    println("Make Indexed Distributions")
    val logInitialTr: Array[Array[Double]] = Array.tabulate(numTags) { t1 => Array.tabulate(numTags) { t2 => transitions(allTags(t2), allTags(t1)).logValue } }
    val logInitialEm: Array[Array[Double]] = Array.tabulate(numTags) { t => Array.tabulate(numWords) { w => emissions(allWords(w), allTags(t)).logValue } }

    println(f"Make Prior Counts (from the ${goldLabeledSentences.size} gold labeled sentences)")
    val trGoldCounts: Array[Array[Double]] = Array.fill(numTags) { new Array[Double](numTags) }
    for {
      (a, b) <- goldLabeledSentences.flatMap(s => (tagdict.startTag +: s.map(_._2) :+ tagdict.endTag).sliding2)
      ai <- tagIndex.get(a)
      bi <- tagIndex.get(b)
    } { trGoldCounts(ai)(bi) += 1 }
    val emGoldCounts: Array[Array[Double]] = Array.fill(numTags) { new Array[Double](numWords) }
    for {
      (w, t) <- goldLabeledSentences.flatten
      ti <- tagIndex.get(t)
      wi <- wordIndex.get(w)
    } { emGoldCounts(ti)(wi) += 1 }

    // DEBUG
    //println("\nTRANSITIONS"); println("\t" + (0 until numTags).map(allTags).mkString("\t")); for (t1 <- 0 until numTags) println(allTags(t1) + "\t" + (0 until numTags).map(t2 => if (t1 != 1 && t2 != 0 && !(t1 == 0 && t2 <= 1)) f"${tr(t1)(t2)}%.2f" else "").mkString("\t"))
    //println("\nEMISSIONS"); println("\t" + (0 until numWords).map(allWords).mkString("\t")); for (t <- 0 until numTags) println(allTags(t) + "\t" + (0 until numWords).map(w => if (td(w).contains(t)) f"${em(t)(w)}%.2f" else "").mkString("\t")); println

    val sentsWithTokenTags: Vector[(Array[Int], Array[Array[Int]])] = rawSentencesWithTokenTags.map { sentWithTokenTags =>
      val (s, tokenTags) = sentWithTokenTags.unzip
      val sent = (tagdict.startWord +: s :+ tagdict.endWord).map(wordIndex).toArray
      val tokTags = (Set(tagdict.startTag) +: tokenTags :+ Set(tagdict.endTag)).map(_.map(tagIndex).toArray.sorted).toArray
      (sent, tokTags)
    }

    // Prepare for re-use later: alpha*beta + (gold counts), to be added to data counts before taking dirichlet sample 
    val alphaPriorTr: Array[Array[Double]] = Array.tabulate(numTags, numTags) { (a, b) => alphaT * math.exp(logInitialTr(a)(b)) + trGoldCounts(a)(b) }
    val alphaPriorEm: Array[Array[Double]] = Array.tabulate(numTags, numWords) { (t, w) => alphaE * math.exp(logInitialEm(t)(w)) + emGoldCounts(t)(w) }; alphaPriorEm(0)(0) = 0.0; alphaPriorEm(1)(1) = 0.0

    println("Start Training")
    val (trExpectedLogCounts, emExpectedLogCounts) = doTrain(sentsWithTokenTags, numWords, numTags, rtd, alphaPriorTr, alphaPriorEm, logInitialTr, logInitialEm)

    val trExpectedCountsUnindexed =
      (0 until numTags).map(k1 => allTags(k1) ->
        (0 until numTags).map(k2 => allTags(k2) ->
          (if (k1 < 2 && k2 < 2) LogDouble.zero else new LogDouble(trExpectedLogCounts(k1)(k2)))).toMap).toMap +
        (tagdict.endTag -> Map[Tag, LogDouble]())
    val tagCountsUnindexed = trExpectedCountsUnindexed.mapVals(_.values.sum) + (tagdict.endTag -> trExpectedCountsUnindexed.values.map(_.getOrElse(tagdict.endTag, LogDouble.zero)).sum)
    val emLearnedTr = transitionDistributioner.make(trExpectedCountsUnindexed, tagCountsUnindexed, tagdict)

    val emExpectedCountsUnindexed =
      (2 until numTags).map(t => allTags(t) ->
        rtd(t).map(w => allWords(w) ->
          new LogDouble(emExpectedLogCounts(t)(w))).toMap).toMap +
        (tagdict.startTag -> Map(tagdict.startWord -> LogDouble.one)) + (tagdict.endTag -> Map(tagdict.endWord -> LogDouble.one))
    val emLearnedEm = emissionDistributioner.make(emExpectedCountsUnindexed, tagdict)

    new HmmTagger(emLearnedTr, emLearnedEm, tagdict)
  }

  def doTrain(
    sentsWithTokenTags: Vector[(Array[Int], Array[Array[Int]])],
    numWords: Int, numTags: Int,
    rtd: Array[Array[Int]],
    alphaPriorTr: Array[Array[Double]], alphaPriorEm: Array[Array[Double]],
    logInitialTr: Array[Array[Double]], logInitialEm: Array[Array[Double]]): //
    (Array[Array[Double]], Array[Array[Double]])

  /**
   * Convert, IN-PLACE, counts matrices into conditional
   * probability distribution matrices.
   */
  protected[this] final def convertCountsToProbabilities(
    trCounts: Array[Array[Double]], emCounts: Array[Array[Double]],
    numWords: Int, numTags: Int,
    rtd: Array[Array[Int]]): Unit = {
    // newLogTr
    //   1. Divide by sum (to get probability)
    //   2. Log
    var k1 = 0
    while (k1 < numTags) {
      normalizeAndLog(trCounts(k1), numTags)
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
      activeNormalizeAndLog(emCounts(k), rtdK, rtdKLen)
      k += 1
    }

    // At this point the "counts" are actually log probabilities!!
  }

  /**
   * Convert, IN-PLACE, counts matrices into conditional
   * probability distribution matrices.
   */
  protected[this] final def convertLogCountsToProbabilities(
    trLogCounts: Array[Array[Double]], emLogCounts: Array[Array[Double]],
    numWords: Int, numTags: Int,
    rtd: Array[Array[Int]]): Unit = {
    // newLogTr
    //   1. Divide by sum (to get probability)
    //   2. Log
    var k1 = 0
    while (k1 < numTags) {
      logNormalize(trLogCounts(k1), numTags)
      k1 += 1
    }

    // newLogEm
    //   1. Divide by sum (to get probability) 
    //   2. Log
    emLogCounts(0)(0) = 0.0
    emLogCounts(1)(1) = 0.0
    var k = 2
    while (k < numTags) {
      val rtdK = rtd(k)
      val rtdKLen = rtdK.length
      activeLogNormalize(emLogCounts(k), rtdK, rtdKLen)
      k += 1
    }

    // At this point the "counts" are actually log probabilities!!
  }
}

abstract class EmHmmTaggerTrainer[Tag](
  maxIterations: Int,
  transitionDistributioner: TransitionDistributioner[Tag],
  emissionDistributioner: EmissionDistributioner[Tag],
  alphaT: Double, alphaE: Double,
  convergence: Double)
    extends SemisupervisedHmmTaggerTrainer[Tag](transitionDistributioner, emissionDistributioner, alphaT, alphaE) {

  protected[this] final def makeMatrix(wLen: Int, numTags: Int) = {
    val data = new Array[Array[Double]](wLen)
    var i = 0; while (i < wLen) { data(i) = new Array[Double](numTags); i += 1 }
    data
  }

}

class SoftEmHmmTaggerTrainer[Tag](
  maxIterations: Int,
  transitionDistributioner: TransitionDistributioner[Tag],
  emissionDistributioner: EmissionDistributioner[Tag],
  alphaT: Double = 0.0, alphaE: Double = 0.0,
  convergence: Double = 1e-10)
    extends EmHmmTaggerTrainer[Tag](maxIterations, transitionDistributioner, emissionDistributioner, alphaT, alphaE, convergence) {

  /**
   * @return: Transition and Emission expected log counts
   */
  final override def doTrain(
    sentsWithTokenTags: Vector[(Array[Int], Array[Array[Int]])],
    numWords: Int, numTags: Int,
    rtd: Array[Array[Int]],
    alphaPriorTr: Array[Array[Double]], alphaPriorEm: Array[Array[Double]],
    logInitialTr: Array[Array[Double]], logInitialEm: Array[Array[Double]]) = {

    val alphaPriorLogTr = alphaPriorTr.map(_.map(log))
    val alphaPriorLogEm = alphaPriorEm.map(_.map(log))
    val (expectedTrLogCounts, expectedEmLogCounts) = iterate(sentsWithTokenTags, numWords, numTags, rtd, alphaPriorLogTr, alphaPriorLogEm, logInitialTr, logInitialEm, 1, Double.NegativeInfinity)
    (expectedTrLogCounts, expectedEmLogCounts)
  }

  /**
   * @return: Transition and Emission expected log counts
   */
  @tailrec private[this] final def iterate(
    sentsWithTokenTags: Vector[(Array[Int], Array[Array[Int]])],
    numWords: Int, numTags: Int,
    rtd: Array[Array[Int]],
    alphaPriorLogTr: Array[Array[Double]], alphaPriorLogEm: Array[Array[Double]],
    logTr: Array[Array[Double]], logEm: Array[Array[Double]],
    iteration: Int, prevAvgLogProb: Double //
    ): (Array[Array[Double]], Array[Array[Double]]) = {

    val startTime = System.currentTimeMillis()
    val (expectedTrLogCounts, expectedEmLogCounts, avgLogProb) = reestimate(sentsWithTokenTags, numWords, numTags, rtd, alphaPriorLogTr, alphaPriorLogEm, logTr, logEm)
    println(f"iteration ${(iteration + ":").padRight(4)} ${(System.currentTimeMillis() - startTime) / 1000.0}%.3f sec   avgLogProb=${(avgLogProb + ",").padRight(22)} avgProb=${exp(avgLogProb)}")
    //println("\nTRANSITIONS"); for (t1 <- 0 until numTags) println((0 until numTags).map(t2 => if (t1 != 1 && !(t1 == 0 && t2 <= 1)) f"${exp(expectedTrLogCounts(t1)(t2))}%.4f" else "").mkString("\t"))
    //println("\nEMISSIONS"); for (t <- 0 until numTags) println((0 until numWords).map(w => if (tokenTags(i).contains(t)) f"${exp(newLogEm(t)(w))}%.4f" else "").mkString("\t")); println
    if (iteration >= maxIterations) {
      println(f"MAX ITERATIONS REACHED")
      (expectedTrLogCounts, expectedEmLogCounts)
    }
    else if (avgLogProb < prevAvgLogProb) {
      println(f"DIVERGENCE!")
      assert(false, "DIVERGENCE!")
      (expectedTrLogCounts, expectedEmLogCounts)
    }
    else if (avgLogProb - prevAvgLogProb < convergence) {
      println(f"CONVERGENCE (${avgLogProb - prevAvgLogProb} < $convergence)")
      (expectedTrLogCounts, expectedEmLogCounts)
    }
    else {
      convertLogCountsToProbabilities(expectedTrLogCounts, expectedEmLogCounts, numWords, numTags, rtd)
      // At this point the "counts" are actually log probabilities!!
      iterate(sentsWithTokenTags, numWords, numTags, rtd, alphaPriorLogTr, alphaPriorLogEm, expectedTrLogCounts, expectedEmLogCounts, iteration + 1, avgLogProb)
    }
  }

  private[this] final def reestimate(
    sentsWithTokenTags: Vector[(Array[Int], Array[Array[Int]])],
    numWords: Int, numTags: Int,
    rtd: Array[Array[Int]],
    alphaPriorLogTr: Array[Array[Double]], alphaPriorLogEm: Array[Array[Double]],
    logTr: Array[Array[Double]], logEm: Array[Array[Double]] //
    ) = {

    val expectedTrLogCounts: Array[Array[Double]] = {
      val data = new Array[Array[Double]](numTags)
      var i = 0; while (i < numTags) { val a = new Array[Double](numTags); System.arraycopy(alphaPriorLogTr(i), 0, a, 0, numTags); data(i) = a; i += 1 }
      data
    }
    val expectedEmLogCounts: Array[Array[Double]] = {
      val data = new Array[Array[Double]](numTags)
      var i = 0; while (i < numTags) { val a = new Array[Double](numWords); System.arraycopy(alphaPriorLogEm(i), 0, a, 0, numWords); data(i) = a; i += 1 }
      data
    }

    var logProbSum = 0.0
    for ((s, stags) <- sentsWithTokenTags.seq) {
      logProbSum += contributeExpectations(expectedTrLogCounts, expectedEmLogCounts, s, stags, numWords, numTags, rtd, logTr, logEm)
    }

    //println("\nTRANSITION COUNTS"); for (t1 <- 0 until numTags) println((0 until numTags).map(t2 => if (t1 != 1 && !(t1 == 0 && t2 <= 1)) f"${expectedTrLogCounts(t1)(t2)}%.4f" else "").mkString("\t"))
    //println("\nEMISSION COUNTS"); for (t <- 0 until numTags) println((0 until numWords).map(w => if (td(w).contains(t)) f"${expectedEmLogCounts(t)(w)}%.4f" else "").mkString("\t")); println

    (expectedTrLogCounts, expectedEmLogCounts, logProbSum / sentsWithTokenTags.size)
  }

  /*
   * Forward-Backward
   */
  private[this] final def contributeExpectations(
    expectedTrLogCounts: Array[Array[Double]],
    expectedEmLogCounts: Array[Array[Double]],
    w: Array[Int], tokenTags: Array[Array[Int]],
    numWords: Int, numTags: Int,
    rtd: Array[Array[Int]],
    logTr: Array[Array[Double]], logEm: Array[Array[Double]]): Double = {

    assert(w.head == 0 && w.last == 1)

    val logFwd = calculateForward(w, tokenTags, numWords, numTags, logTr, logEm)
    val logBkd = calculateBackwrd(w, tokenTags, numWords, numTags, logTr, logEm)

    val logFwdP = logFwd.last(1)
    val logBkdP = logBkd.head(0)
    assert(abs(logFwdP - logBkdP) < 1e-10, f"$logFwdP != $logBkdP")

    contributeExpectedTrCounts(expectedTrLogCounts, w, tokenTags, numWords, numTags, logTr, logEm, logFwd, logFwdP, logBkd, logBkdP)
    contributeExpectedEmCounts(expectedEmLogCounts, w, tokenTags, numWords, numTags, logTr, logEm, logFwd, logFwdP, logBkd, logBkdP)

    logFwdP
  }

  private[this] final def calculateForward(
    w: Array[Int], tokenTags: Array[Array[Int]],
    numWords: Int, numTags: Int,
    logTr: Array[Array[Double]], logEm: Array[Array[Double]]) = {
    //println("FORWARD")
    val logFwd = makeMatrix(w.length, numTags)

    // For temporary storage
    val logValueArray = new Array[Double](numTags)

    //logFwd(0)(0) = 0.0
    var i = 1
    while (i < w.length) {
      val curLogFwd = logFwd(i)
      val prevLogFwd = logFwd(i - 1)

      val curW = w(i)
      val curWKs = tokenTags(i)
      val curWKsLen = curWKs.length
      val prevKs = tokenTags(i - 1)
      val prevKsLen = prevKs.length
      assert(prevKsLen > 0, f"prevKsLen = $prevKsLen; td(${w(i - 1)}) = ${tokenTags(i - 1).toVector}") // TODO: REMOVE

      var j = 0
      while (j < curWKsLen) {
        val k = curWKs(j)
        var l = 0
        while (l < prevKsLen) {
          val k1 = prevKs(l)
          //assert(!logTr(k1)(k).isNegInfinity, f"logTr($k1)($k) is infinite") // TODO: REMOVE
          //assert(!prevLogFwd(k1).isNegInfinity, f"prevLogFwd($k1) is infinite") // TODO: REMOVE
          val v = logTr(k1)(k) + prevLogFwd(k1)
          logValueArray(l) = v
          l += 1
        }
        //assert(!logEm(k)(curW).isNegInfinity, f"logEm($k)($curW) is infinite") // TODO: REMOVE
        curLogFwd(k) = logSum(logValueArray, prevKsLen) + logEm(k)(curW)
        //assert(!curLogFwd(k).isNegInfinity, f"curLogFwd($k) is infinite; logSum(${logValueArray.toVector}, $prevKsLen) + ${logEm(k)(curW)}") // TODO: REMOVE
        j += 1
      }
      //println(f"$i%3d: " + curLogFwd.zipWithIndex.map { case (v, k) => if (td(w(i)).contains(k)) exp(v).toString else "" }.map(_.padRight(30)).mkString(" "))
      i += 1
    }
    logFwd
  }

  private[this] final def calculateBackwrd(
    w: Array[Int], tokenTags: Array[Array[Int]],
    numWords: Int, numTags: Int,
    logTr: Array[Array[Double]], logEm: Array[Array[Double]]) = {
    //println("BACKWARD")
    val logBkd = makeMatrix(w.length, numTags)

    // For temporary storage
    val logValueArray = new Array[Double](numTags)

    //logBkd(w.length-1)(0) = 0.0
    var i = w.length - 2
    while (i >= 0) {
      val curLogBkd = logBkd(i)
      val nextLogBkd = logBkd(i + 1)

      val curW = w(i)
      val curWKs = tokenTags(i)
      val curWKsLen = curWKs.length
      val nextW = w(i + 1)
      val nextKs = tokenTags(i + 1)
      val nextKsLen = nextKs.length

      var j = 0
      while (j < curWKsLen) {
        val k = curWKs(j)
        var l = 0
        while (l < nextKsLen) {
          val k2 = nextKs(l)
          assert(!logTr(k)(k2).isNegInfinity, f"logTr($k)($k2) is infinite") // TODO: REMOVE
          assert(!logEm(k2)(nextW).isNegInfinity, f"logEm($k2)($nextW) is infinite") // TODO: REMOVE
          logValueArray(l) = logTr(k)(k2) + logEm(k2)(nextW) + nextLogBkd(k2)
          l += 1
        }
        curLogBkd(k) = logSum(logValueArray, nextKsLen)
        j += 1
      }
      //println(f"$i%3d: " + curLogBkd.zipWithIndex.map { case (v, k) => if (td(w(i)).contains(k)) exp(v).toString else "" }.map(_.padRight(30)).mkString(" "))
      i -= 1
    }
    logBkd
  }

  private[this] final def contributeExpectedTrCounts(
    expectedTrLogCounts: Array[Array[Double]],
    w: Array[Int], tokenTags: Array[Array[Int]],
    numWords: Int, numTags: Int,
    logTr: Array[Array[Double]], logEm: Array[Array[Double]],
    logFwd: Array[Array[Double]], logFwdP: Double,
    logBkd: Array[Array[Double]], logBkdP: Double) = {
    var i = 0
    while (i < w.length - 1) {

      val curW = w(i)
      val curWKs = tokenTags(i)
      val curWKsLen = curWKs.length
      val nextW = w(i + 1)
      val nextWKs = tokenTags(i + 1)
      val nextWKsLen = nextWKs.length

      var j = 0
      while (j < curWKsLen) {
        val k1 = curWKs(j)
        val exLogTrK1 = expectedTrLogCounts(k1)
        var l = 0
        while (l < nextWKsLen) {
          val k2 = nextWKs(l)
          val logEx = logFwd(i)(k1) + logTr(k1)(k2) + logEm(k2)(nextW) + logBkd(i + 1)(k2)
          exLogTrK1(k2) = logSum(exLogTrK1(k2), logEx - logFwdP)
          //println(f"giuhrgis::  expectedTrLogCounts($k1)($k2) = ${expectedTrLogCounts(k1)(k2)}")
          l += 1
        }
        j += 1
      }
      i += 1
    }
    //    println((0 until numTags).flatMap(k1 => (0 until numTags).map(k2 => f"exTr($k1)($k2)=${exTr(k1)(k2)}")).mkString(" "))
  }

  private[this] final def contributeExpectedEmCounts(
    expectedEmLogCounts: Array[Array[Double]],
    w: Array[Int], tokenTags: Array[Array[Int]],
    numWords: Int, numTags: Int,
    logTr: Array[Array[Double]], logEm: Array[Array[Double]],
    logFwd: Array[Array[Double]], logFwdP: Double,
    logBkd: Array[Array[Double]], logBkdP: Double) = {
    var i = 1
    val wLen = w.length - 1
    while (i < wLen) {

      val curW = w(i)
      val curWKs = tokenTags(i)
      val curWKsLen = curWKs.length

      var j = 0
      while (j < curWKsLen) {
        val k = curWKs(j)
        val logEx = logFwd(i)(k) + logBkd(i)(k)
        val exLogEmK = expectedEmLogCounts(k)
        exLogEmK(curW) = logSum(exLogEmK(curW), logEx - logFwdP)
        assert(!expectedEmLogCounts(k)(curW).isNegInfinity, f"expectedEmLogCounts($k)($curW) is infinite; logEx=$logFwdP-logEx=$logFwdP = ${logEx - logFwdP}; curWKs=${curWKs.mkString("[", ",", "]")}") // TODO: REMOVE
        j += 1
      }

      i += 1
    }
  }

  override final def toString = f"SoftEmHmmTaggerTrainer($maxIterations, $transitionDistributioner, $emissionDistributioner, alphaT=${alphaT}%2f, alphaE=${alphaE}%2f)"
}

class HardEmHmmTaggerTrainer[Tag](
  maxIterations: Int,
  transitionDistributioner: TransitionDistributioner[Tag],
  emissionDistributioner: EmissionDistributioner[Tag],
  alphaT: Double = 0.0, alphaE: Double = 0.0,
  convergence: Double = 1e-10)
    extends EmHmmTaggerTrainer[Tag](maxIterations, transitionDistributioner, emissionDistributioner, alphaT, alphaE, convergence) {

  /**
   * @return: Transition and Emission expected counts
   */
  final override def doTrain(
    sentsWithTokenTags: Vector[(Array[Int], Array[Array[Int]])],
    numWords: Int, numTags: Int,
    rtd: Array[Array[Int]],
    alphaPriorTr: Array[Array[Double]], alphaPriorEm: Array[Array[Double]],
    logInitialTr: Array[Array[Double]], logInitialEm: Array[Array[Double]]) = {

    val (expectedTrCounts, expectedEmCounts) = iterate(sentsWithTokenTags, numWords, numTags, rtd, alphaPriorTr, alphaPriorEm, logInitialTr, logInitialEm, 1, Double.NegativeInfinity)
    val expectedTrLogCounts = expectedTrCounts.map(_.map(log))
    val expectedEmLogCounts = expectedEmCounts.map(_.map(log))
    (expectedTrLogCounts, expectedEmLogCounts)
  }

  /**
   * @return: Transition and Emission expected counts
   */
  @tailrec private[this] final def iterate(
    sentsWithTokenTags: Vector[(Array[Int], Array[Array[Int]])],
    numWords: Int, numTags: Int,
    rtd: Array[Array[Int]],
    alphaPriorTr: Array[Array[Double]], alphaPriorEm: Array[Array[Double]],
    logTr: Array[Array[Double]], logEm: Array[Array[Double]],
    iteration: Int, prevAvgLogProb: Double //
    ): (Array[Array[Double]], Array[Array[Double]]) = {

    val startTime = System.currentTimeMillis()
    val (expectedTrCounts, expectedEmCounts, avgLogProb) = reestimate(sentsWithTokenTags, numWords, numTags, alphaPriorTr, alphaPriorEm, logTr, logEm)
    println(f"iteration ${(iteration + ":").padRight(4)} ${(System.currentTimeMillis() - startTime) / 1000.0}%.3f sec   avgLogProb=${(avgLogProb + ",").padRight(22)} avgProb=${exp(avgLogProb)}")
    //println("\nTRANSITIONS"); for (t1 <- 0 until numTags) println((0 until numTags).map(t2 => if (t1 != 1 && !(t1 == 0 && t2 <= 1)) f"${exp(newLogTr(t1)(t2))}%.4f" else "").mkString("\t"))
    //println("\nEMISSIONS"); for (t <- 0 until numTags) println((0 until numWords).map(w => if (td(w).contains(t)) f"${exp(newLogEm(t)(w))}%.4f" else "").mkString("\t")); println
    if (iteration >= maxIterations) {
      println(f"MAX ITERATIONS REACHED")
      (expectedTrCounts, expectedEmCounts)
    }
    else if (avgLogProb < prevAvgLogProb) {
      println(f"DIVERGENCE!")
      //assert(false, "DIVERGENCE!")
      (expectedTrCounts, expectedEmCounts)
    }
    else if (avgLogProb - prevAvgLogProb < convergence) {
      println(f"CONVERGENCE (${avgLogProb - prevAvgLogProb} < $convergence)")
      (expectedTrCounts, expectedEmCounts)
    }
    else {
      convertCountsToProbabilities(expectedTrCounts, expectedEmCounts, numWords, numTags, rtd)
      // At this point the "counts" are actually log probabilities!!
      iterate(sentsWithTokenTags, numWords, numTags, rtd, alphaPriorTr, alphaPriorEm, expectedTrCounts, expectedEmCounts, iteration + 1, avgLogProb)
    }
  }

  private[this] final def reestimate(
    sentsWithTokenTags: Vector[(Array[Int], Array[Array[Int]])],
    numWords: Int, numTags: Int,
    alphaPriorTr: Array[Array[Double]], alphaPriorEm: Array[Array[Double]],
    logTr: Array[Array[Double]], logEm: Array[Array[Double]] //
    ) = {

    val expectedTrCounts: Array[Array[Double]] = {
      val data = new Array[Array[Double]](numTags)
      var i = 0; while (i < numTags) { val a = new Array[Double](numTags); System.arraycopy(alphaPriorTr(i), 0, a, 0, numTags); data(i) = a; i += 1 }
      data
    }
    val expectedEmCounts: Array[Array[Double]] = {
      val data = new Array[Array[Double]](numTags)
      var i = 0; while (i < numTags) { val a = new Array[Double](numWords); System.arraycopy(alphaPriorEm(i), 0, a, 0, numWords); data(i) = a; i += 1 }
      data
    }

    var logProbSum = 0.0
    for ((s, stags) <- sentsWithTokenTags.seq) {
      logProbSum += contributeExpectations(expectedTrCounts, expectedEmCounts, s, stags, numWords, numTags, logTr, logEm)
    }

    //println("\nTRANSITION COUNTS"); for (t1 <- 0 until numTags) println((0 until numTags).map(t2 => if (t1 != 1 && !(t1 == 0 && t2 <= 1)) f"${expectedTrLogCounts(t1)(t2)}%.4f" else "").mkString("\t"))
    //println("\nEMISSION COUNTS"); for (t <- 0 until numTags) println((0 until numWords).map(w => if (td(w).contains(t)) f"${expectedEmLogCounts(t)(w)}%.4f" else "").mkString("\t")); println

    (expectedTrCounts, expectedEmCounts, logProbSum / sentsWithTokenTags.size)
  }

  private[this] final def contributeExpectations(
    expectedTrCounts: Array[Array[Double]],
    expectedEmCounts: Array[Array[Double]],
    w: Array[Int], tokenTags: Array[Array[Int]],
    numWords: Int, numTags: Int,
    logTr: Array[Array[Double]], logEm: Array[Array[Double]]): Double = {

    assert(w.head == 0 && w.last == 1)

    val (logP, backpointers) = calculateForwardViterbi(w, tokenTags, numWords, numTags, logTr, logEm)
    contributeBackwardTagging(w, tokenTags, numWords, numTags, backpointers, logTr, logEm, expectedTrCounts, expectedEmCounts)
    logP
  }

  private[this] final def calculateForwardViterbi(
    w: Array[Int], tokenTags: Array[Array[Int]],
    numWords: Int, numTags: Int,
    logTr: Array[Array[Double]], logEm: Array[Array[Double]]) = {
    //println("FORWARD")
    val wLen = w.length
    val logViterbiTable = makeMatrix(w.length, numTags)
    val backpointers = new Array[Array[Int]](wLen); { var i = 0; while (i < wLen) { backpointers(i) = new Array[Int](numTags); i += 1 } }

    //logViterbiTable(0)(0) = 0.0
    var i = 1
    while (i < w.length) {
      val curViterbi = logViterbiTable(i)
      val prevViterbi = logViterbiTable(i - 1)
      //Console.err.println(f"prevViterbi = ${prevViterbi.toVector}") // TODO: REMOVE

      val curW = w(i)
      val curWKs = tokenTags(i)
      val curWKsLen = curWKs.length
      val curBack = backpointers(i)
      val prevKs = tokenTags(i - 1)
      val prevKsLen = prevKs.length
      assert(prevKsLen > 0, f"prevKsLen = $prevKsLen; td(${w(i - 1)}) = ${tokenTags(i - 1).toVector}") // TODO: REMOVE

      var j = 0
      while (j < curWKsLen) {
        val k = curWKs(j)

        var bestK1 = -1
        var maxScore = Double.NegativeInfinity
        var l = 0
        while (l < prevKsLen) {
          val k1 = prevKs(l)
          //assert(!logTr(k1)(k).isNegInfinity, f"logTr($k1)($k) is infinite") // TODO: REMOVE
          //assert(!prevViterbi(k1).isNegInfinity, f"prevViterbi($k1) is infinite") // TODO: REMOVE
          val score = logTr(k1)(k) + prevViterbi(k1)
          if (score > maxScore) {
            bestK1 = k1
            maxScore = score
          }
          l += 1
        }
        //assert(!logEm(k)(curW).isNegInfinity, f"logEm($k)($curW) is infinite") // TODO: REMOVE
        curViterbi(k) = maxScore + logEm(k)(curW)
        curBack(k) = bestK1
        j += 1
      }
      //println(f"$i%3d: " + curViterbi.zipWithIndex.map { case (v, k) => if (td(w(i)).contains(k)) exp(v).toString else "" }.map(_.padRight(30)).mkString(" "))
      i += 1
    }

    //drawViterbiTable(w, td, logViterbiTable, backpointers)

    (logViterbiTable.last(1), backpointers)
  }

  private[this] final def contributeBackwardTagging(
    w: Array[Int], tokenTags: Array[Array[Int]],
    numWords: Int, numTags: Int,
    backpointers: Array[Array[Int]],
    logTr: Array[Array[Double]], logEm: Array[Array[Double]],
    expectedTrCounts: Array[Array[Double]],
    expectedEmCounts: Array[Array[Double]]): Unit = {
    //println("BACKWARD")

    var i = w.length - 2
    var nextTag = 1
    while (i > 0) {
      val currTag = backpointers(i + 1)(nextTag)
      expectedTrCounts(currTag)(nextTag) += 1
      expectedEmCounts(currTag)(w(i)) += 1
      nextTag = currTag
      i -= 1
    }
    expectedTrCounts(0)(nextTag) += 1
  }

  private[this] def drawViterbiTable(
    w: Array[Int], tokenTags: Array[Array[Int]],
    logViterbiTable: Array[Array[Double]],
    backpointers: Array[Array[Int]]): Unit = {
    for ((currV, k) <- logViterbiTable.transpose.zipWithIndex) {
      println(currV.zipWithIndex.map {
        case (v, i) =>
          (if (tokenTags(i).contains(w(i))) f"${backpointers(i)(k)} <- ${exp(v)}%.5f"
          else "").padLeft(5 + 8)
      }.mkString(" "))
    }
    println
  }

  override final def toString = f"HardEmHmmTaggerTrainer($maxIterations, $transitionDistributioner, $emissionDistributioner, alphaT=${alphaT}%2f, alphaE=${alphaE}%2f)"
}
