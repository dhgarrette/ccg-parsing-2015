package dhg.ccg.tag.learn

import dhg.util._
import math.{ log, exp }
import com.typesafe.scalalogging.slf4j.{ StrictLogging => Logging }
import scalaz._
import Scalaz._
import scala.collection.breakOut
import dhg.ccg.prob._
import dhg.ccg.tagdict.TagDictionary

trait TransitionDistributioner[Tag] {
  type Word = String
  def apply(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    initialTagdict: TagDictionary[Tag]): ConditionalLogProbabilityDistribution[Tag, Tag]

  def make(
    transitionCounts: Map[Tag, Map[Tag, LogDouble]],
    tagCounts: Map[Tag, LogDouble], // INCLUDING START/END
    initialTagdict: TagDictionary[Tag]): ConditionalLogProbabilityDistribution[Tag, Tag]
}

abstract class AbstractTransitionDistributioner[Tag] extends TransitionDistributioner[Tag] {
  def apply(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    initialTagdict: TagDictionary[Tag]): ConditionalLogProbabilityDistribution[Tag, Tag] = {
    val tagdict = initialTagdict.withWords(taggedSentences.flatten.map(_._1).toSet).withTags(taggedSentences.flatten.map(_._2).toSet)
    val transitionCounts =
      taggedSentences
        .flatMap(s => (tagdict.startTag +: s.map(_._2) :+ tagdict.endTag).sliding2)
        .filter { case (at, bt) => !tagdict.excludedTags(at) && !tagdict.excludedTags(bt) }
        .groupByKey.mapVals(_.counts.mapVals(LogDouble(_)))
    val tagCounts = taggedSentences.flatMap(s => (tagdict.startTag +: s.map(_._2) :+ tagdict.endTag)).counts.mapVals(LogDouble(_)) -- tagdict.excludedTags
    make(transitionCounts, tagCounts, tagdict)
  }
}

class UnsmoothedTransitionDistributioner[Tag]()
  extends AbstractTransitionDistributioner[Tag] {
  def make(
    transitionCounts: Map[Tag, Map[Tag, LogDouble]],
    tagCounts: Map[Tag, LogDouble], // INCLUDING START/END
    tagdict: TagDictionary[Tag]) = {
    new SimpleConditionalLogProbabilityDistribution((transitionCounts -- tagdict.excludedTags).mapVals(c => new SimpleLogProbabilityDistribution(c -- tagdict.excludedTags)),
      LogProbabilityDistribution.empty[Tag],
      Some(tagdict.excludedTags + tagdict.endTag))
  }
  override def toString = f"UnsmoothedTransitionDistributioner()"
}

/**
 * tdcutoff 0.1,   lambda=0.23
 * tdcutoff 0.01,  lambda=0.21
 * tdcutoff 0.001, lambda=0.20
 * tdcutoff 0.0,   lambda=0.20
 */
class AddLambdaTransitionDistributioner[Tag](lambda: Double = 0.2)
  extends AbstractTransitionDistributioner[Tag] {
  def make(
    transitionCounts: Map[Tag, Map[Tag, LogDouble]],
    tagCounts: Map[Tag, LogDouble], // INCLUDING START/END
    tagdict: TagDictionary[Tag]) = {
    val allTransitionToAbleTags = Some(tagdict.allTags + tagdict.endTag)
    val startTag = Some(Set(tagdict.startTag) ++ tagdict.excludedTags)
    new SimpleConditionalLogProbabilityDistribution(
      (tagdict.allTags.mapToVal(Map[Tag, LogDouble]()).toMap ++ transitionCounts -- tagdict.excludedTags)
        .mapVals(new LaplaceLogProbabilityDistribution(_, allTransitionToAbleTags, startTag, LogDouble(lambda))) +
        (tagdict.startTag -> new LaplaceLogProbabilityDistribution(transitionCounts.getOrElse(tagdict.startTag, Map.empty), allTransitionToAbleTags, Some(Set(tagdict.startTag, tagdict.endTag) ++ tagdict.excludedTags), LogDouble(lambda))) + // Start Tag can't transition to End Tag
        (tagdict.endTag -> LogProbabilityDistribution.empty[Tag]), // End Tag can't transition to anything
      new LaplaceLogProbabilityDistribution(Map.empty[Tag, LogDouble], allTransitionToAbleTags, startTag, LogDouble(lambda)),
      Some(tagdict.excludedTags + tagdict.endTag))
  }
  override def toString = f"AddLambdaTransitionDistributioner($lambda)"
}

/**
 *                    C(t_{i-1}, t_i) + a(t_{i-1}) * p(t_i)
 * p(t_i | t_{i-1}) = -------------------------------------
 *                          C(t_{i-1}) + a(t_{i-1})
 *     where a(t_{i-1}) = |t_i : C(t_{i-1}, t_i) = 1| + \epsilon
 */
class OneCountTransitionDistributioner[Tag](singletonEpsilon: Double, tagPriorLambda: Double, flatPrior: Boolean = false, flatSingletons: Boolean = false)
  extends AbstractTransitionDistributioner[Tag] {
  def make(
    transitionCounts: Map[Tag, Map[Tag, LogDouble]],
    tagCounts: Map[Tag, LogDouble], // INCLUDING START/END
    tagdict: TagDictionary[Tag]) = {
    val allTransitionToAbleTags = tagdict.allTags + tagdict.endTag
    val someAllTransitionToAbleTags = Some(allTransitionToAbleTags)
    val startTag = Some(Set(tagdict.startTag) ++ tagdict.excludedTags)
    val seTags = Some(Set(tagdict.startTag, tagdict.endTag) ++ tagdict.excludedTags)

    val singletonTransitions =
      if (flatSingletons)
        Map[Tag, LogDouble]().withDefaultValue(LogDouble(tagPriorLambda * someAllTransitionToAbleTags.get.size))
      else
        (tagdict.allTags + tagdict.startTag).mapTo { t => LogDouble(transitionCounts.get(t).fold(0)(_.count(_._2 < LogDouble(1.00000001))) + singletonEpsilon) }.toMap

    val transitionToAbleTagPrior =
      if (flatPrior) {
        if (flatSingletons)
          new DefaultedLogProbabilityDistribution(Map(), someAllTransitionToAbleTags, startTag, LogDouble(1.0 / someAllTransitionToAbleTags.get.size))
        else
          new DefaultedLogProbabilityDistribution(Map(), someAllTransitionToAbleTags, startTag, LogDouble(tagPriorLambda))
      }
      else
        new LaplaceLogProbabilityDistribution(tagCounts -- tagdict.excludedTags, someAllTransitionToAbleTags, startTag, LogDouble(tagPriorLambda))
    val smoothedTransitionCounts =
      (tagdict.allTags + tagdict.startTag).mapTo { tPrime =>
        allTransitionToAbleTags.mapTo { t =>
          val c = (for (tPrimeCounts <- transitionCounts.get(tPrime); c <- tPrimeCounts.get(t)) yield c).getOrElse(LogDouble.zero)
          c + singletonTransitions(tPrime) * transitionToAbleTagPrior(t)
        }.toMap
      }.toMap
    new SimpleConditionalLogProbabilityDistribution[Tag, Tag](
      smoothedTransitionCounts.mapt { (tPrime, counts) =>
        tPrime -> new DefaultedLogProbabilityDistribution(counts, someAllTransitionToAbleTags, startTag, singletonTransitions(tPrime) * transitionToAbleTagPrior.defaultProb)
      } +
        (tagdict.startTag -> {
          val startDestinationTagPrior =
            if (flatPrior) {
              if (flatSingletons)
                new DefaultedLogProbabilityDistribution(Map.empty[Tag, LogDouble], someAllTransitionToAbleTags, startTag, LogDouble(1.0 / someAllTransitionToAbleTags.get.size))
              else
                new DefaultedLogProbabilityDistribution(Map.empty[Tag, LogDouble], someAllTransitionToAbleTags, startTag, LogDouble(tagPriorLambda))
            }
            else
              new LaplaceLogProbabilityDistribution(tagCounts -- tagdict.excludedTags, someAllTransitionToAbleTags, seTags, LogDouble(tagPriorLambda))
          val startSmoothedCounts = tagdict.allTags.mapTo { t => transitionCounts(tagdict.startTag).getOrElse(t, LogDouble.zero) + singletonTransitions(tagdict.startTag) * startDestinationTagPrior(t) }.toMap
          val default = singletonTransitions(tagdict.startTag) * startDestinationTagPrior.defaultProb
          new DefaultedLogProbabilityDistribution(startSmoothedCounts, someAllTransitionToAbleTags, seTags, default) // Start Tag can't transition to End Tag
        }) +
        (tagdict.endTag -> LogProbabilityDistribution.empty[Tag]), // End Tag can't transition to anything
      transitionToAbleTagPrior,
      Some(tagdict.excludedTags + tagdict.endTag))
  }
  override def toString = f"OneCountTransitionDistributioner($singletonEpsilon, $tagPriorLambda)"
}

//
//
//

trait EmissionDistributioner[Tag] {
  type Word = String
  
  def apply(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    initialTagdict: TagDictionary[Tag]): ConditionalLogProbabilityDistribution[Tag, Word]

  def make(
    emissionCounts: Map[Tag, Map[Word, LogDouble]],
    initialTagdict: TagDictionary[Tag]): ConditionalLogProbabilityDistribution[Tag, Word]
}

abstract class AbstractEmissionDistributioner[Tag] extends EmissionDistributioner[Tag] {
  def apply(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    initialTagdict: TagDictionary[Tag]): ConditionalLogProbabilityDistribution[Tag, Word] = {
    val tagdict = initialTagdict.withWords(taggedSentences.flatten.map(_._1).to[Set]).withTags(taggedSentences.flatten.map(_._2).to[Set])
    val emissionCounts = taggedSentences.flatten.filter { case (w, t) => tagdict(w)(t) && !tagdict.excludedTags(t) }.map(_.swap).groupByKey.mapVals(_.counts.mapVals(LogDouble(_)))
    make(emissionCounts, tagdict)
  }
}

class UnsmoothedEmissionDistributioner[Tag]() extends AbstractEmissionDistributioner[Tag] {
  def make(
    emissionCounts: Map[Tag, Map[Word, LogDouble]],
    initialTagdict: TagDictionary[Tag]): ConditionalLogProbabilityDistribution[Tag, Word] = {

    val tagdict = initialTagdict.withWords(emissionCounts.values.flatMap(_.keys).to[Set]).withTags(emissionCounts.keySet)
    val allWordsSet = Some(tagdict.allWords)
    new SimpleConditionalLogProbabilityDistribution(
      (emissionCounts -- tagdict.excludedTags).mapt((t, tcounts) => t -> new LaplaceLogProbabilityDistribution(tcounts, allWordsSet, /*Some(tagdict.entries.keySet -- tagdict.knownWordsForTag(t) +*/ Some(Set(tagdict.startWord, tagdict.endWord)), LogDouble.zero)) +
        (tagdict.startTag -> new SimpleLogProbabilityDistribution(Map(tagdict.startWord -> LogDouble.one))) +
        (tagdict.endTag -> new SimpleLogProbabilityDistribution(Map(tagdict.endWord -> LogDouble.one))),
      LogProbabilityDistribution.empty[Word],
      Some(tagdict.excludedTags))
  }
  override def toString = f"UnsmoothedEmissionDistributioner()"
}

/*
 * tdcutoff 0.1,   lambda=0.10
 * tdcutoff 0.01,  lambda=0.10
 * tdcutoff 0.001, lambda=0.10
 * tdcutoff 0.0,   lambda=0.10
 */
class AddLambdaEmissionDistributioner[Tag](lambda: Double = 0.1) extends AbstractEmissionDistributioner[Tag] {
  def make(
    emissionCounts: Map[Tag, Map[Word, LogDouble]],
    initialTagdict: TagDictionary[Tag]): ConditionalLogProbabilityDistribution[Tag, Word] = {

    val tagdict = initialTagdict.withWords(emissionCounts.values.flatMap(_.keys).to[Set]).withTags(emissionCounts.keySet)
    val allWordsSet = Some(tagdict.allWords)
    new SimpleConditionalLogProbabilityDistribution(
      (tagdict.allTags.mapToVal(Map[Word, LogDouble]()).toMap ++ emissionCounts -- tagdict.excludedTags)
        .mapt((t, tcounts) => t -> new LaplaceLogProbabilityDistribution(tcounts, allWordsSet, /*Some(tagdict.entries.keySet -- tagdict.knownWordsForTag.getOrElse(t, Set.empty) +*/ Some(Set(tagdict.startWord, tagdict.endWord)), LogDouble(lambda))) +
        (tagdict.startTag -> new SimpleLogProbabilityDistribution(Map(tagdict.startWord -> LogDouble.one))) +
        (tagdict.endTag -> new SimpleLogProbabilityDistribution(Map(tagdict.endWord -> LogDouble.one))),
      new LaplaceLogProbabilityDistribution(Map(), allWordsSet, /*Some(tagdict.entries.keySet +*/ Some(Set(tagdict.startWord, tagdict.endWord)), LogDouble(1.0)),
      Some(tagdict.excludedTags))
  }
  override def toString = f"AddLambdaEmissionDistributioner($lambda)"
}

/**
 *            C(t,w) + b(t) * p(w)
 * p(w | t) = --------------------
 *                C(t) + b(t)
 *     where b(t) = |w : C(t,w) = 1| + \epsilon
 */
class OneCountEmissionDistributioner[Tag](singletonEpsilon: Double, wordPriorLambda: Double) extends AbstractEmissionDistributioner[Tag] {
  def make(
    emissionCounts: Map[Tag, Map[Word, LogDouble]],
    initialTagdict: TagDictionary[Tag]): ConditionalLogProbabilityDistribution[Tag, Word] = {

    val tagdict = initialTagdict.withWords(emissionCounts.values.flatMap(_.keys).to[Set]).withTags(emissionCounts.keySet)
    val allWordsSet = Some(tagdict.allWords)

    val singletonEmissions = tagdict.allTags.mapTo { t => LogDouble(emissionCounts.get(t).fold(0)(_.count(_._2 < LogDouble(1.00000001))) + singletonEpsilon) }.toMap

    val wordCounts = emissionCounts.values.reduce(_ |+| _)
    val wordPrior = new LaplaceLogProbabilityDistribution(wordCounts.mapVals(LogDouble(_)), Some(tagdict.allWords), Some(Set(tagdict.startWord, tagdict.endWord)), LogDouble(wordPriorLambda))

    val smoothedEmissionCounts =
      tagdict.allTags.mapTo { t =>
        tagdict.allWords.mapTo { w =>
          val c = (for (tCounts <- emissionCounts.get(t); c <- tCounts.get(w)) yield c).getOrElse(LogDouble.zero)
          c + singletonEmissions(t) * wordPrior(w)
        }.toMap
      }.toMap
    new SimpleConditionalLogProbabilityDistribution(
      (tagdict.allTags.mapToVal(Map[Word, LogDouble]()).toMap ++ smoothedEmissionCounts -- tagdict.excludedTags)
        .mapt { (t, tcounts) =>
          val default = singletonEmissions(t) * wordPrior.defaultProb
          t -> new DefaultedLogProbabilityDistribution(tcounts.mapVals(LogDouble(_)), allWordsSet, /*Some(tagdict.entries.keySet -- tagdict.knownWordsForTag(t) +*/ Some(Set(tagdict.startWord, tagdict.endWord)), default)
        } +
        (tagdict.startTag -> new SimpleLogProbabilityDistribution(Map(tagdict.startWord -> LogDouble.one))) +
        (tagdict.endTag -> new SimpleLogProbabilityDistribution(Map(tagdict.endWord -> LogDouble.one))),
      wordPrior,
      Some(tagdict.excludedTags))
  }
  override def toString = f"OneCountEmissionDistributioner($singletonEpsilon, $wordPriorLambda)"
}
