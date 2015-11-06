package dhg.ccg.tag.learn

import dhg.util._
import scalaz._
import scalaz.Scalaz._
import dhg.ccg.prob._
import dhg.ccg.tagdict.TagDictionary
import com.typesafe.scalalogging.slf4j.{ StrictLogging => Logging }

/**
 * Turn raw sentences and a tagdict into P(Tag->Tag)
 */
trait TransitionInitializer[Tag] {
  type Word = String
  final def fromRaw(sentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Tag]): ConditionalLogProbabilityDistribution[Tag, Tag] = {
    val tagdict = initialTagdict.withWords(sentences.flatten.toSet)
    fromKnownSupertagSets(sentences.map(_.mapTo(tagdict.entries.getOrElse(_, Set.empty))), tagdict)
  }

  /**
   * Each token associated with its set of possible supertags, if such a set is KNOWN; if the set is unknown, it will be EMPTY.
   */
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]): ConditionalLogProbabilityDistribution[Tag, Tag]
}

class TrUniform[Tag]() extends TransitionInitializer[Tag] {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]) = {
    val allTags = Some(initialTagdict.allTags)
    val startTag = Some(initialTagdict.excludedTags + initialTagdict.startTag)
    new SimpleConditionalLogProbabilityDistribution(
      Map(
        initialTagdict.startTag -> new LaplaceLogProbabilityDistribution(Map.empty[Tag, LogDouble], allTags, Some(initialTagdict.excludedTags + initialTagdict.startTag + initialTagdict.endTag), LogDouble(1.0)),
        initialTagdict.endTag -> LogProbabilityDistribution.empty[Tag]),
      new LaplaceLogProbabilityDistribution(Map.empty[Tag, LogDouble], Some(initialTagdict.allTags + initialTagdict.endTag), startTag, LogDouble(1.0)),
      Some(initialTagdict.excludedTags + initialTagdict.endTag))
  }
  override def toString = f"TrUniform()"
}

class TrCheat[Tag](
    taggedSentences: Vector[Vector[(String, Tag)]],
    distributioner: TransitionDistributioner[Tag]) extends TransitionInitializer[Tag] {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]) = {
    val tagdict =
      initialTagdict
        .withWords(sentences.flatten.map(_._1).toSet ++ taggedSentences.flatten.map(_._1))
        .withTags(sentences.flatten.flatMap(_._2).toSet ++ taggedSentences.flatten.map(_._2).toSet)
    // ignore `sentences` since, presumably, it's either included (in tagged form) in `taggedSentences` or of no use. 
    distributioner(taggedSentences, tagdict)
  }
  override def toString = f"TrCheat($distributioner)"
}

/**
 * tdcutoff = 0.1,   lambda = 0.13
 * tdcutoff = 0.01,  lambda = 0.07
 * tdcutoff = 0.001, lambda = 0.06
 * tdcutoff = 0.0,   lambda = 0.05
 */
class TrTagDictEntriesPossibilities[Tag](distributioner: TransitionDistributioner[Tag]) extends TransitionInitializer[Tag] /*with Logging*/ {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.map(_._1).toSet).withTags(sentences.flatten.flatMap(_._2).toSet)
    //logger.info("TrTagDictEntriesPossibilities => sentenceSets")
    val sentenceSets = sentences.map(_.map(_._2))
    //logger.info("TrTagDictEntriesPossibilities => trCounts")
    val potentialTransitions =
      for {
        s <- sentenceSets
        (as, bs) <- (Set(tagdict.startTag) +: s :+ Set(tagdict.endTag)).sliding2
        c = 1.0 / (as.size * bs.size)
        a <- as; b <- bs
      } yield {
        (a, (b, c))
      }
    val trCounts = potentialTransitions.groupByKey.mapVals(_.groupByKey.mapVals(_.sum))
    //logger.info("TrTagDictEntriesPossibilities => tagCounts")
    val tagCounts = sentenceSets.flatten.flatMap(ts => ts.mapToVal(1.0 / ts.size)).groupByKey.mapVals(_.sum)
    //logger.info("TrTagDictEntriesPossibilities => make distribution")
    distributioner.make(trCounts.mapVals(_.mapVals(LogDouble(_))), tagCounts.mapVals(LogDouble(_)), tagdict)
  }
  override def toString = f"TrTagDictEntriesPossibilities($distributioner)"
}

/**
 *
 */
class InterpolatingTransitionInitializer[Tag](delegates: Vector[(TransitionInitializer[Tag], Double)]) extends TransitionInitializer[Tag] {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.map(_._1).toSet).withTags(sentences.flatten.flatMap(_._2).toSet)
    new InterpolatingConditionalLogProbabilityDistribution(
      delegates.map { case (i, p) => i.fromKnownSupertagSets(sentences, tagdict) -> LogDouble(p) })
  }
  override def toString = f"InterpolatingInitializer(${delegates.map { case (i, w) => f"$i -> $w" }.mkString(", ")})"
}

//
//
//

trait EmissionInitializer[Tag] {
  type Word = String
  final def fromRaw(sentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Tag]): ConditionalLogProbabilityDistribution[Tag, Word] = {
    val tagdict = initialTagdict.withWords(sentences.flatten.toSet)
    fromKnownSupertagSets(sentences.map(_.mapTo(tagdict.entries.getOrElse(_, Set.empty))), tagdict)
  }

  /**
   * Each token associated with its set of possible supertags, if such a set is KNOWN; if the set is unknown, it will be EMPTY.
   */
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]): ConditionalLogProbabilityDistribution[Tag, Word]
}

class EmUniform[Tag]() extends EmissionInitializer[Tag] {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]): ConditionalLogProbabilityDistribution[Tag, Word] = {
    val tagdict = initialTagdict.withWords(sentences.flatten.map(_._1).toSet).withTags(sentences.flatten.flatMap(_._2).toSet)
    val knownWordsForTag = tagdict.entries.ungroup.map(_.swap).groupByKey.mapVals(_.toSet).withDefaultValue(Set.empty)
    val knownWords = knownWordsForTag.flatMap(_._2).toSet
    val allWordsSet = Some(tagdict.allWords)
    new SimpleConditionalLogProbabilityDistribution(
      tagdict.allTags.mapTo(t => new LaplaceLogProbabilityDistribution(Map(), allWordsSet, /*Some(knownWords -- knownWordsForTag(t) +*/ Some(Set(tagdict.startWord, tagdict.endWord)), LogDouble(1.0))).toMap +
        (tagdict.startTag -> new SimpleLogProbabilityDistribution(Map(tagdict.startWord -> LogDouble.one))) +
        (tagdict.endTag -> new SimpleLogProbabilityDistribution(Map(tagdict.endWord -> LogDouble.one))),
      new LaplaceLogProbabilityDistribution(Map(), allWordsSet, /*Some(knownWords +*/ Some(Set(tagdict.startWord, tagdict.endWord)), LogDouble(1.0)),
      Some(tagdict.excludedTags))
  }
  override def toString = f"EmUniform()"
}

class EmCheat[Tag](
  taggedSentences: Vector[Vector[(String, Tag)]],
  val distributioner: EmissionDistributioner[Tag])
    extends EmissionInitializer[Tag] {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]) = {
    val tagdict =
      initialTagdict
        .withWords(sentences.flatten.map(_._1).toSet ++ taggedSentences.flatten.map(_._1))
        .withTags(sentences.flatten.flatMap(_._2).toSet ++ taggedSentences.flatten.map(_._2).toSet)
    // ignore `sentences` since, presumably, it's included (in tagged form) in `taggedSentences` 
    distributioner(taggedSentences, tagdict)
  }
  override def toString = f"EmCheat($distributioner)"
}

class TagDictionaryEstimateTagPriorInitializer[Tag](tdCountLambda: Double = 0.26) extends TagPriorInitializer[Tag] {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.map(_._1).toSet).withTags(sentences.flatten.flatMap(_._2).toSet)

    // TODO: BEGIN DUPLICATED
    val sentenceCk = sentences.flatten.flatMap {
      case (w, ts) =>
        val partialCounts = 1.0 / ts.size.toDouble
        ts.mapTo(t => w -> partialCounts)
    }
    val smoothingCk = tagdict.entries.toVector.flatMap {
      case (w, ts) =>
        val partialCounts = tdCountLambda / ts.size.toDouble
        ts.mapTo(t => w -> partialCounts)
    }
    val C_k = (sentenceCk ++ smoothingCk).groupByKey.mapVals(_.groupByKey.mapVals(_.sum))
    // TODO: END DUPLICATED

    // p(t) = sum_w' C_k(t,w') / Z
    new LaplaceLogProbabilityDistribution(tagdict.allTags.mapTo(t => LogDouble(C_k(t).values.sum)).toMap, None, Some(tagdict.excludedTags), LogDouble(0.0))
  }
}

class EmTagDictionaryEstimateFromTagPrior[Tag](tagPrior: LogProbabilityDistribution[Tag], lambda: Double = 0.04, tdCountLambda: Double = 0.26, combineKU: Boolean = false) extends EmissionInitializer[Tag] {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]): SimpleConditionalLogProbabilityDistribution[Tag, Word] = {
    val tagdict = initialTagdict.withWords(sentences.flatten.map(_._1).toSet).withTags(sentences.flatten.flatMap(_._2).toSet)

    /* We normally smooth emissions from C(t,w)
     *   p(w|t) = C(t,w) / C(t)
     *   
     * C(t,w) comes in two varieties:
     * 
     *   1. if w is in the TD: C_k(t,w)
     *      - if t in TD(w): C_k(t,w) = C(w) / |TD(w)|
     *        else:          C_k(t,w) = 0
     *   
     *   2. if w is not in the TD: C_u(t,w)      (td-unknown)
     *      - C_u(t,w) = C(w) * p(t|unk)         (divide the counts of w among all tags according 
     *                                            to likelihood of t given that w is unknown)
     *      - p(t|unk) = p(unk|t) * p(t) / Z     (Bayes rule)
     *      - p(t) = sum_w' C_k(t,w') / Z
     *      - p(unk|t) = C(t,unk) / Z XX
     *                 = |TD(t)| / (sum_t' |TD(t')| for known words only)
     *                 
     * p(w|t) = (C_k(t,w) + C_u(t,w)) / Z
     *   
     */

    val rawSentences = sentences.map(_.map(_._1))
    val sentenceWords = rawSentences.flatten.toSet

    val knownWordsForTag = tagdict.knownWordsForTag
    val knownWords = knownWordsForTag.flatMap(_._2).toSet

    val C = rawSentences.flatten.counts // C(w)

    // C_k(t)(w)
    // TODO: BEGIN DUPLICATED
    val sentenceCk = sentences.flatten.flatMap {
      case (w, ts) =>
        val partialCounts = 1.0 / ts.size.toDouble
        ts.mapTo(t => w -> partialCounts)
    }
    val smoothingCk = tagdict.entries.toVector.flatMap {
      case (w, ts) =>
        val partialCounts = tdCountLambda / ts.size.toDouble
        ts.mapTo(t => w -> partialCounts)
    }
    val C_k = (sentenceCk ++ smoothingCk).groupByKey.mapVals(_.groupByKey.mapVals(cs => LogDouble(cs.sum)))
    // TODO: END DUPLICATED

    val td_unknown_words = sentenceWords -- knownWords
    // p(t)
    val p = tagPrior
    // p(unk|t) = |TD(t)| / (sum_t' |TD(t')| for known words only)
    val `p(unk|t)` = tagdict.allTags.mapTo(t => LogDouble(knownWordsForTag(t).size + 0.001)).toMap.normalizeValues
    // p(t|unk) = p(unk|t) * p(t) / Z
    val `p(t|unk)` = tagdict.allTags.mapTo(t => `p(unk|t)`(t) * p(t)).toMap.normalizeValues
    // C_u(t)(w) = C(w) * p(t|unk)
    val C_u =
      tagdict.allTags.mapTo { t =>
        td_unknown_words.mapTo { w =>
          LogDouble(C(w)) * `p(t|unk)`(t)
        }.toMap
      }.toMap

    val C_ku = tagdict.allTags.mapTo { t =>
      val cut = C_u(t)
      val ckt = C_k(t)
      if (combineKU)
        cut |+| ckt
      else
        cut ++ ckt
    }.toMap

    //TODO: CHECK THAT EVERY TD-VALID ENTRY HAS A NON-ZERO PROBABILITY
    for (w <- sentenceWords; t <- tagdict(w)) {
      assert(C_ku.contains(t), f"C_ku doesn't contain t=$t")
      assert(C_ku(t).contains(w), f"C_ku(t=$t) doesn't contain w=$w")
      assert(C_ku(t)(w) > LogDouble.zero, f"C_ku(t=$t)(w=$w) = ${C_ku(t)(w)}")
    }

    val allWordsSet = Some(tagdict.allWords)
    new SimpleConditionalLogProbabilityDistribution(
      C_ku.mapt((t, counts) => t -> new LaplaceLogProbabilityDistribution(counts.mapVals(LogDouble(_)), allWordsSet, /*Some(knownWords -- knownWordsForTag(t) +*/ Some(Set(tagdict.startWord, tagdict.endWord)), LogDouble(lambda))).toMap +
        (tagdict.startTag -> new SimpleLogProbabilityDistribution(Map(tagdict.startWord -> LogDouble.one))) +
        (tagdict.endTag -> new SimpleLogProbabilityDistribution(Map(tagdict.endWord -> LogDouble.one))),
      new LaplaceLogProbabilityDistribution(C_ku.values.reduce(_ |+| _).mapVals(LogDouble(_)), allWordsSet, /*Some(knownWords*/ Some(Set(tagdict.startWord, tagdict.endWord)), LogDouble(lambda)),
      Some(tagdict.excludedTags))
  }
  override def toString = f"EmTagDictionaryEstimate($lambda, $tdCountLambda, $combineKU)"
}

class EmTagDictionaryEstimate[Tag](tagPriorInit: TagPriorInitializer[Tag], lambda: Double = 0.04, tdCountLambda: Double = 0.26, combineKU: Boolean = false) extends EmissionInitializer[Tag] {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]): SimpleConditionalLogProbabilityDistribution[Tag, Word] = {
    val tagdict = initialTagdict.withWords(sentences.flatten.map(_._1).toSet).withTags(sentences.flatten.flatMap(_._2).toSet)
    val tagPrior = tagPriorInit.fromKnownSupertagSets(sentences, tagdict)
    val emPriorInit = new EmTagDictionaryEstimateFromTagPrior(tagPrior, lambda, tdCountLambda, combineKU)
    emPriorInit.fromKnownSupertagSets(sentences, tagdict)
  }

  override def toString = f"EmTagDictionaryEstimate($lambda, $tdCountLambda, $combineKU)"
}
