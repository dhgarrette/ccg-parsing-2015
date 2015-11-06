package dhg.ccg.parse.pcfg.typesup

import dhg.util._
import scalaz._
import scalaz.Scalaz._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tag.learn.EmissionInitializer
import dhg.ccg.tag.learn.TagPriorInitializer
import dhg.ccg.parse.pcfg._
import dhg.ccg.util._

trait TermDistInitializerI {
  type Word = Int
  type Tag = Int

  def apply(gcs: Vector[CfgGuideChartI],
    knownTerms: Array[Array[Int]], //                 t -> ws
    numWords: Int) //
    : Array[Vec[LogDouble]] //                  t -> w -> p
}

class UniformTermDistInitializerI() extends TermDistInitializerI {
  def apply(gcs: Vector[CfgGuideChartI],
    knownTerms: Array[Array[Int]], //                 t -> ws
    numWords: Int) //
    : Array[Vec[LogDouble]] //                  t -> w -> p
    = {

    val p = LogDouble(1.0 / numWords)
    knownTerms.map { tKnownWords =>
      SparseVec(tKnownWords.map(_ -> p), numWords)
    }
  }

  override def toString = f"UniformTermDistInitializerI()"
}

class TagdictSupervisedTermDistInitializerI(
  tagPrior: Array[LogDouble],
  lambda: Double = 0.04, tdCountLambda: Double = 0.26, combineKU: Boolean = false)
    extends TermDistInitializerI {
  def apply(gcs: Vector[CfgGuideChartI],
    knownTerms: Array[Array[Word]], //                t -> ws
    numWords: Int) //
    : Array[Vec[LogDouble]] //                  t -> w -> p
    = {

    val sentences: Vector[Vector[(Word, Set[Tag])]] = gcs.map(_.wordSupertagsets.map { case (w, tags) => w -> tags.toSet }.toVector)
    val tdEntries: Map[Word, Set[Tag]] = sentences.flatten.groupByKey.mapVals(_.flatten.toSet)
    val allWords: Set[Word] = tdEntries.keySet
    val allTags: Set[Tag] = tdEntries.flatMap(_._2).toSet

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

    val rawSentences: Vector[Vector[Word]] = sentences.map(_.map(_._1))
    val sentenceWords: Set[Word] = rawSentences.flatten.toSet

    val knownWordsForTag: Map[Tag, Set[Word]] = sentences.flatMap(_.flatMap { case (w, ts) => ts.map(_ -> w) }).groupByKey.mapVals(_.toSet)
    val knownWords: Set[Word] = knownWordsForTag.flatMap(_._2).toSet

    val C: Map[Word, Int] = rawSentences.flatten.counts // C(w)

    // C_k(t)(w)
    // TODO: BEGIN DUPLICATED
    val sentenceCk: Vector[(Tag, (Word, Double))] = sentences.flatten.flatMap {
      case (w, ts) =>
        val partialCounts = 1.0 / ts.size.toDouble
        ts.map(_ -> (w -> partialCounts))
    }
    val smoothingCk: Vector[(Tag, (Word, Double))] = tdEntries.toVector.flatMap {
      case (w, ts) =>
        val partialCounts = tdCountLambda / ts.size.toDouble
        ts.mapTo(t => w -> partialCounts)
    }
    val C_k: Map[Tag, Map[Word, LogDouble]] =
      (sentenceCk ++ smoothingCk).groupByKey.mapVals(_.groupByKey.mapVals(cs => LogDouble(cs.sum)))
    // TODO: END DUPLICATED

    val td_unknown_words = sentenceWords -- knownWords
    // p(t)
    val p = tagPrior
    // p(unk|t) = |TD(t)| / (sum_t' |TD(t')| for known words only)
    val `p(unk|t)` = allTags.mapTo(t => LogDouble(knownWordsForTag(t).size + 0.001)).toMap.normalizeValues
    // p(t|unk) = p(unk|t) * p(t) / Z
    val `p(t|unk)` = allTags.mapTo(t => `p(unk|t)`(t) * p(t)).toMap.normalizeValues
    // C_u(t)(w) = C(w) * p(t|unk)
    val C_u =
      allTags.mapTo { t =>
        td_unknown_words.mapTo { w =>
          LogDouble(C(w)) * `p(t|unk)`(t)
        }.toMap
      }.toMap

    val C_ku = allTags.mapTo { t =>
      val cut = C_u(t)
      val ckt = C_k(t)
      if (combineKU)
        cut |+| ckt
      else
        cut ++ ckt
    }.toMap

    val priorTermDist: Array[Vec[LogDouble]] =
      knownTerms.zipWithIndex map {
        case (ws, t) =>
          if (ws != null) {
            val tC_ku = C_ku.getOrElse(t, Map.empty)
            SparseVec(ws.map { w =>
              w -> (tC_ku.getOrElse(w, LogDouble.zero) + LogDouble(lambda))
            }, numWords)
          }
          else null
      }

    //TODO: CHECK THAT EVERY TD-VALID ENTRY HAS A NON-ZERO PROBABILITY
    for ((ws, t) <- knownTerms.zipWithIndex if ws != null; w <- ws) {
      //      assert(C_ku.contains(t), f"C_ku doesn't contain t=$t")
      //      assert(C_ku(t).contains(w), f"C_ku(t=$t) doesn't contain w=$w")
      //      assert(C_ku(t)(w) > LogDouble.zero, f"C_ku(t=$t)(w=$w) = ${C_ku(t)(w)}")
      assert(priorTermDist(t)(w) > LogDouble.zero)
    }

    priorTermDist
  }

  override def toString = f"TagdictSupervisedTermDistInitializerI()"
}
