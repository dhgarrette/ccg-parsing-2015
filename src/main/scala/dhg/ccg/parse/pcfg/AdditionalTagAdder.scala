package dhg.ccg.parse.pcfg

import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import dhg.util._
import scalaz._
import scalaz.Scalaz._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.tagdict.TagDictionary

/**
 * Take a sentence as a sequence of words associated with sets of tags.
 * Each AdditionalTagAdder will output a transformed tagset for each word.
 */
trait AdditionalTagAdder[Tag] {
  type Word = String
  def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: TagDictionary[Tag]): Vector[Set[Tag]]
}

class SequentialAdditionalTagAdder[Tag](delegates: Vector[AdditionalTagAdder[Tag]]) extends AdditionalTagAdder[Tag] {
  override def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: TagDictionary[Tag]): Vector[Set[Tag]] = delegates.foldLeft(tags) { (z, ata) => ata(sentence, z, tagdict) }
  override def toString = f"SequentialAdditionalTagAdder(${delegates.mkString(", ")})"
}

class ParallelAdditionalTagAdder[Tag](delegates: Vector[AdditionalTagAdder[Tag]]) extends AdditionalTagAdder[Tag] {
  override def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: TagDictionary[Tag]): Vector[Set[Tag]] = delegates.map(ata => ata(sentence, tags, tagdict)).transpose.map(_.flatten.toSet)
  override def toString = f"ParallelAdditionalTagAdder(${delegates.mkString(", ")})"
}

class NoOpAdditionalTagAdder[Tag] extends AdditionalTagAdder[Tag] {
  override def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: TagDictionary[Tag]): Vector[Set[Tag]] = tags
  override def toString = f"NoOpAdditionalTagAdder()"
}

/**
 * For each word in the sentence, return its TD entry set if the word appears in the TD (empty set otherwise)
 */
class PresentTagdictAdditionalTagAdder[Tag]() extends AdditionalTagAdder[Tag] {
  override def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: TagDictionary[Tag]): Vector[Set[Tag]] = (sentence zipSafe tags).mapt((w, _) => tagdict.entries.getOrElse(w, Set.empty))
  override def toString = f"PresentTagdictAdditionalTagAdder()"
}

/**
 * For each word in the sentence, if there are no tags associated with the word, then return a set containing all tags known to the TD (empty set otherwise)
 */
class TagdictEntryForMissingAdditionalTagAdder[Tag]() extends AdditionalTagAdder[Tag] {
  override def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: TagDictionary[Tag]): Vector[Set[Tag]] = (sentence zipSafe tags).mapt((w, ts) => if (ts.nonEmpty) Set.empty[Tag] else tagdict.entries.getOrElse(w, Set.empty))
  override def toString = f"FullTagdictTagsetForMissingAdditionalTagAdder()"
}

/**
 * For each word in the sentence, if there are no tags associated with the word, then return a set containing all tags known to the TD (empty set otherwise)
 */
class FullTagdictTagsetForMissingAdditionalTagAdder[Tag]() extends AdditionalTagAdder[Tag] {
  override def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: TagDictionary[Tag]): Vector[Set[Tag]] = tags.map(ts => if (ts.nonEmpty) Set.empty[Tag] else tagdict.allTags)
  override def toString = f"FullTagdictTagsetForMissingAdditionalTagAdder()"
}

class DefaultTagsetForMissingAdditionalTagAdder[Tag](defaultTagset: Set[Tag]) extends AdditionalTagAdder[Tag] {
  override def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: TagDictionary[Tag]): Vector[Set[Tag]] = tags.map(ts => if (ts.nonEmpty) Set.empty[Tag] else defaultTagset)
  override def toString = f"DefaultTagsetForMissingAdditionalTagAdder($defaultTagset)"
}

/*
 * CCG-specific adders
 */

class FwdBkdModAdditionalTagAdder() extends AdditionalTagAdder[Cat] {
  override def apply(sentence: Vector[Word], tags: Vector[Set[Cat]], tagdict: TagDictionary[Cat]): Vector[Set[Cat]] = {
    val n = sentence.length
    val endedTags = None +: tags.map(Option(_)) :+ None
    for (((word, Seq(prevTags, Some(tags), nextTags)), i) <- (sentence zipSafe endedTags.sliding(3)).zipWithIndex) yield {
      val prevs: Iterable[Cat] = prevTags.toSet.flatMap(prev => prev.collect { case prevSupertag: NonPuncCat => prevSupertag \ prevSupertag })
      val nexts: Iterable[Cat] = nextTags.toSet.flatMap(next => next.collect { case nextSupertag: NonPuncCat => nextSupertag / nextSupertag })
      (prevs ++ nexts).toSet
    }
  }
}

//class FwdBkdConsumerAdditionalTagAdder extends AdditionalTagAdder[Cat] {
//  override def apply(sentence: Vector[Word], tags: Vector[Set[Cat]]): Vector[Set[Cat]] = {
//    val n = sentences.length
//    val endedTags = None +: tags.map(Option(_)) :+ None
//    for (((word, Seq(prevTags, Some(tags), nextTags)), i) <- (sentence zipSafe endedTags.sliding(3)).zipWithIndex) yield {
//      val prevs: Iterable[Cat] = for (prev<- prevTags.toSet; prevSupertag <- prev) yield (prevSupertag \ prevSupertag)
//      val nexts: Iterable[Cat] = for (next<- nextTags.toSet; nextSupertag <- next) yield (nextSupertag / nextSupertag)
//      (tags ++ prevs ++ nexts).toSet
//    }
//
//    for (((word, tags), i) <- sentenceWithTags.zipWithIndex) yield {
//      val prevs: Iterable[Cat] = for (prevSupertag <- sentenceWithTags.map(_._2).apply(i); curSupertag <- tags) yield (curSupertag \ prevSupertag) else Set.empty
//      val nexts: Iterable[Cat] = for (nextSupertag <- sentenceWithTags.map(_._2).apply(i + 1); curSupertag <- tags) yield (curSupertag / nextSupertag) else Set.empty
//      (prevs ++ nexts).toSet
//    }
//  }
//}
//
//class AllFwdBkdAdditionalTagAdder extends AdditionalTagAdder[Cat] {
//  override def apply(sentence: Vector[Word], tags: Vector[Set[Cat]]): Vector[Set[Cat]] = {
//    val n = sentenceWithTags.length
//    for (((word, tags), i) <- sentenceWithTags.zipWithIndex) yield {
//      val prevs: Seq[Cat] = for (pTags <- sentenceWithTags.map(_._2).take(i); prevSupertag <- pTags) yield (prevSupertag \ prevSupertag)
//      val nexts: Seq[Cat] = for (nTags <- sentenceWithTags.map(_._2).drop(i + 1); nextSupertag <- nTags) yield (nextSupertag / nextSupertag)
//      // TODO: Maybe this hsould be `curSupertag / nextSupertag` ??
//      (prevs ++ nexts).toSet
//    }
//  }
//}

//

class StandardTagDictAdditionalTagAdder[Tag]() extends AdditionalTagAdder[Tag] {
  private[this] val delegate: AdditionalTagAdder[Tag] =
    new SequentialAdditionalTagAdder[Tag](Vector(
      new ParallelAdditionalTagAdder[Tag](Vector(
        new NoOpAdditionalTagAdder(),
        new TagdictEntryForMissingAdditionalTagAdder())),
      new ParallelAdditionalTagAdder[Tag](Vector(
        new NoOpAdditionalTagAdder(),
        new FullTagdictTagsetForMissingAdditionalTagAdder()))))
  override def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: TagDictionary[Tag]): Vector[Set[Tag]] = delegate(sentence, tags, tagdict)
  override def toString = f"StandardTagDictAdditionalTagAdder()"
}

class StandardTagDictAndFwdBkdAdditionalTagAdder() extends AdditionalTagAdder[Cat] {
  private[this] val delegate: AdditionalTagAdder[Cat] =
    new SequentialAdditionalTagAdder[Cat](Vector(
      new StandardTagDictAdditionalTagAdder(),
      new ParallelAdditionalTagAdder[Cat](Vector(
        new NoOpAdditionalTagAdder(),
        new FwdBkdModAdditionalTagAdder()))))
  override def apply(sentence: Vector[Word], tags: Vector[Set[Cat]], tagdict: TagDictionary[Cat]): Vector[Set[Cat]] = delegate(sentence, tags, tagdict)
  override def toString = f"StandardTagDictAndFwdBkdAdditionalTagAdder()"
}
