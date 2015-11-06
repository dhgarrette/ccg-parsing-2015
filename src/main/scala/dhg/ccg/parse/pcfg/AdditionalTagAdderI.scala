//package dhg.ccg.parse.pcfg
//
//import scala.collection.mutable.{ Map => MMap }
//import scala.collection.mutable.{ Set => MSet }
//import dhg.util._
//import scalaz._
//import scalaz.Scalaz._
//import dhg.ccg.prob._
//import dhg.ccg.cat._
//import dhg.ccg.parse._
//import dhg.ccg.util._
//import dhg.ccg.tagdict.TagDictionary
//import scala.collection.immutable.BitSet
//
///**
// * Take a sentence as a sequence of words associated with sets of tags.
// * Each AdditionalTagAdder will output a transformed tagset for each word.
// */
//trait AdditionalTagAdderI {
//  type Word = Int
//  type Tag = Int
//  type CatSet = BitSet
//
//  def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: IndirectSparseVec[CatSet], allCats: CatSet): Vector[Set[Tag]]
//}
//
//class SequentialAdditionalTagAdderI(delegates: Vector[AdditionalTagAdderI]) extends AdditionalTagAdderI {
//  override def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: IndirectSparseVec[CatSet], allCats: CatSet): Vector[Set[Tag]] = delegates.foldLeft(tags) { (z, ata) => ata(sentence, z, tagdict, allCats) }
//  override def toString = f"SequentialAdditionalTagAdderI(${delegates.mkString(", ")})"
//}
//
//class ParallelAdditionalTagAdderI(delegates: Vector[AdditionalTagAdderI]) extends AdditionalTagAdderI {
//  override def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: IndirectSparseVec[CatSet], allCats: CatSet): Vector[Set[Tag]] = delegates.map(ata => ata(sentence, tags, tagdict, allCats)).transpose.map(_.flatten.toSet)
//  override def toString = f"ParallelAdditionalTagAdderI(${delegates.mkString(", ")})"
//}
//
//class NoOpAdditionalTagAdderI extends AdditionalTagAdderI {
//  override def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: IndirectSparseVec[CatSet], allCats: CatSet): Vector[Set[Tag]] = tags
//  override def toString = f"NoOpAdditionalTagAdderI()"
//}
//
///**
// * For each word in the sentence, return its TD entry set if the word appears in the TD (empty set otherwise)
// */
//class PresentTagdictAdditionalTagAdderI() extends AdditionalTagAdderI {
//  override def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: IndirectSparseVec[CatSet], allCats: CatSet): Vector[Set[Tag]] = (sentence zipSafe tags).mapt((w, _) => tagdict.getOrElse(w, BitSet.empty))
//  override def toString = f"PresentTagdictAdditionalTagAdderI()"
//}
//
///**
// * For each word in the sentence, if there are no tags associated with the word, then return a set containing all tags known to the TD (empty set otherwise)
// */
//class TagdictEntryForMissingAdditionalTagAdderI() extends AdditionalTagAdderI {
//  override def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: IndirectSparseVec[CatSet], allCats: CatSet): Vector[Set[Tag]] = (sentence zipSafe tags).mapt((w, ts) => if (ts.nonEmpty) Set.empty[Tag] else tagdict.getOrElse(w, BitSet.empty))
//  override def toString = f"FullTagdictTagsetForMissingAdditionalTagAdderI()"
//}
//
///**
// * For each word in the sentence, if there are no tags associated with the word, then return a set containing all tags known to the TD (empty set otherwise)
// */
//class FullTagdictTagsetForMissingAdditionalTagAdderI() extends AdditionalTagAdderI {
//  override def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: IndirectSparseVec[CatSet], allCats: CatSet): Vector[Set[Tag]] = tags.map(ts => if (ts.nonEmpty) Set.empty[Tag] else allCats)
//  override def toString = f"FullTagdictTagsetForMissingAdditionalTagAdderI()"
//}
//
//class DefaultTagsetForMissingAdditionalTagAdderI(defaultTagset: Set[Tag]) extends AdditionalTagAdderI {
//  override def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: IndirectSparseVec[CatSet], allCats: CatSet): Vector[Set[Tag]] = tags.map(ts => if (ts.nonEmpty) Set.empty[Tag] else defaultTagset)
//  override def toString = f"DefaultTagsetForMissingAdditionalTagAdderI($defaultTagset)"
//}
//
///*
// * CCG-specific adders
// */
//
//class FwdBkdModAdditionalTagAdderI() extends AdditionalTagAdderI[Cat] {
//  override def apply(sentence: Vector[Word], tags: Vector[Set[Cat]], tagdict: TagDictionary[Cat]): Vector[Set[Cat]] = {
//    val n = sentence.length
//    val endedTags = None +: tags.map(Option(_)) :+ None
//    for (((word, Seq(prevTags, Some(tags), nextTags)), i) <- (sentence zipSafe endedTags.sliding(3)).zipWithIndex) yield {
//      val prevs: Iterable[Cat] = for (prev <- prevTags.toSet; prevSupertag <- prev) yield (prevSupertag \ prevSupertag)
//      val nexts: Iterable[Cat] = for (next <- nextTags.toSet; nextSupertag <- next) yield (nextSupertag / nextSupertag)
//      (prevs ++ nexts).toSet
//    }
//  }
//}
//
////class FwdBkdConsumerAdditionalTagAdder extends AdditionalTagAdderI[Cat] {
////  override def apply(sentence: Vector[Word], tags: Vector[Set[Cat]]): Vector[Set[Cat]] = {
////    val n = sentences.length
////    val endedTags = None +: tags.map(Option(_)) :+ None
////    for (((word, Seq(prevTags, Some(tags), nextTags)), i) <- (sentence zipSafe endedTags.sliding(3)).zipWithIndex) yield {
////      val prevs: Iterable[Cat] = for (prev<- prevTags.toSet; prevSupertag <- prev) yield (prevSupertag \ prevSupertag)
////      val nexts: Iterable[Cat] = for (next<- nextTags.toSet; nextSupertag <- next) yield (nextSupertag / nextSupertag)
////      (tags ++ prevs ++ nexts).toSet
////    }
////
////    for (((word, tags), i) <- sentenceWithTags.zipWithIndex) yield {
////      val prevs: Iterable[Cat] = for (prevSupertag <- sentenceWithTags.map(_._2).apply(i); curSupertag <- tags) yield (curSupertag \ prevSupertag) else Set.empty
////      val nexts: Iterable[Cat] = for (nextSupertag <- sentenceWithTags.map(_._2).apply(i + 1); curSupertag <- tags) yield (curSupertag / nextSupertag) else Set.empty
////      (prevs ++ nexts).toSet
////    }
////  }
////}
////
////class AllFwdBkdAdditionalTagAdder extends AdditionalTagAdderI[Cat] {
////  override def apply(sentence: Vector[Word], tags: Vector[Set[Cat]]): Vector[Set[Cat]] = {
////    val n = sentenceWithTags.length
////    for (((word, tags), i) <- sentenceWithTags.zipWithIndex) yield {
////      val prevs: Seq[Cat] = for (pTags <- sentenceWithTags.map(_._2).take(i); prevSupertag <- pTags) yield (prevSupertag \ prevSupertag)
////      val nexts: Seq[Cat] = for (nTags <- sentenceWithTags.map(_._2).drop(i + 1); nextSupertag <- nTags) yield (nextSupertag / nextSupertag)
////      // TODO: Maybe this hsould be `curSupertag / nextSupertag` ??
////      (prevs ++ nexts).toSet
////    }
////  }
////}
//
////
//
//class StandardTagDictAdditionalTagAdderI() extends AdditionalTagAdderI {
//  private[this] val delegate: AdditionalTagAdderI =
//    new SequentialAdditionalTagAdderI(Vector(
//      new ParallelAdditionalTagAdderI(Vector(
//        new NoOpAdditionalTagAdderI(),
//        new TagdictEntryForMissingAdditionalTagAdderI())),
//      new ParallelAdditionalTagAdderI(Vector(
//        new NoOpAdditionalTagAdderI(),
//        new FullTagdictTagsetForMissingAdditionalTagAdderI()))))
//  override def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: IndirectSparseVec[CatSet], allCats: CatSet): Vector[Set[Tag]] = delegate(sentence, tags, tagdict)
//  override def toString = f"StandardTagDictAdditionalTagAdderI()"
//}
//
//class StandardTagDictAndFwdBkdAdditionalTagAdderI() extends AdditionalTagAdderI[Cat] {
//  private[this] val delegate: AdditionalTagAdderI[Cat] =
//    new SequentialAdditionalTagAdderI[Cat](Vector(
//      new StandardTagDictAdditionalTagAdderI(),
//      new ParallelAdditionalTagAdderI[Cat](Vector(
//        new NoOpAdditionalTagAdderI(),
//        new FwdBkdModAdditionalTagAdderI()))))
//  override def apply(sentence: Vector[Word], tags: Vector[Set[Cat]], tagdict: TagDictionary[Cat]): Vector[Set[Cat]] = delegate(sentence, tags, tagdict)
//  override def toString = f"StandardTagDictAndFwdBkdAdditionalTagAdderI()"
//}
