package dhg.ccg.tagdict

import dhg.util._
import scalaz._
import scalaz.Scalaz._
import dhg.ccg.cat.Cat
import dhg.ccg.cat.StartCat
import dhg.ccg.cat.EndCat

trait TagDictionary[Tag] extends (String => Set[Tag]) {
  type Word = String
  def allWords: Set[Word]; def allTags: Set[Tag]
  def startWord: Word; def startTag: Tag; def endWord: Word; def endTag: Tag
  def excludedTags: Set[Tag]

  def apply(w: Word): Set[Tag]

  final def allWordsSE = allWords + (startWord, endWord)
  final def allTagsSE = allTags + (startTag, endTag)

  def reversed: Map[Tag, Set[Word]]

  def entries: Map[Word, Set[Tag]]
  def knownWordsForTag: Map[Tag, Set[Word]]

  def withWords(words: Set[Word]): TagDictionary[Tag]
  def withTags(tags: Set[Tag]): TagDictionary[Tag]
  def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag]
}

/**
 * ONLY INSTANTIATE THIS VIA THE COMPANION OBJECT
 *
 * A Tag Dictionary is a mapping from words to all of their potential
 * tags. A word not found in the dictionary (including "unknown" words)
 * may take any tag.
 *
 * This class guarantees that looking up the startWord or endWord will
 * return a set containing ony the startTag or endTag, respectively.
 *
 * The allWords property is the complete set of known words excluding
 * the special startWord and endWord.  Likewise for allTags.  For the
 * complete set of known words and tags including these special tags
 * use allWordsSE and allTagsSE.
 */
class SimpleTagDictionary[Tag] private (
  map: Map[String, Set[Tag]],
  val allWords: Set[String], val allTags: Set[Tag],
  val startWord: String, val startTag: Tag, val endWord: String, val endTag: Tag,
  val excludedTags: Set[Tag] = Set.empty)
  extends TagDictionary[Tag] {

  def apply(w: Word): Set[Tag] = {
    map.get(w).map(_ -- excludedTags).filter(_.nonEmpty).getOrElse(allTags) -- excludedTags
  }

  def reversed: Map[Tag, Set[Word]] = ???

  val entries: Map[Word, Set[Tag]] = map.mapVals(_ -- excludedTags).filter(_._2.nonEmpty)
  val knownWordsForTag: Map[Tag, Set[Word]] = allTags.mapToVal(Set.empty[Word]).toMap ++ entries.ungroup.map(_.swap).groupByKey.mapVals(_.toSet)

  def withWords(words: Set[Word]) = new SimpleTagDictionary(map, allWords ++ words, allTags -- excludedTags, startWord, startTag, endWord, endTag, excludedTags)
  def withTags(tags: Set[Tag]) = new SimpleTagDictionary(map, allWords, (allTags ++ tags) -- excludedTags, startWord, startTag, endWord, endTag, excludedTags)
  def withExcludedTags(tags: Set[Tag]) = new SimpleTagDictionary(map, allWords, allTags, startWord, startTag, endWord, endTag, excludedTags ++ tags)
}

object SimpleTagDictionary {
  type Word = String
  def apply[Tag](
    map: Map[Word, Set[Tag]],
    startWord: Word, startTag: Tag, endWord: Word, endTag: Tag,
    additionalWords: Set[Word] = Set[Word](), additionalTags: Set[Tag] = Set[Tag](),
    excludedTags: Set[Tag] = Set[Tag]()) = {

    val allAllWords = additionalWords ++ map.keys
    val allAllTags = additionalTags ++ map.flatMap(_._2) -- excludedTags
    new SimpleTagDictionary(
      map.mapVals(_ -- excludedTags) ++ Map(startWord -> Set(startTag), endWord -> Set(endTag)),
      allAllWords - (startWord, endWord), allAllTags -- excludedTags - (startTag, endTag),
      startWord, startTag, endWord, endTag,
      excludedTags)
  }

  def empty[Tag](startWord: Word, startTag: Tag, endWord: Word, endTag: Tag, excludedTags: Set[Tag] = Set.empty[Tag]) = {
    SimpleTagDictionary(Map(), startWord, startTag, endWord, endTag, excludedTags = excludedTags)
  }
}

object DummyCatTagDictionary extends TagDictionary[Cat] {
  type Tag = Cat
  def allWords: Set[Word] = Set.empty; def allTags: Set[Tag] = Set.empty
  def startWord: Word = "<S>"; def startTag: Tag = StartCat; def endWord: Word = "<E>"; def endTag: Tag = EndCat
  def excludedTags: Set[Tag] = Set.empty

  def apply(w: Word): Set[Tag] = Set.empty

  def reversed: Map[Tag, Set[Word]] = Map.empty

  def entries: Map[Word, Set[Tag]] = Map.empty
  def knownWordsForTag: Map[Tag, Set[Word]] = Map.empty

  def withWords(words: Set[Word]): TagDictionary[Tag] = this
  def withTags(tags: Set[Tag]): TagDictionary[Tag] = this
  def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = this
}

//

trait TagDictionaryFactory[Tag] {
  type Word = String
  def apply[Tag](
    sentences: Vector[Vector[(Word, Tag)]],
    startWord: Word, startTag: Tag, endWord: Word, endTag: Tag,
    additionalWords: Set[Word] = Set[Word](), additionalTags: Set[Tag] = Set[Tag](),
    excludedTags: Set[Tag] = Set[Tag]() //
    ): TagDictionary[Tag]
}

class SimpleTagDictionaryFactory[Tag](tdCutoff: Option[Double] = None) extends TagDictionaryFactory[Tag] {
  override def apply[Tag](
    taggedSentences: Vector[Vector[(Word, Tag)]],
    startWord: Word, startTag: Tag, endWord: Word, endTag: Tag,
    additionalWords: Set[Word], additionalTags: Set[Tag],
    excludedTags: Set[Tag] = Set[Tag]()) = {
    val tagCounts = taggedSentences.flatten.groupByKey.mapVals(_.counts.normalizeValues)
    val cutoff = tdCutoff.getOrElse(0.0)
    val pruned = tagCounts.mapVals(_.collect { case (t, p) if p >= cutoff => t }.toSet -- excludedTags).filter(_._2.nonEmpty)
    SimpleTagDictionary(pruned, startWord, startTag, endWord, endTag,
      additionalWords ++ tagCounts.keys,
      additionalTags -- excludedTags,
      excludedTags)
  }
}

class StartEndSwappedTagDictionary[Tag](wrapped: TagDictionary[Tag]) extends TagDictionary[Tag] {
  def allWords: Set[Word] = wrapped.allWords; def allTags: Set[Tag] = wrapped.allTags
  def startWord: Word = wrapped.endWord; def startTag: Tag = wrapped.endTag; def endWord: Word = wrapped.startWord; def endTag: Tag = wrapped.startTag
  def excludedTags: Set[Tag] = wrapped.excludedTags

  def apply(w: Word): Set[Tag] = wrapped(w)

  def reversed: Map[Tag, Set[Word]] = wrapped.reversed

  def entries: Map[Word, Set[Tag]] = wrapped.entries
  def knownWordsForTag: Map[Tag, Set[Word]] = wrapped.knownWordsForTag

  def withWords(words: Set[Word]): TagDictionary[Tag] = new StartEndSwappedTagDictionary(wrapped.withWords(words))
  def withTags(tags: Set[Tag]): TagDictionary[Tag] = new StartEndSwappedTagDictionary(wrapped.withTags(tags))
  def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = new StartEndSwappedTagDictionary(wrapped.withExcludedTags(tags))
}
