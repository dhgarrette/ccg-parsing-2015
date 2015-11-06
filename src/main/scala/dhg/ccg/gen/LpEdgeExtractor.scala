package dhg.ccg.gen

import scala.collection.breakOut
import dhg.util._

trait LpEdgeExtractor[+A <: LpFeature, +B <: LpFeature] {
  type Word = String
  def apply(rawSentences: Vector[Vector[Word]], allWords: Set[Word]): Iterator[(A, Vector[B])]

  def rawTokens(rawSentences: Vector[Vector[Word]]): Vector[Vector[TokenLpFeature]] = {
    rawSentences.zipWithIndex.map {
      case (sent, sentIdx) => sent.zipWithIndex.map {
        case (tok, tokIdx) => TokenLpFeature(tok, sentIdx, tokIdx)
      }
    }
  }
}

case class Type2TokenLpEdgeExtractor() extends LpEdgeExtractor[TypeLpFeature, TokenLpFeature] {
  override def apply(rawSentences: Vector[Vector[Word]], allWords: Set[Word]) = {
    rawTokens(rawSentences).flatten
      .groupBy(_.word).iterator
      .mapKeys(TypeLpFeature(_))
  }
}

abstract class BigramLpEdgeExtractor[Feature <: LpFeature]() extends LpEdgeExtractor[Feature, TokenLpFeature] {
  protected[this] def tokenBigrams(rawSentences: Vector[Vector[Word]]) = rawTokens(rawSentences).map(s => None +: s.map(w => Some(w)) :+ None).flatMap(_.sliding2)
}

case class TokenPrevLpEdgeExtractor(startWord: String) extends BigramLpEdgeExtractor[PrevWordLpFeat]() {
  override def apply(rawSentences: Vector[Vector[Word]], allWords: Set[Word]) = {
    tokenBigrams(rawSentences)
      .groupBy(_._1.map(_.word)).iterator
      .map { case (word, tokPairs) => PrevWordLpFeat(word, startWord) -> tokPairs.collect { case (_, Some(t)) => t } }
  }
}

case class TokenNextLpEdgeExtractor(endWord: String) extends BigramLpEdgeExtractor[NextWordLpFeat]() {
  override def apply(rawSentences: Vector[Vector[Word]], allWords: Set[Word]) = {
    tokenBigrams(rawSentences)
      .groupBy(_._2.map(_.word)).iterator
      .map { case (word, tokPairs) => NextWordLpFeat(word, endWord) -> tokPairs.collect { case (Some(t), _) => t } }
  }
}

case class WordPrefixLpEdgeExtractor(maxLength: Int) extends LpEdgeExtractor[PrefixLpFeat, TypeLpFeature] {
  override def apply(rawSentences: Vector[Vector[Word]], allWords: Set[Word]) = {
    val completeAllWords = allWords ++ rawSentences.flatten
    (1 to maxLength).flatMap(n => completeAllWords.mapTo(_.toString).collect { case (w, s) if s.size >= n => s.take(n) -> w }.toVector)
      .groupByKey.iterator
      .map { case (prefix, words) => PrefixLpFeat(prefix, prefix.length) -> words.map(TypeLpFeature(_))(breakOut) }
  }
}

case class WordSuffixLpEdgeExtractor(maxLength: Int) extends LpEdgeExtractor[SuffixLpFeat, TypeLpFeature] {
  override def apply(rawSentences: Vector[Vector[Word]], allWords: Set[Word]) = {
    val completeAllWords = allWords ++ rawSentences.flatten
    (1 to maxLength).flatMap(n => completeAllWords.mapTo(_.toString).collect { case (w, s) if s.size >= n => s.takeRight(n) -> w }.toVector)
      .groupByKey.iterator
      .map { case (suffix, words) => SuffixLpFeat(suffix, suffix.length) -> words.map(TypeLpFeature(_))(breakOut) }
  }
}

//case class WordDictposLpEdgeExtractor(dict: ExternalDictionary) extends LpEdgeExtractor {
//  override def apply(rawSentences: Vector[Vector[Word]]): Iterator[(LpFeature, Vector[LpFeature])] =
//    dict.getTags(allSymbols(rawSentences)).iterator
//      .map { case (word, poss) => WordFeat(Some(word)) -> poss.map(p => DictposFeat(p))(breakOut) }
//}
//
//case class WordMorphLpEdgeExtractor(fst: SimpleFst[Sym, Vector[String]]) extends LpEdgeExtractor {
//  override def apply(rawSentences: Vector[Vector[Word]]): Iterator[(LpFeature, Vector[LpFeature])] = {
//    fst.analyze(allSymbols(rawSentences)).iterator.mapVals(_.flatten.distinct)
//      .map { case (word, analyses) => WordFeat(Some(word)) -> analyses.map(p => MorphFeat(p))(breakOut) }
//  }
//}
