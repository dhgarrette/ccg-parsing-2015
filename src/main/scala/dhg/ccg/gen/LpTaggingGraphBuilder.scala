package dhg.ccg.gen

import java.util.ArrayList

import dhg.ccg.tagdict.TagDictionary
import dhg.util._

import junto.algorithm.ModifiedAdsorption
import junto.config.Edge
import junto.config.GraphBuilder
import junto.config.Label
import junto.graph.GraphIo
import junto.util.{ Constants => JuntoConstants }

trait LpTaggingGraphBuilder[Tag, TypeFeature, TokenFeature, Feature] {
  type Word = String
  def makeType2TokenEdges(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    rawSentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Tag]): Iterator[(TypeFeature, Vector[TokenFeature])]
  def makeFeatureEdges(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    rawSentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Tag]): Iterator[(Feature, Vector[Feature])]
  def makeTypeSeeds(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    rawSentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Tag]): Iterator[(TypeFeature, Tag)]
  def makeTokenSeeds(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    rawSentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Tag]): Iterator[(TokenFeature, Tag)]
  def extractTaggedTokens(
    output: Iterator[(TokenFeature, Map[Tag, Double])],
    rawSentences: Vector[Vector[Word]]): Vector[Vector[(Word, Map[Tag, Double])]]
}

class SimpleLpTaggingGraphBuilder[Tag](
  type2tokenEdgeExtractor: LpEdgeExtractor[TypeLpFeature, TokenLpFeature],
  featureEdgeExtractors: Iterable[LpEdgeExtractor[LpFeature, LpFeature]])
  extends LpTaggingGraphBuilder[Tag, TypeLpFeature, TokenLpFeature, LpFeature] {

  override def makeType2TokenEdges(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    rawSentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Tag]) = {
    val tagdict = initialTagdict.withWords(rawSentences.flatten.toSet | taggedSentences.flatten.map(_._1).toSet).withTags(taggedSentences.flatten.map(_._2).toSet)

    val allSentences = rawSentences ++ taggedSentences.map(_.map(_._1)) // always do raw before tagged
    type2tokenEdgeExtractor(allSentences, tagdict.allWords)
  }

  override def makeFeatureEdges(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    rawSentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Tag]) = {
    val tagdict = initialTagdict.withWords(rawSentences.flatten.toSet | taggedSentences.flatten.map(_._1).toSet).withTags(taggedSentences.flatten.map(_._2).toSet)

    val allSentences = rawSentences ++ taggedSentences.map(_.map(_._1)) // always do raw before tagged
    featureEdgeExtractors.iterator.flatMap(ee => ee(allSentences, tagdict.allWords))
  }

  override def makeTypeSeeds(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    rawSentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Tag]) = {
    val tagdict = initialTagdict.withWords(rawSentences.flatten.toSet | taggedSentences.flatten.map(_._1).toSet).withTags(taggedSentences.flatten.map(_._2).toSet)

    for {
      (word, tag) <- tagdict.entries.ungroup
      if !tagdict.excludedTags(tag)
    } yield (TypeLpFeature(word), tag)
  }

  override def makeTokenSeeds(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    rawSentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Tag]) = {
    val tagdict = initialTagdict.withWords(rawSentences.flatten.toSet | taggedSentences.flatten.map(_._1).toSet).withTags(taggedSentences.flatten.map(_._2).toSet)

    for {
      (sent, sentIdx) <- taggedSentences.iterator.zipWithIndex
      ((word, tag), tokIdx) <- sent.zipWithIndex
      if !tagdict.excludedTags(tag)
    } yield (TokenLpFeature(word, sentIdx + rawSentences.size, tokIdx), tag)
  }

  override def extractTaggedTokens(output: Iterator[(TokenLpFeature, Map[Tag, Double])], rawSentences: Vector[Vector[Word]]) = {
    val indexed = output.map { case (TokenLpFeature(word, sentIdx, tokenIdx), tags) => (sentIdx, tokenIdx) -> (word, tags) }.toMap
    rawSentences.zipWithIndex.map {
      case (rawSent, sentIdx) =>
        rawSent.zipWithIndex.map {
          case (word, tokenIdx) =>
            val (lpWord, tags) = indexed.getOrElse((sentIdx, tokenIdx), (word, Map.empty[Tag, Double]))
            assert(word == lpWord, "%s != %s".format(word, lpWord))
            (word, tags)
        }
    }
  }

}

class JuntoAdaptingLpTaggingGraphBuilder[Tag](
  val delegate: LpTaggingGraphBuilder[Tag, TypeLpFeature, TokenLpFeature, LpFeature])
  extends LpTaggingGraphBuilder[Tag, String, String, String] {

  override def makeType2TokenEdges(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    rawSentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Tag]) = {
    val tagdict = initialTagdict.withWords(rawSentences.flatten.toSet | taggedSentences.flatten.map(_._1).toSet).withTags(taggedSentences.flatten.map(_._2).toSet)

    delegate.makeType2TokenEdges(taggedSentences, rawSentences, tagdict).map { case (a, bs) => (featureToString(a), bs.map(featureToString)) }
  }

  override def makeFeatureEdges(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    rawSentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Tag]) = {
    val tagdict = initialTagdict.withWords(rawSentences.flatten.toSet | taggedSentences.flatten.map(_._1).toSet).withTags(taggedSentences.flatten.map(_._2).toSet)

    delegate.makeFeatureEdges(taggedSentences, rawSentences, tagdict).map { case (a, bs) => (featureToString(a), bs.map(featureToString)) }
  }

  override def makeTypeSeeds(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    rawSentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Tag]) = {
    val tagdict = initialTagdict.withWords(rawSentences.flatten.toSet | taggedSentences.flatten.map(_._1).toSet).withTags(taggedSentences.flatten.map(_._2).toSet)

    delegate.makeTypeSeeds(taggedSentences, rawSentences, tagdict).map { case (a, b) => (typeFeatToString(a), b) }
  }

  override def makeTokenSeeds(
    taggedSentences: Vector[Vector[(Word, Tag)]],
    rawSentences: Vector[Vector[Word]],
    initialTagdict: TagDictionary[Tag]) = {
    val tagdict = initialTagdict.withWords(rawSentences.flatten.toSet | taggedSentences.flatten.map(_._1).toSet).withTags(taggedSentences.flatten.map(_._2).toSet)

    delegate.makeTokenSeeds(taggedSentences, rawSentences, tagdict).map { case (a, b) => (tokenFeatToString(a), b) }
  }

  private[this] def typeFeatToString(f: TypeLpFeature) = f"TYPE_${f.word}"
  private[this] def tokenFeatToString(f: TokenLpFeature) = f"TOKEN_${f.sentIdx}_${f.tokenIdx}_${f.word}"
  private[this] def featureToString(f: LpFeature) = f match {
    case f: TypeLpFeature => typeFeatToString(f)
    case f: TokenLpFeature => tokenFeatToString(f)
    case _ => f.toString.replaceAll("\\s+", "")
  }

  override def extractTaggedTokens(output: Iterator[(String, Map[Tag, Double])], rawSentences: Vector[Vector[Word]]) = {
    val TokenRe = """TOKEN_(\d+)_(\d+)_(.+)""".r
    val unpacked = output.collect {
      case (TokenRe(UInt(sentIdx), UInt(tokenIdx), word), tags) => (TokenLpFeature(word, sentIdx, tokenIdx), tags)
    }
    delegate.extractTaggedTokens(unpacked, rawSentences)
  }
}
