package dhg.ccg.parse.gfl

import dhg.util._
import dhg.util.viz._
import dhg.gfl._
import dhg.ccg.parse._
import dhg.ccg.parse.dep.DepTree
import scalaz.{ Node => _, _ }
import Scalaz._
import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.random.RandomGenerator
import dhg.ccg.cat.Cat
import ChunkerAnnoExtractor._
import dhg.ccg.chunk._

trait ChunkerAnnoExtractor { // extends (Vector[Word] => FudgSentence) {
  type Word = String

  def apply(sentence: Vector[Word]): FudgSentence
}

class AugDelegateChunkerAnnoExtractor(chunker: Chunker, annotationTreeAugmenter: AnnotationTreeAugmenter = new NoOpAnnotationTreeAugmenter) extends ChunkerAnnoExtractor {
  private[this] val random: RandomGenerator = new MersenneTwister

  def apply(sentence: Vector[Word]): FudgSentence = {
    val (tokens, tokenNodes) = makeTokens(sentence)
    chunker.chunk(sentence).toFudgSentence(tokens, tokenNodes)
  }

  override def toString = f"AugDelegateChunkerAnnoExtractor($chunker, $annotationTreeAugmenter)"
}

class AugDelegatePartialParserAnnoExtractor(partialParser: PartialParser, annotationTreeAugmenter: AnnotationTreeAugmenter = new NoOpAnnotationTreeAugmenter) extends ChunkerAnnoExtractor {
  private[this] val random: RandomGenerator = new MersenneTwister

  def apply(sentence: Vector[Word]): FudgSentence = {
    val (tokens, tokenNodes) = makeTokens(sentence)
    partialParser.parse(sentence).toFudgSentence(tokens, tokenNodes)
  }

  override def toString = f"AugDelegatePartialParserAnnoExtractor($partialParser, $annotationTreeAugmenter)"
}

object ChunkerAnnoExtractor {
  type Word = String

  def makeTokens(sentence: Vector[Word]) = {
    val tokens = sentence.zipWithIndex.mapt((w, i) => Token(w, i))
    val tokenNodes = tokens.map(tok => WordNode(f"${tok.token}_${tok.index}", tok))
    (tokens, tokenNodes)
  }

}

class ChunkerPseudoGoldAnnoExtractor(cae: ChunkerAnnoExtractor) extends GoldAnnoExtractor {
  def apply(ccgTree: CcgTree): FudgSentence = {
    cae(ccgTree.words)
  }
  override def toString = f"ChunkerPseudoGoldAnnoExtractor($cae)"
}
