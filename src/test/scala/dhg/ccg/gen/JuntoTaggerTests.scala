package dhg.ccg.gen

import org.junit.Test
import dhg.util._
import dhg.util.TestUtil._
import org.junit.Assert._
import dhg.ccg.tag._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tagdict.SimpleTagDictionary

class JuntoTaggerTests {

  @Test
  def test_JuntoTagger {

    val lpTaggingGraphBuilder =
      new JuntoAdaptingLpTaggingGraphBuilder[String](
        new SimpleLpTaggingGraphBuilder[String](
          new Type2TokenLpEdgeExtractor(),
          Vector(
            TokenPrevLpEdgeExtractor("<S>"),
            TokenNextLpEdgeExtractor("<E>"),
            WordPrefixLpEdgeExtractor(3),
            WordSuffixLpEdgeExtractor(3))))

    val juntoTagger = new JuntoTagger[String](lpTaggingGraphBuilder, maxIterations = 200, threshold = 0.1, identity, identity)

    val taggedTrainingSentences = Vector(
      "the|D dog|N barks|V",
      "the|D man|N swims|V")
      .map(_.lsplit("\\s+").map(_.split("\\|").toTuple2))

    val rawSentences = Vector(
      "the dog walks",
      "the dog runs",
      "the man walks the dog")
      .map(_.lsplit("\\s+"))

    val initialTagdict =
      SimpleTagDictionary(Map("the" -> Set("D"), "dog" -> Set("N"), "walks" -> Set("N", "V")),
        startWord = "<S>", startTag = "<S>", endWord = "<E>", endTag = "<E>",
        additionalWords = Set("a", "on", "the", "bird", "sings", "silent", "works", "running"),
        additionalTags = Set("A"))

    val resultTaggedSentences = juntoTagger.tagFromAnnotations(rawSentences, taggedTrainingSentences, initialTagdict)

    assertEquals(3, resultTaggedSentences.size)
    assertEquals(resultTaggedSentences(0), Vector(("the", Map("D" -> 1.0)), ("dog", Map("N" -> 1.0)), ("walks", Map("V" -> 0.5544346982076169, "N" -> 0.44556530179238313))))
    assertEquals(resultTaggedSentences(1), Vector(("the", Map("D" -> 1.0)), ("dog", Map("N" -> 1.0)), ("runs", Map("V" -> 0.8258631721433737, "N" -> 0.17413682785662637))))
    assertEquals(resultTaggedSentences(2), Vector(("the", Map("D" -> 1.0)), ("man", Map("N" -> 1.0)), ("walks", Map("V" -> 0.5778362772353265, "N" -> 0.4221637227646735)), ("the", Map("D" -> 1.0)), ("dog", Map("N" -> 0.8356570366521339, "V" -> 0.16434296334786613))))
  }

}
