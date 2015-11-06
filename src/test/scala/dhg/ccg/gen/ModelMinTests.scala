package dhg.ccg.gen

import org.junit.Test
import dhg.util._
import dhg.util.TestUtil._
import org.junit.Assert._
import dhg.ccg.tag._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tagdict.SimpleTagDictionary

class ModelMinTests {

  @Test
  def test_ModelMinSoftToHardTagger {

    val modelMinTagger = new ModelMinSoftToHardTagger[String]()

    val taggedTrainingSentences = Vector(
      "the|D dog|N barks|V",
      "the|D man|N swims|V")
      .map(_.lsplit("\\s+").map(_.split("\\|").toTuple2))

    val softTaggedSentences = Vector[Vector[(String, Map[String, Double])]](
      Vector(("the", Map("D" -> 1.0)), ("dog", Map()), ("walks", Map("V" -> 0.6, "N" -> 0.4))),
      Vector(("a", Map()), ("dog", Map("N" -> 1.0)), ("runs", Map("V" -> 0.8, "N" -> 0.2))),
      Vector(("the", Map("D" -> 1.0)), ("man", Map("N" -> 0.7, "V" -> 0.2, "D" -> 0.1)), ("walks", Map("V" -> 0.7, "N" -> 0.3)), ("the", Map("D" -> 1.0)), ("dog", Map("N" -> 0.9, "V" -> 0.1))))

    val initialTagdict =
      SimpleTagDictionary(Map("the" -> Set("D"), "dog" -> Set("N"), "walks" -> Set("N", "V")),
        startWord = "<S>", startTag = "<S>", endWord = "<E>", endTag = "<E>",
        additionalWords = Set("a", "on", "the", "bird", "sings", "silent", "works", "running"),
        additionalTags = Set("A"))

    val resultTaggedSentences = modelMinTagger.tagFromSoftTaggedSentences(softTaggedSentences, taggedTrainingSentences, initialTagdict).sortBy(_.map(_._1).mkString("+"))
    assertEquals(Vector(("a", "D"), ("dog", "N"), ("runs", "V")), resultTaggedSentences(0))
    assertEquals(Vector(("the", "D"), ("dog", "N"), ("walks", "V")), resultTaggedSentences(1))
    assertEquals(Vector(("the", "D"), ("man", "N"), ("walks", "V"), ("the", "D"), ("dog", "N")), resultTaggedSentences(2))
    assertEquals(3, resultTaggedSentences.size)
  }

  @Test
  def test_ModelMinSoftToHardTagger_completeTagging {
    ???
  }

  @Test
  def test_ModelMinSoftToHardTagger_coverRecurse {
    ???
  }

  @Test
  def test_ModelMinSoftToHardTagger_cover {
    ???
  }

  @Test
  def test_ModelMinSoftToHardTagger_fillRecurse {
    ???
  }

  @Test
  def test_ModelMinSoftToHardTagger_fill {
    ???
  }

  @Test
  def test_ModelMinSoftToHardTagger_tag {

    val modelMinTagger = new ModelMinSoftToHardTagger[String]()

    val tagdict =
      SimpleTagDictionary(Map("the" -> Set("D"), "dog" -> Set("N"), "walks" -> Set("N", "V")),
        startWord = "<S>", startTag = "<S>", endWord = "<E>", endTag = "<E>",
        additionalWords = Set("a", "on", "the", "bird", "sings", "silent", "works", "running"),
        additionalTags = Set("A"))

    val sentence = Vector(
      ("<S>", Map("<S>" -> 1.0)),
      ("the", Map("D" -> 1.0)),
      ("man", Map("N" -> 0.7, "V" -> 0.2, "D" -> 0.1)),
      ("walks", Map("V" -> 0.7, "N" -> 0.3)),
      ("the", Map("D" -> 1.0)),
      ("dog", Map("N" -> 0.9, "V" -> 0.1)),
      ("<E>", Map("<E>" -> 1.0)))

    // covers:  <S>-the-man-walks-the-dog-<E>
    //           S                         E
    //               D   D         D 
    //                   N    N        N 
    //                   V    V        V

    val r1 = modelMinTagger.tag(sentence, Set(("<S>", "D"), ("D", "N"), ("N", "V"), ("V", "D"), ("N", "<E>")), tagdict)
    assertEquals(Some(Vector(("the", "D"), ("man", "N"), ("walks", "V"), ("the", "D"), ("dog", "N"))), r1)

    assertEquals(None, modelMinTagger.tag(sentence, Set(("<S>", "D"), ("D", "N"), ("N", "V"), ("N", "D"), ("N", "<E>")), tagdict))

    val r2 = modelMinTagger.tag(sentence, Set(("<S>", "D"), ("D", "D"), ("D", "V"), ("V", "D"), ("V", "<E>")), tagdict)
    assertEquals(Some(Vector(("the", "D"), ("man", "D"), ("walks", "V"), ("the", "D"), ("dog", "V"))), r2)

    val r3 = modelMinTagger.tag(sentence, Set(("<S>", "D"), ("D", "D"), ("D", "N"), ("D", "V"), ("V", "D"), ("V", "<E>")), tagdict)
    assertEquals(Some(Vector(("the", "D"), ("man", "D"), ("walks", "V"), ("the", "D"), ("dog", "V"))), r3)

    val r4 = modelMinTagger.tag(sentence, Set(("<S>", "D"), ("D", "D"), ("D", "N"), ("D", "V"), ("V", "D"), ("V", "<E>"), ("N", "<E>")), tagdict)
    assertEquals(Some(Vector(("the", "D"), ("man", "D"), ("walks", "V"), ("the", "D"), ("dog", "N"))), r4)

    val r5 = modelMinTagger.tag(sentence, Set(("<S>", "D"), ("D", "D"), ("D", "N"), ("D", "V"), ("N", "V"), ("V", "D"), ("V", "<E>"), ("N", "<E>")), tagdict)
    assertEquals(Some(Vector(("the", "D"), ("man", "N"), ("walks", "V"), ("the", "D"), ("dog", "N"))), r5)
  }

  @Test
  def test_ModelMinSoftToHardTagger_edgeCoverWeights {
    val sentence = Vector(
      ("<S>", Map("<S>" -> 1.0)),
      ("the", Map("D" -> 1.0)),
      ("man", Map("N" -> 0.7, "V" -> 0.2, "D" -> 0.1)),
      ("walks", Map("V" -> 0.7, "N" -> 0.3)),
      ("the", Map("D" -> 1.0)),
      ("dog", Map("N" -> 0.9, "V" -> 0.1)),
      ("<E>", Map("<E>" -> 1.0)))
    //    val tagdict =
    //      SimpleTagDictionary(Map("the" -> Set("D"), "dog" -> Set("N"), "walks" -> Set("N", "V")),
    //        startWord = "<S>", startTag = "<S>", endWord = "<E>", endTag = "<E>",
    //        additionalWords = Set("a", "on", "the", "bird", "sings", "silent", "works", "running"),
    //        additionalTags = Set("A"))

    val modelMinTagger = new ModelMinSoftToHardTagger[String]()

    val r1 = modelMinTagger.edgeCoverWeights(sentence, Set())
    // covers:  <S> the man walks the dog <E>
    //           S                         E
    //               D   D         D
    //                   N    N        N
    //                   V    V        V
    assertEquals(12, r1.size)
    assertEquals(1.0, r1("<S>" -> "D"), 1e-9)
    assertEquals(1.1, r1("D" -> "D"), 1e-9)
    assertEquals(1.7 + 0.4 + 1.9, r1("D" -> "N"), 1e-9)
    assertEquals(1.2 + 0.8 + 1.1, r1("D" -> "V"), 1e-9)
    assertEquals(1.3, r1("N" -> "D"), 1e-9)
    assertEquals(1.0, r1("N" -> "N"), 1e-9)
    assertEquals(1.4, r1("N" -> "V"), 1e-9)
    assertEquals(0.9, r1("N" -> "<E>"), 1e-9)
    assertEquals(1.7, r1("V" -> "D"), 1e-9)
    assertEquals(0.5, r1("V" -> "N"), 1e-9)
    assertEquals(0.9, r1("V" -> "V"), 1e-9)
    assertEquals(0.1, r1("V" -> "<E>"), 1e-9)

    val r2 = modelMinTagger.edgeCoverWeights(sentence, Set(("N", "V")))
    // covers:  <S> the [man walks] the dog <E>
    //           S                           E
    //               D    D          D
    //                    N    N         N
    //                    V    V         V
    assertEquals(8, r2.size)
    assertEquals(1.0, r2("<S>" -> "D"), 1e-9)
    assertEquals(1.0, r2("D" -> "D"), 1e-9)
    assertEquals(1.0 + 1.9, r2("D" -> "N"), 1e-9)
    assertEquals(1.0 + 1.1, r2("D" -> "V"), 1e-9)
    assertEquals(1.0, r2("N" -> "D"), 1e-9)
    assertEquals(0.9, r2("N" -> "<E>"), 1e-9)
    assertEquals(1.0, r2("V" -> "D"), 1e-9)
    assertEquals(0.1, r2("V" -> "<E>"), 1e-9)

    val r3 = modelMinTagger.edgeCoverWeights(sentence, Set(("D", "D"), ("V", "D"), ("N", "<E>")))
    // covers:  <S> [the man walks the dog] <E>
    //           S                           E
    //               D    D         D
    //                    N    N        N
    //                    V    V        V
    assertEquals(0, r3.size)

    val r4 = modelMinTagger.edgeCoverWeights(sentence, Set(("N", "V"), ("N", "D")))
    // covers:  <S> the [man walks the] dog <E>
    //           S                           E
    //               D    D         D
    //                    N    N         N
    //                    V    V         V
    assertEquals(6, r4.size)
    assertEquals(1.0, r4("<S>" -> "D"), 1e-9)
    assertEquals(1.0, r4("D" -> "D"), 1e-9)
    assertEquals(1.0 + 0.9, r4("D" -> "N"), 1e-9)
    assertEquals(1.0 + 0.1, r4("D" -> "V"), 1e-9)
    assertEquals(0.9, r4("N" -> "<E>"), 1e-9)
    assertEquals(0.1, r4("V" -> "<E>"), 1e-9)

    val r5 = modelMinTagger.edgeCoverWeights(sentence, Set(("N", "V"), ("V", "<E>")))
    // covers:  <S> the [man walks] the [dog <E>]
    //           S                           E
    //               D    D          D
    //                    N    N          N
    //                    V    V          V
    assertEquals(6, r5.size)
    assertEquals(1.0, r5("<S>" -> "D"), 1e-9)
    assertEquals(1.0, r5("D" -> "D"), 1e-9)
    assertEquals(1.0 + 1.0, r5("D" -> "N"), 1e-9)
    assertEquals(1.0 + 1.0, r5("D" -> "V"), 1e-9)
    assertEquals(1.0, r5("N" -> "D"), 1e-9)
    assertEquals(1.0, r5("V" -> "D"), 1e-9)
  }

  @Test
  def test_ModelMinSoftToHardTagger_holeFillWeights {
    val sentence = Vector(
      ("<S>", Map("<S>" -> 1.0)),
      ("the", Map("D" -> 1.0)),
      ("man", Map("N" -> 0.7, "V" -> 0.2, "D" -> 0.1)),
      ("walks", Map("V" -> 0.6, "N" -> 0.4)),
      ("the", Map("D" -> 1.0)),
      ("dog", Map("N" -> 0.9, "V" -> 0.1)),
      ("<E>", Map("<E>" -> 1.0)))
    val tagdict =
      SimpleTagDictionary(Map("the" -> Set("D"), "dog" -> Set("N"), "walks" -> Set("N", "V")),
        startWord = "<S>", startTag = "<S>", endWord = "<E>", endTag = "<E>",
        additionalWords = Set("a", "on", "the", "bird", "sings", "silent", "works", "running"),
        additionalTags = Set("A"))
    val se = Set(("<S>", "<S>"), ("<E>", "<E>"))

    val modelMinTagger = new ModelMinSoftToHardTagger[String]()

    val r0 = modelMinTagger.holeFillWeights(sentence, se | Set(("D", "N"), ("V", "N"))) // , ("V", "V")))
    // covers:  <S> the man walks the dog <E>
    //           S                         E
    //               D   D         D
    //                   N    N        N
    //                   V    V        V
    assertEquals(((1.0) + 1.0 + 1.0 + (0.7)), r0("<S>" -> "D"), 1e-9)
    assertEquals(((0.1 + 0.2) + 0.4 + 1.0 + (0.9)), r0("N" -> "D"), 1e-9)
    assertEquals(((1.0) + 0.9 + 1.0 + (1.0)), r0("N" -> "<E>"), 1e-9)
    assertEquals(3, r0.size)

    val r1 = modelMinTagger.holeFillWeights(sentence, se | Set(("D", "N"), ("V", "N"), ("V", "V")))
    // covers:  <S> the man walks the dog <E>
    //           S                         E
    //               D   D         D
    //                   N    N        N
    //                   V    V        V
    assertEquals(((1.0) + 1.0 + 1.0 + (0.7)), r1("<S>" -> "D"), 1e-9)
    assertEquals(((0.1 + 0.2) + 0.4 + 1.0 + (0.9)), r1("N" -> "D"), 1e-9)
    assertEquals(((0.2) + 0.6 + 1.0 + (0.9)), r1("V" -> "D"), 1e-9)
    assertEquals(((1.0) + 0.9 + 1.0 + (1.0)), r1("N" -> "<E>"), 1e-9)
    assertEquals(4, r1.size)

    val r2 = modelMinTagger.holeFillWeights(sentence, se | Set(("N", "V")))
    // covers:  <S> the man walks the dog <E>
    //           S                         E
    //               D   D         D
    //                   N    N        N
    //                   V    V        V
    assertEquals(0, r2.size)

    val r3 = modelMinTagger.holeFillWeights(sentence, se | Set(("D", "D"), ("V", "D"), ("N", "<E>")))
    // covers:  <S> the man walks the dog <E>
    //          -S                        /E- 
    //               D - D        /D 
    //                   N    N        N/ 
    //                   V    V/       V
    assertEquals(((1.0) + 1.0 + 1.0 + (0.7)), r1("<S>" -> "D"), 1e-9)
    assertEquals(((0.6) + 1.0 + 0.9 + (1.0)), r3("D" -> "N"), 1e-9)
    assertEquals(((1.0) + 0.1 + 0.6 + (1.0)), r3("D" -> "V"), 1e-9)
    assertEquals(3, r3.size)

    val r4 = modelMinTagger.holeFillWeights(sentence, se | Set(("<S>", "D"), ("D", "D"), ("N", "D"), ("V", "D"), ("N", "<E>"), ("V", "<E>")))
    // covers:  <S> the man walks the dog <E>
    //           S                         E
    //               D   D         D 
    //                   N    N        N
    //                   V    V        V
    assertEquals(((1.0) + 0.1 + 0.4 + (1.0)) + ((0.4 + 0.6) + 1.0 + 0.9 + (1.0)), r4("D" -> "N"), 1e-9)
    assertEquals(((1.0) + 0.1 + 0.6 + (1.0)) + ((0.4 + 0.6) + 1.0 + 0.1 + (1.0)), r4("D" -> "V"), 1e-9)
    assertEquals(2, r4.size)

    val r5 = modelMinTagger.holeFillWeights(sentence, se | Set(("<S>", "D"), ("D", "D"), ("N", "D"), ("N", "<E>"), ("V", "<E>")))
    // covers:  <S> the man walks the dog <E>
    //           S                         E
    //               D   D         D 
    //                   N    N        N
    //                   V    V        V
    assertEquals(((1.0) + 0.1 + 0.4 + (1.0)) + ((0.4) + 1.0 + 0.9 + (1.0)), r5("D" -> "N"), 1e-9)
    assertEquals(((0.4) + 1.0 + 0.1 + (1.0)), r5("D" -> "V"), 1e-9)
    assertEquals(2, r5.size)

    val r6 = modelMinTagger.holeFillWeights(sentence, se | Set(("<S>", "D"), ("D", "N"), ("N", "V"), ("V", "D"), ("N", "<E>")))
    // covers:  <S> the man walks the dog <E>
    //           S                         E
    //               D   D         D 
    //                   N    N        N
    //                   V    V        V
    assertEquals(((1.0) + 1.0 + 0.1 + (0.4)), r6("D" -> "D"), 1e-9)
    assertEquals(((0.1) + 0.4 + 1.0 + (0.9)), r6("N" -> "D"), 1e-9)
    assertEquals(2, r6.size)

    val r7 = modelMinTagger.holeFillWeights(sentence, se | Set(("<S>", "D"), ("D", "N"), ("N", "V"), ("V", "D"), ("V", "N"), ("N", "<E>")))
    // covers:  <S> the man walks the dog <E>
    //           S                         E
    //               D   D         D 
    //                   N    N        N
    //                   V    V        V
    assertEquals(((1.0) + 1.0 + 0.1 + (0.4)), r7("D" -> "D"), 1e-9)
    assertEquals(((1.0) + 1.0 + 0.2 + (0.4)), r7("D" -> "V"), 1e-9)
    assertEquals(((0.1 + 0.2) + 0.4 + 1.0 + (0.9)), r7("N" -> "D"), 1e-9)
    assertEquals(3, r7.size)
  }

}
