package dhg.ccg.parse.gfl

import org.junit.Test
import dhg.util._
import org.junit.Assert._
import dhg.ccg.cat._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.dep.DepTree
import dhg.util.viz.TreeViz
import dhg.util._
import dhg.gfl.Fudg

class GoldAnnoExtractorTests {

  @Test
  def test_addDeps {

    /*
     *     c                                                <ROOT>
     *   /   \                                             /   |  \
     *  b      d      =>   ((g a) b) c (d e f)   =>   FE_0_2  c_3  FE_4_6
     *  |    /   \                                    /    \      /  |  \
     *  a   e     f                               FE_0_1    b    d   e   f
     *  |                                          /  \
     *  g                                         g    a
     */
    val dt =
      DepTree("c", 3, cat"X", Vector(
        DepTree("b", 2, cat"X", Vector(
          DepTree("a", 1, cat"X", Vector(
            DepTree("g", 0, cat"X", Vector.empty))))),
        DepTree("d", 4, cat"X", Vector(
          DepTree("e", 5, cat"X", Vector.empty),
          DepTree("f", 6, cat"X", Vector.empty)))))
    assertEquals(Vector(
      (3, 2),
      (2, 1),
      (1, 0),
      (3, 4),
      (4, 5),
      (4, 6)),
      new DepAddingGoldAnnoExtractor(1.0, null)
        .allDeps(dt))
  }

  @Test
  def test_addDepsToFudgSentence {

    /*
     *     c                                                <ROOT>
     *   /   \                                             /   |  \
     *  b      d      =>   ((g a) b) c (d e f)   =>   FE_0_2  c_3  FE_4_6
     *  |    /   \                                    /    \      /  |  \
     *  a   e     f                               FE_0_1    b    d   e   f
     *  |                                          /  \
     *  g                                         g    a
     */
    TreeViz.drawTree(Fudg.fromGfl(
      "g a b c d e f",
      """
      g > a > b > c < d < e  d < f
      """).getOrElseThrow().fudgTree)
    val dt =
      DepTree("c", 3, cat"X", Vector(
        DepTree("b", 2, cat"X", Vector(
          DepTree("a", 1, cat"X", Vector(
            DepTree("g", 0, cat"X", Vector.empty))))),
        DepTree("d", 4, cat"X", Vector(
          DepTree("e", 5, cat"X", Vector.empty),
          DepTree("f", 6, cat"X", Vector.empty)))))
    val fs =
      new DepAddingGoldAnnoExtractor(1.0, null)
        .addDepsToFudgSentence(dt,
          new AugDelegateGoldAnnoExtractor(new BracketAnnotationTreeAugmenter(0.0)).allBracketsInFudgSentence(dt))
    fs.edges foreach println
    TreeViz.drawTree(fs.fudgTree)
  }

  @Test
  def test_addDepsToFudgSentence_half {

    /*
     *     c                                                <ROOT>
     *   /   \                                             /   |  \
     *  b      d      =>   ((g a) b) c (d e f)   =>   FE_0_2  c_3  FE_4_6
     *  |    /   \                                    /    \      /  |  \
     *  a   e     f                               FE_0_1    b    d   e   f
     *  |                                          /  \
     *  g                                         g    a
     */
    val dt =
      DepTree("c", 3, cat"X", Vector(
        DepTree("b", 2, cat"X", Vector(
          DepTree("a", 1, cat"X", Vector(
            DepTree("g", 0, cat"X", Vector.empty))))),
        DepTree("d", 4, cat"X", Vector(
          DepTree("e", 5, cat"X", Vector.empty),
          DepTree("f", 6, cat"X", Vector.empty)))))
    val fs =
      new DepAddingGoldAnnoExtractor(0.5, null)
        .addDepsToFudgSentence(dt,
          new AugDelegateGoldAnnoExtractor(new BracketAnnotationTreeAugmenter(0.0)).allBracketsInFudgSentence(dt))
    fs.edges foreach println
    TreeViz.drawTree(fs.fudgTree)
  }

  @Test
  def test_addDepsToFudgSentence_brackets {

    /*
     *     c                                                <ROOT>
     *   /   \                                             /   |  \
     *  b      d      =>   ((g a) b) c (d e f)   =>   FE_0_2  c_3  FE_4_6
     *  |    /   \                                    /    \      /  |  \
     *  a   e     f                               FE_0_1    b    d   e   f
     *  |                                          /  \
     *  g                                         g    a
     */
    TreeViz.drawTree(Fudg.fromGfl(
      "g a b c d e f",
      """
      ((g a) b) c (d e f)
      g > a > b > c < d < e  d < f
      """).getOrElseThrow().fudgTree)
    val dt =
      DepTree("c", 3, cat"X", Vector(
        DepTree("b", 2, cat"X", Vector(
          DepTree("a", 1, cat"X", Vector(
            DepTree("g", 0, cat"X", Vector.empty))))),
        DepTree("d", 4, cat"X", Vector(
          DepTree("e", 5, cat"X", Vector.empty),
          DepTree("f", 6, cat"X", Vector.empty)))))
    val fs =
      new DepAddingGoldAnnoExtractor(1.0,
        new AugDelegateGoldAnnoExtractor(new BracketAnnotationTreeAugmenter(1.0)))
        .addDepsToFudgSentence(dt,
          new AugDelegateGoldAnnoExtractor(new BracketAnnotationTreeAugmenter(1.0)).allBracketsInFudgSentence(dt))
    fs.edges foreach println
    TreeViz.drawTree(fs.fudgTree)
  }

  @Test
  def test_addDepsToFudgSentence_brackets_halfDeps {

    /*
     *     c                                                <ROOT>
     *   /   \                                             /   |  \
     *  b      d      =>   ((g a) b) c (d e f)   =>   FE_0_2  c_3  FE_4_6
     *  |    /   \                                    /    \      /  |  \
     *  a   e     f                               FE_0_1    b    d   e   f
     *  |                                          /  \
     *  g                                         g    a
     */
    val dt =
      DepTree("c", 3, cat"X", Vector(
        DepTree("b", 2, cat"X", Vector(
          DepTree("a", 1, cat"X", Vector(
            DepTree("g", 0, cat"X", Vector.empty))))),
        DepTree("d", 4, cat"X", Vector(
          DepTree("e", 5, cat"X", Vector.empty),
          DepTree("f", 6, cat"X", Vector.empty)))))
    val fs =
      new DepAddingGoldAnnoExtractor(0.5,
        new AugDelegateGoldAnnoExtractor(new BracketAnnotationTreeAugmenter(1.0)))
        .addDepsToFudgSentence(dt,
          new AugDelegateGoldAnnoExtractor(new BracketAnnotationTreeAugmenter(1.0)).allBracketsInFudgSentence(dt))
    fs.edges foreach println
    TreeViz.drawTree(fs.fudgTree)
  }

  @Test
  def test_allBracketsInFudgSentence {

    /*
     *     c                                                <ROOT>
     *   /   \                                             /   |  \
     *  b      d      =>   ((g a) b) c (d e f)   =>   FE_0_2  c_3  FE_4_6
     *  |    /   \                                    /    \      /  |  \
     *  a   e     f                               FE_0_1    b    d   e   f
     *  |                                          /  \
     *  g                                         g    a
     */
    TreeViz.drawTree(Fudg.fromGfl(
      "g a b c d e f",
      """
      ((g a) b) c (d e f)
      """).getOrElseThrow().fudgTree)
    TreeViz.drawTree(
      new AugDelegateGoldAnnoExtractor(new BracketAnnotationTreeAugmenter())
        .allBracketsInFudgSentence(
          DepTree("c", 3, cat"X", Vector(
            DepTree("b", 2, cat"X", Vector(
              DepTree("a", 1, cat"X", Vector(
                DepTree("g", 0, cat"X", Vector.empty))))),
            DepTree("d", 4, cat"X", Vector(
              DepTree("e", 5, cat"X", Vector.empty),
              DepTree("f", 6, cat"X", Vector.empty)))))).fudgTree)
  }

  @Test
  def test_allBracketsInFudgSentence_half {

    /*
     *     c                                                FE_0_6
     *   /   \                                             /   |  \
     *  b      d      =>   ((g a) b) c (d e f)   =>   FE_0_2  c_3  FE_4_6
     *  |    /   \                                    /    \      /  |  \
     *  a   e     f                               FE_0_1    b    d   e   f
     *  |                                          /  \
     *  g                                         g    a
     */
    TreeViz.drawTree(
      new AugDelegateGoldAnnoExtractor(new BracketAnnotationTreeAugmenter(bracketProportion = 0.5))
        .allBracketsInFudgSentence(
          DepTree("c", 3, cat"X", Vector(
            DepTree("b", 2, cat"X", Vector(
              DepTree("a", 1, cat"X", Vector(
                DepTree("g", 0, cat"X", Vector.empty))))),
            DepTree("d", 4, cat"X", Vector(
              DepTree("e", 5, cat"X", Vector.empty),
              DepTree("f", 6, cat"X", Vector.empty)))))).fudgTree)
  }

  @Test
  def test_allBracketsInFudgSentence_npOnly {

    /*
     *     c                                       
     *   /   \                                     
     *  b      d      =>   NP[(g a) b] c (d e f)   
     *  |    /   \                                 
     *  a   e     f                                
     *  |                                          
     *  g                                          
     */
    TreeViz.drawTree(
      new AugDelegateGoldAnnoExtractor(new BracketAnnotationTreeAugmenter(bracketCats = Set(cat"NP")))
        .allBracketsInFudgSentence(
          DepTree("c", 3, cat"S", Vector(
            DepTree("b", 2, cat"NP", Vector(
              DepTree("a", 1, cat"N", Vector(
                DepTree("g", 0, cat"(N/N)", Vector.empty))))),
            DepTree("d", 4, cat"(S\NP)", Vector(
              DepTree("e", 5, cat"((S\NP)/NP)", Vector.empty),
              DepTree("f", 6, cat"NP", Vector.empty)))))).fudgTree)
    // =>   NP[(g a) b] c NP[d e f]
    TreeViz.drawTree(
      new AugDelegateGoldAnnoExtractor(new BracketAnnotationTreeAugmenter(bracketCats = Set(cat"NP")))
        .allBracketsInFudgSentence(
          DepTree("c", 3, cat"(S\NP)", Vector(
            DepTree("b", 2, cat"NP", Vector(
              DepTree("a", 1, cat"N", Vector(
                DepTree("g", 0, cat"(N/N)", Vector.empty))))),
            DepTree("d", 4, cat"NP", Vector(
              DepTree("e", 5, cat"(NP/N)", Vector.empty),
              DepTree("f", 6, cat"N", Vector.empty)))))).fudgTree)
  }

  @Test
  def test_allBracketsInFudgSentence_npOnly_half {

    /*
     *     c                                       
     *   /   \                                     
     *  b      d      =>   NP[(g a) b] c (d e f)   
     *  |    /   \                                 
     *  a   e     f                                
     *  |                                          
     *  g                                          
     */
    TreeViz.drawTree(
      new AugDelegateGoldAnnoExtractor(new BracketAnnotationTreeAugmenter(
        bracketCats = Set(cat"NP"),
        bracketProportion = 0.5))
        .allBracketsInFudgSentence(
          DepTree("c", 3, cat"S", Vector(
            DepTree("b", 2, cat"NP", Vector(
              DepTree("a", 1, cat"N", Vector(
                DepTree("g", 0, cat"(N/N)", Vector.empty))))),
            DepTree("d", 4, cat"(S\NP)", Vector(
              DepTree("e", 5, cat"((S\NP)/NP)", Vector.empty),
              DepTree("f", 6, cat"NP", Vector.empty)))))).fudgTree)
    // =>   NP[(g a) b] c NP[d e f]
    TreeViz.drawTree(
      new AugDelegateGoldAnnoExtractor(new BracketAnnotationTreeAugmenter(
        bracketCats = Set(cat"NP"),
        bracketProportion = 0.5))
        .allBracketsInFudgSentence(
          DepTree("c", 3, cat"(S\NP)", Vector(
            DepTree("b", 2, cat"NP", Vector(
              DepTree("a", 1, cat"N", Vector(
                DepTree("g", 0, cat"(N/N)", Vector.empty))))),
            DepTree("d", 4, cat"NP", Vector(
              DepTree("e", 5, cat"(NP/N)", Vector.empty),
              DepTree("f", 6, cat"N", Vector.empty)))))).fudgTree)
  }

  @Test
  def test_trainBaseCatsOnly {

    /*
     *     c                                                <ROOT>
     *   /   \                                             /   |  \
     *  b      d      =>   ((g a) b) c (d e f)   =>   FE_0_2  c_3  FE_4_6
     *  |    /   \                                    /    \      /  |  \
     *  a   e     f                               FE_0_1    b    d   e   f
     *  |                                          /  \
     *  g                                         g    a
     */
    //    TreeViz.drawTree(
    //      new AugDelegateGoldAnnoExtractor(new BracketAnnotationTreeAugmenter(bracketCats = Set(cat"NP"))
    //        .allBracketsInFudgSentence(
    //          DepTree("c", 3, cat"S", Vector(
    //            DepTree("b", 2, cat"NP", Vector(
    //              DepTree("a", 1, cat"N", Vector(
    //                DepTree("g", 0, cat"(N/N)", Vector.empty))))),
    //            DepTree("d", 4, cat"(S\NP)", Vector(
    //              DepTree("e", 5, cat"((S\NP)/NP)", Vector.empty),
    //              DepTree("f", 6, cat"NP", Vector.empty)))))).fudgTree)
    // =>   NP[(g a) b] c NP[d e f]
    TreeViz.drawTree(
      new AugDelegateGoldAnnoExtractor(new BracketAnnotationTreeAugmenter(baseCats = false, highCats = true)) //, bracketCats = Set(cat"NP"), bracketProportion=0.5)
        .allBracketsInFudgSentence(
          DepTree("c", 3, cat"(S\NP)", Vector(
            DepTree("b", 2, cat"NP", Vector(
              DepTree("a", 1, cat"N", Vector(
                DepTree("g", 0, cat"(N/N)", Vector.empty))))),
            DepTree("d", 4, cat"NP", Vector(
              DepTree("e", 5, cat"(NP/N)", Vector.empty),
              DepTree("f", 6, cat"N", Vector.empty)))))).fudgTree)
  }

  @Test
  def test_allBrackets {
    assertEquals(Set(
      (1, 3), (4, 7)),
      GoldAnnoExtractor.allBrackets(
        DepTree("c", 3, cat"X", Vector(
          DepTree("b", 2, cat"X", Vector(
            DepTree("a", 1, cat"X", Vector.empty))),
          DepTree("d", 4, cat"X", Vector(
            DepTree("e", 5, cat"X", Vector.empty),
            DepTree("f", 6, cat"X", Vector.empty)))))))
    assertEquals(Set(
      (1, 3)),
      GoldAnnoExtractor.allBrackets(
        DepTree("c", 3, cat"X", Vector(
          DepTree("b", 2, cat"X", Vector(
            DepTree("a", 1, cat"X", Vector.empty))),
          DepTree("d", 4, cat"X", Vector(
            DepTree("e", 5, cat"X", Vector.empty),
            DepTree("g", 7, cat"X", Vector.empty)))))))
  }

  @Test
  def test_indicesCovered {
    assertEquals(Set(2, 3, 4, 5, 6), GoldAnnoExtractor.indicesCovered(
      DepTree("c", 3, cat"X", Vector(
        DepTree("b", 2, cat"X", Vector.empty),
        DepTree("d", 4, cat"X", Vector(
          DepTree("e", 5, cat"X", Vector.empty),
          DepTree("f", 6, cat"X", Vector.empty)))))))
    assertEquals(Set(2, 3, 4, 5, 7), GoldAnnoExtractor.indicesCovered(
      DepTree("c", 3, cat"X", Vector(
        DepTree("b", 2, cat"X", Vector.empty),
        DepTree("d", 4, cat"X", Vector(
          DepTree("e", 5, cat"X", Vector.empty),
          DepTree("g", 7, cat"X", Vector.empty)))))))
  }

  @Test
  def test_indicesAreSpan {
    assertTrue(GoldAnnoExtractor.indicesAreSpan(Set(2, 3, 4)))
    assertFalse(GoldAnnoExtractor.indicesAreSpan(Set(2, 3, 5)))
  }

}
