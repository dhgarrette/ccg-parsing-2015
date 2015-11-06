package dhg.ccg.parse.scg

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.parse.pcfg._
import dhg.ccg.tag.learn._
import dhg.util._
import dhg.ccg.tagdict.TagDictionary

class TypesupScgTests {

  val S = cat"S".asInstanceOf[AtomCat]
  val NP = cat"NP".asInstanceOf[AtomCat]
  val N = cat"N".asInstanceOf[AtomCat]
  val PP = cat"PP".asInstanceOf[AtomCat]

  val s = cat"S".asInstanceOf[AtomCat]
  val np = cat"NP".asInstanceOf[AtomCat]
  val n = cat"N".asInstanceOf[AtomCat]
  val pp = cat"PP".asInstanceOf[AtomCat]

  val STA: Cat = cat"<S>"
  val END: Cat = cat"<E>"

  val A = cat"A".asInstanceOf[AtomCat]
  val B = cat"B".asInstanceOf[AtomCat]
  val C = cat"C".asInstanceOf[AtomCat]
  val D = cat"D".asInstanceOf[AtomCat]
  val E = cat"E".asInstanceOf[AtomCat]
  val F = cat"F".asInstanceOf[AtomCat]
  val X = cat"X".asInstanceOf[AtomCat]

  @Test
  def test_ReversingTrInit_on_TrTagDictEntriesPossibilities {
    val sentences = Vector(
      "the dog walks", //         {S} -> {D} -> { }  -> {V} -> {E}
      "the man walks the dog", // {S} -> {D} -> {NV} -> {V} -> {D} -> { } -> {E}
      "the man runs") //          {S} -> {D} -> {NV} -> { } -> {E}
      .map(_.lsplit(" "))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set("D"),
      "man" -> Set("N", "V"),
      "walks" -> Set("V")),
      "<S>", "<S>", "<E>", "<E>")

    /*    S   D   N   V   E
     * S      3              /3
     * D          1   1      /2
     * N             0.5     /0.5
     * V      1      0.5  1  /2.5
     * E     
     *       --- --- --- ---
     *        4   1   2   1
     */

    val rctxInit = new TrTagDictEntriesPossibilities[String](new AddLambdaTransitionDistributioner(0.2))

    val lctxInit = new ReversingTrInit(rctxInit)
    val lctxDist = lctxInit.fromRaw(sentences, tagdict)
    def tr(lctx: String, e: String) = lctxDist(lctx, e)

    assertEquals(0.0, tr("<S>", "<S>").toDouble, 1e-5)
    assertEquals((3 + 0.2) / (4 + 4 * 0.2), tr("<S>", "D").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (1 + 4 * 0.2), tr("<S>", "N").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (2 + 4 * 0.2), tr("<S>", "V").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (0 + 4 * 0.2), tr("<S>", "default").toDouble, 1e-5)
    assertEquals(0.0, tr("<S>", "<E>").toDouble, 1e-5)

    assertEquals(0.0, tr("D", "<S>").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tr("D", "D").toDouble, 1e-5)
    assertEquals((1 + 0.2) / (1 + 4 * 0.2), tr("D", "N").toDouble, 1e-5)
    assertEquals((1 + 0.2) / (2 + 4 * 0.2), tr("D", "V").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (0 + 4 * 0.2), tr("D", "default").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (1 + 3 * 0.2), tr("D", "<E>").toDouble, 1e-5)

    assertEquals(0.0, tr("N", "<S>").toDouble, 1e-5)
    assertEquals((0.0 + 0.2) / (4 + 4 * 0.2), tr("N", "D").toDouble, 1e-5)
    assertEquals((0.0 + 0.2) / (1 + 4 * 0.2), tr("N", "N").toDouble, 1e-5)
    assertEquals((0.5 + 0.2) / (2 + 4 * 0.2), tr("N", "V").toDouble, 1e-5)
    assertEquals((0.0 + 0.2) / (0 + 4 * 0.2), tr("N", "default").toDouble, 1e-5)
    assertEquals((0.0 + 0.2) / (1 + 3 * 0.2), tr("N", "<E>").toDouble, 1e-5)

    assertEquals(0.0, tr("V", "<S>").toDouble, 1e-5)
    assertEquals((1.0 + 0.2) / (4 + 4 * 0.2), tr("V", "D").toDouble, 1e-5)
    assertEquals((0.0 + 0.2) / (1 + 4 * 0.2), tr("V", "N").toDouble, 1e-5)
    assertEquals((0.5 + 0.2) / (2 + 4 * 0.2), tr("V", "V").toDouble, 1e-5)
    assertEquals((0.0 + 0.2) / (0 + 4 * 0.2), tr("V", "default").toDouble, 1e-5)
    assertEquals((1.0 + 0.2) / (1 + 3 * 0.2), tr("V", "<E>").toDouble, 1e-5)

    assertEquals(0.0, tr("default", "<S>").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tr("default", "D").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (1 + 4 * 0.2), tr("default", "N").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (2 + 4 * 0.2), tr("default", "V").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (0 + 4 * 0.2), tr("default", "default").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (1 + 3 * 0.2), tr("default", "<E>").toDouble, 1e-5)

    assertEquals(0.0, tr("<E>", "<S>").toDouble, 1e-5)
    assertEquals(0.0, tr("<E>", "D").toDouble, 1e-5)
    assertEquals(0.0, tr("<E>", "N").toDouble, 1e-5)
    assertEquals(0.0, tr("<E>", "V").toDouble, 1e-5)
    assertEquals(0.0, tr("<E>", "default").toDouble, 1e-5)
    assertEquals(0.0, tr("<E>", "<E>").toDouble, 1e-5)
  }

  @Test
  def integration_CcgCombinabilityTrInitializer_on_TrTagDictEntriesPossibilities {
    type Word = String
    type Tag = Cat

    val sentences = Vector(
      "the big dog walks", //     {S} -> {N/N} -> {N/N} -> {N} -> {S\N, (S\N)/N}                 -> {E}
      "the man walks the dog", // {S} -> {N/N} ->          {N} -> {S\N, (S\N)/N} -> {N/N} -> {N} -> {E}
      "the man runs") //          {S} -> {N/N} ->          {N} -> {S\N}                          -> {E}
      .map(_.lsplit(" "))
    val mockSentences = Vector[Vector[(Word, Set[Tag])]](
      Vector("the" -> Set(N / N), "big" -> Set(N / N), "dog" -> Set(N), "walks" -> Set(S \ N, (S \ N) / N)),
      Vector("the" -> Set(N / N), "man" -> Set(N), "walks" -> Set(S \ N, (S \ N) / N), "the" -> Set(N / N), "dog" -> Set(N)),
      Vector("the" -> Set(N / N), "man" -> Set(N), "walks" -> Set(S \ N)))

    val tagdict = new TagDictionary[Tag] {
      def allWords: Set[Word] = ???
      def startWord: Word = ???; def endWord: Word = ???
      def reversed: Map[Tag, Set[Word]] = ???
      def entries: Map[Word, Set[Tag]] = ???
      def knownWordsForTag: Map[Tag, Set[Word]] = ???
      def withWords(words: Set[Word]): TagDictionary[Tag] = this
      def withTags(tags: Set[Tag]): TagDictionary[Tag] = this
      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def apply(w: Word): Set[Tag] = ???
      def allTags: Set[Tag] = Set(
        N / N,
        N,
        S \ N)
      def startTag: Tag = STA
      def endTag: Tag = END
      def excludedTags: Set[Tag] = Set.empty
    }

    /*
     *               <S>     N/N     N    S\N   (S\N)/N   <E>
     * <S>                    3                                    /3
     * N/N                    1      4                             /5
     * N                                   2       1       1       /4
     * S\N                   0.5                          1.5      /2
     * (S\N)/N               0.5                          0.5      /1
     * <E>
     * 
     */

    val rctxTdeInit = new TrTagDictEntriesPossibilities[Cat](new AddLambdaTransitionDistributioner(0.2))
    val rctxTdeDist = rctxTdeInit.fromKnownSupertagSets(mockSentences, tagdict)
    def tdeTr(t: Cat, rctx: Cat) = rctxTdeDist(rctx, t)

    assertEquals(0.0, tdeTr(STA, STA).toDouble, 1e-5) //                          
    assertEquals((3 + 0.2) / (3 + 3 * 0.2), tdeTr(STA, N / N).toDouble, 1e-5) //        0.888888888888889
    assertEquals((0 + 0.2) / (3 + 3 * 0.2), tdeTr(STA, N).toDouble, 1e-5) //            0.05555555555555556
    assertEquals((0 + 0.2) / (3 + 3 * 0.2), tdeTr(STA, S \ N).toDouble, 1e-5) //        0.05555555555555556
    assertEquals(0.0, tdeTr(STA, END).toDouble, 1e-5)
    assertEquals((0 + 0.2) / (3 + 3 * 0.2), tdeTr(STA, (S \ N) / N).toDouble, 1e-5) //  0.05555555555555556

    assertEquals(0.0, tdeTr(N / N, STA).toDouble, 1e-5) //                          
    assertEquals((1 + 0.2) / (5 + 4 * 0.2), tdeTr(N / N, N / N).toDouble, 1e-5) //        0.20689655172413793
    assertEquals((4 + 0.2) / (5 + 4 * 0.2), tdeTr(N / N, N).toDouble, 1e-5) //            0.7241379310344828
    assertEquals((0 + 0.2) / (5 + 4 * 0.2), tdeTr(N / N, S \ N).toDouble, 1e-5) //        0.034482758620689655
    assertEquals((0 + 0.2) / (5 + 4 * 0.2), tdeTr(N / N, END).toDouble, 1e-5) //          0.034482758620689655
    assertEquals((0 + 0.2) / (5 + 4 * 0.2), tdeTr(N / N, (S \ N) / N).toDouble, 1e-5) //  0.034482758620689655

    assertEquals(0.0, tdeTr(N, STA).toDouble, 1e-5) //                          
    assertEquals((0 + 0.2) / (4 + 5 * 0.2), tdeTr(N, N / N).toDouble, 1e-5) //        0.04
    assertEquals((0 + 0.2) / (4 + 5 * 0.2), tdeTr(N, N).toDouble, 1e-5) //            0.04
    assertEquals((2 + 0.2) / (4 + 5 * 0.2), tdeTr(N, S \ N).toDouble, 1e-5) //        0.44
    assertEquals((1 + 0.2) / (4 + 5 * 0.2), tdeTr(N, END).toDouble, 1e-5) //          0.24
    assertEquals((1 + 0.2) / (4 + 5 * 0.2), tdeTr(N, (S \ N) / N).toDouble, 1e-5) //  0.24

    assertEquals(0.0, tdeTr(S \ N, STA).toDouble, 1e-5) //                          
    assertEquals((0.5 + 0.2) / (2 + 4 * 0.2), tdeTr(S \ N, N / N).toDouble, 1e-5) //        0.25
    assertEquals((0.0 + 0.2) / (2 + 4 * 0.2), tdeTr(S \ N, N).toDouble, 1e-5) //            0.07142857142857144
    assertEquals((0.0 + 0.2) / (2 + 4 * 0.2), tdeTr(S \ N, S \ N).toDouble, 1e-5) //        0.07142857142857144
    assertEquals((1.5 + 0.2) / (2 + 4 * 0.2), tdeTr(S \ N, END).toDouble, 1e-5) //          0.6071428571428572
    assertEquals((0.0 + 0.2) / (2 + 4 * 0.2), tdeTr(S \ N, (S \ N) / N).toDouble, 1e-5) //  0.07142857142857144

    assertEquals(0.0, tdeTr((S \ N) / N, STA).toDouble, 1e-5) //                          
    assertEquals((0.5 + 0.2) / (1 + 4 * 0.2), tdeTr((S \ N) / N, N / N).toDouble, 1e-5) //        0.38888888888888884
    assertEquals((0.0 + 0.2) / (1 + 4 * 0.2), tdeTr((S \ N) / N, N).toDouble, 1e-5) //            0.11111111111111112
    assertEquals((0.0 + 0.2) / (1 + 4 * 0.2), tdeTr((S \ N) / N, S \ N).toDouble, 1e-5) //        0.11111111111111112
    assertEquals((0.5 + 0.2) / (1 + 4 * 0.2), tdeTr((S \ N) / N, END).toDouble, 1e-5) //          0.38888888888888884
    assertEquals((0.0 + 0.2) / (1 + 4 * 0.2), tdeTr((S \ N) / N, (S \ N) / N).toDouble, 1e-5) //  0.11111111111111112

    //

    val mockCanCombine = new SimpleCatCanCombine(Vector(FA, BA, FC), STA, END)
    val combinableTransitionMass = 0.8

    val combRctxInit = new CcgCombinabilityTrInitializer(rctxTdeInit, mockCanCombine, combinableTransitionMass, totalSmoothing = LogDouble.zero)
    val combRctxDist = combRctxInit.fromKnownSupertagSets(mockSentences, tagdict)

    def tr(t: Cat, rctx: Cat) = combRctxDist(rctx, t)

    assertEquals(0.0, tr(STA, STA).toDouble, 1e-5) //                          
    assertEquals(0.7529411764705883, tr(STA, N / N).toDouble, 1e-5) // 0.88888888888888889                         / 0.94444444444444444 * 0.8 = 0.7529411764705883
    assertEquals(0.04705882352941177, tr(STA, N).toDouble, 1e-5) //    0.05555555555555556                         / 0.94444444444444444 * 0.8 = 0.04705882352941177
    assertEquals(0.2, tr(STA, S \ N).toDouble, 1e-5) //                                     0.05555555555555556    / 0.05555555555555556 * 0.2 = 0.2
    assertEquals(0.0, tr(STA, END).toDouble, 1e-5) //                  -------------------  -------------------                    
    //                                                                 0.94444444444444444  0.05555555555555556
    assertEquals(0.2, tr(STA, (S \ N) / N).toDouble, 1e-5) //                               0.05555555555555556    / 0.05555555555555556 * 0.2 = 0.2

    assertEquals(0.0, tr(N / N, STA).toDouble, 1e-5) //                          
    assertEquals(0.1777777777777778, tr(N / N, N / N).toDouble, 1e-5) // 0.20689655172413793                         / 0.9310344827586207  * 0.8 = 0.1777777777777778
    assertEquals(0.6222222222222222, tr(N / N, N).toDouble, 1e-5) //     0.7241379310344828                          / 0.9310344827586207  * 0.8 = 0.6222222222222222
    assertEquals(0.1, tr(N / N, S \ N).toDouble, 1e-5) //                                     0.034482758620689655   / 0.06896551724137931 * 0.2 = 0.1
    assertEquals(0.1, tr(N / N, END).toDouble, 1e-5) //                                       0.034482758620689655   / 0.06896551724137931 * 0.2 = 0.1
    //                                                                   -------------------  --------------------                   
    //                                                                   0.9310344827586207   0.06896551724137931
    assertEquals(0.1, tr(N / N, (S \ N) / N).toDouble, 1e-5) //                               0.034482758620689655    / 0.06896551724137931 * 0.2 = 0.1

    assertEquals(0.0, tr(N, STA).toDouble, 1e-5) //                          
    assertEquals(0.1, tr(N, N / N).toDouble, 1e-5) //                              0.04    / 0.08 * 0.2 = 0.1
    assertEquals(0.1, tr(N, N).toDouble, 1e-5) //                                  0.04    / 0.08 * 0.2 = 0.1
    assertEquals(0.5176470588235293, tr(N, S \ N).toDouble, 1e-5) //        0.44           / 0.68 * 0.8 = 0.5176470588235293
    assertEquals(0.2823529411764706, tr(N, END).toDouble, 1e-5) //          0.24           / 0.68 * 0.8 = 0.2823529411764706
    //                                                                      ----   ----
    //                                                                      0.68   0.08
    assertEquals(0.2823529411764706, tr(N, (S \ N) / N).toDouble, 1e-5) //  0.24           / 0.68 * 0.8 = 0.2823529411764706

    assertEquals(0.0, tr(S \ N, STA).toDouble, 1e-5) //                          
    assertEquals(0.12727272727272726, tr(S \ N, N / N).toDouble, 1e-5) //                             0.25                   / 0.3928571428571429 * 0.2 = 0.12727272727272726
    assertEquals(0.03636363636363637, tr(S \ N, N).toDouble, 1e-5) //                                 0.07142857142857144    / 0.3928571428571429 * 0.2 = 0.03636363636363637
    assertEquals(0.03636363636363637, tr(S \ N, S \ N).toDouble, 1e-5) //                             0.07142857142857144    / 0.3928571428571429 * 0.2 = 0.03636363636363637
    assertEquals(0.8, tr(S \ N, END).toDouble, 1e-5) //                          0.6071428571428572                          / 0.6071428571428572 * 0.8 = 0.8
    //                                                                           -------------------  -------------------                    
    //                                                                           0.6071428571428572   0.05555555555555556
    assertEquals(0.03636363636363637, tr(S \ N, (S \ N) / N).toDouble, 1e-5) //  0.07142857142857144                         / 0.3928571428571429 * 0.2 = 0.03636363636363637

    assertEquals(0.0, tr((S \ N) / N, STA).toDouble, 1e-5) //                          
    assertEquals(0.6222222222222222, tr((S \ N) / N, N / N).toDouble, 1e-5) //        0.38888888888888884                          / 0.5 * 0.8 = 0.6222222222222222
    assertEquals(0.1777777777777778, tr((S \ N) / N, N).toDouble, 1e-5) //            0.11111111111111112                          / 0.5 * 0.8 = 0.1777777777777778
    assertEquals(0.04444444444444445, tr((S \ N) / N, S \ N).toDouble, 1e-5) //                             0.11111111111111112    / 0.5 * 0.2 = 0.04444444444444445
    assertEquals(0.15555555555555556, tr((S \ N) / N, END).toDouble, 1e-5) //                               0.38888888888888884    / 0.5 * 0.2 = 0.15555555555555556
    //                                                                                -------------------   -------------------                    
    //                                                                                0.5                   0.5
    assertEquals(0.04444444444444445, tr((S \ N) / N, (S \ N) / N).toDouble, 1e-5) //                       0.11111111111111112    / 0.5 * 0.2 = 0.04444444444444445
  }

  @Test
  def integration_ReversingTrInit_on_CcgCombinabilityTrInitializer_on_TrTagDictEntriesPossibilities {
    type Word = String
    type Tag = Cat

    val sentences = Vector(
      "the big dog walks", //     {S} -> {N/N} -> {N/N} -> {N} -> {S\N, (S\N)/N}                 -> {E}
      "the man walks the dog", // {S} -> {N/N} ->          {N} -> {S\N, (S\N)/N} -> {N/N} -> {N} -> {E}
      "the man runs") //          {S} -> {N/N} ->          {N} -> {S\N}                          -> {E}
      .map(_.lsplit(" "))
    val mockSentences = Vector[Vector[(Word, Set[Tag])]](
      Vector("the" -> Set(N / N), "big" -> Set(N / N), "dog" -> Set(N), "walks" -> Set(S \ N, (S \ N) / N)),
      Vector("the" -> Set(N / N), "man" -> Set(N), "walks" -> Set(S \ N, (S \ N) / N), "the" -> Set(N / N), "dog" -> Set(N)),
      Vector("the" -> Set(N / N), "man" -> Set(N), "walks" -> Set(S \ N)))

    val tagdict = new TagDictionary[Tag] {
      def allWords: Set[Word] = Set.empty
      def startWord: Word = STA.toString; def endWord: Word = END.toString
      def reversed: Map[Tag, Set[Word]] = ???
      def entries: Map[Word, Set[Tag]] = Map.empty
      def knownWordsForTag: Map[Tag, Set[Word]] = ???
      def withWords(words: Set[Word]): TagDictionary[Tag] = this
      def withTags(tags: Set[Tag]): TagDictionary[Tag] = this
      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def apply(w: Word): Set[Tag] = ???
      def allTags: Set[Tag] = Set(
        N / N,
        N,
        S \ N)
      def startTag: Tag = STA
      def endTag: Tag = END
      def excludedTags: Set[Tag] = Set.empty
    }

    /*
     *               <S>     N/N     N    S\N   (S\N)/N   <E>
     * <S>                    3                                    /3
     * N/N                    1      4                             /5
     * N                                   2       1       1       /4
     * S\N                   0.5                          1.5      /2
     * (S\N)/N               0.5                          0.5      /1
     * <E>
     *                       ---    ---   ---     ---     ---
     *                        5      4     2       1       3
     * 
     */

    val rctxTdeInit = new TrTagDictEntriesPossibilities[Cat](new AddLambdaTransitionDistributioner(0.2))
    val lctxTdeInit = new ReversingTrInit(rctxTdeInit)
    val lctxTdeDist = lctxTdeInit.fromKnownSupertagSets(mockSentences, tagdict)
    def tdeTr(lctx: Cat, t: Cat) = lctxTdeDist(lctx, t)

    assertEquals(0.0, tdeTr(STA, STA).toDouble, 1e-5) //                          
    assertEquals(0.0, tdeTr(N / N, STA).toDouble, 1e-5) //                          
    assertEquals(0.0, tdeTr(N, STA).toDouble, 1e-5) //                          
    assertEquals(0.0, tdeTr(S \ N, STA).toDouble, 1e-5) //                          
    assertEquals(0.0, tdeTr((S \ N) / N, STA).toDouble, 1e-5) //        

    assertEquals((3.0 + 0.2) / (5 + 5 * 0.2), tdeTr(STA, N / N).toDouble, 1e-5) //            0.5333333333333333
    assertEquals((1.0 + 0.2) / (5 + 5 * 0.2), tdeTr(N / N, N / N).toDouble, 1e-5) //          0.2
    assertEquals((0.0 + 0.2) / (5 + 5 * 0.2), tdeTr(N, N / N).toDouble, 1e-5) //              0.03333333333333333
    assertEquals((0.5 + 0.2) / (5 + 5 * 0.2), tdeTr(S \ N, N / N).toDouble, 1e-5) //          0.11666666666666665
    assertEquals((0.5 + 0.2) / (5 + 5 * 0.2), tdeTr((S \ N) / N, N / N).toDouble, 1e-5) //    0.11666666666666665
    assertEquals((0.0 + 0.2) / (5 + 5 * 0.2), tdeTr(X, N / N).toDouble, 1e-5) //              0.03333333333333333

    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tdeTr(STA, N).toDouble, 1e-5) //              0.04166666666666667
    assertEquals((4 + 0.2) / (4 + 4 * 0.2), tdeTr(N / N, N).toDouble, 1e-5) //            0.8750000000000001
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tdeTr(N, N).toDouble, 1e-5) //                0.04166666666666667
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tdeTr(S \ N, N).toDouble, 1e-5) //            0.04166666666666667
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tdeTr((S \ N) / N, N).toDouble, 1e-5) //      0.04166666666666667

    assertEquals((0 + 0.2) / (2 + 4 * 0.2), tdeTr(STA, S \ N).toDouble, 1e-5) //            0.07142857142857144
    assertEquals((0 + 0.2) / (2 + 4 * 0.2), tdeTr(N / N, S \ N).toDouble, 1e-5) //          0.07142857142857144
    assertEquals((2 + 0.2) / (2 + 4 * 0.2), tdeTr(N, S \ N).toDouble, 1e-5) //              0.7857142857142858
    assertEquals((0 + 0.2) / (2 + 4 * 0.2), tdeTr(S \ N, S \ N).toDouble, 1e-5) //          0.07142857142857144
    assertEquals((0 + 0.2) / (2 + 4 * 0.2), tdeTr((S \ N) / N, S \ N).toDouble, 1e-5) //    0.07142857142857144

    assertEquals(0.0, tdeTr(STA, END).toDouble, 1e-5)
    assertEquals((0.0 + 0.2) / (3 + 4 * 0.2), tdeTr(N / N, END).toDouble, 1e-5) //            0.052631578947368425
    assertEquals((1.0 + 0.2) / (3 + 4 * 0.2), tdeTr(N, END).toDouble, 1e-5) //                0.3157894736842105
    assertEquals((1.5 + 0.2) / (3 + 4 * 0.2), tdeTr(S \ N, END).toDouble, 1e-5) //            0.4473684210526316
    assertEquals((0.5 + 0.2) / (3 + 4 * 0.2), tdeTr((S \ N) / N, END).toDouble, 1e-5) //      0.18421052631578946

    assertEquals((0.0 + 0.2) / (1 + 4 * 0.2), tdeTr(STA, (S \ N) / N).toDouble, 1e-5) //             0.11111111111111112
    assertEquals((0.0 + 0.2) / (1 + 4 * 0.2), tdeTr(N / N, (S \ N) / N).toDouble, 1e-5) //           0.11111111111111112
    assertEquals((1.0 + 0.2) / (1 + 4 * 0.2), tdeTr(N, (S \ N) / N).toDouble, 1e-5) //               0.6666666666666666
    assertEquals((0.0 + 0.2) / (1 + 4 * 0.2), tdeTr(S \ N, (S \ N) / N).toDouble, 1e-5) //           0.11111111111111112
    assertEquals((0.0 + 0.2) / (1 + 4 * 0.2), tdeTr((S \ N) / N, (S \ N) / N).toDouble, 1e-5) //     0.11111111111111112

    //

    val mockBackwardCanCombine = new BackwardCatCanCombine(new SimpleCatCanCombine(Vector(FA, BA, FC), STA, END))
    val combinableTransitionMass = 0.8

    val combLctxInit = new ReversingTrInit(new CcgCombinabilityTrInitializer(rctxTdeInit, mockBackwardCanCombine, combinableTransitionMass, totalSmoothing = LogDouble.zero))
    val combLctxDist = combLctxInit.fromKnownSupertagSets(mockSentences, tagdict)

    def tr(lctx: Cat, t: Cat) = combLctxDist(lctx, t)

    assertEquals(0.5818181818181819, tr(STA, N / N).toDouble, 1e-5) //            0.5333333333333333                         / 0.7333333333333333 * 0.8 = 0.5818181818181819
    assertEquals(0.21818181818181823, tr(N / N, N / N).toDouble, 1e-5) //         0.2                                        / 0.7333333333333333 * 0.8 = 0.21818181818181823
    assertEquals(0.04444444444444445, tr(N, N / N).toDouble, 1e-5) //                                  0.03333333333333333   / 0.15               * 0.2 = 0.04444444444444445
    assertEquals(0.15555555555555556, tr(S \ N, N / N).toDouble, 1e-5) //                              0.11666666666666665   / 0.15               * 0.2 = 0.15555555555555556
    //                                                                            ------------------   -------------------
    //                                                                            0.7333333333333333   0.15
    assertEquals(0.1272727272727273, tr((S \ N) / N, N / N).toDouble, 1e-5) //    0.11666666666666665                        / 0.7333333333333333 * 0.8 = 0.1272727272727273
    assertEquals(0.04444444444444445, tr(X, N / N).toDouble, 1e-5) //                                  0.03333333333333333   / 0.15               * 0.2 = 0.04444444444444445

    assertEquals(0.03636363636363637, tr(STA, N).toDouble, 1e-5) //             0.04166666666666667                         / 0.9166666666666667  * 0.8 = 0.03636363636363637
    assertEquals(0.7636363636363637, tr(N / N, N).toDouble, 1e-5) //            0.8750000000000001                          / 0.9166666666666667  * 0.8 = 0.7636363636363637
    assertEquals(0.1, tr(N, N).toDouble, 1e-5) //                                                      0.04166666666666667  / 0.08333333333333334 * 0.2 = 0.1
    assertEquals(0.1, tr(S \ N, N).toDouble, 1e-5) //                                                  0.04166666666666667  / 0.08333333333333334 * 0.2 = 0.1
    //                                                                          ------------------     -------------------
    //                                                                          0.9166666666666667     0.08333333333333334
    assertEquals(0.03636363636363637, tr((S \ N) / N, N).toDouble, 1e-5) //     0.04166666666666667                         / 0.9166666666666667  * 0.8 = 0.03636363636363637
    assertEquals(0.1, tr(X, N).toDouble, 1e-5) //                                                      0.04166666666666667  / 0.08333333333333334 * 0.2 = 0.1

    assertEquals(0.06666666666666668, tr(STA, S \ N).toDouble, 1e-5) //                                  0.07142857142857144  / 0.2142857142857143 * 0.2 
    assertEquals(0.06666666666666668, tr(N / N, S \ N).toDouble, 1e-5) //                                0.07142857142857144  / 0.2142857142857143 * 0.2
    assertEquals(0.8, tr(N, S \ N).toDouble, 1e-5) //                              0.7857142857142858                         / 0.7857142857142858 * 0.8
    assertEquals(0.06666666666666668, tr(S \ N, S \ N).toDouble, 1e-5) //                                0.07142857142857144  / 0.2142857142857143 * 0.2
    //                                                                             ------------------    -------------------
    //                                                                             0.7857142857142858    0.2142857142857143
    assertEquals(0.06666666666666668, tr((S \ N) / N, S \ N).toDouble, 1e-5) //                          0.07142857142857144  / 0.2142857142857143 * 0.2

    assertEquals(0.0, tr(STA, END).toDouble, 1e-5)
    assertEquals(0.2, tr(N / N, END).toDouble, 1e-5) //                                                 0.052631578947368425  / 0.052631578947368425 * 0.2
    assertEquals(0.3310344827586207, tr(N, END).toDouble, 1e-5) //                0.3157894736842105                          / 0.763157894736842 * 0.8
    assertEquals(0.46896551724137936, tr(S \ N, END).toDouble, 1e-5) //           0.4473684210526316                          / 0.763157894736842 * 0.8
    //                                                                            ------------------    --------------------
    //                                                                            0.763157894736842     0.052631578947368425
    assertEquals(0.7, tr((S \ N) / N, END).toDouble, 1e-5) //                                           0.18421052631578946   / 0.052631578947368425 * 0.2

    assertEquals(0.06666666666666667, tr(STA, (S \ N) / N).toDouble, 1e-5) //                                  0.11111111111111112  / 0.33333333333333337 * 0.2
    assertEquals(0.06666666666666667, tr(N / N, (S \ N) / N).toDouble, 1e-5) //                                0.11111111111111112  / 0.33333333333333337 * 0.2
    assertEquals(0.8, tr(N, (S \ N) / N).toDouble, 1e-5) //                              0.6666666666666666                         / 0.6666666666666666  * 0.8
    assertEquals(0.06666666666666667, tr(S \ N, (S \ N) / N).toDouble, 1e-5) //                                0.11111111111111112  / 0.33333333333333337 * 0.2
    //                                                                                   -------------------   ------------------- 
    //                                                                                   0.8888888888888888    0.11111111111111112
    assertEquals(0.06666666666666667, tr((S \ N) / N, (S \ N) / N).toDouble, 1e-5) //                          0.11111111111111112  / 0.33333333333333337 * 0.2
  }

  def assertEqualsLog(a: LogDouble, b: LogDouble, d: Double) {
    assertEquals(a.toDouble, b.toDouble, d)
  }
}
