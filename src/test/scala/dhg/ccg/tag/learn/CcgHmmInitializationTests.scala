package dhg.ccg.tag.learn

import org.junit.Test
import dhg.util._
import org.junit.Assert._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.prob._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tagdict.SimpleTagDictionary

class CcgHmmInitializationTests {

  val STA = cat"<S>"
  val S = cat"S".asInstanceOf[AtomCat]
  val NP = cat"NP".asInstanceOf[AtomCat]
  val N = cat"N".asInstanceOf[AtomCat]
  val X = cat"X".asInstanceOf[AtomCat]
  val END = cat"<E>"

  val A = cat"A".asInstanceOf[AtomCat]
  val B = cat"B".asInstanceOf[AtomCat]
  val CAN = cat"CAN".asInstanceOf[AtomCat]
  val CANT = cat"CANT".asInstanceOf[AtomCat]

  @Test
  def test_CcgCombinabilityTrInitializer_with_CcgCombinabilityTransitionConditionalLogProbabilityDistribution {
    type Word = String
    type Tag = Cat
    val mockTagdict2 = new TagDictionary[Tag] {
      def allWords: Set[Word] = ???
      def startWord: Word = ???; def endWord: Word = ???
      def excludedTags: Set[Tag] = ???
      def reversed: Map[Tag, Set[Word]] = ???
      def entries: Map[Word, Set[Tag]] = ???
      def knownWordsForTag: Map[Tag, Set[Word]] = ???
      def withWords(words: Set[Word]): TagDictionary[Tag] = ???
      def withTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def apply(w: Word): Set[Tag] = ???

      def allTags: Set[Tag] = Set(
        N,
        S \ N,
        (S \ N) \ (S \ N))
      def startTag: Tag = STA
      def endTag: Tag = END
    }
    val mockTagdict1 = new TagDictionary[Tag] {
      def allWords: Set[Word] = ???
      def startWord: Word = ???; def endWord: Word = ???
      def excludedTags: Set[Tag] = ???
      def reversed: Map[Tag, Set[Word]] = ???
      def entries: Map[Word, Set[Tag]] = ???
      def knownWordsForTag: Map[Tag, Set[Word]] = ???
      def withWords(words: Set[Word]): TagDictionary[Tag] = ???
      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def apply(w: Word): Set[Tag] = ???
      def allTags: Set[Tag] = ???
      def endTag: Tag = ???
      def startTag: Tag = ???

      def withTags(tags: Set[Tag]): TagDictionary[Tag] = {
        mockTagdict2
      }
    }

    val mockTrCPD = new ConditionalLogProbabilityDistribution[Cat, Cat] {
      override def apply(x: Cat, given: Cat): LogDouble = {
        (given, x) match {
          case (STA, STA) /*         */ => sys.error("<S> -> <S>") //   COMB  NC
          case (STA, N) /*           */ => LogDouble(0.70) //           X
          case (STA, S \ N) /*       */ => LogDouble(0.13) //                 X
          case (STA, (S \ N) \ (S \ N)) => LogDouble(0.12) //                 X
          case (STA, END) /*         */ => LogDouble.zero //
          //                                                          0.70  0.25
          case (STA, NP) /*           */ => LogDouble(0.16) //          X
          case (STA, S \ NP) /*       */ => LogDouble(0.29) //                X

          case (N, /**/ STA) /*         */ => sys.error("N -> <S>") //   COMB  NC
          case (N, /**/ N) /*           */ => LogDouble(0.20) //               X
          case (N, /**/ S \ N) /*       */ => LogDouble(0.50) //         X
          case (N, /**/ (S \ N) \ (S \ N)) => LogDouble(0.05) //               X
          case (N, /**/ END) /*         */ => LogDouble(0.35) //         X
          //                                                             0.85  0.25
          case (N, S \ NP) /*           */ => LogDouble(0.33) //         X
          case (N, NP) /*               */ => LogDouble(0.21) //               X

          case (S \ N, /**/ STA) /*         */ => sys.error("S\\N -> <S>") //   COMB  NC
          case (S \ N, /**/ N) /*           */ => LogDouble(0.08) //                  X
          case (S \ N, /**/ S \ N) /*       */ => LogDouble(0.07) //                  X
          case (S \ N, /**/ (S \ N) \ (S \ N)) => LogDouble(0.45) //            X
          case (S \ N, /**/ END) /*         */ => LogDouble(0.20) //            X
          //                                                                    0.65  0.15
          case (S \ N, NP \ (S \ N)) /*     */ => LogDouble(0.26) //            X
          case (S \ N, NP) /*               */ => LogDouble(0.31) //                  X

          case ((S \ N) \ (S \ N), /**/ STA) /*         */ => sys.error("(S\\N)\\(S\\N) -> <S>") //   COMB   NC
          case ((S \ N) \ (S \ N), /**/ N) /*           */ => LogDouble(0.11) //                             X
          case ((S \ N) \ (S \ N), /**/ S \ N) /*       */ => LogDouble(0.06) //                             X
          case ((S \ N) \ (S \ N), /**/ (S \ N) \ (S \ N)) => LogDouble(0.23) //                      X
          case ((S \ N) \ (S \ N), /**/ END) /*         */ => LogDouble(0.60) //                      X
          //                                                                                          0.83  0.17
          case ((S \ N) \ (S \ N), /**/ NP \ (S \ N)) /**/ => LogDouble(0.03) //                      X
          case ((S \ N) \ (S \ N), /**/ NP) /*          */ => LogDouble(0.14) //                             X

          case (X, /**/ STA) /*         */ => sys.error("X -> <S>") //   COMB  NC
          case (X, /**/ N) /*           */ => LogDouble(0.25) //               X
          case (X, /**/ S \ N) /*       */ => LogDouble(0.55) //         X
          case (X, /**/ (S \ N) \ (S \ N)) => LogDouble(0.10) //               X
          case (X, /**/ END) /*         */ => LogDouble(0.40) //         X
          //                                                             0.95  0.35
          case (X, S \ NP) /*           */ => LogDouble(0.38) //         X
          case (X, NP) /*               */ => LogDouble(0.26) //               X

          case (A, /**/ STA) /*         */ => sys.error("A -> <S>")
          case (A, /**/ N) /*           */ => LogDouble(0.29)
          case (A, /**/ S \ N) /*       */ => LogDouble(0.01)
          case (A, /**/ (S \ N) \ (S \ N)) => LogDouble(0.09)
          case (A, /**/ END) /*         */ => LogDouble(0.01)
          //                                              0.32
          case (A, S \ NP) /*           */ => LogDouble(0.41)
          case (A, NP) /*               */ => LogDouble(0.42)

          case (B, /**/ STA) /*         */ => sys.error("B -> <S>")
          case (B, /**/ N) /*           */ => LogDouble(0.46)
          case (B, /**/ S \ N) /*       */ => LogDouble(0.13)
          case (B, /**/ (S \ N) \ (S \ N)) => LogDouble(0.52)
          case (B, /**/ END) /*         */ => LogDouble(0.12)
          //                                               0.26
          case (B, S \ NP) /*           */ => LogDouble(0.43)
          case (B, NP) /*               */ => LogDouble(0.44)

          case (CAN, /**/ STA) /*         */ => sys.error("CAN -> <S>")
          case (CAN, /**/ N) /*           */ => LogDouble(0.23)
          case (CAN, /**/ S \ N) /*       */ => LogDouble(0.02)
          case (CAN, /**/ (S \ N) \ (S \ N)) => LogDouble(0.03)
          case (CAN, /**/ END) /*         */ => LogDouble(0.04)
          //                                              0.32
          case (CAN, S \ NP) /*           */ => LogDouble(0.39)
          case (CAN, NP) /*               */ => LogDouble(0.27)

          case (CANT, /**/ STA) /*         */ => sys.error("CANT -> <S>")
          case (CANT, /**/ N) /*           */ => LogDouble(0.05)
          case (CANT, /**/ S \ N) /*       */ => LogDouble(0.06)
          case (CANT, /**/ (S \ N) \ (S \ N)) => LogDouble(0.07)
          case (CANT, /**/ END) /*         */ => LogDouble(0.08)
          //                                               0.26
          case (CANT, S \ NP) /*           */ => LogDouble(0.37)
          case (CANT, NP) /*               */ => LogDouble(0.28)

          // COMB TOTAL = 0.75 + 0.75 + 0.85 + 0.83 = 3.18
          // NCOM TOTAL = 0.25 + 0.25 + 0.15 + 0.17 = 0.82
          case (END, _) => ???
        }
      }
      override def sample(given: Cat): Cat = ???
    }

    val mockCanCombine = new CatCanCombine {
      def apply(a: Cat, b: Cat): Boolean = (a, b) match {
        case (STA, STA) /*               */ => sys.error("<S> -> <S>")
        case (STA, N) /*                 */ => true
        case (STA, S \ N) /*             */ => false
        case (STA, (S \ N) \ (S \ N)) /* */ => false
        case (STA, END) /*               */ => false
        case (STA, NP) /*                */ => true
        case (STA, S \ NP) /*            */ => false

        case (N, /**/ STA) /*              */ => sys.error("N -> <S>")
        case (N, /**/ N) /*                */ => false
        case (N, /**/ S \ N) /*            */ => true
        case (N, /**/ (S \ N) \ (S \ N)) /**/ => false
        case (N, /**/ END) /*              */ => true
        case (N, NP) /*                    */ => false
        case (N, S \ NP) /*                */ => true

        case (S \ N, /**/ STA) /*         */ => sys.error("S\\N -> <S>")
        case (S \ N, /**/ N) /*           */ => false
        case (S \ N, /**/ S \ N) /*       */ => false
        case (S \ N, /**/ (S \ N) \ (S \ N)) => true
        case (S \ N, /**/ END) /*         */ => true
        case (S \ N, NP \ (S \ N)) /*     */ => true
        case (S \ N, NP) /*               */ => false

        case ((S \ N) \ (S \ N), /**/ STA) /*         */ => sys.error("(S\\N)\\(S\\N) -> <S>")
        case ((S \ N) \ (S \ N), /**/ N) /*           */ => false
        case ((S \ N) \ (S \ N), /**/ S \ N) /*       */ => false
        case ((S \ N) \ (S \ N), /**/ (S \ N) \ (S \ N)) => true
        case ((S \ N) \ (S \ N), /**/ END) /*         */ => true
        case ((S \ N) \ (S \ N), /**/ NP \ (S \ N)) /**/ => true
        case ((S \ N) \ (S \ N), /**/ NP) /*          */ => false

        case (X, /**/ STA) /*              */ => sys.error("X -> <S>")
        case (X, /**/ N) /*                */ => false
        case (X, /**/ S \ N) /*            */ => true
        case (X, /**/ (S \ N) \ (S \ N)) /**/ => false
        case (X, /**/ END) /*              */ => true
        case (X, NP) /*                    */ => false
        case (X, S \ NP) /*                */ => true

        case (A, /**/ STA) /*              */ => sys.error("A -> <S>")
        case (A, /**/ N) /*                */ => true
        case (A, /**/ S \ N) /*            */ => false
        case (A, /**/ (S \ N) \ (S \ N)) /**/ => true
        case (A, /**/ END) /*              */ => false
        case (A, S \ NP) /*                */ => true
        case (A, NP) /*                    */ => false

        case (B, /**/ STA) /*              */ => sys.error("B -> <S>")
        case (B, /**/ N) /*                */ => true
        case (B, /**/ S \ N) /*            */ => false
        case (B, /**/ (S \ N) \ (S \ N)) /**/ => true
        case (B, /**/ END) /*              */ => false
        case (B, S \ NP) /*                */ => true
        case (B, NP) /*                    */ => false

        case (CAN, /**/ STA) /*              */ => sys.error("CAN -> <S>")
        case (CAN, /**/ N) /*                */ => true
        case (CAN, /**/ S \ N) /*            */ => true
        case (CAN, /**/ (S \ N) \ (S \ N)) /**/ => true
        case (CAN, /**/ END) /*              */ => true
        case (CAN, S \ NP) /*                */ => true
        case (CAN, NP) /*                    */ => false

        case (CANT, /**/ STA) /*              */ => sys.error("CANT -> <S>")
        case (CANT, /**/ N) /*                */ => false
        case (CANT, /**/ S \ N) /*            */ => false
        case (CANT, /**/ (S \ N) \ (S \ N)) /**/ => false
        case (CANT, /**/ END) /*              */ => false
        case (CANT, S \ NP) /*                */ => true
        case (CANT, NP) /*                    */ => false

        case (END, _) => ???
      }

      def rules: Vector[CcgRule] = ???
      def startCat: Cat = ???
      def endCat: Cat = ???
    }

    val combinableTransitionMass = 0.97

    def check(cctcpd: CcgCombinabilityTransitionConditionalLogProbabilityDistribution) {
      def p(given: Cat, x: Cat) = cctcpd(x, given)

      //      /*
      //       *  Verify sums make sense
      //       */
      //      assertEquals(4, cctcpd.combinableSplitSums.size)
      //      assertEqualsLog(LogDouble(0.70 / 0.8), cctcpd.combinableSplitSums(STA)._1, 1e-9)
      //      assertEqualsLog(LogDouble(0.25 / 0.2), cctcpd.combinableSplitSums(STA)._2, 1e-9)
      //      assertEqualsLog(LogDouble(0.85 / 0.8), cctcpd.combinableSplitSums(N)._1, 1e-9)
      //      assertEqualsLog(LogDouble(0.25 / 0.2), cctcpd.combinableSplitSums(N)._2, 1e-9)
      //      assertEqualsLog(LogDouble(0.65 / 0.8), cctcpd.combinableSplitSums(S \ N)._1, 1e-9)
      //      assertEqualsLog(LogDouble(0.15 / 0.2), cctcpd.combinableSplitSums(S \ N)._2, 1e-9)
      //      assertEqualsLog(LogDouble(0.83 / 0.8), cctcpd.combinableSplitSums((S \ N) \ (S \ N))._1, 1e-9)
      //      assertEqualsLog(LogDouble(0.17 / 0.2), cctcpd.combinableSplitSums((S \ N) \ (S \ N))._2, 1e-9)

      assertEqualsLog(LogDouble(0.70 / (0.70 / 0.97)), p(STA, N), 1e-9)
      assertEqualsLog(LogDouble(0.13 / (0.25 / 0.03)), p(STA, S \ N), 1e-9)
      assertEqualsLog(LogDouble(0.16 / (0.70 / 0.97)), p(STA, NP), 1e-9)
      assertEqualsLog(LogDouble(0.29 / (0.25 / 0.03)), p(STA, S \ NP), 1e-9)

      assertEqualsLog(LogDouble(0.50 / (0.85 / 0.97)), p(N, S \ N), 1e-9)
      assertEqualsLog(LogDouble(0.20 / (0.25 / 0.03)), p(N, N), 1e-9)
      assertEqualsLog(LogDouble(0.33 / (0.85 / 0.97)), p(N, S \ NP), 1e-9)
      assertEqualsLog(LogDouble(0.21 / (0.25 / 0.03)), p(N, NP), 1e-9)

      assertEqualsLog(LogDouble(0.45 / (0.65 / 0.97)), p(S \ N, (S \ N) \ (S \ N)), 1e-9)
      assertEqualsLog(LogDouble(0.08 / (0.15 / 0.03)), p(S \ N, N), 1e-9)
      assertEqualsLog(LogDouble(0.26 / (0.65 / 0.97)), p(S \ N, NP \ (S \ N)), 1e-9)
      assertEqualsLog(LogDouble(0.31 / (0.15 / 0.03)), p(S \ N, NP), 1e-9)

      assertEqualsLog(LogDouble(0.60 / (0.83 / 0.97)), p((S \ N) \ (S \ N), END), 1e-9)
      assertEqualsLog(LogDouble(0.06 / (0.17 / 0.03)), p((S \ N) \ (S \ N), S \ N), 1e-9)
      assertEqualsLog(LogDouble(0.03 / (0.83 / 0.97)), p((S \ N) \ (S \ N), NP \ (S \ N)), 1e-9)
      assertEqualsLog(LogDouble(0.14 / (0.17 / 0.03)), p((S \ N) \ (S \ N), NP), 1e-9)

      assertEqualsLog(LogDouble(0.55 / (0.95 / 0.97)), p(X, S \ N), 1e-9)
      assertEqualsLog(LogDouble(0.25 / (0.35 / 0.03)), p(X, N), 1e-9)
      assertEqualsLog(LogDouble(0.38 / (0.95 / 0.97)), p(X, S \ NP), 1e-9)
      assertEqualsLog(LogDouble(0.26 / (0.35 / 0.03)), p(X, NP), 1e-9)

      // cantTotal < 1-combinableTransitionMass
      assertEqualsLog(LogDouble(0.29 /* / (0.32 / 0.97) */ ), p(A, N), 1e-9)
      assertEqualsLog(LogDouble(0.01 /* / (0.02 / 0.97) */ ), p(A, S \ N), 1e-9)
      assertEqualsLog(LogDouble(0.41 /* / (0.32 / 0.97) */ ), p(A, S \ NP), 1e-9)
      assertEqualsLog(LogDouble(0.42 /* / (0.02 / 0.03) */ ), p(A, NP), 1e-9)

      // canTotal > combinableTransitionMass
      assertEqualsLog(LogDouble(0.46 /* / (0.98 / 0.03) */ ), p(B, N), 1e-9)
      assertEqualsLog(LogDouble(0.13 /* / (0.25 / 0.03) */ ), p(B, S \ N), 1e-9)
      assertEqualsLog(LogDouble(0.43 /* / (0.98 / 0.97) */ ), p(B, S \ NP), 1e-9)
      assertEqualsLog(LogDouble(0.44 /* / (0.25 / 0.03) */ ), p(B, NP), 1e-9)

      // cantTotal == 0
      assertEqualsLog(LogDouble(0.02 /* / (0.32 / 0.97) */ ), p(CAN, S \ N), 1e-9)
      assertEqualsLog(LogDouble(0.23 /* / (0.32 / 0.97) */ ), p(CAN, N), 1e-9)
      assertEqualsLog(LogDouble(0.39 /* / (0.32 / 0.97) */ ), p(CAN, S \ NP), 1e-9)
      assertEqualsLog(LogDouble(0.27 /* / (0.00 / 0.03) */ ), p(CAN, NP), 1e-9)

      // canTotal == 0
      assertEqualsLog(LogDouble(0.06 / (0.26 / 0.03)), p(CANT, S \ N), 1e-9)
      assertEqualsLog(LogDouble(0.05 / (0.26 / 0.03)), p(CANT, N), 1e-9)
      assertEqualsLog(LogDouble(0.37 /* / (0.00 / 0.97) */ ), p(CANT, S \ NP), 1e-9)
      assertEqualsLog(LogDouble(0.28 / (0.26 / 0.03)), p(CANT, NP), 1e-9)
    }

    /*
     * Test CcgCombinabilityTransitionConditionalLogProbabilityDistribution
     */
    val directCctcpd = new CcgCombinabilityTransitionConditionalLogProbabilityDistribution(
      mockTrCPD, mockTagdict2, mockCanCombine, combinableTransitionMass, totalSmoothing = LogDouble.zero)
    check(directCctcpd)

    /*
     * Test CcgCombinabilityTrInitializer
     */
    val mockSentences = Vector[Vector[(Word, Set[Tag])]](Vector(("junk", Set())))
    val mockTransitionInitializer = new TransitionInitializer[Tag] {
      def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]) = {
        assertSame(mockSentences, sentences)
        mockTrCPD
      }
    }
    val mockInitialTagdict = new TagDictionary[Tag] {
      def allWords: Set[Word] = ???
      def startWord: Word = ???; def endWord: Word = ???; def startTag: Tag = ???; def endTag: Tag = ???
      def excludedTags: Set[Tag] = ???
      def reversed: Map[Tag, Set[Word]] = ???
      def entries: Map[Word, Set[Tag]] = ???
      def knownWordsForTag: Map[Tag, Set[Word]] = ???
      def withTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def apply(w: Word): Set[Tag] = ???
      def allTags: Set[Tag] = ???

      def withWords(words: Set[Word]): TagDictionary[Tag] = {
        assertEquals(mockSentences.flatten.map(_._1).toSet, words)
        mockTagdict1
      }
    }
    val cctcpdInitializer = new CcgCombinabilityTrInitializer(
      mockTransitionInitializer, mockCanCombine, combinableTransitionMass, totalSmoothing = LogDouble.zero)
    check(cctcpdInitializer.fromKnownSupertagSets(mockSentences, mockInitialTagdict))
  }

  @Test
  def test_TagPriorTrInitializer {
    type Word = String
    type Tag = Cat
    val mockSentences = Vector[Vector[(Word, Set[Tag])]](Vector("junk" -> Set.empty))
    val mockExcludedTag: Tag = cat"an excluded tag".asInstanceOf[AtomCat]
    val mockEndTag: Tag = cat"an end tag".asInstanceOf[AtomCat]
    val mockTagdict2 = new TagDictionary[Tag] {
      def allWords: Set[Word] = ???
      def startWord: Word = ???; def endWord: Word = ???; def startTag: Tag = ???; def endTag: Tag = mockEndTag
      def excludedTags: Set[Tag] = Set(mockExcludedTag)
      def reversed: Map[Tag, Set[Word]] = ???
      def entries: Map[Word, Set[Tag]] = ???
      def knownWordsForTag: Map[Tag, Set[Word]] = ???
      def withWords(words: Set[Word]): TagDictionary[Tag] = ???
      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def apply(w: Word): Set[Tag] = ???
      def allTags: Set[Tag] = ???
      def withTags(tags: Set[Tag]): TagDictionary[Tag] = ???
    }
    val mockTagdict1 = new TagDictionary[Tag] {
      def allWords: Set[Word] = ???
      def startWord: Word = ???; def endWord: Word = ???; def startTag: Tag = ???; def endTag: Tag = ???
      def excludedTags: Set[Tag] = ???
      def reversed: Map[Tag, Set[Word]] = ???
      def entries: Map[Word, Set[Tag]] = ???
      def knownWordsForTag: Map[Tag, Set[Word]] = ???
      def withWords(words: Set[Word]): TagDictionary[Tag] = ???
      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def apply(w: Word): Set[Tag] = ???
      def allTags: Set[Tag] = ???

      def withTags(tags: Set[Tag]): TagDictionary[Tag] = {
        mockTagdict2
      }
    }
    val mockInitialTagdict = new TagDictionary[Tag] {
      def allWords: Set[Word] = ???
      def startWord: Word = ???; def endWord: Word = ???; def startTag: Tag = ???; def endTag: Tag = ???
      def excludedTags: Set[Tag] = ???
      def reversed: Map[Tag, Set[Word]] = ???
      def entries: Map[Word, Set[Tag]] = ???
      def knownWordsForTag: Map[Tag, Set[Word]] = ???
      def withTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def apply(w: Word): Set[Tag] = ???
      def allTags: Set[Tag] = ???

      def withWords(words: Set[Word]): TagDictionary[Tag] = {
        assertEquals(mockSentences.flatten.map(_._1).toSet, words)
        mockTagdict1
      }
    }
    val mockTagPrior = new LogProbabilityDistribution[Tag] {
      def apply(b: Tag): LogDouble = b match {
        case N => LogDouble(0.11)
        case `mockEndTag` => LogDouble(0.23)
        case `mockExcludedTag` => LogDouble(0.35)
      }
      def sample(): Tag = ???
      def defaultProb: LogDouble = ???
    }
    val mockTagPriorInitializer = new TagPriorInitializer[Tag] {
      def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]) = {
        assertSame(mockSentences, sentences)
        assertSame(mockTagdict2, initialTagdict)
        mockTagPrior
      }
    }
    val tpti = new TagPriorTrInitializer(mockTagPriorInitializer)
    val tpTrCPD = tpti.fromKnownSupertagSets(mockSentences, mockInitialTagdict)
    def p(given: Cat, x: Cat) = tpTrCPD(x, given)
    assertEqualsLog(LogDouble(0.11), p(X, N), 1e-9)
    assertEqualsLog(LogDouble(0.23), p(X, mockEndTag), 1e-9)
    assertEqualsLog(LogDouble(0.35), p(X, mockExcludedTag), 1e-9)
    assertEqualsLog(LogDouble(0.11), p(S \ NP, N), 1e-9)
    assertEqualsLog(LogDouble(0.23), p(S \ NP, mockEndTag), 1e-9)
    assertEqualsLog(LogDouble(0.35), p(S \ NP, mockExcludedTag), 1e-9)
    assertEqualsLog(LogDouble(0.00), p(mockEndTag, N), 1e-9)
    assertEqualsLog(LogDouble(0.00), p(mockExcludedTag, N), 1e-9)
  }

  def assertEqualsLog(a: LogDouble, b: LogDouble, c: Double) {
    assertEquals(a.toDouble, b.toDouble, c)
  }
}
