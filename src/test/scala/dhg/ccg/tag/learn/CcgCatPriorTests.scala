package dhg.ccg.tag.learn

import org.junit.Test
import dhg.util._
import org.junit.Assert._
import dhg.ccg.cat._
import dhg.ccg.prob._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tagdict.SimpleTagDictionary

class CcgCatPriorTests {

  val A = cat"!S!".asInstanceOf[AtomCat]
  val S = cat"S".asInstanceOf[AtomCat]
  val NP = cat"NP".asInstanceOf[AtomCat]
  val N = cat"N".asInstanceOf[AtomCat]
  val PP = cat"PP".asInstanceOf[AtomCat]
  val Z = cat"!E!".asInstanceOf[AtomCat]
  val X = cat"X".asInstanceOf[AtomCat]

  @Test
  def test_UniformTagPriorInitializer {
    type Word = String
    type Tag = Cat
    val mockSentences = Vector[Vector[(Word, Set[Tag])]](Vector("junk" -> Set.empty))
    val excludedTag: Cat = cat"an excluded cat".asInstanceOf[NonPuncCat]
    val mockTagdict = new TagDictionary[Tag] {
      def allWords: Set[Word] = ???
      def startWord: Word = ???; def endWord: Word = ???
      def reversed: Map[Tag, Set[Word]] = ???
      def entries: Map[Word, Set[Tag]] = ???
      def knownWordsForTag: Map[Tag, Set[Word]] = ???
      def withWords(words: Set[Word]): TagDictionary[Tag] = ???
      def withTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def apply(w: Word): Set[Tag] = ???

      def allTags: Set[Tag] = Set(
        N,
        NP,
        NP / N,
        S \ NP)
      def endTag: Tag = Z
      def startTag: Tag = A
      def excludedTags: Set[Tag] = Set(excludedTag)
    }

    val init = new UniformTagPriorInitializer[Tag]()
    val tp = init.fromKnownSupertagSets(mockSentences, mockTagdict)

    assertEqualsLog(LogDouble(1 / 6.0), tp(N), 1e-9)
    assertEqualsLog(LogDouble(1 / 6.0), tp(NP), 1e-9)
    assertEqualsLog(LogDouble(1 / 6.0), tp(NP / N), 1e-9)
    assertEqualsLog(LogDouble(1 / 6.0), tp(S \ N), 1e-9)
    assertEqualsLog(LogDouble(1 / 6.0), tp(S), 1e-9)
    assertEqualsLog(LogDouble(1 / 6.0), tp(Z), 1e-9)
    assertEqualsLog(LogDouble(1 / 6.0), tp(X), 1e-9)
    assertEqualsLog(LogDouble(0.0), tp(excludedTag), 1e-9)
  }

  @Test
  def test_CatComplexityInitializer_with_CatComplexityLogProbabilityDistribution {

    val s = cat"S".asInstanceOf[AtomCat]
    val n = cat"N".asInstanceOf[AtomCat]

    val init = new TagPriorTrInitializer[Cat](
      new CatComplexityInitializer())

    val td = SimpleTagDictionary[Cat](Map(), "<S>", cat"<S>", "<E>", cat"<E>", Set(),
      Set(n, s / s, s \ s, n / n, n \ n, s \ n, (s \ n) / n))

    val sum = Vector(
      1.0 / 1, //<S>
      1.0 / 1, //<E>
      1.0 / 1, //n
      1.0 / 3, //s/s
      1.0 / 3, //s\s
      1.0 / 3, //n/n
      1.0 / 3, //n\n
      1.0 / 3, //s\n
      1.0 / 5 //(s\n)/n
      ).sum

    val tr = init.fromRaw(Vector(), td)

    assertEquals((1.0 / 1) / sum, tr(s, BadCat).toDouble, 1e-5)
    assertEquals((1.0 / 1) / sum, tr(n, BadCat).toDouble, 1e-5)
    assertEquals((1.0 / 1) / sum, tr(cat"S[xb]".asInstanceOf[AtomCat], BadCat).toDouble, 1e-5)
    assertEquals((1.0 / 3) / sum, tr(s / n, BadCat).toDouble, 1e-5)
    assertEquals((1.0 / 3) / sum, tr(cat"S[xb]".asInstanceOf[AtomCat] / n, BadCat).toDouble, 1e-5)
    assertEquals((1.0 / 3) / sum, tr(s \ n, BadCat).toDouble, 1e-5)
    assertEquals((1.0 / 3) / sum, tr(cat"S[xb]".asInstanceOf[AtomCat] \ n, BadCat).toDouble, 1e-5)

    assertEquals((1.0 / 3) / sum, tr(n / n, BadCat).toDouble, 1e-5)
    assertEquals((1.0 / 3) / sum, tr(s \ s, BadCat).toDouble, 1e-5)
    assertEquals((1.0 / 3) / sum, tr(cat"S[xb]".asInstanceOf[AtomCat] \ s, BadCat).toDouble, 1e-5)

    assertEquals((1.0 / 5) / sum, tr((s \ n) / n, BadCat).toDouble, 1e-5)
    assertEquals((1.0 / 5) / sum, tr((cat"S[xb]".asInstanceOf[AtomCat] \ n) / n, BadCat).toDouble, 1e-5)

    assertEquals((1.0 / 7) / sum, tr((s \ n) / (s \ n), BadCat).toDouble, 1e-5)

    assertEquals((1.0 / 3) / sum, tr(s / n, cat"<S>").toDouble, 1e-5)
    assertEquals(0.0, tr(s / n, cat"<E>").toDouble, 1e-5)
    assertEquals((1.0 / 1) / sum, tr(cat"<E>", BadCat).toDouble, 1e-5)
  }

  @Test
  def test_CheatingTagPriorInitializer {
    type Word = String
    type Tag = Cat
    val excludedTag = cat"an excluded tag".asInstanceOf[NonPuncCat]
    val supvCorpus = Vector(
      raw"the|(NP/N) brown|(N/N) dog|N walks|(S\NP)",
      raw"the|(NP/N) man|N walks|((S\NP)/NP) a|(NP/N) brown|(N/N) dog|N")
      .map(_.splitWhitespace.map(_.rsplit("\\|", 2).toTuple2).mapt((w, t) => w -> NonRemovingCcgBankCatInterner(t)))

    val mockSentences = Vector[Vector[(Word, Set[Tag])]](Vector("junk" -> Set.empty))
    val mockTagdict = new TagDictionary[Tag] {
      def allWords: Set[Word] = ???
      def startWord: Word = ???; def endWord: Word = ???
      def excludedTags: Set[Tag] = Set(excludedTag)
      def reversed: Map[Tag, Set[Word]] = ???
      def entries: Map[Word, Set[Tag]] = ???
      def knownWordsForTag: Map[Tag, Set[Word]] = ???
      def withWords(words: Set[Word]): TagDictionary[Tag] = ???
      def withTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def apply(w: Word): Set[Tag] = ???

      def allTags: Set[Tag] = Set(
        N,
        NP,
        S \ NP,
        (S \ NP) / NP)
      def startTag: Tag = A
      def endTag: Tag = Z
    }

    /*
     * NP/N       3  3.1
     * N          3  3.1
     * S\NP       1  1.1
     * (S\NP)/NP  1  1.1
     * N/N        2  2.1
     * <S>        2  2.1
     * <E>        2  2.1
     * NP         0  0.1
     *              ----
     *              14.8
     */

    val init = new CheatingTagPriorInitializer(supvCorpus, lambda = LogDouble(0.1))
    val pd = init.fromKnownSupertagSets(mockSentences, mockTagdict)
    assertEqualsLog(LogDouble(3.1 / 14.8), pd(NP / N), 1e-9)
    assertEqualsLog(LogDouble(3.1 / 14.8), pd(N), 1e-9)
    assertEqualsLog(LogDouble(1.1 / 14.8), pd(S \ NP), 1e-9)
    assertEqualsLog(LogDouble(1.1 / 14.8), pd((S \ NP) / NP), 1e-9)
    assertEqualsLog(LogDouble(2.1 / 14.8), pd(N / N), 1e-9)
    assertEqualsLog(LogDouble(0.1 / 14.8), pd(NP), 1e-9)
    assertEqualsLog(LogDouble(2.1 / 14.8), pd(A), 1e-9)
    assertEqualsLog(LogDouble(2.1 / 14.8), pd(Z), 1e-9)
    assertEqualsLog(LogDouble(0.1 / 14.8), pd(X), 1e-9)
    assertEqualsLog(LogDouble(0.0), pd(excludedTag), 1e-9)
  }

  @Test
  def test_UniformAtomCatDistInitializer {
    type Word = String
    type Tag = Cat

    val mockSentences = Vector("1 2 3".splitWhitespace, "1 4 4 5".splitWhitespace)
    val excludedTag = cat"an excluded tag".asInstanceOf[AtomCat]

    val mockTagdict = new TagDictionary[Tag] {
      def allWords: Set[Word] = ???
      def startWord: Word = ???; def endWord: Word = ???
      def excludedTags: Set[Tag] = Set(excludedTag)
      def reversed: Map[Tag, Set[Word]] = ???
      def entries: Map[Word, Set[Tag]] = Map.empty
      def knownWordsForTag: Map[Tag, Set[Word]] = ???
      def withWords(words: Set[Word]): TagDictionary[Tag] = ???
      def withTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def apply(w: Word): Set[Tag] = ???

      def allTags: Set[Tag] = Set(
        N,
        NP,
        S \ NP,
        (S \ NP) / NP)
      def startTag: Tag = A
      def endTag: Tag = Z
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
        assertEquals(mockSentences.flatten.toSet, words)
        mockTagdict
      }
    }

    val init = new UniformAtomCatDistInitializer()
    val apd: LogProbabilityDistribution[AtomCat] = init.fromRaw(mockSentences, mockInitialTagdict)

    assertEqualsLog(LogDouble(1.0 / 5), apd(S), 1e-9)
    assertEqualsLog(LogDouble(1.0 / 5), apd(N), 1e-9)
    assertEqualsLog(LogDouble(1.0 / 5), apd(NP), 1e-9)
    assertEqualsLog(LogDouble(1.0 / 5), apd(A), 1e-9)
    assertEqualsLog(LogDouble(1.0 / 5), apd(Z), 1e-9)
    assertEqualsLog(LogDouble(1.0 / 5), apd(X), 1e-9)
    assertEqualsLog(LogDouble(0.0), apd(excludedTag), 1e-9)
  }

  @Test
  def test_TagdictInformedAtomCatDistInitializer {
    type Word = String
    type Tag = Cat

    val mockSentences = Vector("1 2 3".splitWhitespace, "1 4 4 5".splitWhitespace)
    val excludedTag = cat"an excluded tag".asInstanceOf[AtomCat]

    val mockTagdict = new TagDictionary[Tag] {
      def allWords: Set[Word] = ???
      def startWord: Word = ???; def endWord: Word = ???
      def excludedTags: Set[Tag] = Set(excludedTag)
      def reversed: Map[Tag, Set[Word]] = ???
      def knownWordsForTag: Map[Tag, Set[Word]] = ???
      def withWords(words: Set[Word]): TagDictionary[Tag] = this
      def withTags(tags: Set[Tag]): TagDictionary[Tag] = this
      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def apply(w: Word): Set[Tag] = ???

      def entries: Map[Word, Set[Tag]] = Map(
        "1" -> Set(NP / N),
        // "2" -> 
        "3" -> Set(S \ NP, (S \ NP) / NP),
        "4" -> Set(N / N),
        "5" -> Set(N))
      def allTags: Set[Tag] = Set(
        N,
        NP,
        S \ NP,
        (S \ NP) / NP,
        (S \ NP) / PP)
      def startTag: Tag = A
      def endTag: Tag = Z
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
        assertEquals(mockSentences.flatten.toSet, words)
        mockTagdict
      }
    }

    val init = new TagdictInformedAtomCatDistInitializer(atomLambda = 0.1)
    val apd: LogProbabilityDistribution[AtomCat] = init.fromRaw(mockSentences, mockInitialTagdict)

    /*
     * 1      2     3          4       5      <s>      <e>
     * np/n         s\np       n/n     n      <s>      <e>
     *              (s\np)/np  
     *
     * np 1.0       s  1.0     n 2.0   n 1.0  <s> 1.0  <e> 1.0
     * n  1.0       np 1.5
     * 2x     1x    1x         2x      1x     2x       2x
     * 
     * s    1.0   1.1
     * np   3.5   3.6
     * n    7.0   7.1
     * pp   0.0   0.1
     * <s>  2.0   2.1
     * <e>  2.0   2.1
     *     ----  ----
     *     15.5  16.1
     */

    assertEqualsLog(LogDouble(1.1 / 16.1), apd(S), 1e-9)
    assertEqualsLog(LogDouble(3.6 / 16.1), apd(NP), 1e-9)
    assertEqualsLog(LogDouble(7.1 / 16.1), apd(N), 1e-9)
    assertEqualsLog(LogDouble(0.1 / 16.1), apd(PP), 1e-9)
    assertEqualsLog(LogDouble(2.1 / 16.1), apd(A), 1e-9)
    assertEqualsLog(LogDouble(2.1 / 16.1), apd(Z), 1e-9)
    assertEqualsLog(LogDouble(0.1 / 16.1), apd(X), 1e-9)
    assertEqualsLog(LogDouble(0.0), apd(excludedTag), 1e-9)
  }

  @Test
  def test_KnownAtomCatDistInitializer {
    type Word = String
    type Tag = Cat

    val mockSentences = Vector("1 2 3".splitWhitespace, "1 4 4 5".splitWhitespace)
    val mockInitialTagdict = new TagDictionary[Tag] {
      def allWords: Set[Word] = ???
      def startWord: Word = ???; def endWord: Word = ???; def startTag: Tag = ???; def endTag: Tag = ???
      def excludedTags: Set[Tag] = ???
      def reversed: Map[Tag, Set[Word]] = ???
      def entries: Map[Word, Set[Tag]] = Map.empty
      //        new Map[Word, Set[Tag]] {
      //          def +[B1 >: Set[Tag]](kv: (Word, B1)) = ???
      //          def -(key: Word) = ???
      //          def get(key: Word) = ???
      //          def iterator = ???
      //        }
      def knownWordsForTag: Map[Tag, Set[Word]] = ???
      def withWords(words: Set[Word]): TagDictionary[Tag] = this
      def withTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def apply(w: Word): Set[Tag] = ???
      def allTags: Set[Tag] = ???
    }
    val mockAtomDist = new LogProbabilityDistribution[AtomCat] {
      def apply(b: AtomCat): LogDouble = ???
      def sample(): AtomCat = ???
      def defaultProb: LogDouble = ???
    }

    val init = new KnownAtomCatDistInitializer(mockAtomDist)
    val apd: LogProbabilityDistribution[AtomCat] = init.fromRaw(mockSentences, mockInitialTagdict)
    assertSame(mockAtomDist, apd)
  }

  @Test
  def test_CatgramCatPriorInitializer_with_CatgramCatPriorLogProbabilityDistribution {
    type Word = String
    type Tag = Cat
    val ExcludedTag = cat"an excluded tag".asInstanceOf[NonPuncCat]
    val ExcludedTags: Set[Cat] = Set(ExcludedTag)
    val AllTags: Set[Cat] = Set(
      N, //                  0.3          = 0.60 * 0.50
      S \ N, //              0.007776     = (1-0.60) * (1-0.20) * (1-0.55) * (0.60 * 0.30) * (0.60 * 0.50)
      N / N, //              0.0132       = (1-0.60) *    0.20  *    0.55  * (0.60 * 0.50)
      (S \ N) / N) //        0.0004105728 = (1-0.60) * (1-0.20) *    0.55  * ((1-0.60) * (1-0.20) * (1-0.55) * (0.60 * 0.30) * (0.60 * 0.50)) * (0.60 * 0.50)
    def startTag: Cat = A // 0.036        = 0.60 * 0.06
    def endTag: Cat = Z //   0.024        = 0.60 * 0.04

    val mockTagdict = new TagDictionary[Tag] {
      def allWords: Set[Word] = ???
      def startWord: Word = ???; def endWord: Word = ???
      def reversed: Map[Tag, Set[Word]] = ???
      def knownWordsForTag: Map[Tag, Set[Word]] = ???
      def withWords(words: Set[Word]): TagDictionary[Tag] = ???
      def withTags(tags: Set[Tag]): TagDictionary[Tag] = this
      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def apply(w: Word): Set[Tag] = ???
      def entries: Map[Word, Set[Tag]] = ???

      def allTags: Set[Tag] = AllTags
      def excludedTags: Set[Tag] = ExcludedTags
      def startTag: Tag = A
      def endTag: Tag = Z
    }

    val pAtom = new LogProbabilityDistribution[AtomCat] {
      def apply(b: AtomCat): LogDouble = LogDouble(b match {
        case S => 0.30
        case N => 0.50
        case A => 0.06
        case Z => 0.04
        case X => 0.05
        case ExcludedTag => 0.06
      })
      def sample(): AtomCat = ???
      def defaultProb: LogDouble = ???
    }
    val pTerm = 0.60
    val pMod = 0.20
    val pFwd = 0.55

    def check(ccppd: CatgramCatPriorLogProbabilityDistribution, hasExcl: Boolean) {
      assertEqualsLog(LogDouble(0.3), ccppd(N), 1e-9)
      assertEqualsLog(LogDouble(0.007776), ccppd(S \ N), 1e-9)
      assertEqualsLog(LogDouble(0.0132), ccppd(N / N), 1e-9)
      assertEqualsLog(LogDouble(0.0004105728), ccppd((S \ N) / N), 1e-9)
      assertEqualsLog(LogDouble(0.036), ccppd(A), 1e-9)
      assertEqualsLog(LogDouble(0.024), ccppd(Z), 1e-9)
      if (hasExcl) assertEqualsLog(LogDouble(0.0), ccppd(ExcludedTag), 1e-9)
      else assertEqualsLog(LogDouble(0.036), ccppd(ExcludedTag), 1e-9) //         0.60 * 0.06
      assertEqualsLog(LogDouble(0.0019008), ccppd(ExcludedTag / N), 1e-9) //      (1-0.60) * (1-0.20) * 0.55 * (0.60 * 0.06) * (0.60 * 0.50)
      assertEqualsLog(LogDouble(0.03), ccppd(X), 1e-9) //                         0.60 * 0.05
      assertEqualsLog(LogDouble(0.18), ccppd(S), 1e-9) //                         0.60 * 0.30
      assertEqualsLog(LogDouble(0.000342144), ccppd((S \ N) / (S \ N)), 1e-9) //  (1-0.60) * 0.20 * 0.55 * 0.007776
    }

    /*
     * Test CatgramCatPriorLogProbabilityDistribution directly
     */
    check(new CatgramCatPriorLogProbabilityDistribution(pAtom, pTerm, pMod, pFwd, AllTags, Some(ExcludedTags)), true)
    check(new CatgramCatPriorLogProbabilityDistribution(pAtom, pTerm, pMod, pFwd, AllTags, None), false)

    /*
     * Test CatgramCatPriorInitializer
     */
    val mockSentences = Vector[Vector[(Word, Set[Cat])]](Vector(("junk", Set.empty)))
    val mockInitialTagdict = new TagDictionary[Cat] {
      def allWords: Set[Word] = ???
      def startWord: Word = ???; def endWord: Word = ???; def startTag: Cat = ???; def endTag: Cat = ???
      def excludedTags: Set[Cat] = ???
      def reversed: Map[Cat, Set[Word]] = ???
      def entries: Map[Word, Set[Cat]] = ???
      def knownWordsForTag: Map[Cat, Set[Word]] = ???
      def withTags(tags: Set[Cat]): TagDictionary[Cat] = ???
      def withExcludedTags(tags: Set[Cat]): TagDictionary[Cat] = ???
      def apply(w: Word): Set[Cat] = ???
      def allTags: Set[Cat] = ???

      def withWords(words: Set[Word]): TagDictionary[Cat] = {
        assertEquals(mockSentences.flatten.map(_._1).toSet, words)
        mockTagdict
      }
    }
    val mockAtomCatDistInit = new AtomCatDistInitializer {
      def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Cat])]], initialTagdict: TagDictionary[Cat]) = {
        assertSame(mockSentences, sentences)
        assertSame(mockTagdict, initialTagdict)
        pAtom
      }
    }
    val ccpi = new CatgramCatPriorInitializer(mockAtomCatDistInit, pTerm, pMod, pFwd)
    val ccp: CatgramCatPriorLogProbabilityDistribution = ccpi.fromKnownSupertagSets(mockSentences, mockInitialTagdict)
    check(ccp, true)
  }

  @Test
  def test_NormalizingCatgramCatPriorInitializer_with_NormalizingCatgramCatPriorLogProbabilityDistribution {
    type Word = String
    type Tag = Cat
    val ExcludedTag = cat"an excluded tag".asInstanceOf[NonPuncCat]
    val mockTagdict = new TagDictionary[Tag] {
      def allWords: Set[Word] = ???
      def startWord: Word = ???; def endWord: Word = ???
      def excludedTags: Set[Tag] = Set(ExcludedTag)
      def reversed: Map[Tag, Set[Word]] = ???
      def knownWordsForTag: Map[Tag, Set[Word]] = ???
      def withWords(words: Set[Word]): TagDictionary[Tag] = ???
      def withTags(tags: Set[Tag]): TagDictionary[Tag] = this
      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def apply(w: Word): Set[Tag] = ???
      def entries: Map[Word, Set[Tag]] = ???

      def allTags: Set[Tag] = Set(
        N, //                  0.3          = 0.60 * 0.50
        S \ N, //              0.007776     = (1-0.60) * (1-0.20) * (1-0.55) * (0.60 * 0.30) * (0.60 * 0.50)
        N / N, //              0.0132       = (1-0.60) *    0.20  *    0.55  * (0.60 * 0.50)
        (S \ N) / N) //        0.0004105728 = (1-0.60) * (1-0.20) *    0.55  * ((1-0.60) * (1-0.20) * (1-0.55) * (0.60 * 0.30) * (0.60 * 0.50)) * (0.60 * 0.50)
      def startTag: Tag = A // 0.036        = 0.60 * 0.06
      def endTag: Tag = Z //   0.024        = 0.60 * 0.04
      //                       ------------ 
      //                       0.3813865728
    }
    val pAtom = new LogProbabilityDistribution[AtomCat] {
      def apply(b: AtomCat): LogDouble = LogDouble(b match {
        case S => 0.30
        case N => 0.50
        case A => 0.06
        case Z => 0.04
        case X => 0.05
        case ExcludedTag => 0.06
      })
      def sample(): AtomCat = ???
      def defaultProb: LogDouble = ???
    }
    val pTerm = 0.60
    val pMod = 0.20
    val pFwd = 0.55

    def check(ccppd: NormalizingCatgramCatPriorLogProbabilityDistribution) {
      assertEqualsLog(LogDouble(0.3 / 0.3813865728), ccppd(N), 1e-9)
      assertEqualsLog(LogDouble(0.007776 / 0.3813865728), ccppd(S \ N), 1e-9)
      assertEqualsLog(LogDouble(0.0132 / 0.3813865728), ccppd(N / N), 1e-9)
      assertEqualsLog(LogDouble(0.0004105728 / 0.3813865728), ccppd((S \ N) / N), 1e-9)
      assertEqualsLog(LogDouble(0.036 / 0.3813865728), ccppd(A), 1e-9)
      assertEqualsLog(LogDouble(0.024 / 0.3813865728), ccppd(Z), 1e-9)
      assertEqualsLog(LogDouble(0.0), ccppd(ExcludedTag), 1e-9)
      assertEqualsLog(LogDouble(0.0019008 / 0.3813865728), ccppd(ExcludedTag / N), 1e-9) // (1-0.60) * (1-0.20) * 0.55 * (0.60 * 0.06) * (0.60 * 0.50)
      assertEqualsLog(LogDouble(0.03 / 0.3813865728), ccppd(X), 1e-9) //     0.60 * 0.05
      assertEqualsLog(LogDouble(0.18 / 0.3813865728), ccppd(S), 1e-9) //     0.60 * 0.30
      assertEqualsLog(LogDouble(0.000342144 / 0.3813865728), ccppd((S \ N) / (S \ N)), 1e-9) //  (1-0.60) * 0.20 * 0.55 * 0.007776
    }

    /*
     * Test NormalizingCatgramCatPriorLogProbabilityDistribution directly
     */
    check(new NormalizingCatgramCatPriorLogProbabilityDistribution(mockTagdict, pAtom, pTerm, pMod, pFwd))

    /*
     * Test NormalizingCatgramCatPriorInitializer
     */
    val mockSentences = Vector[Vector[(Word, Set[Cat])]](Vector("junk" -> Set.empty))
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
        mockTagdict
      }
    }
    val mockAtomCatDistInit = new AtomCatDistInitializer {
      def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Cat])]], initialTagdict: TagDictionary[Cat]) = {
        assertSame(mockSentences, sentences)
        assertSame(mockTagdict, initialTagdict)
        pAtom
      }
    }
    val ccpi = new NormalizingCatgramCatPriorInitializer(mockAtomCatDistInit, pTerm, pMod, pFwd)
    check(ccpi.fromKnownSupertagSets(mockSentences, mockInitialTagdict))
  }

  def assertEqualsLog(a: LogDouble, b: LogDouble, c: Double) {
    assertEquals(a.toDouble, b.toDouble, c)
  }
}
