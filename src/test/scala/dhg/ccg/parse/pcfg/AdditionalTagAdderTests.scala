package dhg.ccg.parse.pcfg

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.tagdict._

class AdditionalTagAdderTests {

  @Test
  def test_SequentialAdditionalTagAdder {
    type Tag = Int

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
      def allTags: Set[Tag] = ???
      def endTag: Tag = ???
      def startTag: Tag = ???
      def excludedTags: Set[Tag] = ???
    }

    val testSentence = Vector("a", "b", "c")
    val startTags = Vector(Set(1, 2, 3), Set(4, 5), Set(6, 7, 8))

    val mockTags1 = Vector(Set(10), Set(11), Set(12))
    val mockTags2 = Vector(Set(20), Set(21), Set(22))
    val mockTags3 = Vector(Set(30), Set(31), Set(32))

    val mockAta1 = new AdditionalTagAdder[Tag] {
      def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: TagDictionary[Tag]): Vector[Set[Tag]] = {
        assertEquals(testSentence, sentence)
        assertEquals(startTags, tags)
        assertSame(mockTagdict, tagdict)
        mockTags1
      }
    }
    val mockAta2 = new AdditionalTagAdder[Tag] {
      def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: TagDictionary[Tag]): Vector[Set[Tag]] = {
        assertEquals(testSentence, sentence)
        assertEquals(mockTags1, tags)
        assertSame(mockTagdict, tagdict)
        mockTags2
      }
    }
    val mockAta3 = new AdditionalTagAdder[Tag] {
      def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: TagDictionary[Tag]): Vector[Set[Tag]] = {
        assertEquals(testSentence, sentence)
        assertEquals(mockTags2, tags)
        assertSame(mockTagdict, tagdict)
        mockTags3
      }
    }

    val ata = new SequentialAdditionalTagAdder[Int](Vector(mockAta1, mockAta2, mockAta3))
    val newTags = ata(testSentence, startTags, mockTagdict)
    assertEquals(mockTags3, newTags)
  }

  @Test
  def test_ParallelAdditionalTagAdder {
    type Word = Symbol
    type Tag = Int

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
      def allTags: Set[Tag] = ???
      def endTag: Tag = ???
      def startTag: Tag = ???
      def excludedTags: Set[Tag] = ???
    }

    val testSentence = Vector("a", "b", "c")
    val startTags = Vector(Set(1, 2, 3), Set(4, 5), Set(6, 7, 8))

    val mockTags1 = Vector(Set(10), Set(11), Set(12))
    val mockTags2 = Vector(Set(20), Set(21), Set(22))
    val mockTags3 = Vector(Set(30), Set(31), Set(32))

    val mockAta1 = new AdditionalTagAdder[Tag] {
      def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: TagDictionary[Tag]): Vector[Set[Tag]] = {
        assertEquals(testSentence, sentence)
        assertEquals(startTags, tags)
        assertSame(mockTagdict, tagdict)
        mockTags1
      }
    }
    val mockAta2 = new AdditionalTagAdder[Tag] {
      def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: TagDictionary[Tag]): Vector[Set[Tag]] = {
        assertEquals(testSentence, sentence)
        assertEquals(startTags, tags)
        assertSame(mockTagdict, tagdict)
        mockTags2
      }
    }
    val mockAta3 = new AdditionalTagAdder[Tag] {
      def apply(sentence: Vector[Word], tags: Vector[Set[Tag]], tagdict: TagDictionary[Tag]): Vector[Set[Tag]] = {
        assertEquals(testSentence, sentence)
        assertEquals(startTags, tags)
        assertSame(mockTagdict, tagdict)
        mockTags3
      }
    }

    val ata = new ParallelAdditionalTagAdder[Int](Vector(mockAta1, mockAta2, mockAta3))
    val newTags = ata(testSentence, startTags, mockTagdict)
    val expected = Vector(
      Set(10, 20, 30),
      Set(11, 21, 31),
      Set(12, 22, 32))
    assertEquals(expected, newTags)

    val ata2 = new ParallelAdditionalTagAdder[Int](Vector(new NoOpAdditionalTagAdder, mockAta1, mockAta2, mockAta3))
    val newTags2 = ata2(testSentence, startTags, mockTagdict)
    val expected2 = Vector(
      Set(1, 2, 3, 10, 20, 30),
      Set(4, 5, 11, 21, 31),
      Set(6, 7, 8, 12, 22, 32))
    assertEquals(expected2, newTags2)
  }

  @Test
  def test_NoOpAdditionalTagAdder {
    type Word = Symbol
    type Tag = Int
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
      def allTags: Set[Tag] = ???
      def endTag: Tag = ???
      def startTag: Tag = ???
      def excludedTags: Set[Tag] = ???
    }

    val sentence = Vector("a", "b", "c")
    val startTags = Vector(Set(1, 2, 3), Set(4, 5), Set(6, 7, 8))
    val ata = new NoOpAdditionalTagAdder[Int]()
    val newTags = ata(sentence, startTags, mockTagdict)
    assertEquals(startTags, newTags)
  }

  @Test
  def test_PresentTagdictAdditionalTagAdder {
    type Word = Symbol
    type Tag = Int
    val mockTagdict = new TagDictionary[Tag] {
      def allWords: Set[Word] = ???
      def startWord: Word = ???; def endWord: Word = ???
      def reversed: Map[Tag, Set[Word]] = ???
      def knownWordsForTag: Map[Tag, Set[Word]] = ???
      def withWords(words: Set[Word]): TagDictionary[Tag] = ???
      def withTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def apply(w: Word): Set[Tag] = ???
      def allTags: Set[Tag] = ???
      def endTag: Tag = ???
      def startTag: Tag = ???
      def excludedTags: Set[Tag] = ???

      def entries: Map[Word, Set[Tag]] = Map(
        "a" -> Set(3, 4, 5),
        "b" -> Set(),
        "c" -> Set(6),
        "d" -> Set(7, 8))
    }

    val sentence = Vector("a", "b", "c", "d", "d", "e", "e")
    val startTags = Vector[Set[Int]](Set(1, 2, 3), Set(), Set(6), Set(), Set(5, 6), Set(9), Set())
    val ata = new PresentTagdictAdditionalTagAdder[Int]
    val newTags = ata(sentence, startTags, mockTagdict)
    assertEquals(Vector(Set(3, 4, 5), Set(), Set(6), Set(7, 8), Set(7, 8), Set(), Set()), newTags)

    val ata2 = new ParallelAdditionalTagAdder[Int](Vector(new NoOpAdditionalTagAdder, ata))
    val newTags2 = ata2(sentence, startTags, mockTagdict)
    assertEquals(Vector(Set(1, 2, 3, 4, 5), Set(), Set(6), Set(7, 8), Set(5, 6, 7, 8), Set(9), Set()), newTags2)
  }

  @Test
  def test_FullTagdictTagsetForMissingAdditionalTagAdder {
    type Word = Symbol
    type Tag = Int
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
      def endTag: Tag = ???
      def startTag: Tag = ???
      def excludedTags: Set[Tag] = ???

      def allTags: Set[Tag] = Set(7, 8)
    }

    val sentence = Vector("a", "b", "c", "d")
    val startTags = Vector[Set[Int]](Set(1, 2, 3), Set(), Set(6), Set())
    val ata = new FullTagdictTagsetForMissingAdditionalTagAdder[Int]()
    val newTags = ata(sentence, startTags, mockTagdict)
    assertEquals(4, newTags.size)
    assertEquals(Set(), newTags(0))
    assertEquals(Set(7, 8), newTags(1))
    assertEquals(Set(), newTags(2))
    assertEquals(Set(7, 8), newTags(3))

    val ata2 = new ParallelAdditionalTagAdder[Int](Vector(new NoOpAdditionalTagAdder, ata))
    val newTags2 = ata2(sentence, startTags, mockTagdict)
    assertEquals(4, newTags2.size)
    assertEquals(Set(1, 2, 3), newTags2(0))
    assertEquals(Set(7, 8), newTags2(1))
    assertEquals(Set(6), newTags2(2))
    assertEquals(Set(7, 8), newTags2(3))
  }

  @Test
  def test_DefaultTagsetForMissingAdditionalTagAdder {
    type Word = Symbol
    type Tag = Int
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
      def allTags: Set[Tag] = ???
      def endTag: Tag = ???
      def startTag: Tag = ???
      def excludedTags: Set[Tag] = ???
    }

    val sentence = Vector("a", "b", "c", "d")
    val startTags = Vector[Set[Int]](Set(1, 2, 3), Set(), Set(6), Set())
    val ata = new DefaultTagsetForMissingAdditionalTagAdder[Int](Set(7, 8))
    val newTags = ata(sentence, startTags, mockTagdict)
    assertEquals(4, newTags.size)
    assertEquals(Set(), newTags(0))
    assertEquals(Set(7, 8), newTags(1))
    assertEquals(Set(), newTags(2))
    assertEquals(Set(7, 8), newTags(3))

    val ata2 = new ParallelAdditionalTagAdder[Int](Vector(new NoOpAdditionalTagAdder, ata))
    val newTags2 = ata2(sentence, startTags, mockTagdict)
    assertEquals(4, newTags2.size)
    assertEquals(Set(1, 2, 3), newTags2(0))
    assertEquals(Set(7, 8), newTags2(1))
    assertEquals(Set(6), newTags2(2))
    assertEquals(Set(7, 8), newTags2(3))
  }

  //

  @Test
  def test_FwdBkdModAdditionalTagAdder {
    type Word = Symbol
    type Tag = Cat
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
      def allTags: Set[Tag] = ???
      def endTag: Tag = ???
      def startTag: Tag = ???
      def excludedTags: Set[Tag] = ???
    }

    val a = cat"A".asInstanceOf[NonPuncCat]
    val b = cat"B".asInstanceOf[NonPuncCat]
    val c = cat"C".asInstanceOf[NonPuncCat]

    val testSentence = Vector("a", "b", "c", "d", "e", "f")
    val startTags = Vector[Set[Cat]](
      Set(a, a / b, c),
      Set(b, c \ a),
      Set(),
      Set((c / a) \ b, a),
      Set(a),
      Set(a \ a))

    val ata = new FwdBkdModAdditionalTagAdder()
    val newTags = ata(testSentence, startTags, mockTagdict)
    assertEquals(6, newTags.size)
    assertEquals(Set( /*                                      */ b / b, (c \ a) / (c \ a)), newTags(0))
    assertEquals(Set(a \ a, (a / b) \ (a / b), c \ c /*      */ ), newTags(1))
    assertEquals(Set(b \ b, (c \ a) \ (c \ a), /*            */ ((c / a) \ b) / ((c / a) \ b), a / a), newTags(2))
    assertEquals(Set( /*                                          */ a / a), newTags(3))
    assertEquals(Set(((c / a) \ b) \ ((c / a) \ b), a \ a, /**/ (a \ a) / (a \ a)), newTags(4))
    assertEquals(Set(a \ a /*                                */ ), newTags(5))

    val ata2 = new ParallelAdditionalTagAdder[Cat](Vector(new NoOpAdditionalTagAdder, ata))
    val newTags2 = ata2(testSentence, startTags, mockTagdict)
    assertEquals(6, newTags2.size)
    assertEquals(Set(a, a / b, c, /*  */ /*                                      */ b / b, (c \ a) / (c \ a)), newTags2(0))
    assertEquals(Set(b, c \ a, /*     */ a \ a, (a / b) \ (a / b), c \ c /*      */ ), newTags2(1))
    assertEquals(Set( /*              */ b \ b, (c \ a) \ (c \ a), /*            */ ((c / a) \ b) / ((c / a) \ b), a / a), newTags2(2))
    assertEquals(Set((c / a) \ b, a, /*                                          */ a / a), newTags2(3))
    assertEquals(Set(a, /*            */ ((c / a) \ b) \ ((c / a) \ b), a \ a, /**/ (a \ a) / (a \ a)), newTags2(4))
    assertEquals(Set(a \ a, /*        */ a \ a /*                                */ ), newTags2(5))
  }

  //

  @Test
  def test_StandardTagDictAdditionalTagAdder {
    type Word = String
    type Tag = Int
    val mockTagdict = new TagDictionary[Tag] {
      def allWords: Set[Word] = ???
      def startWord: Word = ???; def endWord: Word = ???
      def reversed: Map[Tag, Set[Word]] = ???
      def knownWordsForTag: Map[Tag, Set[Word]] = ???
      def withWords(words: Set[Word]): TagDictionary[Tag] = ???
      def withTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def apply(w: Word): Set[Tag] = ???
      def allTags: Set[Tag] = Set(3, 4, 5, 6, 7, 8, 9)
      def endTag: Tag = ???
      def startTag: Tag = ???
      def excludedTags: Set[Tag] = ???

      def entries: Map[Word, Set[Tag]] = Map(
        "a" -> Set(3, 4, 5),
        "b" -> Set(),
        "c" -> Set(6),
        "d" -> Set(7, 8))
    }

    val sentence = Vector("a", "b", "c", "d", "d", "e", "e")
    val ata = new StandardTagDictAdditionalTagAdder[Int]
    val startTags = Vector[Set[Int]](Set(1, 2, 3), Set(), Set(6), Set(), Set(5, 6), Set(9), Set())
    val newTags2 = ata(sentence, Vector.fill(sentence.size)(Set.empty), mockTagdict)
    assertEquals(Vector(Set(3, 4, 5), Set(3, 4, 5, 6, 7, 8, 9), Set(6), Set(7, 8), Set(7, 8), Set(3, 4, 5, 6, 7, 8, 9), Set(3, 4, 5, 6, 7, 8, 9)), newTags2)
    val newTags1 = ata(sentence, startTags, mockTagdict)
    assertEquals(Vector(Set(1, 2, 3), Set(3, 4, 5, 6, 7, 8, 9), Set(6), Set(7, 8), Set(5, 6), Set(9), Set(3, 4, 5, 6, 7, 8, 9)), newTags1)
  }

  @Test
  def test_StandardTagDictAndFwdBkdAdditionalTagAdder {
    throw new NotImplementedError("Test not written")
  }

}
