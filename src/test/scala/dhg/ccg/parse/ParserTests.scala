package dhg.ccg.parse

import org.junit.Test
import dhg.util._
import org.junit.Assert._
import dhg.ccg.cat._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.parse.pcfg.CfgGuideChart
import dhg.ccg.parse.pcfg.SimpleCfgGuideChartBuilder
import dhg.ccg.parse.pcfg.UnaryProd
import dhg.ccg.parse.pcfg.TermProd
import dhg.ccg.parse.pcfg.BinaryProd
import dhg.ccg.parse.pcfg.GuideChartEntry
import dhg.ccg.parse.pcfg.BinaryGuideChartEntry
import dhg.ccg.parse.pcfg.TermGuideChartEntry
import dhg.ccg.parse.pcfg.UnaryGuideChartEntry
import scala.collection.immutable.ListMap
import dhg.ccg.parse.pcfg.CfgGuideChartBuilder
import dhg.ccg.tagdict.TagDictionary
import dhg.gfl.FudgSentence

class ParserTests {

  val S = cat"S".asInstanceOf[AtomCat]
  val NP = cat"NP".asInstanceOf[AtomCat]
  val N = cat"N".asInstanceOf[AtomCat]
  val PP = cat"PP".asInstanceOf[AtomCat]
  val STA = cat"<S>"
  val END = cat"<E>"

  @Test
  def test_Parser_parse {
    type Word = String
    type Tag = Cat

    val mockSentence = Vector[Word]("junk")
    val mockTagdict = new TagDictionary[Tag] {
      def allWords: Set[Word] = ???; def allTags: Set[Tag] = ???
      def startWord: Word = ???; def startTag: Tag = ???; def endWord: Word = ???; def endTag: Tag = ???
      def excludedTags: Set[Tag] = ???

      def apply(w: Word): Set[Tag] = ???

      def reversed: Map[Tag, Set[Word]] = ???

      def entries: Map[Word, Set[Tag]] = ???
      def knownWordsForTag: Map[Tag, Set[Word]] = ???

      def withWords(words: Set[Word]): TagDictionary[Tag] = ???
      def withTags(tags: Set[Tag]): TagDictionary[Tag] = ???
      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
    }
    val mockReturn = Some((CcgLeaf(N, "stuff", "FAKEPOS"), LogDouble(0.1)))

    val p = new Parser {
      def parseFromBracketed(sentence: Vector[Word], fudgSentence: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[(CcgTree, LogDouble)] = {
        assertSame(mockSentence, sentence)
        assertSame(mockTagdict, tagdict)
        assertTrue(fudgSentence.map(_.brackets).getOrElse(Set.empty).isEmpty)
        mockReturn
      }
    }
    assertSame(mockReturn, p.parse(mockSentence, mockTagdict))
  }

  @Test
  def test_GuideChartParserAdapter {
    throw new NotImplementedError("Test not written")

    //    type Word = String
    //    type Tag = Symbol
    //
    //    val mockSentence = Vector[Word]("junk")
    //    val mockBrackets = Vector[(Int, Int)]((1, 3))
    //    val mockTagdict = new TagDictionary[Tag] {
    //      def allWords: Set[Word] = ???; def allTags: Set[Tag] = ???
    //      def startWord: Word = ???; def startTag: Tag = ???; def endWord: Word = ???; def endTag: Tag = ???
    //      def excludedTags: Set[Tag] = ???
    //
    //      def apply(w: Word): Set[Tag] = ???
    //
    //      def reversed: Map[Tag, Set[Word]] = ???
    //
    //      def entries: Map[Word, Set[Tag]] = ???
    //      def knownWordsForTag: Map[Tag, Set[Word]] = ???
    //
    //      def withWords(words: Set[Word]): TagDictionary[Tag] = ???
    //      def withTags(tags: Set[Tag]): TagDictionary[Tag] = ???
    //      def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
    //    }
    //
    //    val gcpa = new GuideChartParserAdapter(
    //      guideChartBuilder = new CfgGuideChartBuilder {
    //        def buildFromSupertagSetSentence(supertagSetSentence: Vector[(Word, Set[Cat])], fudgSentence: FudgSentence, tagdict: TagDictionary[Cat]): Option[CfgGuideChart] = {
    //          assertSame(
    //        }
    //      },
    //      guideChartParser = new GuideChartParser {
    //
    //      })
    //    val r1 = gcpa.parseFromBracketed(mockSentence, mockBrackets, mockTagdict)
    //
    //    //guideChartBuilder.build(sentence, brackets, tagdict).flatMap(guideChartParser.parseAndProbFromGuideChart)
  }

  @Test
  def test_KBestGuideChartParser_parseAndProbFromGuideChart {
    type Word = String
    val mockGuideChart = CfgGuideChart(Vector.empty, Vector.empty)
    class MockAbstractKBestGuideChartParser(r: Vector[(CcgTree, LogDouble)]) extends AbstractKBestGuideChartParser {
      def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int): Vector[(CcgTree, LogDouble)] = {
        assertSame(mockGuideChart, guideChart)
        assertEquals(1, k)
        r
      }
    }

    val T1 = (CcgLeaf(N, "1", "FAKEPOS"), LogDouble(0.1))
    val T2 = (CcgLeaf(N, "2", "FAKEPOS"), LogDouble(0.2))

    val akbgcp1 = new MockAbstractKBestGuideChartParser(Vector(T1))
    val r1 = akbgcp1.parseAndProbFromGuideChart(mockGuideChart)
    assertTrue(r1.isDefined)
    assertSame(T1, r1.get)

    val akbgcp2 = new MockAbstractKBestGuideChartParser(Vector(T2, T1))
    val r2 = akbgcp2.parseAndProbFromGuideChart(mockGuideChart)
    assertTrue(r2.isDefined)
    assertSame(T2, r2.get)

    val akbgcp3 = new MockAbstractKBestGuideChartParser(Vector())
    val r3 = akbgcp3.parseAndProbFromGuideChart(mockGuideChart)
    assertTrue(r3.isEmpty)
  }

  @Test
  def test_WeightedKBestGuideChartParser_parseAndProbKBestFromGuideChart {
    type Word = String

    val mockGuideChart = CfgGuideChart(Vector.empty, Vector(Vector.empty, Vector.empty, Vector.empty))
    val mockParseVector: Vector[(CcgTree, LogDouble)] = Vector((CcgLeaf(cat"A", "ugh", "FAKEPOS") -> LogDouble(0.1)), (CcgLeaf(cat"B", "blah", "FAKEPOS") -> LogDouble(0.3)))

    val mockParser = new WeightedKBestGuideChartParser {
      def parseAndProbKBestWithWeightsFromGuideChart(guideChart: CfgGuideChart, us: Vector[Vector[Map[Cat, LogDouble]]], k: Int): Vector[(CcgTree, LogDouble)] = {
        assertSame(mockGuideChart, guideChart)
        assertEquals(Vector(
          Vector(Map.empty, Map.empty, Map.empty, Map.empty),
          Vector(Map.empty, Map.empty, Map.empty, Map.empty),
          Vector(Map.empty, Map.empty, Map.empty, Map.empty)), us)
        assertEquals(5, k)
        mockParseVector
      }
    }

    assertSame(mockParseVector, mockParser.parseAndProbKBestFromGuideChart(mockGuideChart, 5))
  }

  @Test
  def test_NumPossibleParsesDelegatingKBestGuideChartParser_parseAndProbKBestFromGuideChart {
    type Word = String

    val mockExactParserResult = Vector[(CcgTree, LogDouble)]((CcgLeaf(S, "something", "FAKEPOS"), LogDouble(1.0)))
    val mockApproxParserResult = Vector[(CcgTree, LogDouble)]((CcgLeaf(N, "another thing", "FAKEPOS"), LogDouble(2.0)))

    //    val sentence = "dogs run".split("\\s+").toVector
    //    val tagdict = SimpleTagDictionary[Cat](Map(
    //      "dogs" -> Set(N, NP),
    //      "run" -> Set(S \ NP, S \ N)),
    //      "<S>", STA, "<E>", END)
    //    val builder = new SimpleCfgGuideChartBuilder(Vector(FA, BA, N2NP))
    //    val Some(table) = builder.build(sentence, Vector.empty, tagdict)
    //    table.draw()
    //    println(table.repr)
    //    println(table.numPossibleParses)
    //  +-------------+-------------+
    //  |             | (0,1)       |
    //  | N -> 1      | S -> 3      |
    //  | NP -> 2     |             |
    //  +-------------+-------------+
    //  |             |             |
    //  |             | (S\NP) -> 1 |
    //  |             | (S\N) -> 1  |
    //  +-------------+-------------+

    val mockGuideChart = CfgGuideChart(Vector.empty, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(ListMap(), ListMap(N -> Set(TermGuideChartEntry(TermProd("dogs"))), NP -> Set(TermGuideChartEntry(TermProd("dogs")), UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(1, BinaryProd(NP, (S \ NP))), BinaryGuideChartEntry(1, BinaryProd(N, (S \ N)))))),
      Vector(ListMap(), ListMap(), ListMap((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))), (S \ N) -> Set(TermGuideChartEntry(TermProd("run")))))))

    val mockExactParser = new AbstractKBestGuideChartParser {
      def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int) = {
        assertSame(mockGuideChart, guideChart)
        assertEquals(5, k)
        mockExactParserResult
      }
    }
    val mockApproxParser = new AbstractKBestGuideChartParser {
      def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int) = {
        assertSame(mockGuideChart, guideChart)
        assertEquals(5, k)
        mockApproxParserResult
      }
    }

    val r4 = new NumPossibleParsesDelegatingKBestGuideChartParser(
      mockExactParser, mockApproxParser, maxExactPossibleParseCount = 4).parseAndProbKBestFromGuideChart(mockGuideChart, 5)
    assertSame(mockExactParserResult, r4)

    val r3 = new NumPossibleParsesDelegatingKBestGuideChartParser(
      mockExactParser, mockApproxParser, maxExactPossibleParseCount = 3).parseAndProbKBestFromGuideChart(mockGuideChart, 5)
    assertSame(mockExactParserResult, r3)

    val r2 = new NumPossibleParsesDelegatingKBestGuideChartParser(
      mockExactParser, mockApproxParser, maxExactPossibleParseCount = 2).parseAndProbKBestFromGuideChart(mockGuideChart, 5)
    assertSame(mockApproxParserResult, r2)
  }

}
