package dhg.ccg.parse.pcfg

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.util._
import scala.collection.immutable.ListMap
import dhg.ccg.tagdict.TagDictionary
import dhg.gfl._
import dhg.ccg.util.SimpleIndexer
import dhg.ccg.util.Indexer
import dhg.ccg.util.IndirectSparseVec
import scala.collection.immutable.BitSet

class CfgGuideChartITests {

  val S = cat"S".asInstanceOf[AtomCat]
  val NP = cat"NP".asInstanceOf[AtomCat]
  val N = cat"N".asInstanceOf[AtomCat]
  val A = cat"A".asInstanceOf[AtomCat]
  val B = cat"B".asInstanceOf[AtomCat]
  val C = cat"C".asInstanceOf[AtomCat]
  val D = cat"D".asInstanceOf[AtomCat]
  val E = cat"E".asInstanceOf[AtomCat]
  val F = cat"F".asInstanceOf[AtomCat]
  val X = cat"X".asInstanceOf[AtomCat]
  val Y = cat"Y".asInstanceOf[AtomCat]
  val Comma = cat",".asInstanceOf[PuncCat]

  @Test
  def i_test_CfgGuideChart {
    val catIndexer = SimpleIndexer(Vector(A, NP / N, NP, S, N, NP \ A, S \ NP))
    val wordIndexer = SimpleIndexer(Vector("the", "dogs", "run"))

    //  +--------------------+--------------------+--------------------+
    //  |                    | (0,1)              | (0,2)              |
    //  | A -> "the"         | NP -> 1:[A (NP\A)] | S -> 2:[NP (S\NP)] |
    //  | (NP/N) -> "the"    |       1:[(NP/N) N] |                    |
    //  +--------------------+--------------------+--------------------+
    //  |                    | NP -> N            | (1,2)              |
    //  |                    | N -> "dogs"        |                    |
    //  |                    | (NP\A) -> "dogs"   |                    |
    //  +--------------------+--------------------+--------------------+
    //  |                    |                    |                    |
    //  |                    |                    | (S\NP) -> "run"    |
    //  |                    |                    |                    |
    //  +--------------------+--------------------+--------------------+
    val t0 = Array[(Int, Array[GuideChartEntryI])](catIndexer(A) -> Array(TermGuideChartEntryI(wordIndexer("the"))), catIndexer(NP / N) -> Array(TermGuideChartEntryI(wordIndexer("the"))))
    val t1 = Array[(Int, Array[GuideChartEntryI])](catIndexer(N) -> Array(TermGuideChartEntryI(wordIndexer("dogs"))), catIndexer(NP) -> Array(UnaryGuideChartEntryI(catIndexer(N))), catIndexer(NP \ A) -> Array(TermGuideChartEntryI(wordIndexer("dogs"))))
    val t2 = Array[(Int, Array[GuideChartEntryI])](catIndexer(S \ NP) -> Array(TermGuideChartEntryI(wordIndexer("run"))))
    val v02 = Array[(Int, Array[GuideChartEntryI])](catIndexer(NP) -> Array(BinaryGuideChartEntryI(1, catIndexer(A), catIndexer(NP \ A)), BinaryGuideChartEntryI(1, catIndexer(NP / N), catIndexer(N))))
    val v03 = Array[(Int, Array[GuideChartEntryI])](catIndexer(S) -> Array(BinaryGuideChartEntryI(2, catIndexer(NP), catIndexer(S \ NP))))
    val mockMatrix = Array[Array[Array[(Int, Array[GuideChartEntryI])]]](
      Array(t0, v02, v03),
      Array(t1, Array()),
      Array(t2))
    val gc = CfgGuideChartI("the dogs run".split("\\s+").map(wordIndexer), new Chart(mockMatrix, 3))
    //gc.draw()

    assertSame(mockMatrix(0)(0), gc(0, 1))
    assertSame(mockMatrix(0)(1), gc(0, 2))
    assertSame(mockMatrix(0)(2), gc(0, 3))
    assertSame(mockMatrix(1)(0), gc(1, 2))
    assertSame(mockMatrix(1)(1), gc(1, 3))
    assertSame(mockMatrix(2)(0), gc(2, 3))

    assertEquals(3, gc.length)

    assertSame(v03, gc.root)
    assertEquals(3, gc.terminals.length)
    assertSame(t0, gc.terminals(0))
    assertSame(t1, gc.terminals(1))
    assertSame(t2, gc.terminals(2))
    val words: Array[Int] = gc.words
    assertEquals(Vector("the", "dogs", "run").map(wordIndexer), words.toVector)
    val supertagSets = gc.supertagSets
    assertEquals(Vector(Vector(A, NP / N), Vector(N, NP \ A), Vector(S \ NP)).map(_.map(catIndexer).sorted), supertagSets.toVector.map(_.toVector))
    val wordSupertagsets = gc.wordSupertagsets
    assertEquals(Vector(wordIndexer("the") -> Vector(A, NP / N).map(catIndexer), wordIndexer("dogs") -> Vector(N, NP \ A).map(catIndexer), wordIndexer("run") -> Vector(S \ NP).map(catIndexer)), wordSupertagsets.toVector.map { case (w, ts) => w -> ts.toVector })

    val bun: IndexedSeq[(Int, Int, Array[(Int, Array[GuideChartEntryI])])] = gc.bottomUpNodes
    assertEquals(Vector(
      (0, 1, t0.map { case (t, es) => t -> es.toVector }.toVector), (1, 2, t1.map { case (t, es) => t -> es.toVector }.toVector), (2, 3, t2.map { case (t, es) => t -> es.toVector }.toVector),
      (0, 2, v02.map { case (t, es) => t -> es.toVector }.toVector), (1, 3, Vector.empty),
      (0, 3, v03.map { case (t, es) => t -> es.toVector }.toVector)),
      bun.toVector.map { case (i, j, a) => (i, j, a.map { case (t, es) => t -> es.toVector }.toVector) })

    val tdn: IndexedSeq[(Int, Int, Array[(Int, Array[GuideChartEntryI])])] = gc.topDownNodes
    assertEquals(Vector(
      (0, 3, v03.map { case (t, es) => t -> es.toVector }.toVector),
      (0, 2, v02.map { case (t, es) => t -> es.toVector }.toVector), (1, 3, Vector()),
      (0, 1, t0.map { case (t, es) => t -> es.toVector }.toVector), (1, 2, t1.map { case (t, es) => t -> es.toVector }.toVector), (2, 3, t2.map { case (t, es) => t -> es.toVector }.toVector)),
      tdn.toVector.map { case (i, j, a) => (i, j, a.map { case (t, es) => t -> es.toVector }.toVector) })
  }

  //

  @Test
  def x_test_SimpleCfgGuideChartBuilder {

    val tagdict = SimpleTagDictionary[Cat](Map(
      "the" -> Set(N / N, NP / N, A),
      "dogs" -> Set(NP, N, B, NP \ A),
      "run" -> Set(S \ NP, S)),
      "<S>", cat"<S>", "<E>", cat"<E>")
    val rules = Set[CcgRule](FA, BA)

    val catIndexer = CatIndexer(tagdict.allTags, rules)
    val numCats = catIndexer.size
    val wordIndexer = SimpleIndexer(tagdict.allWords)

    val allCats = BitSet.empty ++ catIndexer.indices
    val (binaryRulesI, unaryRulesI) = CcgRuleI.makeIndexedRules(rules, catIndexer)
    val tagdictI = IndirectSparseVec((tagdict.entries -- Set(tagdict.startWord, tagdict.endWord)).map { case (word, cats) => (wordIndexer(word), BitSet.empty ++ cats.map(catIndexer)) }, numCats)

    val builderI = new SimpleCfgGuideChartBuilderI(binaryRulesI, unaryRulesI, allCats, allowTerminalDeletion = false)

    //val builder = new SimpleCfgGuideChartBuilder(Vector(FA, BA))
    val sentence = Vector("the", "dogs", "run").map(wordIndexer)
    val Some(tableI) = builderI.build(sentence, None, tagdictI, allCats)
    val table = CfgGuideChartI.from(tableI, catIndexer, wordIndexer)

    //    table.draw()
    //  +------------------------+------------------------+------------------------+
    //  |                        | (0,1)                  | (0,2)                  |
    //  | A -> {(0,the)}         | NP -> {(1,[A (NP\A)]), | S -> {(2,[NP (S\NP)])} |
    //  | (NP/N) -> {(0,the)}    |        (1,[(NP/N) N])} |                        |
    //  +------------------------+------------------------+------------------------+
    //  |                        |                        | (1,2)                  |
    //  |                        | N -> {(1,dogs)}        |                        |
    //  |                        | (NP\A) -> {(1,dogs)}   |                        |
    //  +------------------------+------------------------+------------------------+
    //  |                        |                        |                        |
    //  |                        |                        | (S\NP) -> {(2,run)}    |
    //  |                        |                        |                        |
    //  +------------------------+------------------------+------------------------+

    def assertEmpty(m: Map[_, _]) { assertTrue(m == null || m.isEmpty) }

    assertEmpty(table(0, 0))
    assertEquals(Map(A -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the")))), table(0, 1))
    assertEquals(Map(NP -> Set(BinaryGuideChartEntry(1, BinaryProd(A, (NP \ A))), BinaryGuideChartEntry(1, BinaryProd((NP / N), N)))), table(0, 2))
    assertEquals(Map(S -> Set(BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP))))), table(0, 3))

    assertEmpty(table(1, 0))
    assertEmpty(table(1, 1))
    assertEquals(Map(N -> Set(TermGuideChartEntry(TermProd("dogs"))), (NP \ A) -> Set(TermGuideChartEntry(TermProd("dogs")))), table(1, 2))
    assertEquals(Map(), table(1, 3))

    assertEmpty(table(2, 0))
    assertEmpty(table(2, 1))
    assertEmpty(table(2, 2))
    assertEquals(Map((S \ NP) -> Set(TermGuideChartEntry(TermProd("run")))), table(2, 3))
  }

  @Test
  def x_test_SimpleCfgGuideChartBuilder_withUnary {

    //  +------------------------------+------------------------------+------------------------------+
    //  |                              | (0,1)                        | (0,2)                        |
    //  | NP -> N                      | NP -> 1:[(NP/N) N]           | S -> 2:[NP (S\NP)]           |
    //  | (N/N) -> "old"               |       N                      |      1:[NP (S\NP)]           |
    //  | N -> "old"                   | N -> 1:[(N/N) N]             |                              |
    //  | (NP/N) -> "old"              |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+
    //  |                              |                              | (1,2)                        |
    //  |                              | ((S\NP)/NP) -> "man"         | (S\NP) -> 2:[((S\NP)/NP) NP] |
    //  |                              | N -> "man"                   |                              |
    //  |                              |                              |                              |
    //  |                              |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+
    //  |                              |                              |                              |
    //  |                              |                              | (S\NP) -> "ships"            |
    //  |                              |                              | NP -> N                      |
    //  |                              |                              | N -> "ships"                 |
    //  |                              |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+

    val tagdict1 = SimpleTagDictionary[Cat](Map(
      "old" -> Set(N, N / N, NP / N),
      "man" -> Set(N, (S \ NP) / NP),
      "ships" -> Set(N, S \ NP)),
      "<S>", cat"<S>", "<E>", cat"<E>")
    val rules = Set[CcgRule](FA, BA, N2NP)

    val catIndexer = CatIndexer(tagdict1.allTags, rules)
    val numCats = catIndexer.size
    val wordIndexer = SimpleIndexer(tagdict1.allWords)

    val allCats = BitSet.empty ++ catIndexer.indices
    val (binaryRulesI, unaryRulesI) = CcgRuleI.makeIndexedRules(rules, catIndexer)
    val tagdictI = IndirectSparseVec((tagdict1.entries -- Set(tagdict1.startWord, tagdict1.endWord)).map { case (word, cats) => (wordIndexer(word), BitSet.empty ++ cats.map(catIndexer)) }, numCats)

    val builderI = new SimpleCfgGuideChartBuilderI(binaryRulesI, unaryRulesI, allCats, allowTerminalDeletion = false)

    //val builder = new SimpleCfgGuideChartBuilder(Vector(FA, BA))
    val sentence = Vector("old", "man", "ships").map(wordIndexer)
    val Some(tableI) = builderI.build(sentence, None, tagdictI, allCats)
    val table1 = CfgGuideChartI.from(tableI, catIndexer, wordIndexer)

    def assertEmpty(m: Map[_, _]) { assertTrue(m == null || m.isEmpty) }

    assertEmpty(table1(0)(0))
    assertEquals(Map(
      N -> Set(TermGuideChartEntry(TermProd("old"))),
      (N / N) -> Set(TermGuideChartEntry(TermProd("old"))),
      (NP / N) -> Set(TermGuideChartEntry(TermProd("old"))),
      NP -> Set(UnaryGuideChartEntry(UnaryProd(N)))),
      table1(0)(1))
    assertEquals(Map(
      N -> Set(BinaryGuideChartEntry(1, BinaryProd(N / N, N))),
      NP -> Set(BinaryGuideChartEntry(1, BinaryProd(NP / N, N)), UnaryGuideChartEntry(UnaryProd(N)))),
      table1(0)(2))
    assertEquals(Map(
      S -> Set(
        BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP))),
        BinaryGuideChartEntry(1, BinaryProd(NP, (S \ NP))))),
      table1(0)(3))

    assertEmpty(table1(1)(0))
    assertEmpty(table1(1)(1))
    assertEquals(Map(
      N -> Set(TermGuideChartEntry(TermProd("man"))),
      // NP -> Set(UnaryGuideChartEntry(UnaryProd(N))),
      ((S \ NP) / NP) -> Set(TermGuideChartEntry(TermProd("man")))),
      table1(1)(2))
    assertEquals(Map(
      // S -> Set(BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))),
      (S \ NP) -> Set(BinaryGuideChartEntry(2, BinaryProd((S \ NP) / NP, NP)))),
      table1(1)(3))

    assertEmpty(table1(2)(0))
    assertEmpty(table1(2)(1))
    assertEmpty(table1(2)(2))
    assertEquals(Map(
      N -> Set(TermGuideChartEntry(TermProd("ships"))),
      NP -> Set(UnaryGuideChartEntry(UnaryProd(N))),
      (S \ NP) -> Set(TermGuideChartEntry(TermProd("ships")))),
      table1(2)(3))
  }

  @Test
  def test_SimpleCfgGuideChartBuilder_withBracketings {
    def assertEmpty(m: Map[_, _]) { assertTrue(m.isEmpty) }

    val mockTagdict = SimpleTagDictionary[Cat](Map(
      "the" -> Set(NP / N, N / N),
      "old" -> Set(N, N / N, NP / N),
      "man" -> Set(N, (S \ NP) / NP),
      "ships" -> Set(N, S \ NP),
      "." -> Set(S \ S, NP \ NP)),
      "<S>", cat"<S>", "<E>", cat"<E>")
    val builder = new SimpleCfgGuideChartBuilder(Vector(FA, BA, N2NP), allowTerminalDeletion = false)

    //  +------------------------------+------------------------------+------------------------------+------------------------------+------------------------------+
    //  |                              | (0,1)                        | (0,2)                        | (0,3)                        | (0,4)                        |
    //  | (N/N) -> "the"               | N -> 1:[(N/N) N]             | N -> 1:[(N/N) N]             | S -> 3:[NP (S\NP)]           | S -> 2:[NP (S\NP)]           |
    //  | (NP/N) -> "the"              | NP -> 1:[(NP/N) N]           | NP -> 1:[(NP/N) N]           |      2:[NP (S\NP)]           |      4:[S (S\S)]             |
    //  |                              |       N                      |       N                      |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+------------------------------+------------------------------+
    //  |                              |                              | (1,2)                        | (1,3)                        | (1,4)                        |
    //  |                              | (N/N) -> "old"               | N -> 2:[(N/N) N]             |                              |                              |
    //  |                              | N -> "old"                   |                              |                              |                              |
    //  |                              |                              |                              |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+------------------------------+------------------------------+
    //  |                              |                              |                              | (2,3)                        | (2,4)                        |
    //  |                              |                              | ((S\NP)/NP) -> "man"         | (S\NP) -> 3:[((S\NP)/NP) NP] | (S\NP) -> 3:[((S\NP)/NP) NP] |
    //  |                              |                              | N -> "man"                   |                              |                              |
    //  |                              |                              |                              |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+------------------------------+------------------------------+
    //  |                              |                              |                              |                              | (3,4)                        |
    //  |                              |                              |                              | (S\NP) -> "ships"            | NP -> 4:[NP (NP\NP)]         |
    //  |                              |                              |                              | N -> "ships"                 |                              |
    //  |                              |                              |                              | NP -> N                      |                              |
    //  +------------------------------+------------------------------+------------------------------+------------------------------+------------------------------+
    //  |                              |                              |                              |                              |                              |
    //  |                              |                              |                              |                              | (S\S) -> "."                 |
    //  |                              |                              |                              |                              | (NP\NP) -> "."               |
    //  |                              |                              |                              |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+------------------------------+------------------------------+

    val brackets0: Option[FudgSentence] = None
    val oTable0 = builder.build(Vector("the", "old", "man", "ships", "."), brackets0, mockTagdict)
    assertTrue(oTable0.isDefined)
    val Some(table0) = oTable0
    //table0.draw()

    assertEmpty(table0(0)(0))
    assertEquals(ListMap(
      (N / N) -> Set(TermGuideChartEntry(TermProd("the"))),
      (NP / N) -> Set(TermGuideChartEntry(TermProd("the")))),
      table0(0)(1))
    assertEquals(ListMap(
      N -> Set(BinaryGuideChartEntry(1, BinaryProd(N / N, N))),
      NP -> Set(BinaryGuideChartEntry(1, BinaryProd(NP / N, N)), UnaryGuideChartEntry(UnaryProd(N)))),
      table0(0)(2))
    assertEquals(ListMap(
      N -> Set(BinaryGuideChartEntry(1, BinaryProd(N / N, N))),
      NP -> Set(BinaryGuideChartEntry(1, BinaryProd(NP / N, N)), UnaryGuideChartEntry(UnaryProd(N)))),
      table0(0)(3))
    assertEquals(ListMap(
      S -> Set(
        BinaryGuideChartEntry(3, BinaryProd(NP, (S \ NP))),
        BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP))))),
      table0(0)(4))
    assertEquals(ListMap(
      S -> Set(
        BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP))),
        BinaryGuideChartEntry(4, BinaryProd(S, (S \ S))))),
      table0(0)(5))

    assertEmpty(table0(1)(0))
    assertEmpty(table0(1)(1))
    assertEquals(ListMap(
      N -> Set(TermGuideChartEntry(TermProd("old"))),
      (N / N) -> Set(TermGuideChartEntry(TermProd("old")))),
      table0(1)(2))
    assertEquals(ListMap(
      N -> Set(BinaryGuideChartEntry(2, BinaryProd(N / N, N)))),
      table0(1)(3))
    assertEmpty(table0(1)(4))
    assertEmpty(table0(1)(5))

    assertEmpty(table0(2)(0))
    assertEmpty(table0(2)(1))
    assertEmpty(table0(2)(2))
    assertEquals(ListMap(
      ((S \ NP) / NP) -> Set(TermGuideChartEntry(TermProd("man"))),
      N -> Set(TermGuideChartEntry(TermProd("man")))),
      table0(2)(3))
    assertEquals(ListMap(
      (S \ NP) -> Set(BinaryGuideChartEntry(3, BinaryProd((S \ NP) / NP, NP)))),
      table0(2)(4))
    assertEquals(ListMap(
      (S \ NP) -> Set(BinaryGuideChartEntry(3, BinaryProd((S \ NP) / NP, NP)))),
      table0(2)(5))

    assertEmpty(table0(3)(0))
    assertEmpty(table0(3)(1))
    assertEmpty(table0(3)(2))
    assertEmpty(table0(3)(3))
    assertEquals(ListMap(
      (S \ NP) -> Set(TermGuideChartEntry(TermProd("ships"))),
      N -> Set(TermGuideChartEntry(TermProd("ships"))),
      NP -> Set(UnaryGuideChartEntry(UnaryProd(N)))),
      table0(3)(4))
    assertEquals(ListMap(
      NP -> Set(BinaryGuideChartEntry(4, BinaryProd(NP, NP \ NP)))),
      table0(3)(5))

    assertEmpty(table0(4)(0))
    assertEmpty(table0(4)(1))
    assertEmpty(table0(4)(2))
    assertEmpty(table0(4)(3))
    assertEmpty(table0(4)(4))
    assertEquals(ListMap(
      (S \ S) -> Set(TermGuideChartEntry(TermProd("."))),
      (NP \ NP) -> Set(TermGuideChartEntry(TermProd(".")))),
      table0(4)(5))

    //  +--------------------+--------------------+--------------------+--------------------+--------------------+
    //  |                    | (0,1)              | (0,2)              | (0,3)              | (0,4)              |
    //  | (N/N) -> "the"     |                    | N -> 1:[(N/N) N]   | S -> 3:[NP (S\NP)] | S -> 4:[S (S\S)]   |
    //  | (NP/N) -> "the"    |         X          | NP -> 1:[(NP/N) N] |                    |                    |
    //  |                    |                    |       N            |                    |                    |
    //  +--------------------+--------------------+--------------------+--------------------+--------------------+
    //  |                    |                    | (1,2)              | (1,3)              | (1,4)              |
    //  |                    | (N/N) -> "old"     | N -> 2:[(N/N) N]   |                    |                    |
    //  |                    |                    |                    |                    |          X         |
    //  |                    |                    |                    |                    |                    |
    //  +--------------------+--------------------+--------------------+--------------------+--------------------+
    //  |                    |                    |                    | (2,3)              | (2,4)              |
    //  |                    |                    | N -> "man"         |                    |                    |
    //  |                    |                    |                    |         X          |          X         |
    //  |                    |                    |                    |                    |                    |
    //  +--------------------+--------------------+--------------------+--------------------+--------------------+
    //  |                    |                    |                    |                    | (3,4)              |
    //  |                    |                    |                    | (S\NP) -> "ships"  |                    |
    //  |                    |                    |                    |                    |          X         |
    //  |                    |                    |                    |                    |                    |
    //  +--------------------+--------------------+--------------------+--------------------+--------------------+
    //  |                    |                    |                    |                    |                    |
    //  |                    |                    |                    |                    | (S\S) -> "."       |
    //  |                    |                    |                    |                    |                    |
    //  |                    |                    |                    |                    |                    |
    //  +--------------------+--------------------+--------------------+--------------------+--------------------+
    val brackets1 = Some(BracketOnlyFudgSentence(Vector((1, 3), (0, 4))))
    val oTable1 = builder.build(Vector("the", "old", "man", "ships", "."), brackets1, mockTagdict)
    assertTrue(oTable1.isDefined)
    val Some(table1) = oTable1
    //table1.draw()

    assertEmpty(table1(0)(0))
    assertEquals(ListMap(
      (N / N) -> Set(TermGuideChartEntry(TermProd("the"))),
      (NP / N) -> Set(TermGuideChartEntry(TermProd("the")))),
      table1(0)(1))
    assertEmpty(table1(0)(2))
    assertEquals(ListMap(
      N -> Set(BinaryGuideChartEntry(1, BinaryProd(N / N, N))),
      NP -> Set(BinaryGuideChartEntry(1, BinaryProd(NP / N, N)), UnaryGuideChartEntry(UnaryProd(N)))),
      table1(0)(3))
    assertEquals(ListMap(
      S -> Set(
        BinaryGuideChartEntry(3, BinaryProd(NP, (S \ NP))))),
      table1(0)(4))
    assertEquals(ListMap(
      S -> Set(
        BinaryGuideChartEntry(4, BinaryProd(S, (S \ S))))),
      table1(0)(5))

    assertEmpty(table1(1)(0))
    assertEmpty(table1(1)(1))
    assertEquals(ListMap(
      (N / N) -> Set(TermGuideChartEntry(TermProd("old")))),
      table1(1)(2))
    assertEquals(ListMap(
      N -> Set(BinaryGuideChartEntry(2, BinaryProd(N / N, N)))),
      table1(1)(3))
    assertEmpty(table1(1)(4))
    assertEmpty(table1(1)(5))

    assertEmpty(table1(2)(0))
    assertEmpty(table1(2)(1))
    assertEmpty(table1(2)(2))
    assertEquals(ListMap(
      N -> Set(TermGuideChartEntry(TermProd("man")))),
      table1(2)(3))
    assertEmpty(table1(2)(4))
    assertEmpty(table1(2)(5))

    assertEmpty(table1(3)(0))
    assertEmpty(table1(3)(1))
    assertEmpty(table1(3)(2))
    assertEmpty(table1(3)(3))
    assertEquals(ListMap(
      (S \ NP) -> Set(TermGuideChartEntry(TermProd("ships")))),
      table1(3)(4))
    assertEmpty(table1(3)(5))

    assertEmpty(table1(4)(0))
    assertEmpty(table1(4)(1))
    assertEmpty(table1(4)(2))
    assertEmpty(table1(4)(3))
    assertEmpty(table1(4)(4))
    assertEquals(ListMap(
      (S \ S) -> Set(TermGuideChartEntry(TermProd(".")))),
      table1(4)(5))

    //  +------------------------------+------------------------------+------------------------------+------------------------------+------------------------------+
    //  |                              | (0,1)                        | (0,2)                        | (0,3)                        | (0,4)                        |
    //  | (N/N) -> "the"               | N -> 1:[(N/N) N]             |                              | S -> 2:[NP (S\NP)]           | S -> 4:[S (S\S)]             |
    //  | (NP/N) -> "the"              | NP -> 1:[(NP/N) N]           |                              |                              |                              |
    //  |                              |       N                      |                              |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+------------------------------+------------------------------+
    //  |                              |                              | (1,2)                        | (1,3)                        | (1,4)                        |
    //  |                              | N -> "old"                   |                              |                              |                              |
    //  |                              |                              |              X               |              X               |               X              |
    //  |                              |                              |                              |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+------------------------------+------------------------------+
    //  |                              |                              |                              | (2,3)                        | (2,4)                        |
    //  |                              |                              | ((S\NP)/NP) -> "man"         | (S\NP) -> 3:[((S\NP)/NP) NP] |                              |
    //  |                              |                              |                              |                              |               X              |
    //  |                              |                              |                              |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+------------------------------+------------------------------+
    //  |                              |                              |                              |                              | (3,4)                        |
    //  |                              |                              |                              | N -> "ships"                 |                              |
    //  |                              |                              |                              | NP -> N                      |               X              |
    //  |                              |                              |                              |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+------------------------------+------------------------------+
    //  |                              |                              |                              |                              |                              |
    //  |                              |                              |                              |                              | (S\S) -> "."                 |
    //  |                              |                              |                              |                              |                              |
    //  |                              |                              |                              |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+------------------------------+------------------------------+

    val brackets2 = Some(BracketOnlyFudgSentence(Vector((0, 2), (0, 4))))
    val oTable2 = builder.build(Vector("the", "old", "man", "ships", "."), brackets2, mockTagdict)
    assertTrue(oTable2.isDefined)
    val Some(table2) = oTable2
    //table2.draw()

    assertEmpty(table2(0)(0))
    assertEquals(ListMap(
      (N / N) -> Set(TermGuideChartEntry(TermProd("the"))),
      (NP / N) -> Set(TermGuideChartEntry(TermProd("the")))),
      table2(0)(1))
    assertEquals(ListMap(
      N -> Set(BinaryGuideChartEntry(1, BinaryProd(N / N, N))),
      NP -> Set(BinaryGuideChartEntry(1, BinaryProd(NP / N, N)), UnaryGuideChartEntry(UnaryProd(N)))),
      table2(0)(2))
    assertEmpty(table2(0)(3))
    assertEquals(ListMap(
      S -> Set(
        BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP))))),
      table2(0)(4))
    assertEquals(ListMap(
      S -> Set(
        BinaryGuideChartEntry(4, BinaryProd(S, (S \ S))))),
      table2(0)(5))

    assertEmpty(table2(1)(0))
    assertEmpty(table2(1)(1))
    assertEquals(ListMap(
      N -> Set(TermGuideChartEntry(TermProd("old")))),
      table2(1)(2))
    assertEmpty(table2(1)(3))
    assertEmpty(table2(1)(4))
    assertEmpty(table2(1)(5))

    assertEmpty(table2(2)(0))
    assertEmpty(table2(2)(1))
    assertEmpty(table2(2)(2))
    assertEquals(ListMap(
      ((S \ NP) / NP) -> Set(TermGuideChartEntry(TermProd("man")))),
      table2(2)(3))
    assertEquals(ListMap(
      (S \ NP) -> Set(BinaryGuideChartEntry(3, BinaryProd((S \ NP) / NP, NP)))),
      table2(2)(4))
    assertEmpty(table2(2)(5))

    assertEmpty(table2(3)(0))
    assertEmpty(table2(3)(1))
    assertEmpty(table2(3)(2))
    assertEmpty(table2(3)(3))
    assertEquals(ListMap(
      N -> Set(TermGuideChartEntry(TermProd("ships"))),
      NP -> Set(UnaryGuideChartEntry(UnaryProd(N)))),
      table2(3)(4))
    assertEmpty(table2(3)(5))

    assertEmpty(table2(4)(0))
    assertEmpty(table2(4)(1))
    assertEmpty(table2(4)(2))
    assertEmpty(table2(4)(3))
    assertEmpty(table2(4)(4))
    assertEquals(ListMap(
      (S \ S) -> Set(TermGuideChartEntry(TermProd(".")))),
      table2(4)(5))
  }

  @Test
  def x_test_SimpleCfgGuideChartBuilder_withRootSet {

    val tagdict = SimpleTagDictionary[Cat](Map(
      "the" -> Set(N / N, NP / N, A),
      "dogs" -> Set(NP, N, B, NP \ A),
      "run" -> Set(S \ NP, S, B \ NP)),
      "<S>", cat"<S>", "<E>", cat"<E>")

    val rules = Set[CcgRule](FA, BA)
    val rootSet = Set[Cat](S)

    val catIndexer = CatIndexer(tagdict.allTags, rules)
    val numCats = catIndexer.size
    val wordIndexer = SimpleIndexer(tagdict.allWords)

    val allCats = BitSet.empty ++ catIndexer.indices
    val (binaryRulesI, unaryRulesI) = CcgRuleI.makeIndexedRules(rules, catIndexer)
    val tagdictI = IndirectSparseVec((tagdict.entries -- Set(tagdict.startWord, tagdict.endWord)).map { case (word, cats) => (wordIndexer(word), BitSet.empty ++ cats.map(catIndexer)) }, numCats)

    val builderI = new SimpleCfgGuideChartBuilderI(binaryRulesI, unaryRulesI, BitSet.empty ++ rootSet.map(catIndexer), allowTerminalDeletion = false)

    //val builder = new SimpleCfgGuideChartBuilder(Vector(FA, BA))
    val sentence = Vector("the", "dogs", "run").map(wordIndexer)
    val Some(tableI) = builderI.build(sentence, None, tagdictI, allCats)
    val table = CfgGuideChartI.from(tableI, catIndexer, wordIndexer)

    //    table.draw()
    //  +------------------------+------------------------+------------------------+
    //  |                        | (0,1)                  | (0,2)                  |
    //  | A -> {(0,the)}         | NP -> {(1,[A (NP\A)]), | S -> {(2,[NP (S\NP)])} |
    //  | (NP/N) -> {(0,the)}    |        (1,[(NP/N) N])} |                        |
    //  +------------------------+------------------------+------------------------+
    //  |                        |                        | (1,2)                  |
    //  |                        | N -> {(1,dogs)}        |                        |
    //  |                        | (NP\A) -> {(1,dogs)}   |                        |
    //  +------------------------+------------------------+------------------------+
    //  |                        |                        |                        |
    //  |                        |                        | (S\NP) -> {(2,run)}    |
    //  |                        |                        |                        |
    //  +------------------------+------------------------+------------------------+

    def assertEmpty(m: Map[_, _]) { assertTrue(m == null || m.isEmpty) }

    assertEmpty(table(0)(0))
    assertEquals(Map(A -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the")))), table(0)(1))
    assertEquals(Map(NP -> Set(BinaryGuideChartEntry(1, BinaryProd(A, (NP \ A))), BinaryGuideChartEntry(1, BinaryProd((NP / N), N)))), table(0)(2))
    assertEquals(Map(S -> Set(BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP))))), table(0)(3))

    assertEmpty(table(1)(0))
    assertEmpty(table(1)(1))
    assertEquals(Map(N -> Set(TermGuideChartEntry(TermProd("dogs"))), (NP \ A) -> Set(TermGuideChartEntry(TermProd("dogs")))), table(1)(2))
    assertEquals(Map(), table(1)(3))

    assertEmpty(table(2)(0))
    assertEmpty(table(2)(1))
    assertEmpty(table(2)(2))
    assertEquals(Map((S \ NP) -> Set(TermGuideChartEntry(TermProd("run")))), table(2)(3))
  }

  @Test
  def test_CfgGuideChartSerDeser_commaBugfix {
    val sd = new CfgGuideChartSerDeser()

    //  +--------------------+--------------------+--------------------+
    //  |                    | (0,1)              | (0,2)              |
    //  | (N/N) -> "the"     | N -> 1:[(N/N),,]   | S -> 2:[N (S\N)]   |
    //  | (NP/N) -> "the"    | NP -> 1:[(NP/N),,] |      2:[NP (S\NP)] |
    //  | (N/S) -> "the"     |       N            | N -> 1:[(N/S) S]   |
    //  |                    |                    | NP -> N            |
    //  +--------------------+--------------------+--------------------+
    //  |                    |                    | (1,2)              |
    //  |                    | , -> ","           | S -> 2:[,,(S\N)]   |
    //  |                    | NP -> N            |      2:[NP (S\NP)] |
    //  |                    |                    |                    |
    //  |                    |                    |                    |
    //  +--------------------+--------------------+--------------------+
    //  |                    |                    |                    |
    //  |                    |                    | (S\NP) -> "run"    |
    //  |                    |                    | (S\N) -> "run"     |
    //  |                    |                    |                    |
    //  |                    |                    |                    |
    //  +--------------------+--------------------+--------------------+
    val gc1 = CfgGuideChart("the , run".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(ListMap(), ListMap((N / N) -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the"))), (N / S) -> Set(TermGuideChartEntry(TermProd("the")))), ListMap(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), Comma))), NP -> Set(BinaryGuideChartEntry(1, BinaryProd((NP / N), Comma)), UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))), N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / S), S))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N))))),
      Vector(ListMap(), ListMap(), ListMap(Comma -> Set(TermGuideChartEntry(TermProd(","))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(Comma, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(), ListMap((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))), (S \ N) -> Set(TermGuideChartEntry(TermProd("run")))))))

    val ser1: Vector[String] = sd.ser(gc1)
    //for (x <- ser1) println(x)
    assertEquals("""length=3""", ser1(0))
    assertEquals("""(0,1) ;; (N/N) -> the ;; (NP/N) -> the ;; (N/S) -> the""", ser1(1))
    assertEquals("""(0,2) ;; N -> 1:[(N/N),<COMMA>] ;; NP -> 1:[(NP/N),<COMMA>] [N]""", ser1(2))
    assertEquals("""(0,3) ;; S -> 2:[N,(S\N)] 2:[NP,(S\NP)] ;; N -> 1:[(N/S),S] ;; NP -> [N]""", ser1(3))
    assertEquals("""(1,2) ;; , -> , ;; NP -> [N]""", ser1(4))
    assertEquals("""(1,3) ;; S -> 2:[<COMMA>,(S\N)] 2:[NP,(S\NP)]""", ser1(5))
    assertEquals("""(2,3) ;; (S\NP) -> run ;; (S\N) -> run""", ser1(6))
    val des1 = sd.deser(ser1)
    assertEquals(gc1(0, 0), des1(0, 0))
    assertEquals(gc1(0, 1), des1(0, 1))
    assertEquals(gc1(0, 2), des1(0, 2))
    assertEquals(gc1(0, 3), des1(0, 3))
    assertEquals(gc1(1, 0), des1(1, 0))
    assertEquals(gc1(1, 1), des1(1, 1))
    assertEquals(gc1(1, 2), des1(1, 2))
    assertEquals(gc1(1, 3), des1(1, 3))
    assertEquals(gc1(2, 0), des1(2, 0))
    assertEquals(gc1(2, 1), des1(2, 1))
    assertEquals(gc1(2, 2), des1(2, 2))
    assertEquals(gc1(2, 3), des1(2, 3))
    assertEquals(gc1.matrix, des1.matrix)
  }

  case class BracketOnlyFudgSentence(val b: Vector[(Int, Int)]) extends FudgSentence {
    def tokens: Vector[Token] = ???
    def nodes: Map[String, Node] = ???
    def edges: Vector[Edge] = ???
    def fudgTree: FudgTree = ???
    def brackets: Set[(Int, Int)] = b.toSet
    def token(i: Int): Token = ???
    def node(i: Int): Node = ???
    def addEdge(e: Edge): FudgSentence = ???
    def addEdge(parentIndex: Int, childIndex: Int): FudgSentence = ???
    def bracketsOnly() = ???
  }

}
