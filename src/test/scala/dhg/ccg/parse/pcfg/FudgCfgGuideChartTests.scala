package dhg.ccg.parse.pcfg

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.util._
import dhg.util.viz._
import scala.collection.immutable.ListMap
import dhg.ccg.tagdict.TagDictionary
import dhg.gfl._
import dhg.ccg.parse.dep.DepTree

class FudgCfgGuideChartTests {

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
  val Comma = cat",".asInstanceOf[AtomCat]

  @Test
  def test_SimpleFudgCfgGuideChartBuilder {

    val tagdict = SimpleTagDictionary[Cat](Map(
      "the" -> Set(N / N, NP / N, A),
      "dogs" -> Set(NP, N, B, NP \ A),
      "run" -> Set(S \ NP, S)),
      "<S>", cat"<S>", "<E>", cat"<E>")

    val builder = new SimpleFudgCfgGuideChartBuilder(Vector(FA, BA), allowTerminalDeletion = false)
    val Some((table, deptrees)) = builder.buildWithDepTreesFromSupertagSetSentence(Vector("the", "dogs", "run").mapTo(tagdict), None, tagdict)

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

    def assertEmpty(m: Map[_, _]) { assertTrue(m.isEmpty) }

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

    assertEquals(Set(
      DepTree("run", 2, cat"X", Vector(DepTree("dogs", 1, cat"X", Vector(DepTree("the", 0, cat"X", Vector.empty))))),
      DepTree("run", 2, cat"X", Vector(DepTree("the", 0, cat"X", Vector(DepTree("dogs", 1, cat"X", Vector.empty)))))), deptrees)
  }

  @Test
  def test_SimpleFudgCfgGuideChartBuilder_withUnary {

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
    val builder1 = new SimpleFudgCfgGuideChartBuilder(Vector(FA, BA, N2NP), allowTerminalDeletion = false)
    val Some((table1, dts1)) = builder1.buildWithDepTreesFromSupertagSetSentence(Vector("old", "man", "ships").mapTo(tagdict1), None, tagdict1)
    //table1.draw()

    def assertEmpty(m: Map[_, _]) { assertTrue(m.isEmpty) }

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
  def test_SimpleFudgCfgGuideChartBuilder_withBracketings {
    def assertEmpty(m: Map[_, _]) { assertTrue(m.isEmpty) }

    val mockTagdict = SimpleTagDictionary[Cat](Map(
      "the" -> Set(NP / N, N / N),
      "old" -> Set(N, N / N, NP / N),
      "man" -> Set(N, (S \ NP) / NP),
      "ships" -> Set(N, S \ NP),
      "." -> Set(S \ S, NP \ NP)),
      "<S>", cat"<S>", "<E>", cat"<E>")
    val builder = new SimpleFudgCfgGuideChartBuilder(Vector(FA, BA, N2NP), allowTerminalDeletion = false)

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
    val oTable0 = builder.buildWithDepTreesFromSupertagSetSentence(Vector("the", "old", "man", "ships", ".").mapTo(mockTagdict), brackets0, mockTagdict)
    assertTrue(oTable0.isDefined)
    val Some((table0, dts0)) = oTable0
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
    val tokens = Vector("the", "old", "man", "ships", ".")
    val fudg1 = Some(Fudg.fromGfl(tokens, """(the (old man) ships) .""").getOrElseThrow())
    //println(fudg1)
    val oTable1 = builder.buildWithDepTreesFromSupertagSetSentence(tokens.mapTo(mockTagdict), fudg1, mockTagdict)
    assertTrue(oTable1.isDefined)
    val Some((table1, dts1)) = oTable1
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

    val fudg2 = Some(Fudg.fromGfl(tokens, """((the old) man ships) .""").getOrElseThrow())
    val oTable2 = builder.buildWithDepTreesFromSupertagSetSentence(tokens.mapTo(mockTagdict), fudg2, mockTagdict)
    assertTrue(oTable2.isDefined)
    val Some((table2, dts2)) = oTable2
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
  def test_SimpleFudgCfgGuideChartBuilder_withDependencies_noUnary_toy {
    def assertEmpty(m: Map[_, _]) { assertTrue(m.isEmpty) }

    val builder = new SimpleFudgCfgGuideChartBuilder(Vector(FA, BA), allowTerminalDeletion = false)

    val tokens1 = Vector("it", "works")
    val td1 = SimpleTagDictionary[Cat](Map("the" -> Set(NP), "works" -> Set(S \ NP)), "<S>", cat"<S>", "<E>", cat"<E>")
    val fudg1 = Some(Fudg.fromGfl(tokens1, """it > works""").getOrElseThrow())
    //println(fudg1)
    //TreeViz.drawTree(fudg1.get.fudgTree)
    val oTable1 = builder.buildWithDepTreesFromSupertagSetSentence(tokens1.mapTo(td1), fudg1, td1)
    assertTrue(oTable1.isDefined)
    val Some((table1, dts1)) = oTable1
    //table1.draw()

    assertEmpty(table1(0)(0))
    assertEquals(ListMap(
      NP -> Set(TermGuideChartEntry(TermProd("it")))),
      table1(0)(1))
    assertEquals(ListMap(
      S -> Set(BinaryGuideChartEntry(1, BinaryProd(NP, S \ NP)))),
      table1(0)(2))

    assertEmpty(table1(1)(0))
    assertEmpty(table1(1)(1))
    assertEquals(ListMap(
      (S \ NP) -> Set(TermGuideChartEntry(TermProd("works")))),
      table1(1)(2))

    val fudg3 = Some(Fudg.fromGfl(tokens1, """it < works""").getOrElseThrow())
    //println(fudg3)
    val oTable3 = builder.build(tokens1, fudg3, td1)
    assertTrue(oTable3.isEmpty)

    val tokens2 = Vector("work", "it")
    val td2 = SimpleTagDictionary[Cat](Map("work" -> Set(S / NP), "it" -> Set(NP)), "<S>", cat"<S>", "<E>", cat"<E>")
    val fudg2 = Some(Fudg.fromGfl(tokens2, """work < it""").getOrElseThrow())
    //println(fudg2)
    //TreeViz.drawTree(fudg2.get.fudgTree)
    val oTable2 = builder.buildWithDepTreesFromSupertagSetSentence(tokens2.mapTo(td2), fudg2, td2)
    assertTrue(oTable1.isDefined)
    val Some((table2, dts2)) = oTable2
    //table2.draw()

    assertEmpty(table2(0)(0))
    assertEquals(ListMap(
      (S / NP) -> Set(TermGuideChartEntry(TermProd("work")))),
      table2(0)(1))
    assertEquals(ListMap(
      S -> Set(BinaryGuideChartEntry(1, BinaryProd(S / NP, NP)))),
      table2(0)(2))

    assertEmpty(table2(1)(0))
    assertEmpty(table2(1)(1))
    assertEquals(ListMap(
      NP -> Set(TermGuideChartEntry(TermProd("it")))),
      table2(1)(2))

    val fudg4 = Some(Fudg.fromGfl(tokens2, """work > it""").getOrElseThrow())
    //println(fudg4)
    val oTable4 = builder.build(tokens2, fudg4, td2)
    assertTrue(oTable4.isEmpty)
  }

  @Test
  def test_SimpleFudgCfgGuideChartBuilder_withDependencies_noUnary {
    def assertEmpty(m: Map[_, _]) { assertTrue(m.isEmpty) }

    val tokens = Vector("the", "old", "man", "ships")
    val mockTagdict = SimpleTagDictionary[Cat](Map(
      "the" -> Set(NP / N, NP / NP),
      "old" -> Set(N, NP, N / N, NP / NP),
      "man" -> Set(N, NP, (S \ NP) / NP),
      "ships" -> Set(NP, S \ NP)),
      "<S>", cat"<S>", "<E>", cat"<E>")
    val builder = new SimpleFudgCfgGuideChartBuilder(Vector(FA, BA), allowTerminalDeletion = false) //, N2NP))

    //  +------------------------------+------------------------------+------------------------------+------------------------------+
    //  |                              | (0,1)                        | (0,2)                        | (0,3)                        |
    //  | (NP/NP) -> "the"             | NP -> 1:[(NP/N) N]           | NP -> 1:[(NP/N) N]           | S -> 3:[NP (S\NP)]           |
    //  | (NP/N) -> "the"              |       1:[(NP/NP) NP]         |       1:[(NP/NP) NP]         |      2:[NP (S\NP)]           |
    //  |                              |                              |                              |                              |
    //  |                              |                              |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+------------------------------+
    //  |                              |                              | (1,2)                        | (1,3)                        |
    //  |                              | NP -> "old"                  | NP -> 2:[(NP/NP) NP]         |                              |
    //  |                              | (N/N) -> "old"               | N -> 2:[(N/N) N]             |                              |
    //  |                              | N -> "old"                   |                              |                              |
    //  |                              | (NP/NP) -> "old"             |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+------------------------------+
    //  |                              |                              |                              | (2,3)                        |
    //  |                              |                              | NP -> "man"                  | (S\NP) -> 3:[((S\NP)/NP) NP] |
    //  |                              |                              | ((S\NP)/NP) -> "man"         |                              |
    //  |                              |                              | N -> "man"                   |                              |
    //  |                              |                              |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+------------------------------+
    //  |                              |                              |                              |                              |
    //  |                              |                              |                              | (S\NP) -> "ships"            |
    //  |                              |                              |                              | NP -> "ships"                |
    //  |                              |                              |                              |                              |
    //  |                              |                              |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+------------------------------+

    val fudg0: Option[FudgSentence] = None
    val oTable0 = builder.buildWithDepTreesFromSupertagSetSentence(tokens.mapTo(mockTagdict), fudg0, mockTagdict)
    assertTrue(oTable0.isDefined)
    val Some((table0, dts0)) = oTable0
    //table0.draw()

    assertEmpty(table0(0)(0))
    assertEquals(ListMap(
      (NP / NP) -> Set(TermGuideChartEntry(TermProd("the"))),
      (NP / N) -> Set(TermGuideChartEntry(TermProd("the")))),
      table0(0)(1))
    assertEquals(ListMap(
      NP -> Set(BinaryGuideChartEntry(1, BinaryProd(NP / N, N)), BinaryGuideChartEntry(1, BinaryProd(NP / NP, NP)))),
      table0(0)(2))
    assertEquals(ListMap(
      NP -> Set(BinaryGuideChartEntry(1, BinaryProd(NP / N, N)), BinaryGuideChartEntry(1, BinaryProd(NP / NP, NP)))),
      table0(0)(3))
    assertEquals(ListMap(
      S -> Set(
        BinaryGuideChartEntry(3, BinaryProd(NP, (S \ NP))),
        BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP))))),
      table0(0)(4))

    assertEmpty(table0(1)(0))
    assertEmpty(table0(1)(1))
    assertEquals(ListMap(
      N -> Set(TermGuideChartEntry(TermProd("old"))),
      (N / N) -> Set(TermGuideChartEntry(TermProd("old"))),
      NP -> Set(TermGuideChartEntry(TermProd("old"))),
      (NP / NP) -> Set(TermGuideChartEntry(TermProd("old")))),
      table0(1)(2))
    assertEquals(ListMap(
      NP -> Set(BinaryGuideChartEntry(2, BinaryProd(NP / NP, NP))),
      N -> Set(BinaryGuideChartEntry(2, BinaryProd(N / N, N)))),
      table0(1)(3))
    assertEmpty(table0(1)(4))

    assertEmpty(table0(2)(0))
    assertEmpty(table0(2)(1))
    assertEmpty(table0(2)(2))
    assertEquals(ListMap(
      ((S \ NP) / NP) -> Set(TermGuideChartEntry(TermProd("man"))),
      N -> Set(TermGuideChartEntry(TermProd("man"))),
      NP -> Set(TermGuideChartEntry(TermProd("man")))),
      table0(2)(3))
    assertEquals(ListMap(
      (S \ NP) -> Set(BinaryGuideChartEntry(3, BinaryProd((S \ NP) / NP, NP)))),
      table0(2)(4))

    assertEmpty(table0(3)(0))
    assertEmpty(table0(3)(1))
    assertEmpty(table0(3)(2))
    assertEmpty(table0(3)(3))
    assertEquals(ListMap(
      (S \ NP) -> Set(TermGuideChartEntry(TermProd("ships"))),
      NP -> Set(TermGuideChartEntry(TermProd("ships")))),
      table0(3)(4))

    assertEquals(Set(
      DepTree("man", 2, cat"X", Vector(DepTree("the", 0, cat"X", Vector(DepTree("old", 1, cat"X", Vector.empty))), DepTree("ships", 3, cat"X", Vector.empty))),
      DepTree("man", 2, cat"X", Vector(DepTree("old", 1, cat"X", Vector(DepTree("the", 0, cat"X", Vector.empty))), DepTree("ships", 3, cat"X", Vector.empty))),
      DepTree("ships", 3, cat"X", Vector(DepTree("the", 0, cat"X", Vector(DepTree("man", 2, cat"X", Vector(DepTree("old", 1, cat"X", Vector.empty))))))),
      DepTree("ships", 3, cat"X", Vector(DepTree("man", 2, cat"X", Vector(DepTree("the", 0, cat"X", Vector.empty), DepTree("old", 1, cat"X", Vector.empty)))))), dts0)

    //  +------------------------------+------------------------------+------------------------------+------------------------------+
    //  |                              | (0,1)                        | (0,2)                        | (0,3)                        |
    //  |                              |                              | NP -> 1:[(NP/N) N]           | S -> 3:[NP (S\NP)]           |
    //  | (NP/N) -> "the"              |                              |                              |                              |
    //  |                              |                              |                              |                              |
    //  |                              |                              |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+------------------------------+
    //  |                              |                              | (1,2)                        | (1,3)                        |
    //  |                              |                              |                              |                              |
    //  |                              | (N/N) -> "old"               | N -> 2:[(N/N) N]             |                              |
    //  |                              |                              |                              |                              |
    //  |                              |                              |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+------------------------------+
    //  |                              |                              |                              | (2,3)                        |
    //  |                              |                              |                              |                              |
    //  |                              |                              |                              |                              |
    //  |                              |                              | N -> "man"                   |                              |
    //  |                              |                              |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+------------------------------+
    //  |                              |                              |                              |                              |
    //  |                              |                              |                              | (S\NP) -> "ships"            |
    //  |                              |                              |                              |                              |
    //  |                              |                              |                              |                              |
    //  |                              |                              |                              |                              |
    //  +------------------------------+------------------------------+------------------------------+------------------------------+
    val fudg1 = Some(Fudg.fromGfl(tokens, """the old man ships      man > the""").getOrElseThrow())
    //println(fudg1)
    //TreeViz.drawTree(fudg1.get.fudgTree)
    assertTrue(fudg1.get.brackets.isEmpty)
    val oTable1 = builder.buildWithDepTreesFromSupertagSetSentence(tokens.mapTo(mockTagdict), fudg1, mockTagdict)
    assertTrue(oTable1.isDefined)
    val Some((table1, dts1)) = oTable1
    //table1.draw()
    assertEquals(Set(
      DepTree("ships", 3, cat"X", Vector(DepTree("the", 0, cat"X", Vector(DepTree("man", 2, cat"X", Vector(DepTree("old", 1, cat"X", Vector.empty)))))))), dts1)

    assertEmpty(table1(0)(0))
    assertEquals(ListMap(
      (NP / N) -> Set(TermGuideChartEntry(TermProd("the")))),
      table1(0)(1))
    assertEmpty(table1(0)(2))
    assertEquals(ListMap(
      NP -> Set(BinaryGuideChartEntry(1, BinaryProd(NP / N, N)))),
      table1(0)(3))
    assertEquals(ListMap(
      S -> Set(BinaryGuideChartEntry(3, BinaryProd(NP, (S \ NP))))),
      table1(0)(4))

    assertEmpty(table1(1)(0))
    assertEmpty(table1(1)(1))
    assertEquals(ListMap(
      (N / N) -> Set(TermGuideChartEntry(TermProd("old")))),
      table1(1)(2))
    assertEquals(ListMap(
      N -> Set(BinaryGuideChartEntry(2, BinaryProd(N / N, N)))),
      table1(1)(3))
    assertEmpty(table1(1)(4))

    assertEmpty(table1(2)(0))
    assertEmpty(table1(2)(1))
    assertEmpty(table1(2)(2))
    assertEquals(ListMap(
      N -> Set(TermGuideChartEntry(TermProd("man")))),
      table1(2)(3))
    assertEmpty(table1(2)(4))

    assertEmpty(table1(3)(0))
    assertEmpty(table1(3)(1))
    assertEmpty(table1(3)(2))
    assertEmpty(table1(3)(3))
    assertEquals(ListMap(
      (S \ NP) -> Set(TermGuideChartEntry(TermProd("ships")))),
      table1(3)(4))

    //  +------------------------------+------------------------------+------------------------------+------------------------------+

    val oTable2 = builder.buildWithDepTreesFromSupertagSetSentence(tokens.mapTo(mockTagdict), Some(Fudg.fromGfl(tokens, """the old man ships      ships > man """).getOrElseThrow()), mockTagdict)
    assertEquals(Set(
      DepTree("man", 2, cat"X", Vector(DepTree("the", 0, cat"X", Vector(DepTree("old", 1, cat"X", Vector.empty))), DepTree("ships", 3, cat"X", Vector.empty))),
      DepTree("man", 2, cat"X", Vector(DepTree("old", 1, cat"X", Vector(DepTree("the", 0, cat"X", Vector.empty))), DepTree("ships", 3, cat"X", Vector.empty)))), oTable2.get._2)

    val oTable3 = builder.buildWithDepTreesFromSupertagSetSentence(tokens.mapTo(mockTagdict), Some(Fudg.fromGfl(tokens, """the old man ships      the > ships """).getOrElseThrow()), mockTagdict)
    assertEquals(Set(
      DepTree("ships", 3, cat"X", Vector(DepTree("the", 0, cat"X", Vector(DepTree("man", 2, cat"X", Vector(DepTree("old", 1, cat"X", Vector.empty)))))))), oTable3.get._2)

    val oTable4 = builder.buildWithDepTreesFromSupertagSetSentence(tokens.mapTo(mockTagdict), Some(Fudg.fromGfl(tokens, """the old man ships      ships > man    old > the""").getOrElseThrow()), mockTagdict)
    assertEquals(Set(
      DepTree("man", 2, cat"X", Vector(DepTree("the", 0, cat"X", Vector(DepTree("old", 1, cat"X", Vector.empty))), DepTree("ships", 3, cat"X", Vector.empty)))), oTable4.get._2)
  }

  //  case class BracketOnlyFudgSentence(val b: Vector[(Int, Int)]) extends FudgSentence {
  //    def tokens: Vector[Token] = ???
  //    def nodes: Map[String, Node] = ???
  //    def edges: Vector[Edge] = Vector.empty[Edge]
  //    def fudgTree: FudgTree = ???
  //    def brackets: Set[(Int, Int)] = b.toSet
  //    def token(i: Int): Token = ???
  //    def node(i: Int): Node = ???
  //    def addEdge(e: Edge): FudgSentence = ???
  //    def addEdge(parentIndex: Int, childIndex: Int): FudgSentence = ???
  //  }

}
