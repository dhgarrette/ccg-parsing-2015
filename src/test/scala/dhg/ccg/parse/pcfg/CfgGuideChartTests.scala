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
import dhg.ccg.tagdict.DummyCatTagDictionary

class CfgGuideChartTests {

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
  def test_CfgGuideChart {
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
    val t0 = Map[Cat, Set[GuideChartEntry]](A -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the"))))
    val t1 = Map[Cat, Set[GuideChartEntry]](NP -> Set(UnaryGuideChartEntry(UnaryProd(N))), N -> Set(TermGuideChartEntry(TermProd("dogs"))), (NP \ A) -> Set(TermGuideChartEntry(TermProd("dogs"))))
    val t2 = Map[Cat, Set[GuideChartEntry]]((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))))
    val v02 = Map[Cat, Set[GuideChartEntry]](NP -> Set(BinaryGuideChartEntry(1, BinaryProd(A, (NP \ A))), BinaryGuideChartEntry(1, BinaryProd((NP / N), N))))
    val v03 = Map[Cat, Set[GuideChartEntry]](S -> Set(BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))
    val mockMatrix = Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(Map(), t0, v02, v03),
      Vector(Map(), Map(), t1, Map()),
      Vector(Map(), Map(), Map(), t2))
    val gc = CfgGuideChart("the dogs run".splitWhitespace, mockMatrix)
    //gc.draw()

    assertSame(mockMatrix(0), gc(0))
    assertSame(mockMatrix(1), gc(1))
    assertSame(mockMatrix(2), gc(2))

    assertSame(mockMatrix(0)(0), gc(0, 0))
    assertSame(mockMatrix(0)(1), gc(0, 1))
    assertSame(mockMatrix(0)(2), gc(0, 2))
    assertSame(mockMatrix(0)(3), gc(0, 3))
    assertSame(mockMatrix(1)(0), gc(1, 0))
    assertSame(mockMatrix(1)(1), gc(1, 1))
    assertSame(mockMatrix(1)(2), gc(1, 2))
    assertSame(mockMatrix(1)(3), gc(1, 3))
    assertSame(mockMatrix(2)(0), gc(2, 0))
    assertSame(mockMatrix(2)(1), gc(2, 1))
    assertSame(mockMatrix(2)(2), gc(2, 2))
    assertSame(mockMatrix(2)(3), gc(2, 3))

    assertEquals(3, gc.length)

    assertSame(v03, gc.root)
    assertEquals(Vector(t0, t1, t2), gc.terminals)
    assertEquals(Vector("the", "dogs", "run"), gc.words)
    assertEquals(Vector(Set(A, NP / N), Set(N, NP \ A), Set(S \ NP)), gc.supertagSets)
    assertEquals(Vector("the" -> Set(A, NP / N), "dogs" -> Set(N, NP \ A), "run" -> Set(S \ NP)), gc.wordSupertagsets)

    val bun: IndexedSeq[(Int, Int, Map[Cat, Set[GuideChartEntry]])] = gc.bottomUpNodes
    assertEquals(Vector(
      (0, 1, t0), (1, 2, t1), (2, 3, t2),
      (0, 2, v02), (1, 3, Map()),
      (0, 3, v03)), bun)

    val tdn: IndexedSeq[(Int, Int, Map[Cat, Set[GuideChartEntry]])] = gc.topDownNodes
    assertEquals(Vector(
      (0, 3, v03),
      (0, 2, v02), (1, 3, Map()),
      (0, 1, t0), (1, 2, t1), (2, 3, t2)),
      tdn)
  }

  @Test
  def test_CfgGuideChart_numPossibleParses() {
    //  +------------------+------------------+------------------+
    //  |                  | (0,1)            | (0,2)            |
    //  | (N/N) -> "the"   | N -> 1:[(N/N) N] | N -> 1:[(N/N) N] |
    //  |                  |                  |      2:[N (N\N)] |
    //  +------------------+------------------+------------------+
    //  |                  |                  | (1,2)            |
    //  |                  | N -> "dogs"      | N -> 2:[N (N\N)] |
    //  |                  |                  |                  |
    //  +------------------+------------------+------------------+
    //  |                  |                  |                  |
    //  |                  |                  | (N\N) -> "run"   |
    //  |                  |                  |                  |
    //  +------------------+------------------+------------------+
    val gc1 = CfgGuideChart("the dogs run".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(Map(), Map((N / N) -> Set(TermGuideChartEntry(TermProd("the")))), Map(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N)))), Map(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N)), BinaryGuideChartEntry(2, BinaryProd(N, (N \ N)))))),
      Vector(Map(), Map(), Map(N -> Set(TermGuideChartEntry(TermProd("dogs")))), Map(N -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (N \ N)))))),
      Vector(Map(), Map(), Map(), Map((N \ N) -> Set(TermGuideChartEntry(TermProd("run")))))))
    assertEquals(BigInt(2), gc1.numPossibleParses)

    //  +--------------------------+--------------------------+--------------------------+
    //  |                          | (0,1)                    | (0,2)                    |
    //  | A -> "the"               | NP -> 1:[A (NP\A)]       | C -> 1:[A (C\A)]         |
    //  | (NP/N) -> "the"          |       1:[(NP/N) N]       | S -> 1:[A (S\A)]         |
    //  |                          |                          |      2:[NP (S\NP)]       |
    //  +--------------------------+--------------------------+--------------------------+
    //  |                          |                          | (1,2)                    |
    //  |                          | B -> "dogs"              | (S\A) -> 2:[B ((S\A)\B)] |
    //  |                          | N -> "dogs"              | (C\A) -> 2:[B ((C\A)\B)] |
    //  |                          | (NP\A) -> "dogs"         |                          |
    //  +--------------------------+--------------------------+--------------------------+
    //  |                          |                          |                          |
    //  |                          |                          | (S\NP) -> "run"          |
    //  |                          |                          | ((C\A)\B) -> "run"       |
    //  |                          |                          | ((S\A)\B) -> "run"       |
    //  +--------------------------+--------------------------+--------------------------+
    val gc2 = CfgGuideChart("the dogs run".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(Map(), Map(A -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the")))), Map(NP -> Set(BinaryGuideChartEntry(1, BinaryProd(A, (NP \ A))), BinaryGuideChartEntry(1, BinaryProd((NP / N), N)))), Map(C -> Set(BinaryGuideChartEntry(1, BinaryProd(A, (C \ A)))), S -> Set(BinaryGuideChartEntry(1, BinaryProd(A, (S \ A))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(Map(), Map(), Map(B -> Set(TermGuideChartEntry(TermProd("dogs"))), N -> Set(TermGuideChartEntry(TermProd("dogs"))), (NP \ A) -> Set(TermGuideChartEntry(TermProd("dogs")))), Map((S \ A) -> Set(BinaryGuideChartEntry(2, BinaryProd(B, ((S \ A) \ B)))), (C \ A) -> Set(BinaryGuideChartEntry(2, BinaryProd(B, ((C \ A) \ B)))))),
      Vector(Map(), Map(), Map(), Map((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))), ((C \ A) \ B) -> Set(TermGuideChartEntry(TermProd("run"))), ((S \ A) \ B) -> Set(TermGuideChartEntry(TermProd("run")))))))
    assertEquals(BigInt(4), gc2.numPossibleParses)

    //  +--------------------------+--------------------------+--------------------------+--------------------------+
    //  |                          | (0,1)                    | (0,2)                    | (0,3)                    |
    //  | (B/C) -> "1"             | (A/B) -> 1:[((A/B)/C) C] |                          | A -> 2:[(A/B) B]         |
    //  | ((A/B)/C) -> "1"         | B -> 1:[(B/C) C]         |                          |      2:[B (A\B)]         |
    //  +--------------------------+--------------------------+--------------------------+--------------------------+
    //  |                          |                          | (1,2)                    | (1,3)                    |
    //  |                          | C -> "2"                 |                          |                          |
    //  |                          |                          |                          |                          |
    //  +--------------------------+--------------------------+--------------------------+--------------------------+
    //  |                          |                          |                          | (2,3)                    |
    //  |                          |                          | C -> "3"                 | (A\B) -> 3:[C ((A\B)\C)] |
    //  |                          |                          |                          | B -> 3:[C (B\C)]         |
    //  +--------------------------+--------------------------+--------------------------+--------------------------+
    //  |                          |                          |                          |                          |
    //  |                          |                          |                          | (B\C) -> "4"             |
    //  |                          |                          |                          | ((A\B)\C) -> "4"         |
    //  +--------------------------+--------------------------+--------------------------+--------------------------+
    val gc3 = CfgGuideChart("1 2 3 4".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(Map(), Map((B / C) -> Set(TermGuideChartEntry(TermProd("1"))), ((A / B) / C) -> Set(TermGuideChartEntry(TermProd("1")))), Map((A / B) -> Set(BinaryGuideChartEntry(1, BinaryProd(((A / B) / C), C))), B -> Set(BinaryGuideChartEntry(1, BinaryProd((B / C), C)))), Map(), Map(A -> Set(BinaryGuideChartEntry(2, BinaryProd((A / B), B)), BinaryGuideChartEntry(2, BinaryProd(B, (A \ B)))))),
      Vector(Map(), Map(), Map(C -> Set(TermGuideChartEntry(TermProd("2")))), Map(), Map()),
      Vector(Map(), Map(), Map(), Map(C -> Set(TermGuideChartEntry(TermProd("3")))), Map((A \ B) -> Set(BinaryGuideChartEntry(3, BinaryProd(C, ((A \ B) \ C)))), B -> Set(BinaryGuideChartEntry(3, BinaryProd(C, (B \ C)))))),
      Vector(Map(), Map(), Map(), Map(), Map((B \ C) -> Set(TermGuideChartEntry(TermProd("4"))), ((A \ B) \ C) -> Set(TermGuideChartEntry(TermProd("4")))))))
    assertEquals(BigInt(2), gc3.numPossibleParses)

    //  +--------------------------+--------------------------+--------------------------+--------------------------+
    //  |                          | (0,1)                    | (0,2)                    | (0,3)                    |
    //  | (B/C) -> "1"             | (A/B) -> 1:[((A/B)/C) C] |                          | A -> 2:[(A/B) B]         |
    //  | C -> "1"                 | B -> 1:[(B/C) C]         |                          |      2:[B (A\B)]         |
    //  | ((A/B)/C) -> "1"         |      1:[C (B\C)]         |                          |                          |
    //  +--------------------------+--------------------------+--------------------------+--------------------------+
    //  |                          |                          | (1,2)                    | (1,3)                    |
    //  |                          | (B\C) -> "2"             |                          |                          |
    //  |                          | C -> "2"                 |                          |                          |
    //  |                          |                          |                          |                          |
    //  +--------------------------+--------------------------+--------------------------+--------------------------+
    //  |                          |                          |                          | (2,3)                    |
    //  |                          |                          | C -> "3"                 | (A\B) -> 3:[C ((A\B)\C)] |
    //  |                          |                          |                          | B -> 3:[C (B\C)]         |
    //  |                          |                          |                          |                          |
    //  +--------------------------+--------------------------+--------------------------+--------------------------+
    //  |                          |                          |                          |                          |
    //  |                          |                          |                          | (B\C) -> "4"             |
    //  |                          |                          |                          | ((A\B)\C) -> "4"         |
    //  |                          |                          |                          |                          |
    //  +--------------------------+--------------------------+--------------------------+--------------------------+
    val gc4 = CfgGuideChart("1 2 3 4".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(Map(), Map((B / C) -> Set(TermGuideChartEntry(TermProd("1"))), C -> Set(TermGuideChartEntry(TermProd("1"))), ((A / B) / C) -> Set(TermGuideChartEntry(TermProd("1")))), Map((A / B) -> Set(BinaryGuideChartEntry(1, BinaryProd(((A / B) / C), C))), B -> Set(BinaryGuideChartEntry(1, BinaryProd((B / C), C)), BinaryGuideChartEntry(1, BinaryProd(C, (B \ C))))), Map(), Map(A -> Set(BinaryGuideChartEntry(2, BinaryProd((A / B), B)), BinaryGuideChartEntry(2, BinaryProd(B, (A \ B)))))),
      Vector(Map(), Map(), Map((B \ C) -> Set(TermGuideChartEntry(TermProd("2"))), C -> Set(TermGuideChartEntry(TermProd("2")))), Map(), Map()),
      Vector(Map(), Map(), Map(), Map(C -> Set(TermGuideChartEntry(TermProd("3")))), Map((A \ B) -> Set(BinaryGuideChartEntry(3, BinaryProd(C, ((A \ B) \ C)))), B -> Set(BinaryGuideChartEntry(3, BinaryProd(C, (B \ C)))))),
      Vector(Map(), Map(), Map(), Map(), Map((B \ C) -> Set(TermGuideChartEntry(TermProd("4"))), ((A \ B) \ C) -> Set(TermGuideChartEntry(TermProd("4")))))))
    assertEquals(BigInt(3), gc4.numPossibleParses)

    //  +--------------------------+--------------------------+--------------------------+--------------------------+
    //  |                          | (0,1)                    | (0,2)                    | (0,3)                    |
    //  | (B/C) -> "1"             | (A/B) -> 1:[((A/B)/C) C] |                          | B -> 2:[(B/B) B]         |
    //  | ((B/B)/C) -> "1"         | B -> 1:[(B/C) C]         |                          |      2:[B (B\B)]         |
    //  | C -> "1"                 |      1:[C (B\C)]         |                          | A -> 2:[(A/B) B]         |
    //  | ((A/B)/C) -> "1"         | (B/B) -> 1:[((B/B)/C) C] |                          |      2:[B (A\B)]         |
    //  +--------------------------+--------------------------+--------------------------+--------------------------+
    //  |                          |                          | (1,2)                    | (1,3)                    |
    //  |                          | (B\C) -> "2"             |                          |                          |
    //  |                          | C -> "2"                 |                          |                          |
    //  |                          |                          |                          |                          |
    //  |                          |                          |                          |                          |
    //  +--------------------------+--------------------------+--------------------------+--------------------------+
    //  |                          |                          |                          | (2,3)                    |
    //  |                          |                          | C -> "3"                 | (A\B) -> 3:[C ((A\B)\C)] |
    //  |                          |                          |                          | B -> 3:[C (B\C)]         |
    //  |                          |                          |                          | (B\B) -> 3:[C ((B\B)\C)] |
    //  |                          |                          |                          |                          |
    //  +--------------------------+--------------------------+--------------------------+--------------------------+
    //  |                          |                          |                          |                          |
    //  |                          |                          |                          | (B\C) -> "4"             |
    //  |                          |                          |                          | ((B\B)\C) -> "4"         |
    //  |                          |                          |                          | ((A\B)\C) -> "4"         |
    //  |                          |                          |                          |                          |
    //  +--------------------------+--------------------------+--------------------------+--------------------------+
    val gc5 = CfgGuideChart("1 2 3 4".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(Map(), Map((B / C) -> Set(TermGuideChartEntry(TermProd("1"))), ((B / B) / C) -> Set(TermGuideChartEntry(TermProd("1"))), C -> Set(TermGuideChartEntry(TermProd("1"))), ((A / B) / C) -> Set(TermGuideChartEntry(TermProd("1")))), Map((A / B) -> Set(BinaryGuideChartEntry(1, BinaryProd(((A / B) / C), C))), B -> Set(BinaryGuideChartEntry(1, BinaryProd((B / C), C)), BinaryGuideChartEntry(1, BinaryProd(C, (B \ C)))), (B / B) -> Set(BinaryGuideChartEntry(1, BinaryProd(((B / B) / C), C)))), Map(), Map(B -> Set(BinaryGuideChartEntry(2, BinaryProd((B / B), B)), BinaryGuideChartEntry(2, BinaryProd(B, (B \ B)))), A -> Set(BinaryGuideChartEntry(2, BinaryProd((A / B), B)), BinaryGuideChartEntry(2, BinaryProd(B, (A \ B)))))),
      Vector(Map(), Map(), Map((B \ C) -> Set(TermGuideChartEntry(TermProd("2"))), C -> Set(TermGuideChartEntry(TermProd("2")))), Map(), Map()),
      Vector(Map(), Map(), Map(), Map(C -> Set(TermGuideChartEntry(TermProd("3")))), Map((A \ B) -> Set(BinaryGuideChartEntry(3, BinaryProd(C, ((A \ B) \ C)))), B -> Set(BinaryGuideChartEntry(3, BinaryProd(C, (B \ C)))), (B \ B) -> Set(BinaryGuideChartEntry(3, BinaryProd(C, ((B \ B) \ C)))))),
      Vector(Map(), Map(), Map(), Map(), Map((B \ C) -> Set(TermGuideChartEntry(TermProd("4"))), ((B \ B) \ C) -> Set(TermGuideChartEntry(TermProd("4"))), ((A \ B) \ C) -> Set(TermGuideChartEntry(TermProd("4")))))))
    assertEquals(BigInt(6), gc5.numPossibleParses)
  }

  @Test
  def test_CfgGuideChart_numPossibleParses_withUnary() {
    //    //  +------------------------------+------------------------------+------------------------------+
    //    //  |                              | (0,2)                        | (0,3)                        |
    //    //  | N -> "old"                   | N -> 1:[(N/N) N]             | S -> 2:[NP (S\NP)]           |
    //    //  | (N/N) -> "old"               | NP -> 1:[(NP/N) N]           |      1:[NP (S\NP)]           |
    //    //  | (NP/N) -> "old"              |       N                      |                              |
    //    //  | NP -> "old"                  |                              |                              |
    //    //  | NP -> N                      |                              |                              |
    //    //  +------------------------------+------------------------------+------------------------------+
    //    //  |                              |                              | (2,3)                        |
    //    //  |                              | ((S\NP)/NP) -> "man"         | (S\NP) -> 2:[((S\NP)/NP) NP] |
    //    //  |                              | N -> "man"                   |                              |
    //    //  |                              |                              |                              |
    //    //  |                              |                              |                              |
    //    //  +------------------------------+------------------------------+------------------------------+
    //    //  |                              |                              |                              |
    //    //  |                              |                              | (S\NP) -> "ships"            |
    //    //  |                              |                              | N -> "ships"                 |    
    //    //  |                              |                              | NP -> N                      |
    //    //  |                              |                              |                              |
    //    //  +------------------------------+------------------------------+------------------------------+
    //    val gc1 = CfgGuideChart("old man ships".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
    //      Vector(ListMap(), ListMap(N -> Set(TermGuideChartEntry(TermProd("old"))), (N / N) -> Set(TermGuideChartEntry(TermProd("old"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("old"))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N)), TermGuideChartEntry(TermProd("old")))), ListMap(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N))), NP -> Set(BinaryGuideChartEntry(1, BinaryProd((NP / N), N)), UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP))), BinaryGuideChartEntry(1, BinaryProd(NP, (S \ NP)))))),
    //      Vector(ListMap(), ListMap(), ListMap(((S \ NP) / NP) -> Set(TermGuideChartEntry(TermProd("man"))), N -> Set(TermGuideChartEntry(TermProd("man")))), ListMap((S \ NP) -> Set(BinaryGuideChartEntry(2, BinaryProd(((S \ NP) / NP), NP))))),
    //      Vector(ListMap(), ListMap(), ListMap(), ListMap((S \ NP) -> Set(TermGuideChartEntry(TermProd("ships"))), N -> Set(TermGuideChartEntry(TermProd("ships"))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N)))))))
    //    // TODO: UNCOMMENT    assertEquals(BigInt(4), gc1.numPossibleParses)
    //
    //    /*
    //     *                   D
    //     *                 /   \
    //     *               C      D\C
    //     *             / |       |
    //     *            B  |       F
    //     *              \|       z
    //     *               A
    //     *               y
    //     */
    //    //    val td2 = SimpleTagDictionary[Cat](Map(
    //    //      "y" -> Set(A),
    //    //      "z" -> Set(F)),
    //    //      "<S>", cat"<S>", "<E>", cat"<E>")
    //    //    val builder2 = new SimpleCfgGuideChartBuilder(Vector(FA, BA,
    //    //      new UnaryCcgRule { def apply(c: Cat) = c match { case A => Some(B); case _ => None } },
    //    //      new UnaryCcgRule { def apply(c: Cat) = c match { case A => Some(C); case _ => None } },
    //    //      new UnaryCcgRule { def apply(c: Cat) = c match { case B => Some(C); case _ => None } },
    //    //      new UnaryCcgRule { def apply(c: Cat) = c match { case F => Some(D \ C); case _ => None } }))
    //    //    val Some(table2) = builder2.build("y z".splitWhitespace, Vector.empty, td2)
    //    //    println(">>> table2")
    //    //    table2.draw()
    //    //    println(table2.repr)
    //    //  +------------------+------------------+
    //    //  |                  | (0,1)            |
    //    //  | A -> "y"         | D -> 1:[C (D\C)] |
    //    //  | B -> A           |                  |
    //    //  | C -> A           |                  |
    //    //  |      B           |                  |
    //    //  +------------------+------------------+
    //    //  |                  |                  |
    //    //  |                  | F -> "z"         |
    //    //  |                  | (D\C) -> F       |
    //    //  |                  |                  |
    //    //  |                  |                  |
    //    //  +------------------+------------------+
    //    val gc2 = CfgGuideChart("y z".splitWhitespace, (Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
    //      Vector(ListMap(), ListMap(A -> Set(TermGuideChartEntry(TermProd("y"))), B -> Set(UnaryGuideChartEntry(UnaryProd(A))), C -> Set(UnaryGuideChartEntry(UnaryProd(A)), UnaryGuideChartEntry(UnaryProd(B)))), ListMap(D -> Set(BinaryGuideChartEntry(1, BinaryProd(C, (D \ C)))))),
    //      Vector(ListMap(), ListMap(), ListMap(F -> Set(TermGuideChartEntry(TermProd("z"))), (D \ C) -> Set(UnaryGuideChartEntry(UnaryProd(F)))))))
    //    // TODO: UNCOMMENT    assertEquals(BigInt(2), gc2.numPossibleParses)
    //
    //    /*
    //     *                   D
    //     *                 /   \
    //     *               C      D\C
    //     *             / |       |
    //     *            B  |       F
    //     *              \|       z
    //     *               A
    //     *               y
    //     */
    //    //    val td3 = SimpleTagDictionary[Cat](Map(
    //    //      "y" -> Set(A, B),
    //    //      "z" -> Set(F, D \ C)),
    //    //      "<S>", cat"<S>", "<E>", cat"<E>")
    //    //    val builder3 = new SimpleCfgGuideChartBuilder(Vector(FA, BA,
    //    //      new UnaryCcgRule { def apply(c: Cat) = c match { case A => Some(B); case _ => None } },
    //    //      new UnaryCcgRule { def apply(c: Cat) = c match { case A => Some(C); case _ => None } },
    //    //      new UnaryCcgRule { def apply(c: Cat) = c match { case B => Some(C); case _ => None } },
    //    //      new UnaryCcgRule { def apply(c: Cat) = c match { case F => Some(D \ C); case _ => None } }))
    //    //    val Some(table3) = builder3.build("y z".splitWhitespace, Vector.empty, td3)
    //    //    println(">>> table3")
    //    //    table3.draw()
    //    //    println(table3.repr)
    //    //  +------------------+------------------+
    //    //  |                  | (0,1)            |
    //    //  | A -> "y"         | D -> 1:[C (D\C)] |
    //    //  | B -> A           |                  |
    //    //  |      "y"         |                  |
    //    //  | C -> A           |                  |
    //    //  |      B           |                  |
    //    //  +------------------+------------------+
    //    //  |                  |                  |
    //    //  |                  | F -> "z"         |
    //    //  |                  | (D\C) -> "z"     |
    //    //  |                  |          F       |
    //    //  |                  |                  |
    //    //  |                  |                  |
    //    //  +------------------+------------------+
    //    val gc3 = CfgGuideChart("y z".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
    //      Vector(ListMap(), ListMap(A -> Set(TermGuideChartEntry(TermProd("y"))), B -> Set(UnaryGuideChartEntry(UnaryProd(A)), TermGuideChartEntry(TermProd("y"))), C -> Set(UnaryGuideChartEntry(UnaryProd(A)), UnaryGuideChartEntry(UnaryProd(B)))), ListMap(D -> Set(BinaryGuideChartEntry(1, BinaryProd(C, (D \ C)))))),
    //      Vector(ListMap(), ListMap(), ListMap(F -> Set(TermGuideChartEntry(TermProd("z"))), (D \ C) -> Set(TermGuideChartEntry(TermProd("z")), UnaryGuideChartEntry(UnaryProd(F)))))))
    //    // TODO: UNCOMMENT    assertEquals(BigInt(6), gc3.numPossibleParses)
    //
    //    /*
    //     *                   D
    //     *                 /   \
    //     *               C      D\C
    //     *             / |       |
    //     *            B  |       F
    //     *              \|       z
    //     *               A
    //     *               y
    //     */
    //    //    val td4 = SimpleTagDictionary[Cat](Map(
    //    //      "y" -> Set(A, B, C),
    //    //      "z" -> Set(F, D \ C)),
    //    //      "<S>", cat"<S>", "<E>", cat"<E>")
    //    //    val builder4 = new SimpleCfgGuideChartBuilder(Vector(FA, BA,
    //    //      new UnaryCcgRule { def apply(c: Cat) = c match { case A => Some(B); case _ => None } },
    //    //      new UnaryCcgRule { def apply(c: Cat) = c match { case A => Some(C); case _ => None } },
    //    //      new UnaryCcgRule { def apply(c: Cat) = c match { case B => Some(C); case _ => None } },
    //    //      new UnaryCcgRule { def apply(c: Cat) = c match { case F => Some(D \ C); case _ => None } }))
    //    //    val Some(table4) = builder4.build("y z".splitWhitespace, Vector.empty, td4)
    //    //    println(">>> table4")
    //    //    table4.draw()
    //    //    println(table4.repr)
    //    //  +------------------+------------------+
    //    //  |                  | (0,1)            |
    //    //  | A -> "y"         | D -> 1:[C (D\C)] |
    //    //  | B -> A           |                  |
    //    //  |      "y"         |                  |
    //    //  | C -> A           |                  |
    //    //  |      "y"         |                  |
    //    //  |      B           |                  |
    //    //  +------------------+------------------+
    //    //  |                  |                  |
    //    //  |                  | F -> "z"         |
    //    //  |                  | (D\C) -> "z"     |
    //    //  |                  |          F       |
    //    //  |                  |                  |
    //    //  |                  |                  |
    //    //  |                  |                  |
    //    //  +------------------+------------------+
    //    val gc4 = CfgGuideChart("y z".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
    //      Vector(ListMap(), ListMap(A -> Set(TermGuideChartEntry(TermProd("y"))), B -> Set(UnaryGuideChartEntry(UnaryProd(A)), TermGuideChartEntry(TermProd("y"))), C -> Set(UnaryGuideChartEntry(UnaryProd(A)), TermGuideChartEntry(TermProd("y")), UnaryGuideChartEntry(UnaryProd(B)))), ListMap(D -> Set(BinaryGuideChartEntry(1, BinaryProd(C, (D \ C)))))),
    //      Vector(ListMap(), ListMap(), ListMap(F -> Set(TermGuideChartEntry(TermProd("z"))), (D \ C) -> Set(TermGuideChartEntry(TermProd("z")), UnaryGuideChartEntry(UnaryProd(F)))))))
    //    assertEquals(BigInt(8), gc4.numPossibleParses)

    //    val sentence6 = "the dogs run".split("\\s+").toVector
    //    val tagdict6 = SimpleTagDictionary[Cat](Map(
    //      "the" -> Set(N / N, NP / N, N / S),
    //      "dogs" -> Set(N),
    //      "run" -> Set(S \ N, S \ NP, S)),
    //      "<S>", cat"<S>", "<E>", cat"<E>")
    //    val builder = new SimpleCfgGuideChartBuilder(Vector(FA, BA, N2NP))
    //    val Some(table) = builder.build(sentence6, Vector.empty, tagdict6)
    //    table.draw()
    //    println(table.repr)
    //  +--------------------+--------------------+--------------------+
    //  |                    | (0,1)              | (0,2)              |
    //  | (N/N) -> "the"     | N -> 1:[(N/N) N]   | S -> 2:[N (S\N)]   |
    //  | (NP/N) -> "the"    | NP -> 1:[(NP/N) N] |      2:[NP (S\NP)] |
    //  | (N/S) -> "the"     |       N            | N -> 1:[(N/S) S]   |
    //  |                    |                    | NP -> N            |
    //  +--------------------+--------------------+--------------------+
    //  |                    |                    | (1,2)              |
    //  |                    | N -> "dogs"        | S -> 2:[N (S\N)]   |
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
    val gc6 = CfgGuideChart("the dogs run".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(ListMap(), ListMap((N / N) -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the"))), (N / S) -> Set(TermGuideChartEntry(TermProd("the")))), ListMap(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N))), NP -> Set(BinaryGuideChartEntry(1, BinaryProd((NP / N), N)), UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))), N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / S), S))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N))))),
      Vector(ListMap(), ListMap(), ListMap(N -> Set(TermGuideChartEntry(TermProd("dogs"))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(), ListMap((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))), (S \ N) -> Set(TermGuideChartEntry(TermProd("run")))))))
    assertEquals(BigInt(7), gc6.numPossibleParses)

    //    /*
    //     *                   D
    //     *                 /   \
    //     *               C      D\C
    //     *             / |       |
    //     *            B  |       F
    //     *          /   \|       z
    //     *        /      A
    //     *      / \     / \
    //     *    B/X  X  A/X  X
    //     *     X  B\X  X  A\X
    //     *    B/Y  Y
    //     *     v   w   x   y
    //     */
    //
    //    val td4 = SimpleTagDictionary[Cat](Map(
    //      "v" -> Set(X, B / X, B / Y),
    //      "w" -> Set(X, B \ X, Y),
    //      "x" -> Set(A / X, X),
    //      "y" -> Set(X, A \ X),
    //      "z" -> Set(D \ C, F)),
    //      "<S>", cat"<S>", "<E>", cat"<E>")
    //    val builder4 = new SimpleCfgGuideChartBuilder(Vector(FA, BA,
    //      new UnaryCcgRule { def apply(c: Cat) = c match { case A => Some(B); case _ => None } },
    //      new UnaryCcgRule { def apply(c: Cat) = c match { case A => Some(C); case _ => None } },
    //      new UnaryCcgRule { def apply(c: Cat) = c match { case B => Some(C); case _ => None } },
    //      new UnaryCcgRule { def apply(c: Cat) = c match { case F => Some(D \ C); case _ => None } }))
    //    val Some(table4) = builder4.build("v w x y z".splitWhitespace, Vector.empty, td4)
    //    println(">>> table4")
    //    table4.draw()
    //    println(table4.repr)
    //    val gc4 = table4
    //    assertEquals(BigInt(0), gc4.numPossibleParses)
  }

  //

  @Test
  def test_SimpleCfgGuideChartBuilder {

    val tagdict = SimpleTagDictionary[Cat](Map(
      "the" -> Set(N / N, NP / N, A),
      "dogs" -> Set(NP, N, B, NP \ A),
      "run" -> Set(S \ NP, S)),
      "<S>", cat"<S>", "<E>", cat"<E>")

    val builder = new SimpleCfgGuideChartBuilder(Vector(FA, BA), allowTerminalDeletion = false)
    val Some(table) = builder.build(Vector("the", "dogs", "run"), None, tagdict)

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
  }

  @Test
  def test_SimpleCfgGuideChartBuilder_withUnary {

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
    val builder1 = new SimpleCfgGuideChartBuilder(Vector(FA, BA, N2NP), allowTerminalDeletion = false)
    val Some(table1) = builder1.build(Vector("old", "man", "ships"), None, tagdict1)
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
  def test_SimpleCfgGuideChartBuilder_withUnary_allowTerminalDeletion {

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
      "the" -> Set(NP / N),
      "old" -> Set(NP, NP / NP),
      "man" -> Set(N),
      "walks" -> Set(S \ NP)),
      "<S>", cat"<S>", "<E>", cat"<E>")
    val builder1 = new SimpleCfgGuideChartBuilder(Vector(FA, BA, N2NP), allowTerminalDeletion = true)
    val Some(table1) = builder1.build(Vector("the", "old", "man", "walks"), None, tagdict1)
    table1.draw()

    def assertEmpty(m: Map[_, _]) { assertTrue(m.isEmpty) }

    ???
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
  def test_SimpleCfgGuideChartBuilder_withRootSet {

    val tagdict = SimpleTagDictionary[Cat](Map(
      "the" -> Set(N / N, NP / N, A),
      "dogs" -> Set(NP, N, B, NP \ A),
      "run" -> Set(S \ NP, S, B \ NP)),
      "<S>", cat"<S>", "<E>", cat"<E>")

    val builder = new SimpleCfgGuideChartBuilder(Vector(FA, BA), rootSet = Set(S), allowTerminalDeletion = false)
    val Some(table) = builder.build(Vector("the", "dogs", "run"), None, tagdict)

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
  }

  @Test
  def test_PunctBracketingCfgGuideChartBuilder {
    val mockSentence = Vector("the", "big", ",", "brown", ",", "dogs", "run", ".")
    val mockBrackets = Some(BracketOnlyFudgSentence(Vector((0, 6), (1, 4))))
    val mockTd = SimpleTagDictionary[Cat](Map(
      "old" -> Set(N, N / N, NP / N),
      "man" -> Set(N, (S \ NP) / NP),
      "ships" -> Set(N, S \ NP)),
      "<S>", cat"<S>", "<E>", cat"<E>")
    val mockGC = CfgGuideChart(mockSentence, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(ListMap(), ListMap((N / N) -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the"))), (N / S) -> Set(TermGuideChartEntry(TermProd("the")))), ListMap(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N))), NP -> Set(BinaryGuideChartEntry(1, BinaryProd((NP / N), N)), UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))), N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / S), S))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N))))),
      Vector(ListMap(), ListMap(), ListMap(N -> Set(TermGuideChartEntry(TermProd("dogs"))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(), ListMap((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))), (S \ N) -> Set(TermGuideChartEntry(TermProd("run")))))))
    val delegate = new CfgGuideChartBuilder {
      def buildFromSupertagSetSentence(supertagSetSentence: Vector[(String, Set[Cat])], fudgSentence: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[CfgGuideChart] = {
        assertEquals(mockSentence, supertagSetSentence.map(_._1))
        assertEquals(mockBrackets ++ Set((0, 2), (3, 4), (5, 7)), fudgSentence.map(_.brackets).getOrElse(Set.empty))
        Some(mockGC)
      }
      def buildUnpruned(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence] = None, tagdict: TagDictionary[Cat] = DummyCatTagDictionary): CfgGuideChart = ???
    }
    val gcb = new PunctBracketingCfgGuideChartBuilder(Set(",", "."), delegate)
    val Some(result) = gcb.build(mockSentence, mockBrackets, mockTd)
    assertSame(mockGC, result)
  }

  @Test
  def test_PunctBracketingCfgGuideChartBuilder_edgecases {
    val mockGC = CfgGuideChart(Vector.empty, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](Vector(ListMap(X -> Set()))))

    // 1: Starts with punct
    val mockSentence1 = Vector(".", "the", "big", ",", "brown", ",", "dogs", "run")
    val mockBrackets1 = Some(BracketOnlyFudgSentence(Vector((0, 6), (1, 4))))
    val mockTd1 = SimpleTagDictionary[Cat](Map(
      "old" -> Set(N, N / N, NP / N),
      "man" -> Set(N, (S \ NP) / NP),
      "ships" -> Set(N, S \ NP)),
      "<S>", cat"<S>", "<E>", cat"<E>")
    val delegate1 = new CfgGuideChartBuilder {
      def buildFromSupertagSetSentence(supertagSetSentence: Vector[(String, Set[Cat])], fudgSentence: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[CfgGuideChart] = {
        assertEquals(mockSentence1, supertagSetSentence.map(_._1))
        assertEquals(mockBrackets1 ++ Set((1, 3), (4, 5), (6, 8)), fudgSentence.map(_.brackets).getOrElse(Set.empty))
        Some(mockGC)
      }
      def buildUnpruned(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence] = None, tagdict: TagDictionary[Cat] = DummyCatTagDictionary): CfgGuideChart = ???
    }
    val gcb1 = new PunctBracketingCfgGuideChartBuilder(Set(",", "."), delegate1)
    val Some(result1) = gcb1.build(mockSentence1, mockBrackets1, mockTd1)
    assertSame(mockGC, result1)

    // 2: Adjacent punct
    val mockSentence2 = Vector(".", "the", "big", ",", ".", "brown", ",", "dogs", "run")
    val mockBrackets2: Option[FudgSentence] = None
    val mockTd2 = SimpleTagDictionary[Cat](Map(
      "old" -> Set(N, N / N, NP / N),
      "man" -> Set(N, (S \ NP) / NP),
      "ships" -> Set(N, S \ NP)),
      "<S>", cat"<S>", "<E>", cat"<E>")
    val mockGC2 = CfgGuideChart(Vector.empty, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](Vector(ListMap(X -> Set()))))
    val delegate2 = new CfgGuideChartBuilder {
      def buildFromSupertagSetSentence(supertagSetSentence: Vector[(String, Set[Cat])], fudgSentence: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[CfgGuideChart] = {
        assertEquals(mockSentence2, supertagSetSentence.map(_._1))
        assertEquals(mockBrackets2 ++ Set((1, 3), (5, 6), (7, 9)), fudgSentence.map(_.brackets).getOrElse(Set.empty))
        Some(mockGC)
      }
      def buildUnpruned(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence] = None, tagdict: TagDictionary[Cat] = DummyCatTagDictionary): CfgGuideChart = ???
    }
    val gcb2 = new PunctBracketingCfgGuideChartBuilder(Set(",", "."), delegate2)
    val Some(result2) = gcb2.build(mockSentence2, mockBrackets2, mockTd2)
    assertSame(mockGC, result2)

    // 3: Only punct
    val mockSentence3 = Vector(".", ",", ".")
    val mockBrackets3: Option[FudgSentence] = None
    val mockTd3 = SimpleTagDictionary[Cat](Map(
      "old" -> Set(N, N / N, NP / N),
      "man" -> Set(N, (S \ NP) / NP),
      "ships" -> Set(N, S \ NP)),
      "<S>", cat"<S>", "<E>", cat"<E>")
    val mockGC3 = CfgGuideChart(Vector.empty, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](Vector(ListMap(X -> Set()))))
    val delegate3 = new CfgGuideChartBuilder {
      def buildFromSupertagSetSentence(supertagSetSentence: Vector[(String, Set[Cat])], fudgSentence: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[CfgGuideChart] = {
        assertEquals(mockSentence3, supertagSetSentence.map(_._1))
        assertEquals(mockBrackets3 ++ Set(), fudgSentence.map(_.brackets).getOrElse(Set.empty))
        Some(mockGC)
      }
      def buildUnpruned(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence] = None, tagdict: TagDictionary[Cat] = DummyCatTagDictionary): CfgGuideChart = ???
    }
    val gcb3 = new PunctBracketingCfgGuideChartBuilder(Set(",", "."), delegate3)
    val Some(result3) = gcb3.build(mockSentence3, mockBrackets3, mockTd3)
    assertSame(mockGC, result3)

    // 4: One token: punct
    val mockSentence4 = Vector(".")
    val mockBrackets4: Option[FudgSentence] = None
    val mockTd4 = SimpleTagDictionary[Cat](Map(
      "old" -> Set(N, N / N, NP / N),
      "man" -> Set(N, (S \ NP) / NP),
      "ships" -> Set(N, S \ NP)),
      "<S>", cat"<S>", "<E>", cat"<E>")
    val mockGC4 = CfgGuideChart(Vector.empty, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](Vector(ListMap(X -> Set()))))
    val delegate4 = new CfgGuideChartBuilder {
      def buildFromSupertagSetSentence(supertagSetSentence: Vector[(String, Set[Cat])], fudgSentence: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[CfgGuideChart] = {
        assertEquals(mockSentence4, supertagSetSentence.map(_._1))
        assertEquals(mockBrackets4 ++ Set(), fudgSentence.map(_.brackets).getOrElse(Set.empty))
        Some(mockGC)
      }
      def buildUnpruned(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence] = None, tagdict: TagDictionary[Cat] = DummyCatTagDictionary): CfgGuideChart = ???
    }
    val gcb4 = new PunctBracketingCfgGuideChartBuilder(Set(",", "."), delegate4)
    val Some(result4) = gcb4.build(mockSentence4, mockBrackets4, mockTd4)
    assertSame(mockGC, result4)
  }

  @Test
  def test_CfgGuideChartSerDeser {
    val sd = new CfgGuideChartSerDeser()

    //  +--------------------+--------------------+--------------------+
    //  |                    | (0,1)              | (0,2)              |
    //  | (N/N) -> "the"     | N -> 1:[(N/N) N]   | S -> 2:[N (S\N)]   |
    //  | (NP/N) -> "the"    | NP -> 1:[(NP/N) N] |      2:[NP (S\NP)] |
    //  | (N/S) -> "the"     |       N            | N -> 1:[(N/S) S]   |
    //  |                    |                    | NP -> N            |
    //  +--------------------+--------------------+--------------------+
    //  |                    |                    | (1,2)              |
    //  |                    | N -> "dogs"        | S -> 2:[N (S\N)]   |
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
    val gc1 = CfgGuideChart("the dogs run".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(ListMap(), ListMap((N / N) -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the"))), (N / S) -> Set(TermGuideChartEntry(TermProd("the")))), ListMap(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N))), NP -> Set(BinaryGuideChartEntry(1, BinaryProd((NP / N), N)), UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))), N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / S), S))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N))))),
      Vector(ListMap(), ListMap(), ListMap(N -> Set(TermGuideChartEntry(TermProd("dogs"))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(), ListMap((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))), (S \ N) -> Set(TermGuideChartEntry(TermProd("run")))))))

    val ser1: Vector[String] = sd.ser(gc1)
    //for (x <- ser1) println(x)
    assertEquals("""length=3""", ser1(0))
    assertEquals("""(0,1) ;; (N/N) -> the ;; (NP/N) -> the ;; (N/S) -> the""", ser1(1))
    assertEquals("""(0,2) ;; N -> 1:[(N/N),N] ;; NP -> 1:[(NP/N),N] [N]""", ser1(2))
    assertEquals("""(0,3) ;; S -> 2:[N,(S\N)] 2:[NP,(S\NP)] ;; N -> 1:[(N/S),S] ;; NP -> [N]""", ser1(3))
    assertEquals("""(1,2) ;; N -> dogs ;; NP -> [N]""", ser1(4))
    assertEquals("""(1,3) ;; S -> 2:[N,(S\N)] 2:[NP,(S\NP)]""", ser1(5))
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
