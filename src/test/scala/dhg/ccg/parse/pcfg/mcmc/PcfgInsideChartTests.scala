package dhg.ccg.parse.pcfg.mcmc

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.cat._
import dhg.ccg.prob._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.util._
import scala.collection.immutable.ListMap

class PcfgInsideChartTests {

  val S: AtomCat = cat"S".asInstanceOf[AtomCat]
  val NP: AtomCat = cat"NP".asInstanceOf[AtomCat]
  val N: AtomCat = cat"N".asInstanceOf[AtomCat]
  val PP: AtomCat = cat"PP".asInstanceOf[AtomCat]
  val STA = cat"<S>"
  val END = cat"<E>"

  val A: AtomCat = cat"A".asInstanceOf[AtomCat]
  val B: AtomCat = cat"B".asInstanceOf[AtomCat]
  val C: AtomCat = cat"C".asInstanceOf[AtomCat]
  val D: AtomCat = cat"D".asInstanceOf[AtomCat]
  val E: AtomCat = cat"E".asInstanceOf[AtomCat]
  val F: AtomCat = cat"F".asInstanceOf[AtomCat]

  @Test
  def test_PcfgInsideChart() {
    val t0 = Map[Cat, LogDouble](A -> LogDouble(0.1))
    val t1 = Map[Cat, LogDouble](A -> LogDouble(0.2), B -> LogDouble(0.3))
    val t2 = Map[Cat, LogDouble](C -> LogDouble(0.4))
    val v02 = Map[Cat, LogDouble](D -> LogDouble(0.5))
    val v03 = Map[Cat, LogDouble](E -> LogDouble(0.6))
    val mockMatrix = Vector[Vector[Map[Cat, LogDouble]]](
      Vector(Map(), t0, v02, v03),
      Vector(Map(), Map(), t1, Map()),
      Vector(Map(), Map(), Map(), t2))
    val ic = new PcfgInsideChart(mockMatrix)
    //ic.draw()

    assertSame(mockMatrix(0), ic(0))
    assertSame(mockMatrix(1), ic(1))
    assertSame(mockMatrix(2), ic(2))

    assertSame(mockMatrix(0)(0), ic(0, 0))
    assertSame(mockMatrix(0)(1), ic(0, 1))
    assertSame(mockMatrix(0)(2), ic(0, 2))
    assertSame(mockMatrix(0)(3), ic(0, 3))
    assertSame(mockMatrix(1)(0), ic(1, 0))
    assertSame(mockMatrix(1)(1), ic(1, 1))
    assertSame(mockMatrix(1)(2), ic(1, 2))
    assertSame(mockMatrix(1)(3), ic(1, 3))
    assertSame(mockMatrix(2)(0), ic(2, 0))
    assertSame(mockMatrix(2)(1), ic(2, 1))
    assertSame(mockMatrix(2)(2), ic(2, 2))
    assertSame(mockMatrix(2)(3), ic(2, 3))

    assertEquals(3, ic.length)

    assertSame(mockMatrix(0)(3), ic.root)

    val tdn: IndexedSeq[(Int, Int, Map[Cat, LogDouble])] = ic.topDownNodes
    assertEquals(Vector(
      (0, 3, v03),
      (0, 2, v02), (1, 3, Map()),
      (0, 1, t0), (1, 2, t1), (2, 3, t2)),
      tdn)
  }

  @Test
  def test_SimplePcfgInsideChartBuilder_buildInsideChart_withUnary {

    //    val sentence = "the dogs run".split("\\s+").toVector
    //    val tagdict = SimpleTagDictionary[Cat](Map(
    //      "the" -> Set(N / N, NP / N, N / S),
    //      "dogs" -> Set(N),
    //      "run" -> Set(S \ N, S \ NP, S)),
    //      "<S>", STA, "<E>", END)
    //    val builder = new SimpleCfgGuideChartBuilder(Vector(FA, BA, N2NP))
    //    val Some(table) = builder.build(sentence, Vector.empty, tagdict)
    //    table.draw()
    //    println(table.repr)
    //println(f"${table.numPossibleParses}")
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
    //  |                    |       "dogs"       |                    |
    //  |                    |                    |                    |
    //  +--------------------+--------------------+--------------------+
    //  |                    |                    |                    |
    //  |                    |                    | (S\NP) -> "run"    |
    //  |                    |                    | (S\N) -> "run"     |
    //  |                    |                    |                    |
    //  |                    |                    |                    |
    //  +--------------------+--------------------+--------------------+
    val guideChart = CfgGuideChart("the dogs run".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(ListMap(), ListMap((N / N) -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the"))), (N / S) -> Set(TermGuideChartEntry(TermProd("the")))), ListMap(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N))), NP -> Set(BinaryGuideChartEntry(1, BinaryProd((NP / N), N)), UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))), N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / S), S))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N))))),
      Vector(ListMap(), ListMap(), ListMap(N -> Set(TermGuideChartEntry(TermProd("dogs"))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N)), TermGuideChartEntry(TermProd("dogs")))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(), ListMap((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))), (S \ N) -> Set(TermGuideChartEntry(TermProd("run")))))))

    val icb = new SimplePcfgInsideChartBuilder()

    val prodDist = new ConditionalLogProbabilityDistribution[Cat, Prod] {
      def apply(x: Prod, given: Cat): LogDouble = (given, x) match {
        case ((NP / N), /**/ TermProd("the")) /* */ => LogDouble(0.21)
        case ((N / N), /* */ TermProd("the")) /* */ => LogDouble(0.02)
        case ((N / S), /* */ TermProd("the")) /* */ => LogDouble(0.01)
        case ((N), /*     */ TermProd("dogs")) /**/ => LogDouble(0.05)
        case ((NP), /*    */ TermProd("dogs")) /**/ => LogDouble(0.04)
        case ((S \ NP), /**/ TermProd("run")) /* */ => LogDouble(0.06)
        case ((S \ N), /* */ TermProd("run")) /* */ => LogDouble(0.01)
        case ((S), /*     */ TermProd("run")) /* */ => LogDouble(0.03)

        case ((NP), BinaryProd(NP / N, N)) /**/ => LogDouble(0.45)
        case ((N), BinaryProd(N / N, N)) /*  */ => LogDouble(0.25)
        case ((N), BinaryProd(N / S, S)) /*  */ => LogDouble(0.10)
        case ((S), BinaryProd(NP, S \ NP)) /**/ => LogDouble(0.65)
        case ((S), BinaryProd(N, S \ N)) /*  */ => LogDouble(0.15)

        case (NP, UnaryProd(N)) => LogDouble(0.07)
      }
      def sample(given: Cat): Prod = ???
    }

    /*
     * v01(n/n):  p(the|n/n)  = 0.02             = 0.02
     * v01(np/n): p(the|np/n) = 0.21             = 0.21
     * v01(n/s):  p(the|n/s)  = 0.01             = 0.01
     * 
     * v12(n):  p(dogs|n)           = 0.05       = 0.05
     * v12(np): p(dogs|np)          = 0.04       = 0.0435
     *          p(np -> n) * v12(n) = 0.0035
     * 
     * v23(s\np): p(run|s\np) = 0.06             = 0.06
     * v23(s\n):  p(run|s\n)  = 0.01             = 0.01
     *
     * 
     * v02(n):  p(n  -> n/n  n) * v01(n/n)  * v12(n) = 0.25 * 0.02 * 0.05 = 0.000250
     * v02(np): p(np -> np/n n) * v01(np/n) * v12(n) = 0.45 * 0.21 * 0.05 = 0.004725        = 0.0047425
     *          p(np -> n)      * v02(n)             = 0.07 * 0.000250    = 0.0000175
     * 
     * v13(s): p(s -> n  s\n)  * v12(n)  * v23(s\n)  = 0.15 * 0.05   * 0.01 = 0.000075      = 0.0017715
     *         p(s -> np s\np) * v12(np) * v23(s\np) = 0.65 * 0.0435 * 0.06 = 0.0001365
     * 
     * 
     * v03(s):  p(s -> np s\np) * v02(np)  * v23(s\np) = 0.65 * 0.0047425 * 0.06 = 1.849575E-4  = 1.853325E-4
     *          p(s -> n  s\n ) * v02(n)   * v23(s\n)  = 0.15 * 0.000250  * 0.01 = 3.75E-7
     * v03(n):  p(n -> n/s s)   * v01(n/s) * v13(s)    = 0.10 * 0.01 * 0.0017715 = 1.7715E-6    = 1.7715E-6
     * v03(np): p(np -> n)      * v03(n)               = 0.07 * 1.7715E-6        = 1.24005E-7   = 1.24005E-7
		 */

    val ic = icb.buildInsideChart(guideChart, prodDist)

    assertEqualsLog(LogDouble(0.02), ic(0, 1)(N / N), 1e-9)
    assertEqualsLog(LogDouble(0.21), ic(0, 1)(NP / N), 1e-9)
    assertEqualsLog(LogDouble(0.01), ic(0, 1)(N / S), 1e-9)

    assertEqualsLog(LogDouble(0.05), ic(1, 2)(N), 1e-9)
    assertEqualsLog(LogDouble(0.0435), ic(1, 2)(NP), 1e-9)

    assertEqualsLog(LogDouble(0.06), ic(2, 3)(S \ NP), 1e-9)
    assertEqualsLog(LogDouble(0.01), ic(2, 3)(S \ N), 1e-9)

    assertEqualsLog(LogDouble(0.000250), ic(0, 2)(N), 1e-9)
    assertEqualsLog(LogDouble(0.0047425), ic(0, 2)(NP), 1e-9)

    assertEqualsLog(LogDouble(0.0017715), ic(1, 3)(S), 1e-9)

    assertEqualsLog(LogDouble(1.853325E-4), ic(0, 3)(S), 1e-9)
    assertEqualsLog(LogDouble(1.7715E-6), ic(0, 3)(N), 1e-9)
    assertEqualsLog(LogDouble(1.24005E-7), ic(0, 3)(NP), 1e-9)
  }

  def assertEqualsLog(expected: LogDouble, actual: LogDouble, err: Double) {
    assertEquals(expected.toDouble, actual.toDouble, err)
  }
}