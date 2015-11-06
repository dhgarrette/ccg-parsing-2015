package dhg.ccg.parse.pcfg.mcmc

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.util._
import scala.collection.immutable.ListMap

class PcfgTreeSamplerTests {

  val S = cat"S".asInstanceOf[AtomCat]
  val NP = cat"NP".asInstanceOf[AtomCat]
  val N = cat"N".asInstanceOf[AtomCat]
  val PP = cat"PP".asInstanceOf[AtomCat]
  val STA = cat"<S>"
  val END = cat"<E>"

  val A = cat"A".asInstanceOf[AtomCat]
  val B = cat"B".asInstanceOf[AtomCat]
  val C = cat"C".asInstanceOf[AtomCat]
  val D = cat"D".asInstanceOf[AtomCat]
  val E = cat"E".asInstanceOf[AtomCat]
  val F = cat"F".asInstanceOf[AtomCat]

  @Test
  def test_SimplePcfgTreeSampler_sample {
    type Word = String

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
    //  |                    |                    |                    |
    //  |                    |                    |                    |
    //  +--------------------+--------------------+--------------------+
    //  |                    |                    |                    |
    //  |                    |                    | (S\NP) -> "run"    |
    //  |                    |                    | (S\N) -> "run"     |
    //  |                    |                    |                    |
    //  |                    |                    |                    |
    //  +--------------------+--------------------+--------------------+
    val mockGuideChart = CfgGuideChart("the dogs run".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(ListMap(), ListMap((N / N) -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the"))), (N / S) -> Set(TermGuideChartEntry(TermProd("the")))), ListMap(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N))), NP -> Set(BinaryGuideChartEntry(1, BinaryProd((NP / N), N)), UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))), N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / S), S))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N))))),
      Vector(ListMap(), ListMap(), ListMap(N -> Set(TermGuideChartEntry(TermProd("dogs"))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(), ListMap((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))), (S \ N) -> Set(TermGuideChartEntry(TermProd("run")))))))

    val mockRootDist = new LogProbabilityDistribution[Cat] {
      def apply(b: Cat): LogDouble = b match {
        case S => LogDouble(0.5)
        case NP => LogDouble(0.3)
        case N => LogDouble(0.2)
      }
      def sample(): Cat = ???
      def defaultProb: LogDouble = ???
    }
    val mockProdDist = new ConditionalLogProbabilityDistribution[Cat, Prod] {
      def apply(x: Prod, given: Cat): LogDouble = (given, x) match {
        case ((NP / N), /**/ TermProd("the")) /* */ => LogDouble(0.03)
        case ((N / N), /* */ TermProd("the")) /* */ => LogDouble(0.02)
        case ((N / S), /* */ TermProd("the")) /* */ => LogDouble(0.01)
        case ((N), /*     */ TermProd("dogs")) /**/ => LogDouble(0.05)
        case ((NP), /*    */ TermProd("dogs")) /**/ => LogDouble(0.04)
        case ((S \ NP), /**/ TermProd("run")) /* */ => LogDouble(0.06)
        case ((S \ N), /* */ TermProd("run")) /* */ => LogDouble(0.05)
        case ((S), /*     */ TermProd("run")) /* */ => LogDouble(0.04)

        case ((NP), BinaryProd(NP / N, N)) /**/ => LogDouble(0.30)
        case ((N), BinaryProd(N / N, N)) /*  */ => LogDouble(0.25)
        case ((N), BinaryProd(N / S, S)) /*  */ => LogDouble(0.40)
        case ((S), BinaryProd(NP, S \ NP)) /**/ => LogDouble(0.35)
        case ((S), BinaryProd(N, S \ N)) /*  */ => LogDouble(0.20)

        case (NP, UnaryProd(N)) => LogDouble(0.85)
      }
      def sample(given: Cat): Prod = ???
    }

    val T1 = CcgBinode(S, CcgBinode(NP, CcgLeaf((NP / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ NP), "run", "FAKEPOS"))
    val T2 = CcgBinode(S, CcgUnode(NP, CcgBinode(N, CcgLeaf((N / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS"))), CcgLeaf((S \ NP), "run", "FAKEPOS"))
    val T3 = CcgBinode(S, CcgBinode(N, CcgLeaf((N / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ N), "run", "FAKEPOS"))
    val T4 = CcgUnode(NP, CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ NP), "run", "FAKEPOS"))))
    val T5 = CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ NP), "run", "FAKEPOS")))
    val T6 = CcgUnode(NP, CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(N, "dogs", "FAKEPOS"), CcgLeaf((S \ N), "run", "FAKEPOS"))))
    val T7 = CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(N, "dogs", "FAKEPOS"), CcgLeaf((S \ N), "run", "FAKEPOS")))

    //    val trees = new PcfgParser(mockRootDist, mockProdDist).parseAndProbKBestFromGuideChart(mockGuideChart, 100)
    //    for (((t, p), mt) <- trees.map { case (t, p) => (t, p / trees.map(_._2).sum) } zipSafe Vector(T1, T2, T3, T4, T5, T6, T7)) {
    //      //println(f"          $mt")
    //      println(f"${p.toDouble}%.4f -> $t")
    //    }
    //    (Vector(T1, T2, T3, T4, T5, T6, T7) zipSafe trees).foreach { case (mt, (t, _)) => assertEquals(mt, t) }
    //    val ic = new SimplePcfgInsideChartBuilder().buildInsideChart(mockGuideChart, mockProdDist)
    //    ic.draw()

    //  +-----------------+-----------------+-----------------+
    //  |                 | (0,1)           | (0,2)           |
    //  | (N/N) -> 0.02   | NP -> 6.625E-4  | NP -> 4.7345E-6 |
    //  | (NP/N) -> 0.03  | N -> 2.5E-4     | N -> 5.57E-6    |
    //  | (N/S) -> 0.01   |                 | S -> 1.64125E-5 |
    //  +-----------------+-----------------+-----------------+
    //  |                 |                 | (1,2)           |
    //  |                 | NP -> 0.0425    | S -> 0.0013925  |
    //  |                 | N -> 0.05       |                 |
    //  |                 |                 |                 |
    //  +-----------------+-----------------+-----------------+
    //  |                 |                 |                 |
    //  |                 |                 | (S\NP) -> 0.06  |
    //  |                 |                 | (S\N) -> 0.05   |
    //  |                 |                 |                 |
    //  +-----------------+-----------------+-----------------+
    val mockInsideChart = new PcfgInsideChart(Vector(
      Vector(Map(),
        Map((N / N) -> LogDouble(0.02), (NP / N) -> LogDouble(0.03), (N / S) -> LogDouble(0.01)),
        Map(NP -> LogDouble(6.625E-4), N -> LogDouble(2.5E-4)),
        Map(NP -> LogDouble(4.7345E-6), N -> LogDouble(5.57E-6), S -> LogDouble(1.64125E-5))),
      Vector(Map(), Map(),
        Map(NP -> LogDouble(0.0425), N -> LogDouble(0.05)),
        Map(S -> LogDouble(0.0013925))),
      Vector(Map(), Map(), Map(),
        Map((S \ NP) -> LogDouble(0.06), (S \ N) -> LogDouble(0.05)))))
    //    ic.draw()
    //    mockInsideChart.draw()
    //    for (i <- 0 until 3; j <- i + 1 to 3) assertEqualsLogMap(ic(i,j), mockInsideChart(i,j), 1e-9)
    val mockInsideChartBuilder = new PcfgInsideChartBuilder {
      def buildInsideChart(
        guideChart: CfgGuideChart,
        prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): PcfgInsideChart = {
        assertSame(mockGuideChart, guideChart)
        assertSame(mockProdDist, prodDist)
        mockInsideChart
      }
    }

    //  0.4399 -> [S [NP [(NP/N) the] [N dogs]] [(S\NP) run]]
    //  0.2077 -> [S [NP [N [(N/N) the] [N dogs]]] [(S\NP) run]]
    //  0.1164 -> [S [N [(N/N) the] [N dogs]] [(S\N) run]]
    //  0.0848 -> [NP [N [(N/S) the] [S [NP [N dogs]] [(S\NP) run]]]]
    //  0.0665 -> [N [(N/S) the] [S [NP [N dogs]] [(S\NP) run]]]
    //  0.0475 -> [NP [N [(N/S) the] [S [N dogs] [(S\N) run]]]]
    //  0.0372 -> [N [(N/S) the] [S [N dogs] [(S\N) run]]]

    val ts = new SimplePcfgTreeSampler(mockInsideChartBuilder)
    val sampleCounts = time("k=1", Iterator.fill(100000)(ts.sample(mockGuideChart, mockRootDist, mockProdDist)).counts)
    assertEquals(7, sampleCounts.size)
    assertEquals(0.4399, sampleCounts.getOrElse(T1, 0) / 100000.0, 5e-2)
    assertEquals(0.2077, sampleCounts.getOrElse(T2, 0) / 100000.0, 5e-2)
    assertEquals(0.1164, sampleCounts.getOrElse(T3, 0) / 100000.0, 5e-2)
    assertEquals(0.0848, sampleCounts.getOrElse(T4, 0) / 100000.0, 5e-2)
    assertEquals(0.0665, sampleCounts.getOrElse(T5, 0) / 100000.0, 5e-2)
    assertEquals(0.0475, sampleCounts.getOrElse(T6, 0) / 100000.0, 5e-2)
    assertEquals(0.0372, sampleCounts.getOrElse(T7, 0) / 100000.0, 5e-2)

    val sampleCounts2 = time("k", ts.samples(mockGuideChart, mockRootDist, mockProdDist, k = 100000).counts)
    assertEquals(7, sampleCounts.size)
    assertEquals(0.4399, sampleCounts2.getOrElse(T1, 0) / 100000.0, 5e-2)
    assertEquals(0.2077, sampleCounts2.getOrElse(T2, 0) / 100000.0, 5e-2)
    assertEquals(0.1164, sampleCounts2.getOrElse(T3, 0) / 100000.0, 5e-2)
    assertEquals(0.0848, sampleCounts2.getOrElse(T4, 0) / 100000.0, 5e-2)
    assertEquals(0.0665, sampleCounts2.getOrElse(T5, 0) / 100000.0, 5e-2)
    assertEquals(0.0475, sampleCounts2.getOrElse(T6, 0) / 100000.0, 5e-2)
    assertEquals(0.0372, sampleCounts2.getOrElse(T7, 0) / 100000.0, 5e-2)
  }

  private[this] def assertEqualsLogMap[T](e: Map[T, LogDouble], a: Map[T, LogDouble], r: Double) = {
    assertEquals(e.keySet, a.keySet)
    for ((k, ev) <- e) assertEquals(ev.toDouble, a(k).toDouble, r)
  }
}
