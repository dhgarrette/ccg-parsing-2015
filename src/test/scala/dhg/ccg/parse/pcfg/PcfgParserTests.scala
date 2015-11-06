package dhg.ccg.parse.pcfg

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.util._
import scala.collection.immutable.ListMap
import dhg.ccg.parse.pcfg.mcmc.PcfgTreeSampler

class PcfgParserTests {

  val S = cat"S".asInstanceOf[AtomCat]
  val NP = cat"NP".asInstanceOf[AtomCat]
  val N = cat"N".asInstanceOf[AtomCat]
  val PP = cat"PP".asInstanceOf[AtomCat]
  val STA: Cat = cat"<S>"
  val END: Cat = cat"<E>"

  val A = cat"A".asInstanceOf[AtomCat]
  val B = cat"B".asInstanceOf[AtomCat]
  val C = cat"C".asInstanceOf[AtomCat]
  val D = cat"D".asInstanceOf[AtomCat]
  val E = cat"E".asInstanceOf[AtomCat]
  val F = cat"F".asInstanceOf[AtomCat]

  @Test
  def test_PcfgParser_parseAndProbKBestWithWeightsFromGuideChart() {

    //    val sentence = "the dogs run".split("\\s+").toVector
    //    val tagdict = SimpleTagDictionary[Cat](Map(
    //      "the" -> Set(N / N, NP / N, N / S),
    //      "dogs" -> Set(NP, N),
    //      "run" -> Set(S \ N, S \ NP, S)),
    //      "<S>", STA, "<E>", END)
    //    val builder = new SimpleCfgGuideChartBuilder(Vector(FA, BA))
    //    val Some(table) = builder.build(sentence, Vector.empty, tagdict)
    //    table.draw()
    //    println(table.repr)
    //  +--------------------+--------------------+--------------------+
    //  |                    | (0,1)              | (0,2)              |
    //  | (N/N) -> "the"     | NP -> 1:[(NP/N) N] | N -> 1:[(N/S) S]   |
    //  | (NP/N) -> "the"    | N -> 1:[(N/N) N]   | S -> 2:[N (S\N)]   |
    //  | (N/S) -> "the"     |                    |      2:[NP (S\NP)] |
    //  +--------------------+--------------------+--------------------+
    //  |                    |                    | (1,2)              |
    //  |                    | NP -> "dogs"       | S -> 2:[N (S\N)]   |
    //  |                    | N -> "dogs"        |      2:[NP (S\NP)] |
    //  |                    |                    |                    |
    //  +--------------------+--------------------+--------------------+
    //  |                    |                    |                    |
    //  |                    |                    | (S\NP) -> "run"    |
    //  |                    |                    | (S\N) -> "run"     |
    //  |                    |                    |                    |
    //  +--------------------+--------------------+--------------------+
    val guideChart = CfgGuideChart("the dogs run".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(ListMap(), ListMap((N / N) -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the"))), (N / S) -> Set(TermGuideChartEntry(TermProd("the")))), ListMap(NP -> Set(BinaryGuideChartEntry(1, BinaryProd((NP / N), N))), N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N)))), ListMap(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / S), S))), S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(NP -> Set(TermGuideChartEntry(TermProd("dogs"))), N -> Set(TermGuideChartEntry(TermProd("dogs")))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(), ListMap((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))), (S \ N) -> Set(TermGuideChartEntry(TermProd("run")))))))

    val parser = new PcfgParser(
      rootDist = new LogProbabilityDistribution[Cat] {
        def apply(b: Cat): LogDouble = b match {
          case S => LogDouble(0.7)
          case N => LogDouble(0.2)
        }
        def sample(): Cat = ???
        def defaultProb: LogDouble = ???
      },
      prodDist = new ConditionalLogProbabilityDistribution[Cat, Prod] {
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
        }
        def sample(given: Cat): Prod = ???
      })

    /*
     * v01(n/n):  p(the|n/n)  = 0.02                                                               CcgLeaf((N / N), "the")
     * v01(np/n): p(the|np/n) = 0.21                                                               CcgLeaf((NP / N), "the")
     * v01(n/s):  p(the|n/s)  = 0.01                                                               CcgLeaf((N / S), "the")
     * 
     * v12(n):  p(dogs|n)  = 0.0500                                                                CcgLeaf(N, "dogs")
     * v12(np): p(dogs|np) = 0.0035                                                                CcgLeaf(NP, "dogs")
     * 
     * v23(s\np): p(run|s\np) = 0.06                                                               CcgLeaf((S \ NP), "run")
     * v23(s\n):  p(run|s\n)  = 0.01                                                               CcgLeaf((S \ N), "run")
     * 
     *
     * v02(np): p(np -> np/n n) * v01(np/n) * v12(n) = 0.45 * 0.21 * 0.05 = 0.004725               CcgBinode(NP, CcgLeaf((NP / N), "the"), CcgLeaf(N, "dogs"))
     * v02(n):  p(n  -> n/n  n) * v01(n/n)  * v12(n) = 0.25 * 0.02 * 0.05 = 0.000250               CcgBinode(N, CcgLeaf((N / N), "the"), CcgLeaf(N, "dogs"))
     * 
     * v13a(s): p(s -> np s\np) * v12(np) * v23(s\np) = 0.65 * 0.04 * 0.06 = 0.001560              CcgBinode(S, CcgLeaf(NP, "dogs"), CcgLeaf((S \ NP), "run"))
     * v13b(s): p(s -> n  s\n ) * v12(n)  * v23(s\n)  = 0.15 * 0.05 * 0.01 = 0.000075              CcgBinode(S, CcgLeaf(N, "dogs"), CcgLeaf((S \ N), "run"))
     * 
     * 
     * v03a(s): p(s -> np s\np) * v02(np) * v23(s\np) = 0.65 * 0.004725 * 0.06 = 0.000184275       CcgBinode(S, CcgBinode(NP, CcgLeaf((NP / N), "the"), CcgLeaf(N, "dogs")), CcgLeaf((S \ NP), "run"))
     * v03b(s): p(s -> n  s\n ) * v02(n)  * v23(s\n)  = 0.15 * 0.000250 * 0.01 = 0.000000375       CcgBinode(S, CcgBinode(N, CcgLeaf((N / N), "the"), CcgLeaf(N, "dogs")), CcgLeaf((S \ N), "run"))
     * v03a(n): p(n -> n/s  s)  * v01(n/s) * v13a(s)  = 0.10 * 0.01 * 0.001560 = 0.000001560       CcgBinode(N, CcgLeaf((N / S), "the"), CcgBinode(S, CcgLeaf(NP, "dogs"), CcgLeaf((S \ NP), "run")))
     * v03b(n): p(n -> n/s  s)  * v01(n/s) * v13b(s)  = 0.10 * 0.01 * 0.000075 = 0.000000075       CcgBinode(N, CcgLeaf((N / S), "the"), CcgBinode(S, CcgLeaf(N, "dogs"), CcgLeaf((S \ N), "run")))
     * 
     * p(s) * v03a(s) = 0.7 * 0.000184275 = 0.0001289925                                           CcgBinode(S, CcgBinode(NP, CcgLeaf((NP / N), "the"), CcgLeaf(N, "dogs")), CcgLeaf((S \ NP), "run"))
     * p(s) * v03b(s) = 0.7 * 0.000000375 = 0.0000002625                                           CcgBinode(S, CcgBinode(N, CcgLeaf((N / N), "the"), CcgLeaf(N, "dogs")), CcgLeaf((S \ N), "run"))
     * p(n) * v03a(n) = 0.2 * 0.000001560 = 0.0000003120                                           CcgBinode(N, CcgLeaf((N / S), "the"), CcgBinode(S, CcgLeaf(NP, "dogs"), CcgLeaf((S \ NP), "run")))
     * p(n) * v03b(n) = 0.2 * 0.000000075 = 0.0000000150                                           CcgBinode(N, CcgLeaf((N / S), "the"), CcgBinode(S, CcgLeaf(N, "dogs"), CcgLeaf((S \ N), "run")))
		 */

    val exT1 = CcgBinode(S, CcgBinode(NP, CcgLeaf((NP / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ NP), "run", "FAKEPOS"))
    val exT2 = CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(NP, "dogs", "FAKEPOS"), CcgLeaf((S \ NP), "run", "FAKEPOS")))
    val exT3 = CcgBinode(S, CcgBinode(N, CcgLeaf((N / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ N), "run", "FAKEPOS"))
    val exT4 = CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(N, "dogs", "FAKEPOS"), CcgLeaf((S \ N), "run", "FAKEPOS")))

    val k1 = parser.parseAndProbKBestFromGuideChart(guideChart, 1)
    assertEquals(1, k1.size)
    assertEquals(exT1, k1(0)._1)
    assertEqualsLog(LogDouble(0.0001289925), k1(0)._2, 1e-10)

    val k2 = parser.parseAndProbKBestFromGuideChart(guideChart, 2)
    assertEquals(2, k2.size)
    assertEquals(exT1, k2(0)._1)
    assertEqualsLog(LogDouble(0.0001289925), k2(0)._2, 1e-10)
    assertEquals(exT2, k2(1)._1)
    assertEqualsLog(LogDouble(0.0000003120), k2(1)._2, 1e-10)

    val k3 = parser.parseAndProbKBestFromGuideChart(guideChart, 3)
    assertEquals(3, k3.size)
    assertEquals(exT1, k3(0)._1)
    assertEqualsLog(LogDouble(0.0001289925), k3(0)._2, 1e-10)
    assertEquals(exT2, k3(1)._1)
    assertEqualsLog(LogDouble(0.0000003120), k3(1)._2, 1e-10)
    assertEquals(exT3, k3(2)._1)
    assertEqualsLog(LogDouble(0.0000002625), k3(2)._2, 1e-10)

    val k4 = parser.parseAndProbKBestFromGuideChart(guideChart, 4)
    assertEquals(4, k4.size)
    assertEquals(exT1, k4(0)._1)
    assertEqualsLog(LogDouble(0.0001289925), k4(0)._2, 1e-10)
    assertEquals(exT2, k4(1)._1)
    assertEqualsLog(LogDouble(0.0000003120), k4(1)._2, 1e-10)
    assertEquals(exT3, k4(2)._1)
    assertEqualsLog(LogDouble(0.0000002625), k4(2)._2, 1e-10)
    assertEquals(exT4, k4(3)._1)
    assertEqualsLog(LogDouble(0.0000000150), k4(3)._2, 1e-10)

    assertEquals(k4, parser.parseAndProbKBestFromGuideChart(guideChart, 5)) // only 4 parses total
    assertEquals(k4, parser.parseAndProbKBestFromGuideChart(guideChart, 6)) // only 4 parses total
  }

  @Test
  def test_PcfgParser_parseAndProbKBestWithWeightsFromGuideChart_withUnary() {

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
    val guideChart = CfgGuideChart("the dogs run".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(ListMap(), ListMap((N / N) -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the"))), (N / S) -> Set(TermGuideChartEntry(TermProd("the")))), ListMap(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N))), NP -> Set(BinaryGuideChartEntry(1, BinaryProd((NP / N), N)), UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))), N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / S), S))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N))))),
      Vector(ListMap(), ListMap(), ListMap(N -> Set(TermGuideChartEntry(TermProd("dogs"))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(), ListMap((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))), (S \ N) -> Set(TermGuideChartEntry(TermProd("run")))))))

    val parser = new PcfgParser(
      rootDist = new LogProbabilityDistribution[Cat] {
        def apply(b: Cat): LogDouble = b match {
          case S => LogDouble(0.7)
          case N => LogDouble(0.2)
          case NP => LogDouble(0.3)
        }
        def sample(): Cat = ???
        def defaultProb: LogDouble = ???
      },
      prodDist = new ConditionalLogProbabilityDistribution[Cat, Prod] {
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
      })

    /*
     * v01(n/n):  p(the|n/n)  = 0.02                                                               CcgLeaf(N, "the")
     * v01(np/n): p(the|np/n) = 0.21                                                               CcgLeaf((NP / N), "the")
     * v01(n/s):  p(the|n/s)  = 0.01                                                               CcgLeaf((N / S), "the")
     * 
     * v12(n):  p(dogs|n)  = 0.0500                                                                CcgLeaf(N, "dogs")
     * v12(np): p(np -> n) = 0.0035                                                                CcgUnode(NP, CcgLeaf(N, "dogs"))
     * 
     * v23(s\np): p(run|s\np) = 0.06                                                               CcgLeaf((S \ NP), "run")
     * v23(s\n):  p(run|s\n)  = 0.01                                                               CcgLeaf((S \ N), "run")
     *
     * 
     * v02(n):   p(n  -> n/n  n) * v01(n/n)  * v12(n) = 0.25 * 0.02 * 0.05 = 0.000250              CcgBinode(N, CcgLeaf((N / N), "the"), CcgLeaf(N, "dogs"))
     * v02a(np): p(np -> np/n n) * v01(np/n) * v12(n) = 0.45 * 0.21 * 0.05 = 0.004725              CcgBinode(NP, CcgLeaf((NP / N), "the"), CcgLeaf(N, "dogs"))
     * v02b(np): p(np -> n)      * v02a(n)            = 0.07 * 0.000250    = 0.0000175             CcgUnode(NP, CcgBinode(N, CcgLeaf((N / N), "the"), CcgLeaf(N, "dogs")))
     * 
     * v13a(s): p(s -> n  s\n ) * v12(n)  * v23(s\n)  = 0.15 * 0.05   * 0.01 = 0.000075            CcgBinode(S, CcgLeaf(N, "dogs"), CcgLeaf((S \ N), "run"))
     * v13b(s): p(s -> np s\np) * v12(np) * v23(s\np) = 0.65 * 0.0035 * 0.06 = 0.0001365           CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs")), CcgLeaf((S \ NP), "run"))
     * 
     * 
     * v03a(s):  p(s -> np s\np) * v02a(np) * v23(s\np) = 0.65 * 0.004725  * 0.06 = 1.84275E-4     CcgBinode(S, CcgBinode(NP, CcgLeaf((NP / N), "the"), CcgLeaf(N, "dogs")), CcgLeaf((S \ NP), "run"))
     * v03b(s):  p(s -> np s\np) * v02b(np) * v23(s\np) = 0.65 * 0.0000175 * 0.06 = 6.825E-7       CcgBinode(S, CcgUnode(NP, CcgBinode(N, CcgLeaf((N / N), "the"), CcgLeaf(N, "dogs"))), CcgLeaf((S \ NP), "run"))
     * v03c(s):  p(s -> n  s\n ) * v02(n)   * v23(s\n)  = 0.15 * 0.000250  * 0.01 = 3.75E-7        CcgBinode(S, CcgBinode(N, CcgLeaf((N / N), "the"), CcgLeaf(N, "dogs")), CcgLeaf((S \ N), "run"))
     * v03a(n):  p(n -> n/s s)   * v01(n/s) * v13a(s)   = 0.10 * 0.01 * 0.000075  = 7.5E-8         CcgBinode(N, CcgLeaf((N / S), "the"), CcgBinode(S, CcgLeaf(N, "dogs"), CcgLeaf((S \ N), "run")))
     * v03b(n):  p(n -> n/s s)   * v01(n/s) * v13b(s)   = 0.10 * 0.01 * 0.0001365 = 1.365E-7       CcgBinode(N, CcgLeaf((N / S), "the"), CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs")), CcgLeaf((S \ NP), "run")))
     * v03a(np): p(np -> n)      * v03a(n)              = 0.07 * 7.5E-8           = 5.25E-9        CcgUnode(NP, CcgBinode(N, CcgLeaf((N / S), "the"), CcgBinode(S, CcgLeaf(N, "dogs"), CcgLeaf((S \ N), "run"))))
     * v03b(np): p(np -> n)      * v03b(n)              = 0.07 * 1.365E-7         = 9.555E-9       CcgUnode(NP, CcgBinode(N, CcgLeaf((N / S), "the"), CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs")), CcgLeaf((S \ NP), "run"))))
     *
     * p(s)  * v03a(s):  0.7 * 1.84275E-4  = 1.289925E-4                                           CcgBinode(S, CcgBinode(NP, CcgLeaf((NP / N), "the"), CcgLeaf(N, "dogs")), CcgLeaf((S \ NP), "run"))
     * p(s)  * v03b(s):  0.7 * 6.825E-7    = 4.7775E-7                                             CcgBinode(S, CcgUnode(NP, CcgBinode(N, CcgLeaf((N / N), "the"), CcgLeaf(N, "dogs"))), CcgLeaf((S \ NP), "run"))
     * p(s)  * v03c(s):  0.7 * 3.75E-7     = 2.625E-7                                              CcgBinode(S, CcgBinode(N, CcgLeaf((N / N), "the"), CcgLeaf(N, "dogs")), CcgLeaf((S \ N), "run"))
     * p(n)  * v03b(n):  0.2 * 1.365E-7    = 2.73E-8                                               CcgBinode(N, CcgLeaf((N / S), "the"), CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs")), CcgLeaf((S \ NP), "run")))
     * p(n)  * v03a(n):  0.2 * 7.5E-8      = 1.5E-8                                                CcgBinode(N, CcgLeaf((N / S), "the"), CcgBinode(S, CcgLeaf(N, "dogs"), CcgLeaf((S \ N), "run")))
     * p(np) * v03b(np): 0.3 * 9.555E-9    = 2.8665E-9                                             CcgUnode(NP, CcgBinode(N, CcgLeaf((N / S), "the"), CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs")), CcgLeaf((S \ NP), "run"))))
     * p(np) * v03a(np): 0.3 * 5.25E-9     = 1.575E-9                                              CcgUnode(NP, CcgBinode(N, CcgLeaf((N / S), "the"), CcgBinode(S, CcgLeaf(N, "dogs"), CcgLeaf((S \ N), "run"))))
		 */
    val T1 = CcgBinode(S, CcgBinode(NP, CcgLeaf((NP / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ NP), "run", "FAKEPOS"))
    val T2 = CcgBinode(S, CcgUnode(NP, CcgBinode(N, CcgLeaf((N / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS"))), CcgLeaf((S \ NP), "run", "FAKEPOS"))
    val T3 = CcgBinode(S, CcgBinode(N, CcgLeaf((N / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ N), "run", "FAKEPOS"))
    val T4 = CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ NP), "run", "FAKEPOS")))
    val T5 = CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(N, "dogs", "FAKEPOS"), CcgLeaf((S \ N), "run", "FAKEPOS")))
    val T6 = CcgUnode(NP, CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ NP), "run", "FAKEPOS"))))
    val T7 = CcgUnode(NP, CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(N, "dogs", "FAKEPOS"), CcgLeaf((S \ N), "run", "FAKEPOS"))))

    val k1 = parser.parseAndProbKBestFromGuideChart(guideChart, 1)
    assertEquals(1, k1.size)
    assertEquals(T1, k1(0)._1)
    assertEqualsLog(LogDouble(1.289925E-4), k1(0)._2, 1e-10)

    val k2 = parser.parseAndProbKBestFromGuideChart(guideChart, 2)
    assertEquals(2, k2.size)
    assertEquals(T1, k2(0)._1)
    assertEqualsLog(LogDouble(1.289925E-4), k2(0)._2, 1e-10)
    assertEquals(T2, k2(1)._1)
    assertEqualsLog(LogDouble(4.7775E-7), k2(1)._2, 1e-10)

    val k3 = parser.parseAndProbKBestFromGuideChart(guideChart, 3)
    assertEquals(3, k3.size)
    assertEquals(T1, k3(0)._1)
    assertEqualsLog(LogDouble(1.289925E-4), k3(0)._2, 1e-10)
    assertEquals(T2, k3(1)._1)
    assertEqualsLog(LogDouble(4.7775E-7), k3(1)._2, 1e-10)
    assertEquals(T3, k3(2)._1)
    assertEqualsLog(LogDouble(2.625E-7), k3(2)._2, 1e-10)

    val k4 = parser.parseAndProbKBestFromGuideChart(guideChart, 4)
    assertEquals(4, k4.size)
    assertEquals(T1, k4(0)._1)
    assertEqualsLog(LogDouble(1.289925E-4), k4(0)._2, 1e-10)
    assertEquals(T2, k4(1)._1)
    assertEqualsLog(LogDouble(4.7775E-7), k4(1)._2, 1e-10)
    assertEquals(T3, k4(2)._1)
    assertEqualsLog(LogDouble(2.625E-7), k4(2)._2, 1e-10)
    assertEquals(T4, k4(3)._1)
    assertEqualsLog(LogDouble(2.73E-8), k4(3)._2, 1e-10)

    // ...

    val k7 = parser.parseAndProbKBestFromGuideChart(guideChart, 7)
    assertEquals(7, k7.size)
    assertEquals(T1, k7(0)._1)
    assertEqualsLog(LogDouble(1.289925E-4), k7(0)._2, 1e-10)
    assertEquals(T2, k7(1)._1)
    assertEqualsLog(LogDouble(4.7775E-7), k7(1)._2, 1e-10)
    assertEquals(T3, k7(2)._1)
    assertEqualsLog(LogDouble(2.625E-7), k7(2)._2, 1e-10)
    assertEquals(T4, k7(3)._1)
    assertEqualsLog(LogDouble(2.73E-8), k7(3)._2, 1e-10)
    assertEquals(T5, k7(4)._1)
    assertEqualsLog(LogDouble(1.5E-8), k7(4)._2, 1e-10)
    assertEquals(T6, k7(5)._1)
    assertEqualsLog(LogDouble(2.866537E-9), k7(5)._2, 1e-10)
    assertEquals(T7, k7(6)._1)
    assertEqualsLog(LogDouble(1.575E-9), k7(6)._2, 1e-10)

    assertEquals(k7, parser.parseAndProbKBestFromGuideChart(guideChart, 8)) // only 7 parses total
    assertEquals(k7, parser.parseAndProbKBestFromGuideChart(guideChart, 100)) // only 7 parses total
  }

  @Test
  def test_SamplingPcfgParser {
    type Word = String

    val mockK = 7

    val mockGuideChart = CfgGuideChart(Vector.empty, Vector.empty)

    val mockRootDist = new LogProbabilityDistribution[Cat] {
      def apply(b: Cat): LogDouble = ???
      def sample(): Cat = ???
      def defaultProb: LogDouble = ???
    }
    val mockProdDist = new ConditionalLogProbabilityDistribution[Cat, Prod] {
      def apply(x: Prod, given: Cat): LogDouble = ???
      def sample(given: Cat): Prod = ???
    }

    val T1: CcgTree = CcgLeaf(A, "t1", "FAKEPOS")
    val T2: CcgTree = CcgLeaf(B, "t2", "FAKEPOS")
    val T3: CcgTree = CcgLeaf(C, "t3", "FAKEPOS")
    //val T4: CcgTree = CcgLeaf(D, "t4")

    val mockPcfgWeighter = new PcfgWeighter {
      def weight(tree: CcgTree,
        rootDist: LogProbabilityDistribution[Cat],
        prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): LogDouble = {
        assertSame(mockRootDist, rootDist)
        assertSame(mockProdDist, prodDist)
        LogDouble(tree match {
          case T1 => 0.4
          case T2 => 0.3
          case T3 => 0.2
        })
      }
    }

    val mockPcfgTreeSampler1 = new PcfgTreeSampler {
      def samples(
        guideChart: CfgGuideChart,
        rootDist: LogProbabilityDistribution[Cat],
        prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
        k: Int): Vector[CcgTree] = {
        assertSame(mockGuideChart, guideChart)
        assertSame(mockRootDist, rootDist)
        assertSame(mockProdDist, prodDist)
        assertEquals(mockK, k)
        Vector(T3, T2, T2, T1, T3, T2, T3)
      }
    }

    val sp1 = new SamplingPcfgParser(mockRootDist, mockProdDist, mockPcfgTreeSampler1, mockPcfgWeighter)
    val kbest = sp1.parseAndProbKBestFromGuideChart(mockGuideChart, mockK)
    assertEquals(3, kbest.size)
    assertEquals(T1, kbest(0)._1)
    assertEqualsLog(LogDouble(0.4), kbest(0)._2, 1e-9)
    assertEquals(T2, kbest(1)._1)
    assertEqualsLog(LogDouble(0.3), kbest(1)._2, 1e-9)
    assertEquals(T3, kbest(2)._1)
    assertEqualsLog(LogDouble(0.2), kbest(2)._2, 1e-9)

    val mockPcfgTreeSampler2 = new PcfgTreeSampler {
      def samples(
        guideChart: CfgGuideChart,
        rootDist: LogProbabilityDistribution[Cat],
        prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
        k: Int): Vector[CcgTree] = {
        assertSame(mockGuideChart, guideChart)
        assertSame(mockRootDist, rootDist)
        assertSame(mockProdDist, prodDist)
        assertEquals(1, k)
        Vector(T2)
      }
    }

    val sp2 = new SamplingPcfgParser(mockRootDist, mockProdDist, mockPcfgTreeSampler2, mockPcfgWeighter)
    val onebest = sp2.parseAndProbFromGuideChart(mockGuideChart)
    assertTrue(onebest.isDefined)
    val (onebestt, onebestp) = onebest.get
    assertEquals(T2, onebestt)
    assertEqualsLog(LogDouble(0.3), onebestp, 1e-9)
  }

  @Test
  def test_CompositePcfgParser {
    type Word = String

    val mockGuideChart = CfgGuideChart(Vector.empty, Vector.empty)

    val mockK = 12
    val mockKA = 7

    val T1: CcgTree = CcgLeaf(A, "t1", "FAKEPOS")
    val T2: CcgTree = CcgLeaf(B, "t2", "FAKEPOS")
    val T3: CcgTree = CcgLeaf(C, "t3", "FAKEPOS")
    val T4: CcgTree = CcgLeaf(D, "t4", "FAKEPOS")

    def p(t: CcgTree) = LogDouble(t match {
      case T1 => 0.4
      case T2 => 0.3
      case T3 => 0.2
      case T4 => 0.1
    })

    val d1a = new AbstractKBestGuideChartParser {
      def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int): Vector[(CcgTree, LogDouble)] = {
        assertSame(mockGuideChart, guideChart)
        assertEquals(mockKA, k)
        Vector(T3, T2, T2, T1, T3, T2, T3).mapTo(p)
      }
    }
    val d1b = new AbstractKBestGuideChartParser {
      def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int): Vector[(CcgTree, LogDouble)] = {
        assertSame(mockGuideChart, guideChart)
        assertEquals(mockK - mockKA, k)
        Vector(T2, T4, T4, T1, T2).mapTo(p)
      }
    }
    val cpp1 = new CompositePcfgParser(d1a, mockKA, d1b)
    val kbest = cpp1.parseAndProbKBestFromGuideChart(mockGuideChart, mockK)
    assertEquals(4, kbest.size)
    assertEquals(T1, kbest(0)._1)
    assertEqualsLog(LogDouble(0.4), kbest(0)._2, 1e-9)
    assertEquals(T2, kbest(1)._1)
    assertEqualsLog(LogDouble(0.3), kbest(1)._2, 1e-9)
    assertEquals(T3, kbest(2)._1)
    assertEqualsLog(LogDouble(0.2), kbest(2)._2, 1e-9)
    assertEquals(T4, kbest(3)._1)
    assertEqualsLog(LogDouble(0.1), kbest(3)._2, 1e-9)

    val d2a = new AbstractKBestGuideChartParser {
      def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int): Vector[(CcgTree, LogDouble)] = {
        assertSame(mockGuideChart, guideChart)
        assertEquals(1, k)
        Vector(T3).mapTo(p)
      }
    }
    val d2b = new AbstractKBestGuideChartParser {
      def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int): Vector[(CcgTree, LogDouble)] = ???
    }
    val cpp2 = new CompositePcfgParser(d2a, mockKA, d2b)
    val onebest = cpp2.parseAndProbFromGuideChart(mockGuideChart)
    assertTrue(onebest.isDefined)
    val (onebestt, onebestp) = onebest.get
    assertEquals(T3, onebestt)
    assertEqualsLog(LogDouble(0.2), onebestp, 1e-9)

    val d3a = new AbstractKBestGuideChartParser {
      def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int): Vector[(CcgTree, LogDouble)] = {
        assertSame(mockGuideChart, guideChart)
        assertEquals(7, k)
        Vector(T3, T2, T2, T1, T3, T2, T3).mapTo(p)
      }
    }
    val d3b = new AbstractKBestGuideChartParser {
      def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int): Vector[(CcgTree, LogDouble)] = ???
    }
    val cpp3 = new CompositePcfgParser(d3a, 15, d3b)
    val kbest3 = cpp3.parseAndProbKBestFromGuideChart(mockGuideChart, 7)
    assertEquals(3, kbest3.size)
    assertEquals(T1, kbest3(0)._1)
    assertEqualsLog(LogDouble(0.4), kbest3(0)._2, 1e-9)
    assertEquals(T2, kbest3(1)._1)
    assertEqualsLog(LogDouble(0.3), kbest3(1)._2, 1e-9)
    assertEquals(T3, kbest3(2)._1)
    assertEqualsLog(LogDouble(0.2), kbest3(2)._2, 1e-9)
  }

  def assertEqualsLog(a: LogDouble, b: LogDouble, t: Double) {
    assertEquals(a.toDouble, b.toDouble, t)
  }
}
