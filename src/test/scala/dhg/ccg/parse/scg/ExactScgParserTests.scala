package dhg.ccg.parse.scg

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.parse.pcfg._
import dhg.util._
import scala.collection.immutable.ListMap
import dhg.ccg.tagdict.SimpleStartEndTags

class ScgParserTests {
  
  val S: NonPuncCat = cat"S".asInstanceOf[AtomCat]
  val NP: NonPuncCat = cat"NP".asInstanceOf[AtomCat]
  val N: NonPuncCat = cat"N".asInstanceOf[AtomCat]
  val PP: NonPuncCat = cat"PP".asInstanceOf[AtomCat]
  val STA = cat"<S>"
  val END = cat"<E>"

  val A: NonPuncCat = cat"A".asInstanceOf[AtomCat]
  val B: NonPuncCat = cat"B".asInstanceOf[AtomCat]
  val C: NonPuncCat = cat"C".asInstanceOf[AtomCat]
  val D: NonPuncCat = cat"D".asInstanceOf[AtomCat]
  val E: NonPuncCat = cat"E".asInstanceOf[AtomCat]
  val F: NonPuncCat = cat"F".asInstanceOf[AtomCat]
  val G: NonPuncCat = cat"G".asInstanceOf[AtomCat]
  val H: NonPuncCat = cat"H".asInstanceOf[AtomCat]

  val se = SimpleStartEndTags[Cat](STA, END)

  @Test
  def test_ExactScgParser_parseAndProbKBestFromGuideChart_1 {
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

    val parser = new ExactScgParser(
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
      },
      lctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(left: Cat, given: Cat): LogDouble = LogDouble((left, given) match {
          case (STA, /**/ N / N) /*  */ => 0.31
          case (STA, /**/ NP / N) /* */ => 0.32
          case (STA, /**/ N / S) /*  */ => 0.33
          case (STA, /**/ NP) /*     */ => 0.34
          case (STA, /**/ N) /*      */ => 0.35
          case (STA, /**/ S) /*      */ => 0.36
          case (N / N, /* */ NP) /*  */ => 0.37
          case (N / N, /* */ N) /*   */ => 0.38
          case (N / N, /* */ S) /*   */ => 0.39
          case (NP / N, /**/ NP) /*  */ => 0.41
          case (NP / N, /**/ N) /*   */ => 0.42
          case (NP / N, /**/ S) /*   */ => 0.43
          case (N / S, /* */ NP) /*  */ => 0.44
          case (N / S, /* */ N) /*   */ => 0.45
          case (N / S, /* */ S) /*   */ => 0.46
          case (NP, /* */ S \ NP) /*  */ => 0.47
          case (NP, /* */ S \ N) /*   */ => 0.48
          case (N, /* */ S \ NP) /*   */ => 0.49
          case (N, /* */ S \ N) /*    */ => 0.51
        })
        def sample(given: Cat): Cat = ???
      },
      rctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(right: Cat, given: Cat): LogDouble = LogDouble((given, right) match {
          case (S, /* */ END) /*     */ => 0.61
          case (N, /* */ S \ NP) /*  */ => 0.62
          case (N, /* */ S \ N) /*   */ => 0.63
          case (N, /* */ END) /*     */ => 0.64
          case (NP, /* */ S \ NP) /* */ => 0.65
          case (NP, /* */ S \ N) /*  */ => 0.66
          case (N / N, /* */ NP) /*  */ => 0.67
          case (N / N, /* */ N) /*   */ => 0.68
          case (NP / N, /**/ NP) /*  */ => 0.69
          case (NP / N, /**/ N) /*   */ => 0.71
          case (N / S, /* */ NP) /*  */ => 0.72
          case (N / S, /* */ N) /*   */ => 0.73
          case (S \ NP, /* */ END) /**/ => 0.74
          case (S \ N, /* */ END) /* */ => 0.75
        })
        def sample(given: Cat): Cat = ???
      })(se)

    /*
     * v01(<s> <- n/n  -> np): p(the|n/n)  * l(<s> <- n/n)  * r(n/n  -> np) = 0.02 * 0.31 * 0.67 = 0.004154
     * v01(<s> <- n/n  -> n):  p(the|n/n)  * l(<s> <- n/n)  * r(n/n  -> n)  = 0.02 * 0.31 * 0.68 = 0.004216
     * v01(<s> <- np/n -> np): p(the|np/n) * l(<s> <- np/n) * r(np/n -> np) = 0.21 * 0.32 * 0.69 = 0.046368
     * v01(<s> <- np/n -> n):  p(the|np/n) * l(<s> <- np/n) * r(np/n -> n)  = 0.21 * 0.32 * 0.71 = 0.047712
     * v01(<s> <- n/s  -> np): p(the|n/s)  * l(<s> <- n/s)  * r(n/s  -> np) = 0.01 * 0.33 * 0.72 = 0.002376
     * v01(<s> <- n/s  -> n):  p(the|n/s)  * l(<s> <- n/s)  * r(n/s  -> n)  = 0.01 * 0.33 * 0.73 = 0.002409
     *
     * v12(n/n  <- np -> s\np): p(dogs|np) * l(n/n <- np)  * r(np -> s\np) = 0.04 * 0.37 * 0.65 = 0.009620
     * v12(n/n  <- np -> s\n):  p(dogs|np) * l(n/n <- np)  * r(np -> s\n)  = 0.04 * 0.37 * 0.66 = 0.009768
     * v12(np/n <- np -> s\np): p(dogs|np) * l(np/n <- np) * r(np -> s\np) = 0.04 * 0.41 * 0.65 = 0.010660
     * v12(np/n <- np -> s\n):  p(dogs|np) * l(np/n <- np) * r(np -> s\n)  = 0.04 * 0.41 * 0.66 = 0.010824
     * v12(n/s  <- np -> s\np): p(dogs|np) * l(n/s <- np)  * r(np -> s\np) = 0.04 * 0.44 * 0.65 = 0.011440
     * v12(n/s  <- np -> s\n):  p(dogs|np) * l(n/s <- np)  * r(np -> s\n)  = 0.04 * 0.44 * 0.66 = 0.011616
     * v12(n/n  <- n  -> s\np): p(dogs|n)  * l(n/n <- n)   * r(n  -> s\np) = 0.05 * 0.38 * 0.62 = 0.011780
     * v12(n/n  <- n  -> s\n):  p(dogs|n)  * l(n/n <- n)   * r(n  -> s\n)  = 0.05 * 0.38 * 0.63 = 0.011970
     * v12(np/n <- n  -> s\np): p(dogs|n)  * l(np/n <- n)  * r(n  -> s\np) = 0.05 * 0.42 * 0.62 = 0.013020
     * v12(np/n <- n  -> s\n):  p(dogs|n)  * l(np/n <- n)  * r(n  -> s\n)  = 0.05 * 0.42 * 0.63 = 0.013230
     * v12(n/s  <- n  -> s\np): p(dogs|n)  * l(n/s <- n)   * r(n  -> s\np) = 0.05 * 0.45 * 0.62 = 0.013950
     * v12(n/s  <- n  -> s\n):  p(dogs|n)  * l(n/s <- n)   * r(n  -> s\n)  = 0.05 * 0.45 * 0.63 = 0.014175
     *
     * v23(np <- s\np -> <e>): p(run|s\np) * l(np <- s\np) * r(s\np -> <e>) = 0.06 * 0.47 * 0.74 = 0.020868
     * v23(n  <- s\np -> <e>): p(run|s\np) * l(n  <- s\np) * r(s\np -> <e>) = 0.06 * 0.49 * 0.74 = 0.021756
     * v23(np <- s\n  -> <e>): p(run|s\n)  * l(np <- s\n)  * r(s\n  -> <e>) = 0.01 * 0.48 * 0.75 = 0.003600
     * v23(n  <- s\n  -> <e>): p(run|s\n)  * l(n  <- s\n)  * r(s\n  -> <e>) = 0.01 * 0.51 * 0.75 = 0.003825
     *
     *  
     * v02(<s> <- np -> s\np): p(np => np/n n) * v01(<s> <- np/n -> n) * v12(np/n <- n  -> s\np) * l(<s> <- np) * r(np -> s\np) = 0.45 * 0.047712 * 0.013020 * 0.34 * 0.65 = 6.177935836800001E-5
     * v02(<s> <- np -> s\n):  p(np => np/n n) * v01(<s> <- np/n -> n) * v12(np/n <- n  -> s\n)  * l(<s> <- np) * r(np -> s\n)  = 0.45 * 0.047712 * 0.013230 * 0.34 * 0.66 = 6.37415811648E-5
     * v02(<s> <- n  -> s\np): p(n  => n/n  n) * v01(<s> <- n/n  -> n) * v12(n/n  <- n  -> s\np) * l(<s> <- n)  * r(n  -> s\np) = 0.25 * 0.004216 * 0.011780 * 0.35 * 0.62 = 2.6942980400000003E-6
     * v02(<s> <- n  -> s\n):  p(n  => n/n  n) * v01(<s> <- n/n  -> n) * v12(n/n  <- n  -> s\n)  * l(<s> <- n)  * r(n  -> s\n)  = 0.25 * 0.004216 * 0.011970 * 0.35 * 0.63 = 2.78191179E-6
     * 
     * v13a(n/n  <- s -> <e>): p(s => np s\np) * v12(n/n  <- np -> s\np) * v23(np <- s\np -> <e>) * l(n/n  <- s) * r(s -> <e>) = 0.65 * 0.009620 * 0.020868 * 0.39 * 0.61  = 3.1043000991600005E-5
     * v13a(np/n <- s -> <e>): p(s => np s\np) * v12(np/n <- np -> s\np) * v23(np <- s\np -> <e>) * l(np/n <- s) * r(s -> <e>) = 0.65 * 0.010660 * 0.020868 * 0.43 * 0.61  = 3.7927103775599995E-5
     * v13a(n/s  <- s -> <e>): p(s => np s\np) * v12(n/s  <- np -> s\np) * v23(np <- s\np -> <e>) * l(n/s  <- s) * r(s -> <e>) = 0.65 * 0.011440 * 0.020868 * 0.46 * 0.61  = 4.354195010880001E-5
     * v13b(n/n  <- s -> <e>): p(s => n  s\n)  * v12(n/n  <- n  -> s\n)  * v23(n  <- s\n  -> <e>) * l(n/n  <- s) * r(s -> <e>) = 0.15 * 0.011970 * 0.003825 * 0.39 * 0.61  = 1.6338466462499996E-6
     * v13b(np/n <- s -> <e>): p(s => n  s\n)  * v12(np/n <- n  -> s\n)  * v23(n  <- s\n  -> <e>) * l(np/n <- s) * r(s -> <e>) = 0.15 * 0.013230 * 0.003825 * 0.43 * 0.61  = 1.99104388875E-6
     * v13b(n/s  <- s -> <e>): p(s => n  s\n)  * v12(n/s  <- n  -> s\n)  * v23(n  <- s\n  -> <e>) * l(n/s  <- s) * r(s -> <e>) = 0.15 * 0.014175 * 0.003825 * 0.46 * 0.61  = 2.28209349375E-6
     * 
     * 
     * v03a(<s> <- s -> <e>): p(s -> np s\np) * v02(<s> <- np -> s\np) * v23(n    <- s\np -> <e>) * l(<s> <- s) * r(s -> <e>) = 0.65 * 6.1779358368E-5 * 0.021756         * 0.36 * 0.61 = 1.918527974061817E-7
     * v03b(<s> <- s -> <e>): p(s -> n  s\n ) * v02(<s> <- n  -> s\n)  * v23(n    <- s\n  -> <e>) * l(<s> <- s) * r(s -> <e>) = 0.15 * 2.78191179E-6   * 0.003825         * 0.36 * 0.61 = 3.50508366936945E-10
     * v03a(<s> <- n -> <e>): p(n -> n/s  s)  * v01(<s> <- n/s -> np)  * v13a(n/s <- s    -> <e>) * l(<s> <- n) * r(n -> <e>) = 0.10 * 0.002376        * 4.35419501088E-5 * 0.35 * 0.64 = 2.317407085470598E-9
     * v03b(<s> <- n -> <e>): p(n -> n/s  s)  * v01(<s> <- n/s -> n)   * v13b(n/s <- s    -> <e>) * l(<s> <- n) * r(n -> <e>) = 0.10 * 0.002409        * 2.28209349375E-6 * 0.35 * 0.64 = 1.2314541627234001E-10
     * 
     * p(s) * v03a(<s> <- s -> <e>) = 0.7 * 1.918527974061817E-7   = 1.342969581843272E-7
     * p(s) * v03b(<s> <- s -> <e>) = 0.7 * 3.50508366936945E-10   = 2.453558568558615E-10
     * p(n) * v03a(<s> <- n -> <e>) = 0.2 * 2.317407085470598E-9   = 4.634814170941196E-10
     * p(n) * v03b(<s> <- n -> <e>) = 0.2 * 1.2314541627234001E-10 = 2.4629083254468005E-11
		 */

    val exT1 = CcgBinode(S, CcgBinode(NP, CcgLeaf((NP / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ NP), "run", "FAKEPOS"))
    val exT2 = CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(NP, "dogs", "FAKEPOS"), CcgLeaf((S \ NP), "run", "FAKEPOS")))
    val exT3 = CcgBinode(S, CcgBinode(N, CcgLeaf((N / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ N), "run", "FAKEPOS"))
    val exT4 = CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(N, "dogs", "FAKEPOS"), CcgLeaf((S \ N), "run", "FAKEPOS")))

    val k1 = parser.parseAndProbKBestFromGuideChart(guideChart, 1)
    assertEquals(1, k1.size)
    assertEquals(exT1, k1(0)._1)
    assertEqualsLog(LogDouble(1.342969581843272E-7), k1(0)._2, 1e-10)

    val k2 = parser.parseAndProbKBestFromGuideChart(guideChart, 2)
    assertEquals(2, k2.size)
    assertEquals(exT1, k2(0)._1)
    assertEqualsLog(LogDouble(1.342969581843272E-7), k2(0)._2, 1e-10)
    assertEquals(exT2, k2(1)._1)
    assertEqualsLog(LogDouble(4.634814170941196E-10), k2(1)._2, 1e-10)

    val k3 = parser.parseAndProbKBestFromGuideChart(guideChart, 3)
    assertEquals(3, k3.size)
    assertEquals(exT1, k3(0)._1)
    assertEqualsLog(LogDouble(1.342969581843272E-7), k3(0)._2, 1e-10)
    assertEquals(exT2, k3(1)._1)
    assertEqualsLog(LogDouble(4.634814170941196E-10), k3(1)._2, 1e-10)
    assertEquals(exT3, k3(2)._1)
    assertEqualsLog(LogDouble(2.453558568558615E-10), k3(2)._2, 1e-10)

    val k4 = parser.parseAndProbKBestFromGuideChart(guideChart, 4)
    assertEquals(4, k4.size)
    assertEquals(exT1, k4(0)._1)
    assertEqualsLog(LogDouble(1.342969581843272E-7), k4(0)._2, 1e-10)
    assertEquals(exT2, k4(1)._1)
    assertEqualsLog(LogDouble(4.634814170941196E-10), k4(1)._2, 1e-10)
    assertEquals(exT3, k4(2)._1)
    assertEqualsLog(LogDouble(2.453558568558615E-10), k4(2)._2, 1e-10)
    assertEquals(exT4, k4(3)._1)
    assertEqualsLog(LogDouble(2.4629083254468005E-11), k4(3)._2, 1e-10)

    assertEquals(k4, parser.parseAndProbKBestFromGuideChart(guideChart, 5)) // only 4 parses total
    assertEquals(k4, parser.parseAndProbKBestFromGuideChart(guideChart, 6)) // only 4 parses total
  }

  @Test
  def test_ExactScgParser_parseAndProbKBestFromGuideChart_reorderWithLeftCtx {

    val guideChart = CfgGuideChart("the dogs run".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(ListMap(), ListMap((N / N) -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the"))), (N / S) -> Set(TermGuideChartEntry(TermProd("the")))), ListMap(NP -> Set(BinaryGuideChartEntry(1, BinaryProd((NP / N), N))), N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N)))), ListMap(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / S), S))), S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(NP -> Set(TermGuideChartEntry(TermProd("dogs"))), N -> Set(TermGuideChartEntry(TermProd("dogs")))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(), ListMap((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))), (S \ N) -> Set(TermGuideChartEntry(TermProd("run")))))))

    val rules = Vector(FA, BA)
    val parser = new ExactScgParser(
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
      },
      lctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(left: Cat, given: Cat): LogDouble = LogDouble((left, given) match {
          case (STA, /**/ N / N) /*  */ => 0.31
          case (STA, /**/ NP / N) /* */ => 0.32
          case (STA, /**/ N / S) /*  */ => 0.33
          case (STA, /**/ NP) /*     */ => 0.34
          case (STA, /**/ N) /*      */ => 0.35
          case (STA, /**/ S) /*      */ => 0.36
          case (N / N, /* */ NP) /*  */ => 0.37
          case (N / N, /* */ N) /*   */ => 0.38
          case (N / N, /* */ S) /*   */ => 0.39
          case (NP / N, /**/ NP) /*  */ => 0.41
          case (NP / N, /**/ N) /*   */ => 0.42
          case (NP / N, /**/ S) /*   */ => 0.43
          case (N / S, /* */ NP) /*  */ => 0.44
          case (N / S, /* */ N) /*   */ => 1e100
          case (N / S, /* */ S) /*   */ => 0.46
          case (NP, /* */ S \ NP) /*  */ => 0.47
          case (NP, /* */ S \ N) /*   */ => 0.48
          case (N, /* */ S \ NP) /*   */ => 0.49
          case (N, /* */ S \ N) /*    */ => 0.51
        })
        def sample(given: Cat): Cat = ???
      },
      rctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(right: Cat, given: Cat): LogDouble = LogDouble((given, right) match {
          case (S, /* */ END) /*     */ => 0.61
          case (N, /* */ S \ NP) /*  */ => 0.62
          case (N, /* */ S \ N) /*   */ => 0.63
          case (N, /* */ END) /*     */ => 0.64
          case (NP, /* */ S \ NP) /* */ => 0.65
          case (NP, /* */ S \ N) /*  */ => 0.66
          case (N / N, /* */ NP) /*  */ => 0.67
          case (N / N, /* */ N) /*   */ => 0.68
          case (NP / N, /**/ NP) /*  */ => 0.69
          case (NP / N, /**/ N) /*   */ => 0.71
          case (N / S, /* */ NP) /*  */ => 0.72
          case (N / S, /* */ N) /*   */ => 0.73
          case (S \ NP, /* */ END) /**/ => 0.74
          case (S \ N, /* */ END) /* */ => 0.75
        })
        def sample(given: Cat): Cat = ???
      })(se)

    val T1 = CcgBinode(S, CcgBinode(NP, CcgLeaf((NP / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ NP), "run", "FAKEPOS"))
    val T2 = CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(NP, "dogs", "FAKEPOS"), CcgLeaf((S \ NP), "run", "FAKEPOS")))
    val T3 = CcgBinode(S, CcgBinode(N, CcgLeaf((N / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ N), "run", "FAKEPOS"))
    val T4 = CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(N, "dogs", "FAKEPOS"), CcgLeaf((S \ N), "run", "FAKEPOS")))

    val k1 = parser.parseAndProbKBestFromGuideChart(guideChart, 1)
    assertEquals(1, k1.size)
    assertEquals(T4, k1(0)._1)

    val k2 = parser.parseAndProbKBestFromGuideChart(guideChart, 2)
    assertEquals(2, k2.size)
    assertEquals(T4, k2(0)._1)
    assertEquals(T1, k2(1)._1)

    val k3 = parser.parseAndProbKBestFromGuideChart(guideChart, 3)
    assertEquals(3, k3.size)
    assertEquals(T4, k3(0)._1)
    assertEquals(T1, k3(1)._1)
    assertEquals(T2, k3(2)._1)

    val k4 = parser.parseAndProbKBestFromGuideChart(guideChart, 4)
    assertEquals(4, k4.size)
    assertEquals(T4, k4(0)._1)
    assertEquals(T1, k4(1)._1)
    assertEquals(T2, k4(2)._1)
    assertEquals(T3, k4(3)._1)

    assertEquals(k4, parser.parseAndProbKBestFromGuideChart(guideChart, 5)) // only 4 parses total
    assertEquals(k4, parser.parseAndProbKBestFromGuideChart(guideChart, 6)) // only 4 parses total
  }

  @Test
  def test_ExactScgParser_parseAndProbKBestFromGuideChart_reorderWithRightCtx {

    val guideChart = CfgGuideChart("the dogs run".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(ListMap(), ListMap((N / N) -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the"))), (N / S) -> Set(TermGuideChartEntry(TermProd("the")))), ListMap(NP -> Set(BinaryGuideChartEntry(1, BinaryProd((NP / N), N))), N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N)))), ListMap(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / S), S))), S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(NP -> Set(TermGuideChartEntry(TermProd("dogs"))), N -> Set(TermGuideChartEntry(TermProd("dogs")))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(), ListMap((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))), (S \ N) -> Set(TermGuideChartEntry(TermProd("run")))))))

    val rules = Vector(FA, BA)
    val parser = new ExactScgParser(
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
      },
      lctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(left: Cat, given: Cat): LogDouble = LogDouble((left, given) match {
          case (STA, /**/ N / N) /*  */ => 0.31
          case (STA, /**/ NP / N) /* */ => 0.32
          case (STA, /**/ N / S) /*  */ => 0.33
          case (STA, /**/ NP) /*     */ => 0.34
          case (STA, /**/ N) /*      */ => 0.35
          case (STA, /**/ S) /*      */ => 0.36
          case (N / N, /* */ NP) /*  */ => 0.37
          case (N / N, /* */ N) /*   */ => 0.38
          case (N / N, /* */ S) /*   */ => 0.39
          case (NP / N, /**/ NP) /*  */ => 0.41
          case (NP / N, /**/ N) /*   */ => 0.42
          case (NP / N, /**/ S) /*   */ => 0.43
          case (N / S, /* */ NP) /*  */ => 0.44
          case (N / S, /* */ N) /*   */ => 0.45
          case (N / S, /* */ S) /*   */ => 0.46
          case (NP, /* */ S \ NP) /*  */ => 0.47
          case (NP, /* */ S \ N) /*   */ => 0.48
          case (N, /* */ S \ NP) /*   */ => 0.49
          case (N, /* */ S \ N) /*    */ => 0.51
        })
        def sample(given: Cat): Cat = ???
      },
      rctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(right: Cat, given: Cat): LogDouble = LogDouble((given, right) match {
          case (S, /* */ END) /*     */ => 0.61
          case (N, /* */ S \ NP) /*  */ => 0.62
          case (N, /* */ S \ N) /*   */ => 0.63
          case (N, /* */ END) /*     */ => 0.64
          case (NP, /* */ S \ NP) /* */ => 0.65
          case (NP, /* */ S \ N) /*  */ => 0.66
          case (N / N, /* */ NP) /*  */ => 0.67
          case (N / N, /* */ N) /*   */ => 1e100
          case (NP / N, /**/ NP) /*  */ => 0.69
          case (NP / N, /**/ N) /*   */ => 0.71
          case (N / S, /* */ NP) /*  */ => 0.72
          case (N / S, /* */ N) /*   */ => 0.73
          case (S \ NP, /* */ END) /**/ => 0.74
          case (S \ N, /* */ END) /* */ => 0.75
        })
        def sample(given: Cat): Cat = ???
      })(se)

    val T1 = CcgBinode(S, CcgBinode(NP, CcgLeaf((NP / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ NP), "run", "FAKEPOS"))
    val T2 = CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(NP, "dogs", "FAKEPOS"), CcgLeaf((S \ NP), "run", "FAKEPOS")))
    val T3 = CcgBinode(S, CcgBinode(N, CcgLeaf((N / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ N), "run", "FAKEPOS"))
    val T4 = CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(N, "dogs", "FAKEPOS"), CcgLeaf((S \ N), "run", "FAKEPOS")))

    val k1 = parser.parseAndProbKBestFromGuideChart(guideChart, 1)
    assertEquals(1, k1.size)
    assertEquals(T3, k1(0)._1)

    val k2 = parser.parseAndProbKBestFromGuideChart(guideChart, 2)
    assertEquals(2, k2.size)
    assertEquals(T3, k2(0)._1)
    assertEquals(T1, k2(1)._1)

    val k3 = parser.parseAndProbKBestFromGuideChart(guideChart, 3)
    assertEquals(3, k3.size)
    assertEquals(T3, k3(0)._1)
    assertEquals(T1, k3(1)._1)
    assertEquals(T2, k3(2)._1)

    val k4 = parser.parseAndProbKBestFromGuideChart(guideChart, 4)
    assertEquals(4, k4.size)
    assertEquals(T3, k4(0)._1)
    assertEquals(T1, k4(1)._1)
    assertEquals(T2, k4(2)._1)
    assertEquals(T4, k4(3)._1)

    assertEquals(k4, parser.parseAndProbKBestFromGuideChart(guideChart, 5)) // only 4 parses total
    assertEquals(k4, parser.parseAndProbKBestFromGuideChart(guideChart, 6)) // only 4 parses total
  }

  @Test
  def test_ExactScgParser_parseAndProbKBestFromGuideChart_withUnary() {
    //    val sentence = "the dogs run".split("\\s+").toVector
    //    val tagdict = SimpleTagDictionary[Cat](Map(
    //      "the" -> Set(N / N, NP / N, N / S),
    //      "dogs" -> Set(N),
    //      "run" -> Set(S \ N, S \ NP, S)),
    //      "<S>", STA, "<E>", END)
    //    val builder = new SimpleCfgGuideChartBuilder(Vector(FA, BA))
    //    val Some(table) = builder.build(sentence, Vector.empty, tagdict)
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
    val guideChart = CfgGuideChart("the dogs run".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(ListMap(), ListMap((N / N) -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the"))), (N / S) -> Set(TermGuideChartEntry(TermProd("the")))), ListMap(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N))), NP -> Set(BinaryGuideChartEntry(1, BinaryProd((NP / N), N)), UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))), N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / S), S))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N))))),
      Vector(ListMap(), ListMap(), ListMap(N -> Set(TermGuideChartEntry(TermProd("dogs"))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(), ListMap((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))), (S \ N) -> Set(TermGuideChartEntry(TermProd("run")))))))

    val parser = new ExactScgParser(
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
      },
      lctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(left: Cat, given: Cat): LogDouble = LogDouble((left, given) match {
          case (STA, /**/ N / N) /*  */ => 0.31
          case (STA, /**/ NP / N) /* */ => 0.32
          case (STA, /**/ N / S) /*  */ => 0.33
          case (STA, /**/ NP) /*     */ => 0.34
          case (STA, /**/ N) /*      */ => 0.35
          case (STA, /**/ S) /*      */ => 0.36
          case (N / N, /* */ NP) /*  */ => 0.37
          case (N / N, /* */ N) /*   */ => 0.38
          case (N / N, /* */ S) /*   */ => 0.39
          case (NP / N, /**/ NP) /*  */ => 0.41
          case (NP / N, /**/ N) /*   */ => 0.42
          case (NP / N, /**/ S) /*   */ => 0.43
          case (N / S, /* */ NP) /*  */ => 0.44
          case (N / S, /* */ N) /*   */ => 0.45
          case (N / S, /* */ S) /*   */ => 0.46
          case (NP, /* */ S \ NP) /*  */ => 0.47
          case (NP, /* */ S \ N) /*   */ => 0.48
          case (N, /* */ S \ NP) /*   */ => 0.49
          case (N, /* */ S \ N) /*    */ => 0.51
        })
        def sample(given: Cat): Cat = ???
      },
      rctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(right: Cat, given: Cat): LogDouble = LogDouble((given, right) match {
          case (S, /* */ END) /*     */ => 0.61
          case (N, /* */ S \ NP) /*  */ => 0.62
          case (N, /* */ S \ N) /*   */ => 0.63
          case (N, /* */ END) /*     */ => 0.64
          case (NP, /* */ END) /*    */ => 0.76
          case (NP, /* */ S \ NP) /* */ => 0.65
          case (NP, /* */ S \ N) /*  */ => 0.66
          case (N / N, /* */ NP) /*  */ => 0.67
          case (N / N, /* */ N) /*   */ => 0.68
          case (NP / N, /**/ NP) /*  */ => 0.69
          case (NP / N, /**/ N) /*   */ => 0.71
          case (N / S, /* */ NP) /*  */ => 0.72
          case (N / S, /* */ N) /*   */ => 0.73
          case (S \ NP, /* */ END) /**/ => 0.74
          case (S \ N, /* */ END) /* */ => 0.75
        })
        def sample(given: Cat): Cat = ???
      })(se)

    /*
     * v01(<s> <- n/n  -> n):  p(the|n/n)  * l(<s> <- n/n)  * r(n/n  -> n)  = 0.02 * 0.31 * 0.68 = 0.004216                                             CcgLeaf((N / N), "the")
     * v01(<s> <- n/n  -> np): p(the|n/n)  * l(<s> <- n/n)  * r(n/n  -> np) = 0.02 * 0.31 * 0.67 = 0.004154                                             CcgLeaf((N / N), "the")
     * v01(<s> <- np/n -> n):  p(the|np/n) * l(<s> <- np/n) * r(np/n -> n)  = 0.21 * 0.32 * 0.71 = 0.047712                                             CcgLeaf((NP / N), "the")
     * v01(<s> <- np/n -> np): p(the|np/n) * l(<s> <- np/n) * r(np/n -> np) = 0.21 * 0.32 * 0.69 = 0.046368                                             CcgLeaf((NP / N), "the")
     * v01(<s> <- n/s  -> n):  p(the|n/s)  * l(<s> <- n/s)  * r(n/s  -> n)  = 0.01 * 0.33 * 0.73 = 0.002409                                             CcgLeaf((N / S), "the")
     * v01(<s> <- n/s  -> np): p(the|n/s)  * l(<s> <- n/s)  * r(n/s  -> np) = 0.01 * 0.33 * 0.72 = 0.002376                                             CcgLeaf((N / S), "the")
     * 
     * v12(n/n  <- n  -> s\np): p(dogs|n)                            * l(n/n <- n)   * r(n  -> s\np) = 0.05            * 0.38 * 0.62 = 0.011780         CcgLeaf(N, "dogs")
     * v12(n/n  <- n  -> s\n):  p(dogs|n)                            * l(n/n <- n)   * r(n  -> s\n)  = 0.05            * 0.38 * 0.63 = 0.011970         CcgLeaf(N, "dogs")
     * v12(np/n <- n  -> s\np): p(dogs|n)                            * l(np/n <- n)  * r(n  -> s\np) = 0.05            * 0.42 * 0.62 = 0.013020         CcgLeaf(N, "dogs")
     * v12(np/n <- n  -> s\n):  p(dogs|n)                            * l(np/n <- n)  * r(n  -> s\n)  = 0.05            * 0.42 * 0.63 = 0.013230         CcgLeaf(N, "dogs")
     * v12(n/s  <- n  -> s\np): p(dogs|n)                            * l(n/s <- n)   * r(n  -> s\np) = 0.05            * 0.45 * 0.62 = 0.013950         CcgLeaf(N, "dogs")
     * v12(n/s  <- n  -> s\n):  p(dogs|n)                            * l(n/s <- n)   * r(n  -> s\n)  = 0.05            * 0.45 * 0.63 = 0.014175         CcgLeaf(N, "dogs")
     * v12(n/n  <- np -> s\np): p(np => n) * v12(n/n  <- n  -> s\np) * l(n/n <- np)  * r(np -> s\np) = 0.07 * 0.011780 * 0.37 * 0.65 = 1.983163E-4      CcgUnode(NP, CcgLeaf(N, "dogs"))
     * v12(n/n  <- np -> s\n):  p(np => n) * v12(n/n  <- n  -> s\n)  * l(n/n <- np)  * r(np -> s\n)  = 0.07 * 0.011970 * 0.37 * 0.66 = 2.0461518E-4     CcgUnode(NP, CcgLeaf(N, "dogs"))
     * v12(np/n <- np -> s\np): p(np => n) * v12(np/n <- n  -> s\np) * l(np/n <- np) * r(np -> s\np) = 0.07 * 0.013020 * 0.41 * 0.65 = 2.428881E-4      CcgUnode(NP, CcgLeaf(N, "dogs"))
     * v12(np/n <- np -> s\n):  p(np => n) * v12(np/n <- n  -> s\n)  * l(np/n <- np) * r(np -> s\n)  = 0.07 * 0.013230 * 0.41 * 0.66 = 2.5060266E-4     CcgUnode(NP, CcgLeaf(N, "dogs"))
     * v12(n/s  <- np -> s\np): p(np => n) * v12(n/s  <- n  -> s\np) * l(n/s <- np)  * r(np -> s\np) = 0.07 * 0.013950 * 0.44 * 0.65 = 2.79279E-4       CcgUnode(NP, CcgLeaf(N, "dogs"))
     * v12(n/s  <- np -> s\n):  p(np => n) * v12(n/s  <- n  -> s\n)  * l(n/s <- np)  * r(np -> s\n)  = 0.07 * 0.014175 * 0.44 * 0.66 = 2.881494E-4      CcgUnode(NP, CcgLeaf(N, "dogs"))
     *
     * v23(np <- s\np -> <e>): p(run|s\np) * l(np <- s\np) * r(s\np -> <e>) = 0.06 * 0.47 * 0.74 = 0.020868                                             CcgLeaf((S \ NP), "run")
     * v23(n  <- s\np -> <e>): p(run|s\np) * l(n  <- s\np) * r(s\np -> <e>) = 0.06 * 0.49 * 0.74 = 0.021756                                             CcgLeaf((S \ NP), "run")
     * v23(np <- s\n  -> <e>): p(run|s\n)  * l(np <- s\n)  * r(s\n  -> <e>) = 0.01 * 0.48 * 0.75 = 0.003600                                             CcgLeaf((S \ N), "run")
     * v23(n  <- s\n  -> <e>): p(run|s\n)  * l(n  <- s\n)  * r(s\n  -> <e>) = 0.01 * 0.51 * 0.75 = 0.003825                                             CcgLeaf((S \ N), "run")
     *
     *  
     * v02(<s>  <- n  -> s\np): p(n  => n/n  n) * v01(<s> <- n/n  -> n) * v12(n/n  <- n  -> s\np) * l(<s> <- n)  * r(n  -> s\np) = 0.25 * 0.004216 * 0.011780 * 0.35 * 0.62 = 2.69429804E-6          CcgBinode(N,  CcgLeaf((N / N), "the"),  CcgLeaf(N, "dogs"))
     * v02(<s>  <- n  -> s\n):  p(n  => n/n  n) * v01(<s> <- n/n  -> n) * v12(n/n  <- n  -> s\n)  * l(<s> <- n)  * r(n  -> s\n)  = 0.25 * 0.004216 * 0.011970 * 0.35 * 0.63 = 2.78191179E-6          CcgBinode(N,  CcgLeaf((N / N), "the"),  CcgLeaf(N, "dogs"))
     * v02a(<s> <- np -> s\np): p(np => np/n n) * v01(<s> <- np/n -> n) * v12(np/n <- n  -> s\np) * l(<s> <- np) * r(np -> s\np) = 0.45 * 0.047712 * 0.013020 * 0.34 * 0.65 = 6.1779358368E-5        CcgBinode(NP, CcgLeaf((NP / N), "the"), CcgLeaf(N, "dogs"))
     * v02a(<s> <- np -> s\n):  p(np => np/n n) * v01(<s> <- np/n -> n) * v12(np/n <- n  -> s\n)  * l(<s> <- np) * r(np -> s\n)  = 0.45 * 0.047712 * 0.013230 * 0.34 * 0.66 = 6.37415811648E-5       CcgBinode(NP, CcgLeaf((NP / N), "the"), CcgLeaf(N, "dogs"))
     * v02b(<s> <- np -> s\np): p(np => n)      * v02(<s> <- n  -> s\np)                          * l(<s> <- np) * r(np -> s\np) = 0.07 * 2.69429804E-6       * 0.34 * 0.65 = 4.16807906788E-8       CcgUnode(NP,  CcgBinode(N,  CcgLeaf((N / N), "the"),  CcgLeaf(N, "dogs")))
     * v02b(<s> <- np -> s\n):  p(np => n)      * v02(<s> <- n  -> s\n)                           * l(<s> <- np) * r(np -> s\n)  = 0.07 * 2.78191179E-6       * 0.34 * 0.66 = 4.369827039732E-8      CcgUnode(NP,  CcgBinode(N,  CcgLeaf((N / N), "the"),  CcgLeaf(N, "dogs")))
     * 
     * v13a(n/n  <- s -> <e>): p(s => np s\np) * v12(n/n  <- np -> s\np) * v23(n <- s\np -> <e>) * l(n/n  <- s) * r(s -> <e>) = 0.65 * 1.983163E-4 * 0.021756 * 0.39 * 0.61 = 6.67183442694678E-7    CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs")), CcgLeaf((S \ NP), "run"))
     * v13a(np/n <- s -> <e>): p(s => np s\np) * v12(np/n <- np -> s\np) * v23(n <- s\np -> <e>) * l(np/n <- s) * r(s -> <e>) = 0.65 * 2.428881E-4 * 0.021756 * 0.43 * 0.61 = 9.00942210996282E-7    CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs")), CcgLeaf((S \ NP), "run"))
     * v13a(n/s  <- s -> <e>): p(s => np s\np) * v12(n/s  <- np -> s\np) * v23(n <- s\np -> <e>) * l(n/s  <- s) * r(s -> <e>) = 0.65 * 2.79279E-4  * 0.021756 * 0.46 * 0.61 = 1.1082005317983603E-6  CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs")), CcgLeaf((S \ NP), "run"))
     * v13b(n/n  <- s -> <e>): p(s => n  s\n)  * v12(n/n  <- n  -> s\n)  * v23(n <- s\n  -> <e>) * l(n/n  <- s) * r(s -> <e>) = 0.15 * 0.011970    * 0.003825 * 0.39 * 0.61 = 1.63384664625E-6       CcgBinode(S, CcgLeaf(N, "dogs"), CcgLeaf((S \ N), "run"))
     * v13b(np/n <- s -> <e>): p(s => n  s\n)  * v12(np/n <- n  -> s\n)  * v23(n <- s\n  -> <e>) * l(np/n <- s) * r(s -> <e>) = 0.15 * 0.013230    * 0.003825 * 0.43 * 0.61 = 1.99104388875E-6       CcgBinode(S, CcgLeaf(N, "dogs"), CcgLeaf((S \ N), "run"))
     * v13b(n/s  <- s -> <e>): p(s => n  s\n)  * v12(n/s  <- n  -> s\n)  * v23(n <- s\n  -> <e>) * l(n/s  <- s) * r(s -> <e>) = 0.15 * 0.014175    * 0.003825 * 0.46 * 0.61 = 2.28209349375E-6       CcgBinode(S, CcgLeaf(N, "dogs"), CcgLeaf((S \ N), "run"))
     * 
     * 
     * v03a(<s> <- s  -> <e>): p(s  => np s\np) * v02a(<s> <- np -> s\np) * v23(n    <- s\np -> <e>) * l(<s> <- s)  * r(s  -> <e>) = 0.65 * 6.1779358368E-5  * 0.021756              * 0.36 * 0.61 = 1.918527974061817E-7     CcgBinode(S, CcgBinode(NP, CcgLeaf((NP / N), "the"), CcgLeaf(N, "dogs")), CcgLeaf((S \ NP), "run"))
     * v03c(<s> <- s  -> <e>): p(s  => np s\np) * v02b(<s> <- np -> s\np) * v23(n    <- s\np -> <e>) * l(<s> <- s)  * r(s  -> <e>) = 0.65 * 4.16807906788E-8 * 0.021756              * 0.36 * 0.61 = 1.2943767143381805E-10   CcgBinode(S, CcgUnode(NP,  CcgBinode(N,  CcgLeaf((N / N), "the"),  CcgLeaf(N, "dogs"))), CcgLeaf((S \ NP), "run"))
     * v03b(<s> <- s  -> <e>): p(s  => n  s\n)  * v02(<s>  <- n  -> s\n)  * v23(n    <- s\n  -> <e>) * l(<s> <- s)  * r(s  -> <e>) = 0.15 * 2.78191179E-6    * 0.003825              * 0.36 * 0.61 = 3.50508366936945E-10     CcgBinode(S, CcgBinode(N,  CcgLeaf((N / N), "the"),  CcgLeaf(N, "dogs")), CcgLeaf((S \ N), "run")) 
     * v03a(<s> <- n  -> <e>): p(n  => n/s  s)  * v01(<s> <- n/s -> n)    * v13a(n/s <- s    -> <e>) * l(<s> <- n)  * r(n  -> <e>) = 0.10 * 0.002409         * 1.1082005317983603E-6 * 0.35 * 0.64 = 5.98002738166904E-11     CcgBinode(N, CcgLeaf((N / S), "the"), CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs")), CcgLeaf((S \ NP), "run"))) 
     * v03b(<s> <- n  -> <e>): p(n  => n/s  s)  * v01(<s> <- n/s -> n)    * v13b(n/s <- s    -> <e>) * l(<s> <- n)  * r(n  -> <e>) = 0.10 * 0.002409         * 2.28209349375E-6      * 0.35 * 0.64 = 1.2314541627234E-10      CcgBinode(N, CcgLeaf((N / S), "the"), CcgLeaf(N, "dogs"), CcgLeaf((S \ N), "run"))
     * v03a(<s> <- np -> <e>): p(np => n)       * v03a(<s> <- n  -> <e>)                             * l(<s> <- np) * r(np -> <e>) = 0.07 * 5.98002738166904E-11                     * 0.34 * 0.76 = 1.081667352796296E-12    CcgUnode(NP, CcgBinode(N, CcgLeaf((N / S), "the"), CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs")), CcgLeaf((S \ NP), "run")))) 
     * v03b(<s> <- np -> <e>): p(np => n)       * v03b(<s> <- n  -> <e>)                             * l(<s> <- np) * r(np -> <e>) = 0.07 * 1.2314541627234E-10                      * 0.34 * 0.76 = 2.2274542895340862E-12   CcgUnode(NP, CcgBinode(N, CcgLeaf((N / S), "the"), CcgLeaf(N, "dogs"), CcgLeaf((S \ N), "run")))
     * 
     * p(s)  * v03a(<s> <- s  -> <e>) = 0.7 * 1.918527974061817E-7   = 1.342969581843272E-7      CcgBinode(S, CcgBinode(NP, CcgLeaf((NP / N), "the"), CcgLeaf(N, "dogs")), CcgLeaf((S \ NP), "run"))
     * p(s)  * v03b(<s> <- s  -> <e>) = 0.7 * 3.50508366936945E-10   = 2.453558568558615E-10     CcgBinode(S, CcgBinode(N,  CcgLeaf((N / N), "the"),  CcgLeaf(N, "dogs")), CcgLeaf((S \ N), "run")) 
     * p(s)  * v03c(<s> <- s  -> <e>) = 0.7 * 1.2943767143381805E-10 = 9.060637000367263E-11     CcgBinode(S, CcgUnode(NP,  CcgBinode(N,  CcgLeaf((N / N), "the"),  CcgLeaf(N, "dogs"))), CcgLeaf((S \ NP), "run"))
     * p(n)  * v03b(<s> <- n  -> <e>) = 0.2 * 1.2314541627234E-10    = 2.4629083254468E-11       CcgBinode(N, CcgLeaf((N / S), "the"), CcgLeaf(N, "dogs"), CcgLeaf((S \ N), "run"))
     * p(n)  * v03a(<s> <- n  -> <e>) = 0.2 * 5.98002738166904E-11   = 1.196005476333808E-11     CcgBinode(N, CcgLeaf((N / S), "the"), CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs")), CcgLeaf((S \ NP), "run"))) 
     * p(np) * v03b(<s> <- np -> <e>) = 0.3 * 2.2274542895340862E-12 = 6.682362868602258E-13     CcgUnode(NP, CcgBinode(N, CcgLeaf((N / S), "the"), CcgLeaf(N, "dogs"), CcgLeaf((S \ N), "run")))
     * p(np) * v03a(<s> <- np -> <e>) = 0.3 * 1.081667352796296E-12  = 3.245002058388888E-13     CcgUnode(NP, CcgBinode(N, CcgLeaf((N / S), "the"), CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs")), CcgLeaf((S \ NP), "run")))) 
		 */

    val T1 = CcgBinode(S, CcgBinode(NP, CcgLeaf((NP / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ NP), "run", "FAKEPOS"))
    val T2 = CcgBinode(S, CcgBinode(N, CcgLeaf((N / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ N), "run", "FAKEPOS"))
    val T3 = CcgBinode(S, CcgUnode(NP, CcgBinode(N, CcgLeaf((N / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS"))), CcgLeaf((S \ NP), "run", "FAKEPOS"))
    val T4 = CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(N, "dogs", "FAKEPOS"), CcgLeaf((S \ N), "run", "FAKEPOS")))
    val T5 = CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ NP), "run", "FAKEPOS")))
    val T6 = CcgUnode(NP, CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(N, "dogs", "FAKEPOS"), CcgLeaf((S \ N), "run", "FAKEPOS"))))
    val T7 = CcgUnode(NP, CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ NP), "run", "FAKEPOS"))))

    val k1 = parser.parseAndProbKBestFromGuideChart(guideChart, 1)
    assertEquals(1, k1.size)
    assertEquals(T1, k1(0)._1)
    assertEqualsLog(LogDouble(1.3429695818432720E-7), k1(0)._2, 1e-10)

    val k2 = parser.parseAndProbKBestFromGuideChart(guideChart, 2)
    assertEquals(2, k2.size)
    assertEquals(T1, k2(0)._1)
    assertEqualsLog(LogDouble(1.3429695818432720E-7), k2(0)._2, 1e-10)
    assertEquals(T2, k2(1)._1)
    assertEqualsLog(LogDouble(2.453558568558615E-10), k2(1)._2, 1e-10)

    val k3 = parser.parseAndProbKBestFromGuideChart(guideChart, 3)
    assertEquals(3, k3.size)
    assertEquals(T1, k3(0)._1)
    assertEqualsLog(LogDouble(1.3429695818432720E-7), k3(0)._2, 1e-10)
    assertEquals(T2, k3(1)._1)
    assertEqualsLog(LogDouble(2.453558568558615E-10), k3(1)._2, 1e-10)
    assertEquals(T3, k3(2)._1)
    assertEqualsLog(LogDouble(9.060637000367263E-11), k3(2)._2, 1e-10)

    val k4 = parser.parseAndProbKBestFromGuideChart(guideChart, 4)
    assertEquals(4, k4.size)
    assertEquals(T1, k4(0)._1)
    assertEqualsLog(LogDouble(1.3429695818432720E-7), k4(0)._2, 1e-10)
    assertEquals(T2, k4(1)._1)
    assertEqualsLog(LogDouble(2.453558568558615E-10), k4(1)._2, 1e-10)
    assertEquals(T3, k4(2)._1)
    assertEqualsLog(LogDouble(9.060637000367263E-11), k4(2)._2, 1e-10)
    assertEquals(T4, k4(3)._1)
    assertEqualsLog(LogDouble(2.462908325446800E-11), k4(3)._2, 1e-10)

    // ...

    val k7 = parser.parseAndProbKBestFromGuideChart(guideChart, 7)
    assertEquals(7, k7.size)
    assertEquals(T1, k7(0)._1)
    assertEqualsLog(LogDouble(1.3429695818432720E-7), k7(0)._2, 1e-10)
    assertEquals(T2, k7(1)._1)
    assertEqualsLog(LogDouble(2.453558568558615E-10), k7(1)._2, 1e-10)
    assertEquals(T3, k7(2)._1)
    assertEqualsLog(LogDouble(9.060637000367263E-11), k7(2)._2, 1e-10)
    assertEquals(T4, k7(3)._1)
    assertEqualsLog(LogDouble(2.462908325446800E-11), k7(3)._2, 1e-10)
    assertEquals(T5, k7(4)._1)
    assertEqualsLog(LogDouble(1.196005476333808E-11), k7(4)._2, 1e-10)
    assertEquals(T6, k7(5)._1)
    assertEqualsLog(LogDouble(6.682362868602258E-13), k7(5)._2, 1e-10)
    assertEquals(T7, k7(6)._1)
    assertEqualsLog(LogDouble(3.245002058388888E-13), k7(6)._2, 1e-10)

    assertEquals(k7, parser.parseAndProbKBestFromGuideChart(guideChart, 8)) // only 7 parses total
    assertEquals(k7, parser.parseAndProbKBestFromGuideChart(guideChart, 100)) // only 7 parses total
  }

  @Test
  def test_ExactScgParser_weight {
    type Word = Symbol
    val weighter = new ExactScgParser(
      rootDist = new LogProbabilityDistribution[Cat] {
        def apply(b: Cat): LogDouble = LogDouble(b match {
          case A => 0.11
        })
        def sample(): Cat = ???
        def defaultProb: LogDouble = ???
      },
      prodDist = new ConditionalLogProbabilityDistribution[Cat, Prod] {
        def apply(x: Prod, given: Cat): LogDouble = LogDouble((given, x) match {
          case (A, BinaryProd(B, C)) => 0.21
          case (B, TermProd("b")) => 0.22
          case (C, BinaryProd(D, E)) => 0.23
          case (D, TermProd("d")) => 0.24
          case (E, TermProd("e")) => 0.25
        })
        def sample(given: Cat): Prod = ???
      },
      lctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(left: Cat, given: Cat): LogDouble = LogDouble((left, given) match {
          case (STA, A) => 0.31
          case (STA, B) => 0.33
          case (B, C) => 0.32
          case (B, D) => 0.34
          case (D, E) => 0.35
        })
        def sample(given: Cat): Cat = ???
      },
      rctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(right: Cat, given: Cat): LogDouble = LogDouble((given, right) match {
          case (A, END) => 0.41
          case (B, D) => 0.42
          case (C, END) => 0.43
          case (D, E) => 0.44
          case (E, END) => 0.45
        })
        def sample(given: Cat): Cat = ???
      })(SimpleStartEndTags[Cat](STA, END))

    val tree = CcgBinode(A, CcgLeaf(B, "b", "FAKEPOS"), CcgBinode(C, CcgLeaf(D, "d", "FAKEPOS"), CcgLeaf(E, "e", "FAKEPOS")))
    /*
     *                 A
     *           +-----+-----+
     *           |           C
     *           |       +---+---+
     *  <S>      B       D       E      <E>
     *           b       d       e
     *     
     */

    val expected =
      Vector(
        0.11,
        0.21, 0.22, 0.23, 0.24, 0.25,
        0.31, 0.32, 0.33, 0.34, 0.35,
        0.41, 0.42, 0.43, 0.44, 0.45).map(LogDouble(_)).product
    assertEqualsLog(expected, weighter.weight(tree), 1e-9)
  }

  @Test
  def test_ExactScgParser_pcfgWeight {
    type Word = Symbol
    val weighter = new ExactScgParser(
      rootDist = new LogProbabilityDistribution[Cat] {
        def apply(b: Cat): LogDouble = LogDouble(b match {
          case A => 0.11
        })
        def sample(): Cat = ???
        def defaultProb: LogDouble = ???
      },
      prodDist = new ConditionalLogProbabilityDistribution[Cat, Prod] {
        def apply(x: Prod, given: Cat): LogDouble = LogDouble((given, x) match {
          case (A, BinaryProd(B, C)) => 0.21
          case (B, TermProd("b")) => 0.22
          case (C, BinaryProd(D, E)) => 0.23
          case (D, TermProd("d")) => 0.24
          case (E, TermProd("e")) => 0.25
        })
        def sample(given: Cat): Prod = ???
      },
      lctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(left: Cat, given: Cat): LogDouble = ???
        def sample(given: Cat): Cat = ???
      },
      rctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(right: Cat, given: Cat): LogDouble = ???
        def sample(given: Cat): Cat = ???
      })(SimpleStartEndTags[Cat](STA, END))

    val tree = CcgBinode(A, CcgLeaf(B, "b", "FAKEPOS"), CcgBinode(C, CcgLeaf(D, "d", "FAKEPOS"), CcgLeaf(E, "e", "FAKEPOS")))
    /*
     *                 A
     *           +-----+-----+
     *           |           C
     *           |       +---+---+
     *  <S>      B       D       E      <E>
     *           b       d       e
     *     
     */

    val expected =
      Vector(
        0.11,
        0.21, 0.22, 0.23, 0.24, 0.25).map(LogDouble(_)).product
    assertEqualsLog(expected, weighter.pcfgWeight(tree), 1e-9)
  }

  @Test
  def test_ExactScgParser_ctxWeight {
    type Word = Symbol
    val weighter = new ExactScgParser(
      rootDist = new LogProbabilityDistribution[Cat] {
        def apply(b: Cat): LogDouble = LogDouble(b match {
          case A => 0.11
        })
        def sample(): Cat = ???
        def defaultProb: LogDouble = ???
      },
      prodDist = new ConditionalLogProbabilityDistribution[Cat, Prod] {
        def apply(x: Prod, given: Cat): LogDouble = LogDouble((given, x) match {
          case (A, BinaryProd(B, C)) => 0.21
          case (B, TermProd("b")) => 0.22
          case (C, BinaryProd(D, E)) => 0.23
          case (D, TermProd("d")) => 0.24
          case (E, TermProd("e")) => 0.25
        })
        def sample(given: Cat): Prod = ???
      },
      lctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(left: Cat, given: Cat): LogDouble = LogDouble((left, given) match {
          case (STA, A) => 0.31
          case (STA, B) => 0.33
          case (B, C) => 0.32
          case (B, D) => 0.34
          case (D, E) => 0.35
        })
        def sample(given: Cat): Cat = ???
      },
      rctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(right: Cat, given: Cat): LogDouble = LogDouble((given, right) match {
          case (A, END) => 0.41
          case (B, D) => 0.42
          case (C, END) => 0.43
          case (D, E) => 0.44
          case (E, END) => 0.45
        })
        def sample(given: Cat): Cat = ???
      })(SimpleStartEndTags[Cat](STA, END))

    val tree = CcgBinode(A, CcgLeaf(B, "b", "FAKEPOS"), CcgBinode(C, CcgLeaf(D, "d", "FAKEPOS"), CcgLeaf(E, "e", "FAKEPOS")))
    /*
     *                 A
     *           +-----+-----+
     *           |           C
     *           |       +---+---+
     *  <S>      B       D       E      <E>
     *           b       d       e
     *       0       1       2       3
     */

    val expected =
      Vector(
        0.31, 0.32, 0.33, 0.34, 0.35,
        0.41, 0.42, 0.43, 0.44, 0.45).map(LogDouble(_)).product
    assertEqualsLog(expected, weighter.ctxWeight(tree), 1e-9)
  }

  @Test
  def test_ExactScgParser_weight_withUnary {
    type Word = Symbol
    val weighter = new ExactScgParser(
      rootDist = new LogProbabilityDistribution[Cat] {
        def apply(b: Cat): LogDouble = LogDouble(b match {
          case A => 0.11
        })
        def sample(): Cat = ???
        def defaultProb: LogDouble = ???
      },
      prodDist = new ConditionalLogProbabilityDistribution[Cat, Prod] {
        def apply(x: Prod, given: Cat): LogDouble = LogDouble((given, x) match {
          case (A, BinaryProd(H, G)) => 0.21
          case (H, UnaryProd(B)) => 0.28
          case (B, TermProd("b")) => 0.22
          case (G, UnaryProd(C)) => 0.23
          case (C, BinaryProd(D, F)) => 0.26
          case (D, TermProd("d")) => 0.24
          case (F, UnaryProd(E)) => 0.27
          case (E, TermProd("e")) => 0.25
        })
        def sample(given: Cat): Prod = ???
      },
      lctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(left: Cat, given: Cat): LogDouble = LogDouble((left, given) match {
          case (STA, A) => 0.31
          case (STA, H) => 0.36
          case (STA, B) => 0.33
          case (B, G) => 0.38
          case (B, C) => 0.32
          case (B, D) => 0.34
          case (D, F) => 0.37
          case (D, E) => 0.35
        })
        def sample(given: Cat): Cat = ???
      },
      rctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(right: Cat, given: Cat): LogDouble = LogDouble((given, right) match {
          case (A, END) => 0.41
          case (H, D) => 0.48
          case (B, D) => 0.42
          case (G, END) => 0.46
          case (C, END) => 0.43
          case (D, E) => 0.44
          case (F, END) => 0.47
          case (E, END) => 0.45
        })
        def sample(given: Cat): Cat = ???
      })(SimpleStartEndTags[Cat](STA, END))

    val tree =
      CcgBinode(A,
        CcgUnode(H, CcgLeaf(B, "b", "FAKEPOS")),
        CcgUnode(G,
          CcgBinode(C,
            CcgLeaf(D, "d", "FAKEPOS"),
            CcgUnode(F, CcgLeaf(E, "e", "FAKEPOS")))))
    /*
     *                 A
     *           +-----+-----+
     *           |           |
     *           |           G
     *           |           |
     *           |           C
     *           |       +---+---+
     *  <S>      H       D       F      <E>
     *           |       d       |
     *           B               E
     *           b               e
     */

    val expected =
      Vector(
        0.11,
        0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27, 0.28,
        0.31, 0.32, 0.33, 0.34, 0.35, 0.36, 0.37, 0.38,
        0.41, 0.42, 0.43, 0.44, 0.45, 0.46, 0.47, 0.48).map(LogDouble(_)).product
    assertEqualsLog(expected, weighter.weight(tree), 1e-9)
  }

  @Test
  def test_ExactScgParser_pcfgWeight_withUnary {
    type Word = Symbol
    val weighter = new ExactScgParser(
      rootDist = new LogProbabilityDistribution[Cat] {
        def apply(b: Cat): LogDouble = LogDouble(b match {
          case A => 0.11
        })
        def sample(): Cat = ???
        def defaultProb: LogDouble = ???
      },
      prodDist = new ConditionalLogProbabilityDistribution[Cat, Prod] {
        def apply(x: Prod, given: Cat): LogDouble = LogDouble((given, x) match {
          case (A, BinaryProd(H, G)) => 0.21
          case (H, UnaryProd(B)) => 0.28
          case (B, TermProd("b")) => 0.22
          case (G, UnaryProd(C)) => 0.23
          case (C, BinaryProd(D, F)) => 0.26
          case (D, TermProd("d")) => 0.24
          case (F, UnaryProd(E)) => 0.27
          case (E, TermProd("e")) => 0.25
        })
        def sample(given: Cat): Prod = ???
      },
      lctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(left: Cat, given: Cat): LogDouble = ???
        def sample(given: Cat): Cat = ???
      },
      rctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(right: Cat, given: Cat): LogDouble = ???
        def sample(given: Cat): Cat = ???
      })(SimpleStartEndTags[Cat](STA, END))

    val tree =
      CcgBinode(A,
        CcgUnode(H, CcgLeaf(B, "b", "FAKEPOS")),
        CcgUnode(G,
          CcgBinode(C,
            CcgLeaf(D, "d", "FAKEPOS"),
            CcgUnode(F, CcgLeaf(E, "e", "FAKEPOS")))))
    /*
     *                 A
     *           +-----+-----+
     *           |           |
     *           |           G
     *           |           |
     *           |           C
     *           |       +---+---+
     *  <S>      H       D       F      <E>
     *           |       d       |
     *           B               E
     *           b               e
     */

    val expected =
      Vector(
        0.11,
        0.21, 0.22, 0.23, 0.24, 0.25, 0.26, 0.27, 0.28).map(LogDouble(_)).product
    assertEqualsLog(expected, weighter.pcfgWeight(tree), 1e-9)
  }

  @Test
  def test_ExactScgParser_ctxWeight_withUnary {
    type Word = Symbol
    val weighter = new ExactScgParser(
      rootDist = new LogProbabilityDistribution[Cat] {
        def apply(b: Cat): LogDouble = ???
        def sample(): Cat = ???
        def defaultProb: LogDouble = ???
      },
      prodDist = new ConditionalLogProbabilityDistribution[Cat, Prod] {
        def apply(x: Prod, given: Cat): LogDouble = ???
        def sample(given: Cat): Prod = ???
      },
      lctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(left: Cat, given: Cat): LogDouble = LogDouble((left, given) match {
          case (STA, A) => 0.31
          case (STA, H) => 0.36
          case (STA, B) => 0.33
          case (B, G) => 0.38
          case (B, C) => 0.32
          case (B, D) => 0.34
          case (D, F) => 0.37
          case (D, E) => 0.35
        })
        def sample(given: Cat): Cat = ???
      },
      rctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
        def apply(right: Cat, given: Cat): LogDouble = LogDouble((given, right) match {
          case (A, END) => 0.41
          case (H, D) => 0.48
          case (B, D) => 0.42
          case (G, END) => 0.46
          case (C, END) => 0.43
          case (D, E) => 0.44
          case (F, END) => 0.47
          case (E, END) => 0.45
        })
        def sample(given: Cat): Cat = ???
      })(SimpleStartEndTags[Cat](STA, END))

    val tree =
      CcgBinode(A,
        CcgUnode(H, CcgLeaf(B, "b", "FAKEPOS")),
        CcgUnode(G,
          CcgBinode(C,
            CcgLeaf(D, "d", "FAKEPOS"),
            CcgUnode(F, CcgLeaf(E, "e", "FAKEPOS")))))
    /*
     *                 A
     *           +-----+-----+
     *           |           |
     *           |           G
     *           |           |
     *           |           C
     *           |       +---+---+
     *  <S>      H       D       F      <E>
     *           |       d       |
     *           B               E
     *           b               e
     */

    val expected =
      Vector(
        0.31, 0.32, 0.33, 0.34, 0.35, 0.36, 0.37, 0.38,
        0.41, 0.42, 0.43, 0.44, 0.45, 0.46, 0.47, 0.48).map(LogDouble(_)).product
    assertEqualsLog(expected, weighter.ctxWeight(tree), 1e-9)
  }

  def assertEqualsLog(a: LogDouble, b: LogDouble, e: Double) {
    assertEquals(a.toDouble, b.toDouble, e)
  }
}
