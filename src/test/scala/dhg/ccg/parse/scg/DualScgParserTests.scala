package dhg.ccg.parse.scg

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.parse.pcfg._
import scala.collection.immutable.ListMap
import dhg.ccg.tagdict.SimpleStartEndTags
import dhg.util._

class DualScgParserTests {

  val s = cat"S".asInstanceOf[AtomCat]
  val np = cat"NP".asInstanceOf[AtomCat]
  val n = cat"N".asInstanceOf[AtomCat]
  val pp = cat"PP".asInstanceOf[AtomCat]

  val A: NonPuncCat = cat"A".asInstanceOf[AtomCat]
  val B: NonPuncCat = cat"B".asInstanceOf[AtomCat]
  val C: NonPuncCat = cat"C".asInstanceOf[AtomCat]
  val D: NonPuncCat = cat"D".asInstanceOf[AtomCat]
  val E: NonPuncCat = cat"E".asInstanceOf[AtomCat]
  val F: NonPuncCat = cat"F".asInstanceOf[AtomCat]

  val S: NonPuncCat = cat"S".asInstanceOf[AtomCat]
  val NP: NonPuncCat = cat"NP".asInstanceOf[AtomCat]
  val N: NonPuncCat = cat"N".asInstanceOf[AtomCat]
  val PP: NonPuncCat = cat"PP".asInstanceOf[AtomCat]
  val STA = cat"<S>"
  val END = cat"<E>"
  val SE = new SimpleStartEndTags(STA: Cat, END: Cat)

  @Test
  def test_DualScgParser {
    /*
     * 
     *                S
     *         +------+-------+
     *        S/B             |
     *    +----+----+         |
     *    D      (S/B)\A      B
     *
     *  
     *                S
     *         +------+-------+
     *         |              |
     *         D             S\D
     *         |              |
     *         |             S\A
     *         |         +----+----+
     *         C      (S\A)/B      B
     * 
     */

    //  +--------------------------+--------------------------+--------------------------+
    //  |                          | (0,1)                    | (0,2)                    |
    //  | C -> "1"                 | (S/B) -> 1:[C ((S/B)\C)] | S -> 2:[(S/B) B]         |
    //  | D -> C                   |                          |      1:[D (S\D)]         |
    //  |      "1"                 |                          |                          |
    //  +--------------------------+--------------------------+--------------------------+
    //  |                          |                          | (1,2)                    |
    //  |                          | ((S/B)\C) -> "2"         | (S\A) -> 2:[((S\A)/B) B] |
    //  |                          | ((S\A)/B) -> "2"         | (S\D) -> (S\A)           |
    //  |                          |                          |                          |
    //  +--------------------------+--------------------------+--------------------------+
    //  |                          |                          |                          |
    //  |                          |                          | B -> "3"                 |
    //  |                          |                          |                          |
    //  |                          |                          |                          |
    //  +--------------------------+--------------------------+--------------------------+
    //    val tagdict = SimpleTagDictionary[Cat](Map(
    //      "1" -> Set(C, D),
    //      "2" -> Set((S \ A) / B, (S / B) \ C),
    //      "3" -> Set(B)),
    //      "<S>", STA, "<E>", END)
    //    val rules = Vector(FA, BA,
    //      new UnaryCcgRule { override def apply(sub: Cat): Option[Cat] = sub match { case C => Some(D); case _ => None } },
    //      new UnaryCcgRule { override def apply(sub: Cat): Option[Cat] = sub match { case S \ A => Some(S \ D); case _ => None } })
    //    val gcb = new SimpleCfgGuideChartBuilder(rules)
    //    val Some(gc) = gcb.build(Vector("1", "2", "3"), Vector.empty, tagdict)
    //    println(gc.repr)
    //    gc.draw()

    val gc = CfgGuideChart("1 2 3".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(ListMap(), ListMap(C -> Set(TermGuideChartEntry(TermProd("1"))), D -> Set(UnaryGuideChartEntry(UnaryProd(C)), TermGuideChartEntry(TermProd("1")))), ListMap((S / B) -> Set(BinaryGuideChartEntry(1, BinaryProd(C, ((S / B) \ C))))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd((S / B), B)), BinaryGuideChartEntry(1, BinaryProd(D, (S \ D)))))),
      Vector(ListMap(), ListMap(), ListMap(((S / B) \ C) -> Set(TermGuideChartEntry(TermProd("2"))), ((S \ A) / B) -> Set(TermGuideChartEntry(TermProd("2")))), ListMap((S \ A) -> Set(BinaryGuideChartEntry(2, BinaryProd(((S \ A) / B), B))), (S \ D) -> Set(UnaryGuideChartEntry(UnaryProd((S \ A)))))),
      Vector(ListMap(), ListMap(), ListMap(), ListMap(B -> Set(TermGuideChartEntry(TermProd("3")))))))

    val rootDist = new LaplaceExpProbabilityDistribution[Cat](Map(), None, None, 0.2, 5.0)
    val prodDist = new SimpleConditionalExpProbabilityDistribution[Cat, Prod](Map(
      (S) -> new LaplaceExpProbabilityDistribution[Prod](Map(
        BinaryProd(A, S \ A) -> 4.0,
        BinaryProd(S / B, B) -> 6.0), None, None, 0.2, 5.0)), None, None, new LaplaceExpProbabilityDistribution[Prod](Map(), None, None, 0.2, 5.0))
    val lctxDist = new SimpleConditionalExpProbabilityDistribution[Cat, Cat](Map(
      (S \ A) -> new LaplaceExpProbabilityDistribution[Cat](Map(
        (A) -> 40.0), None, None, 0.2, 5.0)), None, None, new LaplaceExpProbabilityDistribution[Cat](Map(), None, None, 0.2, 5.0))
    val rctxDist = new SimpleConditionalExpProbabilityDistribution[Cat, Cat](Map(
      (S / B) -> new LaplaceExpProbabilityDistribution[Cat](Map(
        (B) -> 1.0), None, None, 0.2, 5.0)), None, None, new LaplaceExpProbabilityDistribution[Cat](Map(), None, None, 0.2, 5.0))

    val parser = new DualScgParser(
      new Double2LogProbabilityDistributionAdapter(rootDist), new Double2LogConditionalProbabilityDistributionAdapter(prodDist), new Double2LogConditionalProbabilityDistributionAdapter(lctxDist), new Double2LogConditionalProbabilityDistributionAdapter(rctxDist),
      //new PcfgParser(new Double2LogProbabilityDistributionAdapter(rootDist), new Double2LogConditionalProbabilityDistributionAdapter(prodDist)),
      maxIterations = 50,
      //new SimpleScgWeighter(),
      verbose = true)(SE)
    //parser.parseFromSupertags(Vector("she" -> n, "eats" -> (s \ n)))
    val ot = parser.parseAndProbFromGuideChart(gc)
    println(ot)
    println(ot.fold("none")(_._1.pretty))
  }

  @Test
  def test_DualScgParser_parseAndProbFromGuideChart_1 {
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

    val rootDist = new LogProbabilityDistribution[Cat] {
      def apply(b: Cat): LogDouble = b match {
        case S => LogDouble(0.7)
        case N => LogDouble(0.2)
      }
      def sample(): Cat = ???
      def defaultProb: LogDouble = ???
    }
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
      }
      def sample(given: Cat): Prod = ???
    }
    val lctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
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
    }
    val rctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
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
    }
    val parser = new DualScgParser(
      rootDist, prodDist, lctxDist, rctxDist,
      //new PcfgParser(rootDist, prodDist),
      maxIterations = 50,
      //new SimpleScgWeighter(),
      verbose = false)(SE)

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

    val k1 = parser.parseAndProbFromGuideChart(guideChart)
    assertTrue(k1.isDefined)
    assertEquals(exT1, k1.get._1)
    assertEqualsLog(LogDouble(1.342969581843272E-7), k1.get._2, 1e-10)
    assertEquals(1, parser.ddConverge.intValue)
    assertEquals(1, parser.ddAttempts.intValue)
  }

  @Test
  def test_DualScgParser_parseAndProbFromGuideChart_reorderWithLeftCtx {

    val guideChart = CfgGuideChart("the dogs run".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(ListMap(), ListMap((N / N) -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the"))), (N / S) -> Set(TermGuideChartEntry(TermProd("the")))), ListMap(NP -> Set(BinaryGuideChartEntry(1, BinaryProd((NP / N), N))), N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N)))), ListMap(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / S), S))), S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(NP -> Set(TermGuideChartEntry(TermProd("dogs"))), N -> Set(TermGuideChartEntry(TermProd("dogs")))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(), ListMap((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))), (S \ N) -> Set(TermGuideChartEntry(TermProd("run")))))))

    val rootDist = new LogProbabilityDistribution[Cat] {
      def apply(b: Cat): LogDouble = b match {
        case S => LogDouble(0.7)
        case N => LogDouble(0.2)
      }
      def sample(): Cat = ???
      def defaultProb: LogDouble = ???
    }
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
      }
      def sample(given: Cat): Prod = ???
    }
    val lctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
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
    }
    val rctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
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
    }
    val parser = new DualScgParser(
      rootDist, prodDist, lctxDist, rctxDist,
      //new PcfgParser(rootDist, prodDist),
      maxIterations = 50,
      //new SimpleScgWeighter(),
      verbose = true)(SE)

    val T4 = CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(N, "dogs", "FAKEPOS"), CcgLeaf((S \ N), "run", "FAKEPOS")))
    val T1 = CcgBinode(S, CcgBinode(NP, CcgLeaf((NP / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ NP), "run", "FAKEPOS"))
    val T2 = CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(NP, "dogs", "FAKEPOS"), CcgLeaf((S \ NP), "run", "FAKEPOS")))
    val T3 = CcgBinode(S, CcgBinode(N, CcgLeaf((N / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ N), "run", "FAKEPOS"))

    val k1 = parser.parseAndProbFromGuideChart(guideChart)
    assertTrue(k1.isDefined)
    println(">>> " + k1.get._1.repr)
    assertEquals(T4, k1.get._1)
    assertEquals(0, parser.ddConverge.intValue)
    assertEquals(1, parser.ddAttempts.intValue)
  }

  @Test
  def test_DualScgParser_parseAndProbFromGuideChart_reorderWithRightCtx {

    val guideChart = CfgGuideChart("the dogs run".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(ListMap(), ListMap((N / N) -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the"))), (N / S) -> Set(TermGuideChartEntry(TermProd("the")))), ListMap(NP -> Set(BinaryGuideChartEntry(1, BinaryProd((NP / N), N))), N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N)))), ListMap(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / S), S))), S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(NP -> Set(TermGuideChartEntry(TermProd("dogs"))), N -> Set(TermGuideChartEntry(TermProd("dogs")))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(), ListMap((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))), (S \ N) -> Set(TermGuideChartEntry(TermProd("run")))))))

    val rootDist = new LogProbabilityDistribution[Cat] {
      def apply(b: Cat): LogDouble = b match {
        case S => LogDouble(0.7)
        case N => LogDouble(0.2)
      }
      def sample(): Cat = ???
      def defaultProb: LogDouble = ???
    }
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
      }
      def sample(given: Cat): Prod = ???
    }
    val lctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
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
    }
    val rctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
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
    }
    val parser = new DualScgParser(
      rootDist, prodDist, lctxDist, rctxDist,
      //new PcfgParser(rootDist, prodDist),
      maxIterations = 50,
      //new SimpleScgWeighter(),
      verbose = false)(SE)

    val T3 = CcgBinode(S, CcgBinode(N, CcgLeaf((N / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ N), "run", "FAKEPOS"))
    val T1 = CcgBinode(S, CcgBinode(NP, CcgLeaf((NP / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ NP), "run", "FAKEPOS"))
    val T2 = CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(NP, "dogs", "FAKEPOS"), CcgLeaf((S \ NP), "run", "FAKEPOS")))
    val T4 = CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(N, "dogs", "FAKEPOS"), CcgLeaf((S \ N), "run", "FAKEPOS")))

    val k1 = parser.parseAndProbFromGuideChart(guideChart)
    assertTrue(k1.isDefined)
    assertEquals(T3, k1.get._1)
    assertEquals(1, parser.ddConverge.intValue)
    assertEquals(1, parser.ddAttempts.intValue)
  }

  @Test
  def test_DualScgParser_parseAndProbFromGuideChart_withUnary() {
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

    val rootDist = new LogProbabilityDistribution[Cat] {
      def apply(b: Cat): LogDouble = b match {
        case S => LogDouble(0.7)
        case N => LogDouble(0.2)
        case NP => LogDouble(0.3)
      }
      def sample(): Cat = ???
      def defaultProb: LogDouble = ???
    }
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
    val lctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
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
    }
    val rctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
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
    }
    val parser = new DualScgParser(
      rootDist, prodDist, lctxDist, rctxDist,
      //new PcfgParser(rootDist, prodDist),
      maxIterations = 50,
      //new SimpleScgWeighter(),
      verbose = false)(SE)

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

    val k1 = parser.parseAndProbFromGuideChart(guideChart)
    assertTrue(k1.isDefined)
    assertEquals(T1, k1.get._1)
    assertEqualsLog(LogDouble(1.3429695818432720E-7), k1.get._2, 1e-10)
    assertEquals(1, parser.ddConverge.intValue)
    assertEquals(1, parser.ddAttempts.intValue)
  }

  def assertEqualsLog(a: LogDouble, b: LogDouble, e: Double) {
    assertEquals(a.toDouble, b.toDouble, e)
  }
}
