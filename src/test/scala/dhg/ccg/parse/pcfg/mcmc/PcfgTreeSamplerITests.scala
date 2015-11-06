package dhg.ccg.parse.pcfg.mcmc

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.util._
import scala.collection.immutable.ListMap
import scalaz._
import Scalaz._
import dhg.ccg.util._
import scala.collection.immutable.BitSet
import org.apache.commons.math3.random.MersenneTwister
import scala.util.Random

class PcfgTreeSamplerITests {

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
  def i_test_SimplePcfgTreeSampler_sample {
    type Word = String

    //    val sentence = "the dogs run".split("\\s+").toVector
    val tagdict = SimpleTagDictionary[Cat](Map(
      "the" -> Set(N / N, NP / N, N / S),
      "dogs" -> Set(N),
      "run" -> Set(S \ N, S \ NP, S)),
      "<S>", STA, "<E>", END)
    val rules = Set[CcgRule](FA, BA, N2NP)

    val catIndexer = CatIndexer(tagdict.allTags, rules)
    val wordIndexer = SimpleIndexer(tagdict.allWords)

    val allCats = BitSet.empty ++ catIndexer.indices
    val numCats = catIndexer.size
    val mockNumCats = numCats
    val numWords = wordIndexer.size
    //val (binaryRulesI, unaryRulesI) = CcgRuleI.makeIndexedRules(rules, catIndexer)
    val (bProdsO, uProdsO, tProdsO) = CcgRule.allDerivableProds(rules, tagdict)
    val prodsO: Map[Cat, Set[Prod]] = (bProdsO.mapVals(_.asInstanceOf[Set[Prod]]) |+| uProdsO.mapVals(_.asInstanceOf[Set[Prod]]) |+| tProdsO.mapVals(_.asInstanceOf[Set[Prod]]))
    val allProdsO: Set[Prod] = prodsO.flatMap(_._2).toSet
    val allProds: Set[ProdI] = allProdsO.map(ProdI.to(_, catIndexer, wordIndexer))

    val prodIndexer = SimpleIndexer[Prod](allProdsO)

    //    val builder = new SimpleCfgGuideChartBuilder(rules)
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
    val mockGuideChartO = CfgGuideChart("the dogs run".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(ListMap(), ListMap((N / N) -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the"))), (N / S) -> Set(TermGuideChartEntry(TermProd("the")))), ListMap(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N))), NP -> Set(BinaryGuideChartEntry(1, BinaryProd((NP / N), N)), UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))), N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / S), S))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N))))),
      Vector(ListMap(), ListMap(), ListMap(N -> Set(TermGuideChartEntry(TermProd("dogs"))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(), ListMap((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))), (S \ N) -> Set(TermGuideChartEntry(TermProd("run")))))))
    val mockGuideChart = CfgGuideChartI.to(mockGuideChartO, catIndexer, wordIndexer)

    val rootDistO = Map(
      S -> LogDouble(0.5),
      N -> LogDouble(0.3),
      NP -> LogDouble(0.2))

    val binyDistO = Vector[(Cat, (BinaryProd, LogDouble))](
      ((NP), /**/ (BinaryProd(NP / N, N) /* */ , LogDouble(0.30))),
      ((N), /* */ (BinaryProd(N / N, N) /*  */ , LogDouble(0.25))),
      ((N), /* */ (BinaryProd(N / S, S) /*  */ , LogDouble(0.40))),
      ((S), /* */ (BinaryProd(NP, S \ NP) /**/ , LogDouble(0.35))),
      ((S), /* */ (BinaryProd(N, S \ N) /*  */ , LogDouble(0.20))))
      .groupByKey.mapVals(_.toMap)

    val unryDistO = Vector[(Cat, (UnaryProd, LogDouble))](
      (NP, (UnaryProd(N), LogDouble(0.85))))
      .groupByKey.mapVals(_.toMap)

    val termDistO = Vector[(Cat, (TermProd, LogDouble))](
      ((NP / N), /**/ (TermProd("the") /* */ , LogDouble(0.03))),
      ((N / N), /* */ (TermProd("the") /* */ , LogDouble(0.02))),
      ((N / S), /* */ (TermProd("the") /* */ , LogDouble(0.01))),
      ((N), /*     */ (TermProd("dogs") /**/ , LogDouble(0.05))),
      ((NP), /*    */ (TermProd("dogs") /**/ , LogDouble(0.04))),
      ((S \ NP), /**/ (TermProd("run") /* */ , LogDouble(0.06))),
      ((S \ N), /* */ (TermProd("run") /* */ , LogDouble(0.05))),
      ((S), /*     */ (TermProd("run") /* */ , LogDouble(0.04))))
      .groupByKey.mapVals(_.toMap)

    val mockLogRootDist: IndirectSparseVec[Double] = IndirectSparseVec(rootDistO.map { case (t, p) => catIndexer(t) -> p.logValue }, numCats)
    val mockLogPmixDist = catIndexer.objects.toArray.map { t => Array(LogDouble(Random.nextDouble).logValue, LogDouble(Random.nextDouble).logValue, LogDouble(Random.nextDouble).logValue) }
    val mockLogBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]] = DenseVec(binyDistO.map { case (t, prods) => catIndexer(t) -> IndirectSparseVec(prods.toVector.collect { case (BinaryProd(u, v), p) => (catIndexer(u), (catIndexer(v), p.logValue - mockLogPmixDist(catIndexer(t))(0))) }.groupByKey.mapVals(vps => IndirectSparseVec(vps.toMap, numCats)), numCats) }, numCats).values
    val mockLogUnryDist: IndirectSparseVec[IndirectSparseVec[Double]] = IndirectSparseVec(unryDistO.map { case (t, prods) => catIndexer(t) -> IndirectSparseVec(prods.collect { case (UnaryProd(u), p) => (catIndexer(u), p.logValue - mockLogPmixDist(catIndexer(t))(1)) }, numCats) }, numCats)
    val mockLogTermDist: Array[Vec[Double]] = DenseVec(termDistO.map { case (t, prods) => catIndexer(t) -> SparseVec(prods.collect { case (TermProd(w), p) => (wordIndexer(w), p.logValue - mockLogPmixDist(catIndexer(t))(2)) }, numWords) }, numCats).values

    val T1 = CcgTreeI.to(CcgBinode(S, CcgBinode(NP, CcgLeaf((NP / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ NP), "run", "FAKEPOS")), catIndexer, wordIndexer)
    val T2 = CcgTreeI.to(CcgBinode(S, CcgUnode(NP, CcgBinode(N, CcgLeaf((N / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS"))), CcgLeaf((S \ NP), "run", "FAKEPOS")), catIndexer, wordIndexer)
    val T3 = CcgTreeI.to(CcgBinode(S, CcgBinode(N, CcgLeaf((N / N), "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ N), "run", "FAKEPOS")), catIndexer, wordIndexer)
    val T4 = CcgTreeI.to(CcgUnode(NP, CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ NP), "run", "FAKEPOS")))), catIndexer, wordIndexer)
    val T5 = CcgTreeI.to(CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgUnode(NP, CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf((S \ NP), "run", "FAKEPOS"))), catIndexer, wordIndexer)
    val T6 = CcgTreeI.to(CcgUnode(NP, CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(N, "dogs", "FAKEPOS"), CcgLeaf((S \ N), "run", "FAKEPOS")))), catIndexer, wordIndexer)
    val T7 = CcgTreeI.to(CcgBinode(N, CcgLeaf((N / S), "the", "FAKEPOS"), CcgBinode(S, CcgLeaf(N, "dogs", "FAKEPOS"), CcgLeaf((S \ N), "run", "FAKEPOS"))), catIndexer, wordIndexer)

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
    val mockMatrix = Array(
      Array[IndirectSparseVec[LogDouble]](IndirectSparseVec.empty[LogDouble](numCats),
        IndirectSparseVec(Map((N / N) -> LogDouble(0.02), (NP / N) -> LogDouble(0.03), (N / S) -> LogDouble(0.01)).mapKeys(catIndexer), numCats),
        IndirectSparseVec(Map(NP -> LogDouble(6.625E-4), N -> LogDouble(2.5E-4)).mapKeys(catIndexer), numCats),
        IndirectSparseVec(Map(NP -> LogDouble(4.7345E-6), N -> LogDouble(5.57E-6), S -> LogDouble(1.64125E-5)).mapKeys(catIndexer), numCats)),
      Array[IndirectSparseVec[LogDouble]](IndirectSparseVec.empty[LogDouble](numCats), IndirectSparseVec.empty[LogDouble](numCats),
        IndirectSparseVec(Map(NP -> LogDouble(0.0425), N -> LogDouble(0.05)).mapKeys(catIndexer), numCats),
        IndirectSparseVec(Map(S -> LogDouble(0.0013925)).mapKeys(catIndexer), numCats)),
      Array[IndirectSparseVec[LogDouble]](IndirectSparseVec.empty[LogDouble](numCats), IndirectSparseVec.empty[LogDouble](numCats), IndirectSparseVec.empty[LogDouble](numCats),
        IndirectSparseVec(Map((S \ NP) -> LogDouble(0.06), (S \ N) -> LogDouble(0.05)).mapKeys(catIndexer), numCats)))
    val mockLogChart = Chart.tabulate[IndirectSparseVec[Double]](3) { (i, j) => IndirectSparseVec(mockMatrix(i)(j).activePairs.map { case (a, b) => (a, b.logValue) }, numCats) }
    val mockInsideChart = new PcfgInsideChartI(mockLogChart)
    //    ic.draw()
    //    mockInsideChart.draw()
    //    for (i <- 0 until 3; j <- i + 1 to 3) assertEqualsLogMap(ic(i,j), mockInsideChart(i,j), 1e-9)
    val mockInsideChartBuilder = new PcfgInsideChartBuilderI {
      def buildInsideChart(
        guideChart: CfgGuideChartI,
        //logRootDist: IndirectSparseVec[Double], //                         t -> p
        logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
        logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
        logTermDist: Array[Vec[Double]], //                  t -> w -> p
        logPmixDist: Array[Array[Double]], //                      t -> p
        numCats: Int): PcfgInsideChartI = {
        assertSame(mockGuideChart, guideChart)
        //assertSame(mockRootDist, logRootDist)
        assertSame(mockLogBinyDist, logBinyDist)
        assertSame(mockLogUnryDist, logUnryDist)
        assertSame(mockLogTermDist, logTermDist)
        assertSame(mockLogPmixDist, logPmixDist)
        assertEquals(mockNumCats, numCats)
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

    val rand = new MersenneTwister()
    val ts = new SimplePcfgTreeSamplerI(mockInsideChartBuilder, rand)(catIndexer: Indexer[Cat], wordIndexer: Indexer[String])

    val n = 100000
    def check(samples: Array[CcgTreeI]) {
      val sampleCounts = samples.toVector.counts
      val nd = n.toDouble
      assertEquals(7, sampleCounts.size)
      assertEquals(0.4399, sampleCounts.getOrElse(T1, 0) / nd, 5e-2)
      assertEquals(0.2077, sampleCounts.getOrElse(T2, 0) / nd, 5e-2)
      assertEquals(0.1164, sampleCounts.getOrElse(T3, 0) / nd, 5e-2)
      assertEquals(0.0848, sampleCounts.getOrElse(T4, 0) / nd, 5e-2)
      assertEquals(0.0665, sampleCounts.getOrElse(T5, 0) / nd, 5e-2)
      assertEquals(0.0475, sampleCounts.getOrElse(T6, 0) / nd, 5e-2)
      assertEquals(0.0372, sampleCounts.getOrElse(T7, 0) / nd, 5e-2)
    }

    check(
      time("k=1",
        Array.fill(n)(ts.sample(mockGuideChart,
          mockLogRootDist: IndirectSparseVec[Double], //                         t -> p
          mockLogBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
          mockLogUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
          mockLogTermDist: Array[Vec[Double]], //                  t -> w -> p
          mockLogPmixDist: Array[Array[Double]], //                       t -> (bmix,umix,tmix)
          mockNumCats: Int)) //
          ) //
          )

    check(
      time("k=2",
        Array.fill(n / 10)(ts.samples(mockGuideChart,
          mockLogRootDist: IndirectSparseVec[Double], //                         t -> p
          mockLogBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
          mockLogUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
          mockLogTermDist: Array[Vec[Double]], //                  t -> w -> p
          mockLogPmixDist: Array[Array[Double]], //                       t -> (bmix,umix,tmix)
          mockNumCats: Int,
          k = 10)) //
          ) //
        .flatten)

    check(
      time("k>1",
        ts.samples(mockGuideChart,
          mockLogRootDist: IndirectSparseVec[Double], //                         t -> p
          mockLogBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
          mockLogUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
          mockLogTermDist: Array[Vec[Double]], //                  t -> w -> p
          mockLogPmixDist: Array[Array[Double]], //                       t -> (bmix,umix,tmix)
          mockNumCats: Int,
          k = n) //
          ) // 
          )

  }

  private[this] def assertEqualsLogMap[T](e: Map[T, LogDouble], a: Map[T, LogDouble], r: Double) = {
    assertEquals(e.keySet, a.keySet)
    for ((k, ev) <- e) assertEquals(ev.toDouble, a(k).toDouble, r)
  }
}
