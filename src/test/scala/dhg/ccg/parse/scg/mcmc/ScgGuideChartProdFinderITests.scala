package dhg.ccg.parse.scg.mcmc

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.scg._
import dhg.ccg.tagdict.StartEndTags
import dhg.util._
import dhg.ccg.tagdict._
import dhg.ccg.test.TestUtil.DoubleIteratorRandomGenerator
import org.apache.commons.math3.random.MersenneTwister
import scala.collection.immutable.ListMap
import scalaz._
import Scalaz._
import dhg.ccg.util._
import scala.collection.immutable.BitSet

class ScgGuideChartProdFinderITests {

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
  def x_test_SimpleScgGuideChartProdFinder {

    type Word = String

    //    val sentence = "the dogs run".split("\\s+").toVector
    val tagdict = SimpleTagDictionary[Cat](Map(
      "the" -> Set(N / N, NP / N, N / S, NP / NP),
      "dogs" -> Set(N),
      "run" -> Set(S \ N, S \ NP, S)),
      "<S>", STA, "<E>", END)
    val rules = Set[CcgRule](FA, BA, N2NP)

    val catIndexer = new SimpleIndexer(Vector(STA, END) ++ CcgRule.allDerivable(rules, tagdict.allTags -- Vector(STA, END)).toVector.sorted)
    //println(f"catIndexer.length=${catIndexer.size}  ::   ${catIndexer.objects}")
    val wordIndexer = SimpleIndexer(tagdict.allWords ++ Vector("something", "else"))

    val allCats = BitSet.empty ++ catIndexer.indices
    val numCats = catIndexer.size
    val numWords = wordIndexer.size
    //val (binaryRulesI, unaryRulesI) = CcgRuleI.makeIndexedRules(rules, catIndexer)
    val (bProdsO, uProdsO, tProdsO) = CcgRule.allDerivableProds(rules, tagdict)
    val prodsO: Map[Cat, Set[Prod]] = (bProdsO.mapVals(_.asInstanceOf[Set[Prod]]) |+| uProdsO.mapVals(_.asInstanceOf[Set[Prod]]) |+| tProdsO.mapVals(_.asInstanceOf[Set[Prod]]))
    val allProdsO: Set[Prod] = prodsO.flatMap(_._2).toSet
    val allProds: Set[ProdI] = allProdsO.map(ProdI.to(_, catIndexer, wordIndexer))

    //    val binyProds = DenseVec(bProdsO.mapt { (t, prods) => catIndexer(t) -> IndirectSparseVec(prods.map { case prod @ BinaryProd(u, v) => (catIndexer(u), (catIndexer(v), prodIndexer(prod))) }.groupByKey.mapVals(xs => IndirectSparseVec(xs.toMap, numCats)), numCats) }, numCats).values
    //    val unryProds = IndirectSparseVec(uProdsO.mapt { (t, prods) => catIndexer(t) -> IndirectSparseVec(prods.map { case prod @ UnaryProd(u) => (catIndexer(u), prodIndexer(prod)) }.toMap, numCats) }, numCats)
    //    val termProds = DenseVec(tProdsO.mapt { (t, prods) => catIndexer(t) -> IndirectSparseVec(prods.map { case prod @ TermProd(w) => (wordIndexer(w), prodIndexer(prod)) }.toMap, numWords) }, numCats).values

    //    println("all biny prods indexed")
    //    binyProds.activePairs.map {
    //      case (t, us) =>
    //        println("  " + catIndexer.obj(t))
    //        us.activePairs.map {
    //          case (u, vs) =>
    //            println("    " + catIndexer.obj(u))
    //            vs.activePairs.map {
    //              case (v, p) =>
    //                println("      " + catIndexer.obj(v) + " " + prodIndexer.obj(p))
    //
    //            }
    //        }
    //    }

    //    val builder = new SimpleCfgGuideChartBuilder(rules)
    //    val Some(table) = builder.build(sentence, Vector.empty, tagdict)
    //    table.draw()
    //    println(table.repr)
    //println(f"${table.numPossibleParses}")
    //  +--------------------+--------------------+--------------------+
    //  |                    | (0,1)              | (0,2)              |
    //  | (N/N) -> "the"   X | N -> 1:[(N/N) N] * | S -> 2:[N (S\N)]   |
    //  | (NP/N) -> "the"    | NP -> 1:[(NP/N) N] |      2:[NP (S\NP)] |
    //  | (N/S) -> "the"     |       N            | N -> 1:[(N/S) S]   |
    //  |                    |                    | NP -> N            |
    //  +--------------------+--------------------+--------------------+
    //  |                    |                    | (1,2)              |
    //  |                    | N -> "dogs"        | S -> 2:[N (S\N)] X |
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
    val mockGuideChart1 = CfgGuideChart("the dogs run".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(ListMap(), ListMap((N / N) -> Set(TermGuideChartEntry(TermProd("the"))), (NP / N) -> Set(TermGuideChartEntry(TermProd("the"))), (N / S) -> Set(TermGuideChartEntry(TermProd("the")))), ListMap(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N))), NP -> Set(BinaryGuideChartEntry(1, BinaryProd((NP / N), N)), UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))), N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / S), S))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N))))),
      Vector(ListMap(), ListMap(), ListMap(N -> Set(TermGuideChartEntry(TermProd("dogs"))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(), ListMap((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))), (S \ N) -> Set(TermGuideChartEntry(TermProd("run")))))))
    val mockGuideChart2 = CfgGuideChart("the dogs run".splitWhitespace, Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](
      Vector(ListMap(), ListMap((NP / N) -> Set(TermGuideChartEntry(TermProd("the"))), (N / S) -> Set(TermGuideChartEntry(TermProd("the")))), ListMap(N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / N), N))), NP -> Set(BinaryGuideChartEntry(1, BinaryProd((NP / N), N)), BinaryGuideChartEntry(2, BinaryProd(NP / NP, NP)), UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(N, (S \ N))), BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))), N -> Set(BinaryGuideChartEntry(1, BinaryProd((N / S), S))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N))))),
      Vector(ListMap(), ListMap(), ListMap(N -> Set(TermGuideChartEntry(TermProd("dogs"))), NP -> Set(UnaryGuideChartEntry(UnaryProd(N)))), ListMap(S -> Set(BinaryGuideChartEntry(2, BinaryProd(NP, (S \ NP)))))),
      Vector(ListMap(), ListMap(), ListMap(), ListMap((S \ NP) -> Set(TermGuideChartEntry(TermProd("run"))), (S \ N) -> Set(TermGuideChartEntry(TermProd("run")))))))
    val guideChart1I = CfgGuideChartI.to(mockGuideChart1, catIndexer, wordIndexer)
    val guideChart2I = CfgGuideChartI.to(mockGuideChart2, catIndexer, wordIndexer)

    val gcu = new SimpleScgGuideChartProdFinderI(catIndexer, wordIndexer)
    val (gcRoots, gcBinys, gcUnrys, gcTerms, gcLctxs, gcRctxs) = gcu.all(Array(guideChart1I, guideChart2I), numCats, numWords, 0, 1)

    assertEquals(3, gcRoots.length)
    assertEquals(Set(S, N, NP), gcRoots.map(catIndexer.obj).toSet)

    assertEquals(11, gcBinys.length)
    assertEquals(3, gcBinys.count(_ != null))
    assertEquals(11, gcBinys(catIndexer(N)).length)
    assertEquals(2, gcBinys(catIndexer(N)).activeCount)
    assertEquals(1, gcBinys(catIndexer(N))(catIndexer(N / S)).length)
    assertEquals(Set(S), gcBinys(catIndexer(N))(catIndexer(N / S)).map(catIndexer.obj).toSet)
    assertEquals(1, gcBinys(catIndexer(N))(catIndexer(N / N)).length)
    assertEquals(Set(N), gcBinys(catIndexer(N))(catIndexer(N / N)).map(catIndexer.obj).toSet)
    assertEquals(11, gcBinys(catIndexer(S)).length)
    assertEquals(2, gcBinys(catIndexer(S)).activeCount)
    assertEquals(1, gcBinys(catIndexer(S))(catIndexer(N)).length)
    assertEquals(Set(S \ N), gcBinys(catIndexer(S))(catIndexer(N)).map(catIndexer.obj).toSet)
    assertEquals(1, gcBinys(catIndexer(S))(catIndexer(NP)).length)
    assertEquals(Set(S \ NP), gcBinys(catIndexer(S))(catIndexer(NP)).map(catIndexer.obj).toSet)
    assertEquals(11, gcBinys(catIndexer(NP)).length)
    assertEquals(2, gcBinys(catIndexer(NP)).activeCount)
    assertEquals(1, gcBinys(catIndexer(NP))(catIndexer(NP / N)).length)
    assertEquals(Set(N), gcBinys(catIndexer(NP))(catIndexer(NP / N)).map(catIndexer.obj).toSet)
    assertEquals(1, gcBinys(catIndexer(NP))(catIndexer(NP / NP)).length)
    assertEquals(Set(NP), gcBinys(catIndexer(NP))(catIndexer(NP / NP)).map(catIndexer.obj).toSet)

    assertEquals(11, gcUnrys.length)
    assertEquals(1, gcUnrys.activeCount)
    assertEquals(1, gcUnrys(catIndexer(NP)).length)
    assertEquals(Set(N), gcUnrys(catIndexer(NP)).map(catIndexer.obj).toSet)

    assertEquals(11, gcTerms.length)
    assertEquals(6, gcTerms.count(_ != null))
    assertEquals(1, gcTerms(catIndexer(N / N)).length)
    assertEquals(Set("the"), gcTerms(catIndexer(N / N)).map(wordIndexer.obj).toSet)
    assertEquals(1, gcTerms(catIndexer(NP / N)).length)
    assertEquals(Set("the"), gcTerms(catIndexer(NP / N)).map(wordIndexer.obj).toSet)
    assertEquals(1, gcTerms(catIndexer(N / S)).length)
    assertEquals(Set("the"), gcTerms(catIndexer(N / S)).map(wordIndexer.obj).toSet)
    assertEquals(1, gcTerms(catIndexer(N)).length)
    assertEquals(Set("dogs"), gcTerms(catIndexer(N)).map(wordIndexer.obj).toSet)
    assertEquals(1, gcTerms(catIndexer(S \ NP)).length)
    assertEquals(Set("run"), gcTerms(catIndexer(S \ NP)).map(wordIndexer.obj).toSet)
    assertEquals(1, gcTerms(catIndexer(S \ N)).length)
    assertEquals(Set("run"), gcTerms(catIndexer(S \ N)).map(wordIndexer.obj).toSet)
    
    ???
  }

}
