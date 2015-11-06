package dhg.ccg.parse.pcfg.typesup

import dhg.util._
import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse.pcfg._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.util._
import scala.collection.immutable.ListMap
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.parse.scg.mcmc.SimpleScgGuideChartProdFinderI

class CtxDistInitializerITests {

  val S: AtomCat = cat"S".asInstanceOf[AtomCat]
  val NP: AtomCat = cat"NP".asInstanceOf[AtomCat]
  val N: AtomCat = cat"N".asInstanceOf[AtomCat]
  val PP: AtomCat = cat"PP".asInstanceOf[AtomCat]
  val STA = cat"<S>"
  val END = cat"<E>"

  @Test
  def test_TagdictCtxDistInitializerI {

    val combinableMultiplier = 10.0

    val tagdict = SimpleTagDictionary[Cat](Map(
      "the" -> Set(N / N, NP / N, N / S, NP / NP),
      "dogs" -> Set(N),
      "run" -> Set(S \ N, S \ NP, S)),
      "<S>", STA, "<E>", END)
    val rules = Set[CcgRule](FA, BA, N2NP)

    val catIndexer = new SimpleIndexer(Vector(STA, END) ++ CcgRule.allDerivable(rules, tagdict.allTags -- Vector(STA, END)).toVector.sorted)
    val wordIndexer = SimpleIndexer(tagdict.allWords ++ Vector("something", "else"))
    val numCats = catIndexer.size
    val numWords = wordIndexer.size

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
    val gcs = Vector(guideChart1I, guideChart2I)

    val gcu = new SimpleScgGuideChartProdFinderI(catIndexer, wordIndexer)
    val (knownRoots, knownBinys, knownUnrys, knownTerms,
      knownLctxs: Array[Array[Int]], //                    t -> ls
      knownRctxs: Array[Array[Int]]) //                    t -> rs
      =
      gcu.all(gcs.toArray, numCats, numWords, startCat = 0, endCat = 1)

      
      
//    val canCombine = new SimpleCatCanCombine(Vector(FA, FAn2np, BA, BAn2np, FC, FC2, BC, /*FX,*/ BX, BX2), StartCat, EndCat)
//    knownRctxs.zipWithIndex.foreach{
//      case (rs,t) => 
//        if(rs!=null)
//        rs.foreach { r => 
//      println(f"ckjx::  ${catIndexer.obj(t)} -> ${catIndexer.obj(r)} -> ${canCombine(catIndexer.obj(t), catIndexer.obj(r))}")
//      }
//    }
    
    
    val lcdi = new UniformCombLctxDistInitializerI(catIndexer, wordIndexer)
    val lctxDist = lcdi.apply(
      gcs, //: Vector[CfgGuideChartI],
      knownLctxs, //: Array[Array[Int]], //                 t -> rs
      combinableMultiplier,
      numCats)

    // (0,1)
    assertEquals(LogDouble(10/ 100.0), lctxDist(catIndexer(N / N) ) (catIndexer(STA)) )
    assertEquals(LogDouble(10/ 100.0), lctxDist(catIndexer(NP / N)) (catIndexer(STA)) )
    assertEquals(LogDouble(10/ 100.0), lctxDist(catIndexer(N / S) ) (catIndexer(STA)) )

    // (1,2)
    assertEquals(LogDouble(10/ 100.0), lctxDist(catIndexer(N) )  (catIndexer(N/N)))
    assertEquals(LogDouble(10/ 100.0), lctxDist(catIndexer(N) )  (catIndexer(NP/N)))
    assertEquals(LogDouble(1 / 100.0), lctxDist(catIndexer(N) )  (catIndexer(N/S)))
    assertEquals(LogDouble(1 / 100.0), lctxDist(catIndexer(NP) )  (catIndexer(N/N)))
    assertEquals(LogDouble(1 / 100.0), lctxDist(catIndexer(NP) )  (catIndexer(NP/N)))
    assertEquals(LogDouble(1 / 100.0), lctxDist(catIndexer(NP) )  (catIndexer(N/S)))

    // (2,3)
    assertEquals(LogDouble(10/ 100.0), lctxDist(catIndexer(S \ NP)) (catIndexer(N)))
    assertEquals(LogDouble(10/ 100.0), lctxDist(catIndexer(S \ N)) (catIndexer(N)))

    // (0,2)
    assertEquals(LogDouble(10/ 100.0), lctxDist(catIndexer(N) ) (catIndexer(STA)))
    assertEquals(LogDouble(10/ 100.0), lctxDist(catIndexer(NP)) (catIndexer(STA)))

    // (1,3)
    assertEquals(LogDouble(1 / 100.0), lctxDist(catIndexer(S) )  (catIndexer(N/N)))
    assertEquals(LogDouble(1 / 100.0), lctxDist(catIndexer(S) )  (catIndexer(NP/N)))
    assertEquals(LogDouble(10/ 100.0), lctxDist(catIndexer(S) )  (catIndexer(N/S)))

    // (0,3)
    assertEquals(LogDouble(10/ 100.0), lctxDist(catIndexer(S) ) (catIndexer(STA)))
    assertEquals(LogDouble(10/ 100.0), lctxDist(catIndexer(N) ) (catIndexer(STA)))
    assertEquals(LogDouble(10/ 100.0), lctxDist(catIndexer(NP)) (catIndexer(STA)))

    
    
    val rcdi = new UniformCombRctxDistInitializerI(catIndexer, wordIndexer)
    val rctxDist = rcdi.apply(
      gcs, //: Vector[CfgGuideChartI],
      knownRctxs, //: Array[Array[Int]], //                 t -> rs
      combinableMultiplier,
      numCats)

    // (0,1)
    assertEquals(LogDouble(10/ 100.0), rctxDist(catIndexer(N / N) ) (catIndexer(N)) )
    assertEquals(LogDouble(10/ 100.0), rctxDist(catIndexer(NP / N)) (catIndexer(N)) )
    assertEquals(LogDouble(1 / 100.0), rctxDist(catIndexer(N / S) ) (catIndexer(N)) )

    // (1,2)
    assertEquals(LogDouble(10/ 100.0), rctxDist(catIndexer(N) )  (catIndexer(S \ NP)))
    assertEquals(LogDouble(10/ 100.0), rctxDist(catIndexer(N) )  (catIndexer(S \ N)))
    assertEquals(LogDouble(10/ 100.0), rctxDist(catIndexer(NP)) (catIndexer(S \ NP)))
    assertEquals(LogDouble(1 / 100.0), rctxDist(catIndexer(NP)) (catIndexer(S \ N)))

    // (2,3)
    assertEquals(LogDouble(10/ 100.0), rctxDist(catIndexer(S \ NP)) (catIndexer(END)))
    assertEquals(LogDouble(10/ 100.0), rctxDist(catIndexer(S \ N)) (catIndexer(END)))

    // (0,2)
    assertEquals(LogDouble(10/ 100.0), rctxDist(catIndexer(N) ) (catIndexer(S \ NP)))
    assertEquals(LogDouble(10/ 100.0), rctxDist(catIndexer(N) ) (catIndexer(S \ N)))
    assertEquals(LogDouble(10/ 100.0), rctxDist(catIndexer(NP)) (catIndexer(S \ NP)))
    assertEquals(LogDouble(1 / 100.0), rctxDist(catIndexer(NP)) (catIndexer(S \ N)))

    // (1,3)
    assertEquals(LogDouble(10/ 100.0), rctxDist(catIndexer(S)) (catIndexer(END)))

    // (0,3)
    assertEquals(LogDouble(10/ 100.0), rctxDist(catIndexer(S) ) (catIndexer(END)))
    assertEquals(LogDouble(10/ 100.0), rctxDist(catIndexer(N) ) (catIndexer(END)))
    assertEquals(LogDouble(10/ 100.0), rctxDist(catIndexer(NP)) (catIndexer(END)))

  }

}

