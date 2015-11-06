package dhg.ccg.parse.scg.mcmc

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
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

class ScgProductionCounterITests {

  val A: Cat = cat"A"
  val B: Cat = cat"B"
  val C: Cat = cat"C"
  val D: Cat = cat"D"
  val E: Cat = cat"E"
  val F: Cat = cat"F"
  val G: Cat = cat"G"
  val H: Cat = cat"H"

  val S: Cat = cat"S"
  val NP: Cat = cat"NP"
  val N: Cat = cat"N"
  val PP: Cat = cat"PP"
  val STA: Cat = cat"<S>"
  val END: Cat = cat"<E>"

  @Test
  def i_test_SimpleScgProductionFinder {
    type Word = String

    val tagdict = SimpleTagDictionary[Cat](Map(
      "a1" -> Set(A),
      "a2" -> Set(A),
      "a3" -> Set(A),
      "b1" -> Set(B),
      "c1" -> Set(C),
      "d1" -> Set(D),
      "e1" -> Set(E),
      "e2" -> Set(E)),
      "<S>", STA, "<E>", END)
    val rules = Set[CcgRule](FA, BA, N2NP)

    val catIndexer = new SimpleIndexer(Vector(STA, END) ++ CcgRule.allDerivable(rules, tagdict.allTags -- Vector(STA, END)).toVector.sorted)
    val wordIndexer = SimpleIndexer(tagdict.allWords)

    val allCats = BitSet.empty ++ catIndexer.indices
    val numCats = catIndexer.size
    val numWords = wordIndexer.size

    /*
     *              A
     *            /   \  
     *         B         C
     *         |       /   \
     *         A      A     B
     *        / \    / \    |
     * <s>   B   C  B   A   A  <e>
     *       b1  c1 b1  a1  a2
     *       
     *              A
     *            /   \  
     *         B         C
     *         |       /   \
     *         A      C     B
     *        / \     |     |
     * <s>   B   C    B     A  <e>
     *       b1  c1   b1   a2
     */

    val t1 =
      CcgBinode(A,
        CcgUnode(B,
          CcgBinode(A,
            CcgLeaf(B, "b1", "FAKEPOS"),
            CcgLeaf(C, "c1", "FAKEPOS"))),
        CcgBinode(C,
          CcgBinode(A,
            CcgLeaf(B, "b1", "FAKEPOS"),
            CcgLeaf(A, "a1", "FAKEPOS")),
          CcgUnode(B,
            CcgLeaf(A, "a2", "FAKEPOS"))))
    val t2 =
      CcgBinode(A,
        CcgUnode(B,
          CcgBinode(A,
            CcgLeaf(B, "b1", "FAKEPOS"),
            CcgLeaf(C, "c1", "FAKEPOS"))),
        CcgBinode(C,
          CcgUnode(C,
            CcgLeaf(B, "b1", "FAKEPOS")),
          CcgUnode(B,
            CcgLeaf(A, "a2", "FAKEPOS"))))
    val trees = Array(t1, t2).map(CcgTreeI.to(_, catIndexer, wordIndexer))

    val bProdsO: Map[Cat, Set[Prod]] = Map(
      A -> Set(BinaryProd(A, C), BinaryProd(B, A), BinaryProd(B, C)),
      B -> Set(BinaryProd(A, C)),
      C -> Set(BinaryProd(A, B), BinaryProd(C, B)),
      D -> Set(BinaryProd(A, B), BinaryProd(C, B)))
    val uProdsO: Map[Cat, Set[Prod]] = Map(
      B -> Set(UnaryProd(A)),
      C -> Set(UnaryProd(A), UnaryProd(B)))
    val tProdsO: Map[Cat, Set[Prod]] = Map(
      A -> Set(TermProd("a1"), TermProd("a2"), TermProd("a3")),
      B -> Set(TermProd("b1"), TermProd("c1")),
      C -> Set(TermProd("b1"), TermProd("c1")),
      D -> Set(TermProd("d1")))

    val knownRoots = Array(catIndexer(A), catIndexer(C))
    val knownBinys: Array[IndirectSparseVec[Array[Int]]] = DenseVec(bProdsO.map { case (t, prods) => catIndexer(t) -> IndirectSparseVec(prods.collect { case BinaryProd(u, v) => (catIndexer(u), catIndexer(v)) }.groupByKey.mapVals(_.toArray.sorted), numCats) }, numCats).values
    val knownUnrys: IndirectSparseVec[Array[Int]] = IndirectSparseVec(uProdsO.map { case (t, prods) => catIndexer(t) -> prods.collect { case UnaryProd(u) => catIndexer(u) }.toArray.sorted }, numCats)
    val knownTerms: Array[Array[Int]] = DenseVec(tProdsO.map { case (t, prods) => catIndexer(t) -> prods.collect { case TermProd(w) => wordIndexer(w) }.toArray.sorted }, numCats).values
    val knownLctxs = DenseVec(Map(
      A -> Set(STA, A, B, C),
      B -> Set(STA, A, B, C),
      C -> Set(STA, B, C),
      D -> Set(A, B))
      .map { case (t, ls) => catIndexer(t) -> ls.map(catIndexer).toArray.sorted }, numCats).values
    val knownRctxs = DenseVec(Map(
      A -> Set(A, B, C, END),
      B -> Set(A, B, C, END),
      C -> Set(A, B, END),
      D -> Set(B, C))
      .map { case (t, rs) => catIndexer(t) -> rs.map(catIndexer).toArray.sorted }, numCats).values

    val counter = new SimpleScgProductionCounterI(catIndexer, wordIndexer)
    val (rootCounts, binyCounts, unryCounts, termCounts, pmixCounts, lctxCounts, rctxCounts) =
      counter.counts(trees: Array[CcgTreeI], trees.length,
        knownRoots: Array[Int], //                           ts
        knownBinys: Array[IndirectSparseVec[Array[Int]]], //         t -> u -> vs
        knownUnrys: IndirectSparseVec[Array[Int]], //                t -> us
        knownTerms: Array[Array[Int]], //                    t -> ws
        knownLctxs: Array[Array[Int]], //                    t -> ls
        knownRctxs: Array[Array[Int]], //                    t -> rs
        numCats: Int, numWords: Int,
        startCat = 0, endCat = 1) //

    assertEquals(7, rootCounts.length)
    assertEquals(2, rootCounts.activeCount)
    assertEquals(2, rootCounts(catIndexer(A)), 1e-9)
    assertEquals(0, rootCounts(catIndexer(C)), 1e-9)

    assertEquals(7, binyCounts.length)
    assertEquals(7, binyCounts(catIndexer(A)).length)
    assertEquals(2, binyCounts(catIndexer(A)).activeCount)
    assertEquals(7, binyCounts(catIndexer(A))(catIndexer(A)).length)
    assertEquals(1, binyCounts(catIndexer(A))(catIndexer(A)).activeCount)
    assertEquals(0, binyCounts(catIndexer(A))(catIndexer(A))(catIndexer(C)), 1e-9)
    assertEquals(7, binyCounts(catIndexer(A))(catIndexer(B)).length)
    assertEquals(2, binyCounts(catIndexer(A))(catIndexer(B)).activeCount)
    assertEquals(1, binyCounts(catIndexer(A))(catIndexer(B))(catIndexer(A)), 1e-9)
    assertEquals(4, binyCounts(catIndexer(A))(catIndexer(B))(catIndexer(C)), 1e-9)
    assertEquals(7, binyCounts(catIndexer(B)).length)
    assertEquals(1, binyCounts(catIndexer(B)).activeCount)
    assertEquals(7, binyCounts(catIndexer(B))(catIndexer(A)).length)
    assertEquals(1, binyCounts(catIndexer(B))(catIndexer(A)).activeCount)
    assertEquals(0, binyCounts(catIndexer(B))(catIndexer(A))(catIndexer(C)), 1e-9)
    assertEquals(7, binyCounts(catIndexer(C)).length)
    assertEquals(2, binyCounts(catIndexer(C)).activeCount)
    assertEquals(7, binyCounts(catIndexer(C))(catIndexer(A)).length)
    assertEquals(1, binyCounts(catIndexer(C))(catIndexer(A)).activeCount)
    assertEquals(1, binyCounts(catIndexer(C))(catIndexer(A))(catIndexer(B)), 1e-9)
    assertEquals(7, binyCounts(catIndexer(C))(catIndexer(C)).length)
    assertEquals(1, binyCounts(catIndexer(C))(catIndexer(C)).activeCount)
    assertEquals(1, binyCounts(catIndexer(C))(catIndexer(C))(catIndexer(B)), 1e-9)
    assertEquals(7, binyCounts(catIndexer(D)).length)
    assertEquals(2, binyCounts(catIndexer(D)).activeCount)
    assertEquals(7, binyCounts(catIndexer(D))(catIndexer(A)).length)
    assertEquals(1, binyCounts(catIndexer(D))(catIndexer(A)).activeCount)
    assertEquals(0, binyCounts(catIndexer(D))(catIndexer(A))(catIndexer(B)), 1e-9)
    assertEquals(7, binyCounts(catIndexer(D))(catIndexer(C)).length)
    assertEquals(1, binyCounts(catIndexer(D))(catIndexer(C)).activeCount)
    assertEquals(0, binyCounts(catIndexer(D))(catIndexer(C))(catIndexer(B)), 1e-9)
    assertNull(binyCounts(catIndexer(E)))

    assertEquals(7, unryCounts.length)
    assertEquals(2, unryCounts.activeCount)
    assertFalse(unryCounts.containsKey(catIndexer(A)))
    assertEquals(7, unryCounts(catIndexer(B)).length)
    assertEquals(1, unryCounts(catIndexer(B)).activeCount)
    assertEquals(4, unryCounts(catIndexer(B))(catIndexer(A)), 1e-9)
    assertEquals(7, unryCounts(catIndexer(C)).length)
    assertEquals(2, unryCounts(catIndexer(C)).activeCount)
    assertEquals(0, unryCounts(catIndexer(C))(catIndexer(A)), 1e-9)
    assertEquals(1, unryCounts(catIndexer(C))(catIndexer(B)), 1e-9)
    assertFalse(unryCounts.containsKey(catIndexer(D)))
    assertFalse(unryCounts.containsKey(catIndexer(E)))

    assertEquals(7, termCounts.length)
    assertEquals(8, termCounts(catIndexer(A)).length)
    assertEquals(3, termCounts(catIndexer(A)).activeCount)
    assertEquals(1, termCounts(catIndexer(A))(wordIndexer("a1")), 1e-9)
    assertEquals(2, termCounts(catIndexer(A))(wordIndexer("a2")), 1e-9)
    assertEquals(0, termCounts(catIndexer(A))(wordIndexer("a3")), 1e-9)
    assertEquals(8, termCounts(catIndexer(B)).length)
    assertEquals(2, termCounts(catIndexer(B)).activeCount)
    assertEquals(4, termCounts(catIndexer(B))(wordIndexer("b1")), 1e-9)
    assertEquals(0, termCounts(catIndexer(B))(wordIndexer("c1")), 1e-9)
    assertEquals(8, termCounts(catIndexer(C)).length)
    assertEquals(2, termCounts(catIndexer(C)).activeCount)
    assertEquals(0, termCounts(catIndexer(C))(wordIndexer("b1")), 1e-9)
    assertEquals(2, termCounts(catIndexer(C))(wordIndexer("c1")), 1e-9)
    assertEquals(8, termCounts(catIndexer(D)).length)
    assertEquals(1, termCounts(catIndexer(D)).activeCount)
    assertEquals(0, termCounts(catIndexer(D))(wordIndexer("d1")), 1e-9)
    assertNull(termCounts(catIndexer(E)))

    assertEquals(7, pmixCounts.length)
    assertEquals(3, pmixCounts(catIndexer(A)).length)
    assertEquals(5, pmixCounts(catIndexer(A))(0), 1e-9)
    assertEquals(0, pmixCounts(catIndexer(A))(1), 1e-9)
    assertEquals(3, pmixCounts(catIndexer(A))(2), 1e-9)
    assertEquals(3, pmixCounts(catIndexer(B)).length)
    assertEquals(0, pmixCounts(catIndexer(B))(0), 1e-9)
    assertEquals(4, pmixCounts(catIndexer(B))(1), 1e-9)
    assertEquals(4, pmixCounts(catIndexer(B))(2), 1e-9)
    assertEquals(3, pmixCounts(catIndexer(C)).length)
    assertEquals(2, pmixCounts(catIndexer(C))(0), 1e-9)
    assertEquals(1, pmixCounts(catIndexer(C))(1), 1e-9)
    assertEquals(2, pmixCounts(catIndexer(C))(2), 1e-9)
    assertEquals(3, pmixCounts(catIndexer(D)).length)
    assertEquals(0, pmixCounts(catIndexer(D))(0), 1e-9)
    assertEquals(0, pmixCounts(catIndexer(D))(1), 1e-9)
    assertEquals(0, pmixCounts(catIndexer(D))(2), 1e-9)
    assertEquals(3, pmixCounts(catIndexer(E)).length)
    assertEquals(0, pmixCounts(catIndexer(E))(0), 1e-9)
    assertEquals(0, pmixCounts(catIndexer(E))(1), 1e-9)
    assertEquals(0, pmixCounts(catIndexer(E))(2), 1e-9)

    assertEquals(7, lctxCounts.length)
    assertEquals(7, lctxCounts(catIndexer(A)).length)
    assertEquals(4, lctxCounts(catIndexer(A)).activeCount)
    assertEquals(4, lctxCounts(catIndexer(A))(catIndexer(STA)), 1e-9)
    assertEquals(1, lctxCounts(catIndexer(A))(catIndexer(A)), 1e-9)
    assertEquals(2, lctxCounts(catIndexer(A))(catIndexer(B)), 1e-9)
    assertEquals(1, lctxCounts(catIndexer(A))(catIndexer(C)), 1e-9)
    assertEquals(7, lctxCounts(catIndexer(B)).length)
    assertEquals(4, lctxCounts(catIndexer(B)).activeCount)
    assertEquals(4, lctxCounts(catIndexer(B))(catIndexer(STA)), 1e-9)
    assertEquals(1, lctxCounts(catIndexer(B))(catIndexer(A)), 1e-9)
    assertEquals(1, lctxCounts(catIndexer(B))(catIndexer(B)), 1e-9)
    assertEquals(2, lctxCounts(catIndexer(B))(catIndexer(C)), 1e-9)
    assertEquals(7, lctxCounts(catIndexer(C)).length)
    assertEquals(3, lctxCounts(catIndexer(C)).activeCount)
    assertEquals(0, lctxCounts(catIndexer(C))(catIndexer(STA)), 1e-9)
    assertEquals(2, lctxCounts(catIndexer(C))(catIndexer(B)), 1e-9)
    assertEquals(3, lctxCounts(catIndexer(C))(catIndexer(C)), 1e-9)
    assertEquals(7, lctxCounts(catIndexer(D)).length)
    assertEquals(2, lctxCounts(catIndexer(D)).activeCount)
    assertEquals(0, lctxCounts(catIndexer(D))(catIndexer(A)), 1e-9)
    assertEquals(0, lctxCounts(catIndexer(D))(catIndexer(B)), 1e-9)
    assertNull(lctxCounts(catIndexer(E)))

    assertEquals(7, rctxCounts.length)
    assertEquals(7, rctxCounts(catIndexer(A)).length)
    assertEquals(4, rctxCounts(catIndexer(A)).activeCount)
    assertEquals(2, rctxCounts(catIndexer(A))(catIndexer(A)), 1e-9)
    assertEquals(2, rctxCounts(catIndexer(A))(catIndexer(B)), 1e-9)
    assertEquals(0, rctxCounts(catIndexer(A))(catIndexer(C)), 1e-9)
    assertEquals(4, rctxCounts(catIndexer(A))(catIndexer(END)), 1e-9)
    assertEquals(7, rctxCounts(catIndexer(B)).length)
    assertEquals(4, rctxCounts(catIndexer(B)).activeCount)
    assertEquals(2, rctxCounts(catIndexer(B))(catIndexer(A)), 1e-9)
    assertEquals(2, rctxCounts(catIndexer(B))(catIndexer(B)), 1e-9)
    assertEquals(2, rctxCounts(catIndexer(B))(catIndexer(C)), 1e-9)
    assertEquals(2, rctxCounts(catIndexer(B))(catIndexer(END)), 1e-9)
    assertEquals(7, rctxCounts(catIndexer(C)).length)
    assertEquals(3, rctxCounts(catIndexer(C)).activeCount)
    assertEquals(1, rctxCounts(catIndexer(C))(catIndexer(A)), 1e-9)
    assertEquals(2, rctxCounts(catIndexer(C))(catIndexer(B)), 1e-9)
    assertEquals(2, rctxCounts(catIndexer(C))(catIndexer(END)), 1e-9)
    assertEquals(7, rctxCounts(catIndexer(D)).length)
    assertEquals(2, rctxCounts(catIndexer(D)).activeCount)
    assertEquals(0, rctxCounts(catIndexer(D))(catIndexer(B)), 1e-9)
    assertEquals(0, rctxCounts(catIndexer(D))(catIndexer(C)), 1e-9)
    assertNull(rctxCounts(catIndexer(E)))

  }

}

