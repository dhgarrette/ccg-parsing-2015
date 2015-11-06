package dhg.ccg.parse.scg.mcmc

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.scg._
import dhg.ccg.util._
import dhg.ccg.tagdict.StartEndTags
import dhg.util._
import dhg.ccg.tagdict._
import dhg.ccg.test.TestUtil.DoubleIteratorRandomGenerator
import org.apache.commons.math3.random.MersenneTwister
import scala.collection.immutable.ListMap
import scalaz._
import Scalaz._
import scala.collection.immutable.BitSet

class ScgAlphaPriorMakerITests {

  val A: AtomCat = cat"A".asInstanceOf[AtomCat]
  val B: AtomCat = cat"B".asInstanceOf[AtomCat]
  val C: AtomCat = cat"C".asInstanceOf[AtomCat]
  val D: AtomCat = cat"D".asInstanceOf[AtomCat]
  val E: AtomCat = cat"E".asInstanceOf[AtomCat]
  val F: AtomCat = cat"F".asInstanceOf[AtomCat]
  val G: AtomCat = cat"G".asInstanceOf[AtomCat]
  val H: AtomCat = cat"H".asInstanceOf[AtomCat]

  val S: AtomCat = cat"S".asInstanceOf[AtomCat]
  val NP: AtomCat = cat"NP".asInstanceOf[AtomCat]
  val N: AtomCat = cat"N".asInstanceOf[AtomCat]
  val PP: AtomCat = cat"PP".asInstanceOf[AtomCat]
  val STA: Cat = cat"<S>"
  val END: Cat = cat"<E>"

  @Test
  def i_test_TrainDataScgAlphaPriorMaker {

    val tagdict = SimpleTagDictionary[Cat](Map(
      "a1" -> Set(A),
      "a2" -> Set(A),
      "a3" -> Set(A),
      "b1" -> Set(B),
      "b2" -> Set(B),
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

    val priorRootDist = IndirectSparseVec(Map(
      catIndexer(A) -> LogDouble(0.02),
      catIndexer(B) -> LogDouble(0.03),
      catIndexer(C) -> LogDouble(0.04)),
      numCats)
    val priorBinyDist = DenseVec(Map(
      catIndexer(A) -> IndirectSparseVec(Map(
        catIndexer(A) -> IndirectSparseVec(Map(
          catIndexer(B) -> LogDouble(0.10)), numCats),
        catIndexer(B) -> IndirectSparseVec(Map(
          catIndexer(A) -> LogDouble(0.11),
          catIndexer(B) -> LogDouble(0.12),
          catIndexer(C) -> LogDouble(0.13)), numCats)),
        numCats),
      catIndexer(B) -> IndirectSparseVec(Map(
        catIndexer(A) -> IndirectSparseVec(Map(
          catIndexer(B) -> LogDouble(0.14)), numCats)),
        numCats),
      catIndexer(C) -> IndirectSparseVec(Map(
        catIndexer(A) -> IndirectSparseVec(Map(
          catIndexer(A) -> LogDouble(0.15),
          catIndexer(B) -> LogDouble(0.16)), numCats),
        catIndexer(B) -> IndirectSparseVec(Map(
          catIndexer(A) -> LogDouble(0.17)), numCats),
        catIndexer(C) -> IndirectSparseVec(Map(
          catIndexer(B) -> LogDouble(0.18)), numCats)),
        numCats),
      catIndexer(D) -> IndirectSparseVec(Map(
        catIndexer(A) -> IndirectSparseVec(Map(
          catIndexer(B) -> LogDouble(0.19)), numCats)),
        numCats)),
      numCats).values
    val priorUnryDist = IndirectSparseVec(Map(
      catIndexer(B) -> IndirectSparseVec(Map(
        catIndexer(A) -> LogDouble(0.21),
        catIndexer(B) -> LogDouble(0.22)), numCats),
      catIndexer(C) -> IndirectSparseVec(Map(
        catIndexer(A) -> LogDouble(0.23),
        catIndexer(B) -> LogDouble(0.24)), numCats),
      catIndexer(D) -> IndirectSparseVec(Map(
        catIndexer(A) -> LogDouble(0.25)), numCats)),
      numCats)
    val priorTermDist = DenseVec(Map(
      catIndexer(A) -> SparseVec(Map(
        wordIndexer("a1") -> LogDouble(0.31),
        wordIndexer("a2") -> LogDouble(0.32),
        wordIndexer("a3") -> LogDouble(0.33)), numWords),
      catIndexer(B) -> SparseVec(Map(
        wordIndexer("b1") -> LogDouble(0.34),
        wordIndexer("b2") -> LogDouble(0.35)), numWords),
      catIndexer(C) -> SparseVec(Map(
        wordIndexer("c1") -> LogDouble(0.36)), numWords),
      catIndexer(D) -> SparseVec(Map(
        wordIndexer("d1") -> LogDouble(0.37)), numWords)),
      numCats).values
    val priorPmixDist = DenseVec(Map(
      catIndexer(STA) -> Array(LogDouble.zero, LogDouble.zero, LogDouble.zero),
      catIndexer(END) -> Array(LogDouble.zero, LogDouble.zero, LogDouble.zero),
      catIndexer(A) -> Array(LogDouble(0.41), LogDouble(0.42), LogDouble(0.43)),
      catIndexer(B) -> Array(LogDouble(0.44), LogDouble(0.45), LogDouble(0.46)),
      catIndexer(C) -> Array(LogDouble(0.47), LogDouble(0.48), LogDouble(0.49)),
      catIndexer(D) -> Array(LogDouble(0.51), LogDouble(0.52), LogDouble(0.53)),
      catIndexer(E) -> Array(LogDouble(0.54), LogDouble(0.55), LogDouble(0.56))),
      numCats).values
    val priorLctxDist = DenseVec(Map(
      catIndexer(A) -> IndirectSparseVec(Map(
        catIndexer(STA) -> LogDouble(0.61),
        catIndexer(A) -> LogDouble(0.62),
        catIndexer(B) -> LogDouble(0.63),
        catIndexer(C) -> LogDouble(0.64),
        catIndexer(D) -> LogDouble(0.65)), numCats),
      catIndexer(B) -> IndirectSparseVec(Map(
        catIndexer(STA) -> LogDouble(0.66),
        catIndexer(A) -> LogDouble(0.67),
        catIndexer(B) -> LogDouble(0.68),
        catIndexer(C) -> LogDouble(0.69),
        catIndexer(D) -> LogDouble(0.71)), numCats),
      catIndexer(C) -> IndirectSparseVec(Map(
        catIndexer(STA) -> LogDouble(0.72),
        catIndexer(B) -> LogDouble(0.73),
        catIndexer(C) -> LogDouble(0.74)), numCats),
      catIndexer(D) -> IndirectSparseVec(Map(
        catIndexer(A) -> LogDouble(0.75),
        catIndexer(B) -> LogDouble(0.76)), numCats),
      catIndexer(E) -> IndirectSparseVec(Map(
        catIndexer(A) -> LogDouble(0.77)), numCats)),
      numCats).values
    val priorRctxDist = DenseVec(Map(
      catIndexer(A) -> IndirectSparseVec(Map(
        catIndexer(A) -> LogDouble(0.81),
        catIndexer(B) -> LogDouble(0.82),
        catIndexer(C) -> LogDouble(0.83),
        catIndexer(D) -> LogDouble(0.84),
        catIndexer(END) -> LogDouble(0.85)), numCats),
      catIndexer(B) -> IndirectSparseVec(Map(
        catIndexer(A) -> LogDouble(0.86),
        catIndexer(B) -> LogDouble(0.87),
        catIndexer(C) -> LogDouble(0.88),
        catIndexer(END) -> LogDouble(0.89),
        catIndexer(D) -> LogDouble(0.91)), numCats),
      catIndexer(C) -> IndirectSparseVec(Map(
        catIndexer(A) -> LogDouble(0.92),
        catIndexer(B) -> LogDouble(0.93),
        catIndexer(END) -> LogDouble(0.94)), numCats),
      catIndexer(D) -> IndirectSparseVec(Map(
        catIndexer(B) -> LogDouble(0.95),
        catIndexer(C) -> LogDouble(0.96)), numCats)),
      numCats).values

    val knownRoots = Array(A, B).map(catIndexer).sorted
    val knownBinys = DenseVec(Map(
      catIndexer(A) -> IndirectSparseVec(Map(
        catIndexer(A) -> Array(B).map(catIndexer).sorted,
        catIndexer(B) -> Array(A, C).map(catIndexer).sorted), numCats),
      catIndexer(C) -> IndirectSparseVec(Map(
        catIndexer(A) -> Array(A, B).map(catIndexer).sorted,
        catIndexer(C) -> Array(B).map(catIndexer).sorted), numCats),
      catIndexer(D) -> IndirectSparseVec(Map(
        catIndexer(A) -> Array(B).map(catIndexer).sorted), numCats)),
      numCats).values
    val knownUnrys = IndirectSparseVec(Map(
      catIndexer(B) -> Array(A).map(catIndexer).sorted,
      catIndexer(C) -> Array(B).map(catIndexer).sorted),
      numCats)
    val knownTerms = DenseVec(Map(
      catIndexer(A) -> Array("a1", "a2", "a3").map(wordIndexer).sorted,
      catIndexer(B) -> Array("b1").map(wordIndexer).sorted,
      catIndexer(C) -> Array("c1").map(wordIndexer).sorted,
      catIndexer(D) -> Array("d1").map(wordIndexer).sorted),
      numCats).values
    val knownLctxs = DenseVec(Map(
      catIndexer(A) -> Array(STA, A, B, C).map(catIndexer).sorted,
      catIndexer(B) -> Array(STA, A, B, C).map(catIndexer).sorted,
      catIndexer(C) -> Array(STA, B, C).map(catIndexer).sorted,
      catIndexer(D) -> Array(A, B).map(catIndexer).sorted),
      numCats).values
    val knownRctxs = DenseVec(Map(
      catIndexer(A) -> Array(A, B, C, END).map(catIndexer).sorted,
      catIndexer(B) -> Array(A, B, C, END).map(catIndexer).sorted,
      catIndexer(C) -> Array(A, B, END).map(catIndexer).sorted,
      catIndexer(D) -> Array(B, C).map(catIndexer).sorted),
      numCats).values

    val counter = new SimpleScgProductionCounterI(catIndexer, wordIndexer)
    val (goldRootCounts, goldBinyCounts, goldUnryCounts, goldTermCounts, goldPmixCounts, goldLctxCounts, goldRctxCounts) =
      counter.counts(trees: Array[CcgTreeI], trees.length,
        knownRoots: Array[Int], //                           ts
        knownBinys: Array[IndirectSparseVec[Array[Int]]], //         t -> u -> vs
        knownUnrys: IndirectSparseVec[Array[Int]], //                t -> us
        knownTerms: Array[Array[Int]], //                    t -> ws
        knownLctxs: Array[Array[Int]], //                    t -> ls
        knownRctxs: Array[Array[Int]], //                    t -> rs
        numCats: Int, numWords: Int,
        startCat = 0, endCat = 1) //

    val alphaPriorMakerI = new TrainDataScgAlphaPriorMakerI(catIndexer, wordIndexer)
    val (alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts, alphaPriorPmixCounts, alphaPriorLctxCounts, alphaPriorRctxCounts) =
      alphaPriorMakerI.makeAll(
        priorRootDist: IndirectSparseVec[LogDouble], //                         t -> p
        priorBinyDist: Array[IndirectSparseVec[IndirectSparseVec[LogDouble]]], //       t -> u -> v -> p
        priorUnryDist: IndirectSparseVec[IndirectSparseVec[LogDouble]], //              t -> u -> p
        priorTermDist: Array[Vec[LogDouble]], //                  t -> w -> p
        priorPmixDist: Array[Array[LogDouble]], //                      t -> p
        priorLctxDist: Array[IndirectSparseVec[LogDouble]], //                  t -> l -> p
        priorRctxDist: Array[IndirectSparseVec[LogDouble]], //                  t -> r -> p
        goldRootCounts: IndirectSparseVec[Double], //                         t -> c
        goldBinyCounts: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> c
        goldUnryCounts: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> c
        goldTermCounts: Array[Vec[Double]], //                  t -> w -> c
        goldPmixCounts: Array[Array[Double]], //                      t -> c
        goldLctxCounts: Array[IndirectSparseVec[Double]], //                  t -> l -> c
        goldRctxCounts: Array[IndirectSparseVec[Double]], //                  t -> r -> c
        alphaRoot = 0.11, alphaBiny = 0.12, alphaUnry = 0.13, alphaTerm = 0.14, alphaPmix = 0.15, alphaLctx = 0.16, alphaRctx = 0.17,
        numCats: Int, numWords: Int): ( //
        IndirectSparseVec[Double], //                     t -> c
        Array[IndirectSparseVec[IndirectSparseVec[Double]]], //   t -> u -> v -> c
        IndirectSparseVec[IndirectSparseVec[Double]], //          t -> u -> c
        Array[Vec[Double]], //              t -> w -> c
        Array[Array[Double]], //                  t -> c
        Array[IndirectSparseVec[Double]], //              t -> l -> c
        Array[IndirectSparseVec[Double]]) //              t -> r -> c

    assertEquals(7, /*         */ alphaPriorRootCounts.length)
    assertEquals(2, /*         */ alphaPriorRootCounts.activeCount)
    assertEquals(0.11 * 0.02 + 2, alphaPriorRootCounts(catIndexer(A)), 1e-9)
    assertEquals(0.11 * 0.03 + 0, alphaPriorRootCounts(catIndexer(B)), 1e-9)

    assertEquals(7, /*         */ alphaPriorBinyCounts.length)
    assertEquals(3, /*         */ alphaPriorBinyCounts.count(_ != null))
    assertEquals(7, /*         */ alphaPriorBinyCounts(catIndexer(A)).length)
    assertEquals(2, /*         */ alphaPriorBinyCounts(catIndexer(A)).activeCount)
    assertEquals(7, /*         */ alphaPriorBinyCounts(catIndexer(A))(catIndexer(A)).length)
    assertEquals(1, /*         */ alphaPriorBinyCounts(catIndexer(A))(catIndexer(A)).activeCount)
    assertEquals(0.12 * 0.10 + 0, alphaPriorBinyCounts(catIndexer(A))(catIndexer(A))(catIndexer(B)), 1e-9)
    assertEquals(7, /*         */ alphaPriorBinyCounts(catIndexer(A))(catIndexer(B)).length)
    assertEquals(2, /*         */ alphaPriorBinyCounts(catIndexer(A))(catIndexer(B)).activeCount)
    assertEquals(0.12 * 0.11 + 1, alphaPriorBinyCounts(catIndexer(A))(catIndexer(B))(catIndexer(A)), 1e-9)
    assertEquals(0.12 * 0.13 + 4, alphaPriorBinyCounts(catIndexer(A))(catIndexer(B))(catIndexer(C)), 1e-9)
    assertEquals(7, /*         */ alphaPriorBinyCounts(catIndexer(C)).length)
    assertEquals(2, /*         */ alphaPriorBinyCounts(catIndexer(C)).activeCount)
    assertEquals(7, /*         */ alphaPriorBinyCounts(catIndexer(C))(catIndexer(A)).length)
    assertEquals(2, /*         */ alphaPriorBinyCounts(catIndexer(C))(catIndexer(A)).activeCount)
    assertEquals(0.12 * 0.15 + 0, alphaPriorBinyCounts(catIndexer(C))(catIndexer(A))(catIndexer(A)), 1e-9)
    assertEquals(0.12 * 0.16 + 1, alphaPriorBinyCounts(catIndexer(C))(catIndexer(A))(catIndexer(B)), 1e-9)
    assertEquals(7, /*         */ alphaPriorBinyCounts(catIndexer(C))(catIndexer(C)).length)
    assertEquals(1, /*         */ alphaPriorBinyCounts(catIndexer(C))(catIndexer(C)).activeCount)
    assertEquals(0.12 * 0.18 + 1, alphaPriorBinyCounts(catIndexer(C))(catIndexer(C))(catIndexer(B)), 1e-9)
    assertEquals(7, /*         */ alphaPriorBinyCounts(catIndexer(D)).length)
    assertEquals(1, /*         */ alphaPriorBinyCounts(catIndexer(D)).activeCount)
    assertEquals(7, /*         */ alphaPriorBinyCounts(catIndexer(D))(catIndexer(A)).length)
    assertEquals(1, /*         */ alphaPriorBinyCounts(catIndexer(D))(catIndexer(A)).activeCount)
    assertEquals(0.12 * 0.19 + 0, alphaPriorBinyCounts(catIndexer(D))(catIndexer(A))(catIndexer(B)), 1e-9)

    assertEquals(7, /*         */ alphaPriorUnryCounts.length)
    assertEquals(2, /*         */ alphaPriorUnryCounts.activeCount)
    assertEquals(7, /*         */ alphaPriorUnryCounts(catIndexer(B)).length)
    assertEquals(1, /*         */ alphaPriorUnryCounts(catIndexer(B)).activeCount)
    assertEquals(0.13 * 0.21 + 4, alphaPriorUnryCounts(catIndexer(B))(catIndexer(A)), 1e-9)
    assertEquals(7, /*         */ alphaPriorUnryCounts(catIndexer(C)).length)
    assertEquals(1, /*         */ alphaPriorUnryCounts(catIndexer(C)).activeCount)
    assertEquals(0.13 * 0.24 + 1, alphaPriorUnryCounts(catIndexer(C))(catIndexer(B)), 1e-9)

    assertEquals(7, /*         */ alphaPriorTermCounts.length)
    assertEquals(4, /*         */ alphaPriorTermCounts.count(_ != null))
    assertEquals(9, /*         */ alphaPriorTermCounts(catIndexer(A)).length)
    assertEquals(3, /*         */ alphaPriorTermCounts(catIndexer(A)).activeCount)
    assertEquals(0.14 * 0.31 + 1, alphaPriorTermCounts(catIndexer(A))(wordIndexer("a1")), 1e-9)
    assertEquals(0.14 * 0.32 + 2, alphaPriorTermCounts(catIndexer(A))(wordIndexer("a2")), 1e-9)
    assertEquals(0.14 * 0.33 + 0, alphaPriorTermCounts(catIndexer(A))(wordIndexer("a3")), 1e-9)
    assertEquals(9, /*         */ alphaPriorTermCounts(catIndexer(B)).length)
    assertEquals(1, /*         */ alphaPriorTermCounts(catIndexer(B)).activeCount)
    assertEquals(0.14 * 0.34 + 4, alphaPriorTermCounts(catIndexer(B))(wordIndexer("b1")), 1e-9)
    assertEquals(9, /*         */ alphaPriorTermCounts(catIndexer(C)).length)
    assertEquals(1, /*         */ alphaPriorTermCounts(catIndexer(C)).activeCount)
    assertEquals(0.14 * 0.36 + 2, alphaPriorTermCounts(catIndexer(C))(wordIndexer("c1")), 1e-9)
    assertEquals(9, /*         */ alphaPriorTermCounts(catIndexer(D)).length)
    assertEquals(1, /*         */ alphaPriorTermCounts(catIndexer(D)).activeCount)
    assertEquals(0.14 * 0.37 + 0, alphaPriorTermCounts(catIndexer(D))(wordIndexer("d1")), 1e-9)

    assertEquals(7, /*         */ alphaPriorPmixCounts.length)
    assertEquals(7, /*         */ alphaPriorPmixCounts.count(_ != null))
    Vector(A, B, C, D, E).foreach(c => assertEquals(3, alphaPriorPmixCounts(catIndexer(c)).length))
    assertEquals(0.15 * 0.41 + 5, alphaPriorPmixCounts(catIndexer(A))(0), 1e-9)
    assertEquals(0.15 * 0.42 + 0, alphaPriorPmixCounts(catIndexer(A))(1), 1e-9)
    assertEquals(0.15 * 0.43 + 3, alphaPriorPmixCounts(catIndexer(A))(2), 1e-9)
    assertEquals(0.15 * 0.44 + 0, alphaPriorPmixCounts(catIndexer(B))(0), 1e-9)
    assertEquals(0.15 * 0.45 + 4, alphaPriorPmixCounts(catIndexer(B))(1), 1e-9)
    assertEquals(0.15 * 0.46 + 4, alphaPriorPmixCounts(catIndexer(B))(2), 1e-9)
    assertEquals(0.15 * 0.47 + 2, alphaPriorPmixCounts(catIndexer(C))(0), 1e-9)
    assertEquals(0.15 * 0.48 + 1, alphaPriorPmixCounts(catIndexer(C))(1), 1e-9)
    assertEquals(0.15 * 0.49 + 2, alphaPriorPmixCounts(catIndexer(C))(2), 1e-9)
    assertEquals(0.15 * 0.51 + 0, alphaPriorPmixCounts(catIndexer(D))(0), 1e-9)
    assertEquals(0.15 * 0.52 + 0, alphaPriorPmixCounts(catIndexer(D))(1), 1e-9)
    assertEquals(0.15 * 0.53 + 0, alphaPriorPmixCounts(catIndexer(D))(2), 1e-9)
    assertEquals(0.15 * 0.54 + 0, alphaPriorPmixCounts(catIndexer(E))(0), 1e-9)
    assertEquals(0.15 * 0.55 + 0, alphaPriorPmixCounts(catIndexer(E))(1), 1e-9)
    assertEquals(0.15 * 0.56 + 0, alphaPriorPmixCounts(catIndexer(E))(2), 1e-9)

    assertEquals(7, /*         */ alphaPriorLctxCounts.length)
    assertEquals(4, /*         */ alphaPriorLctxCounts.count(_ != null))
    assertEquals(7, /*         */ alphaPriorLctxCounts(catIndexer(A)).length)
    assertEquals(4, /*         */ alphaPriorLctxCounts(catIndexer(A)).activeCount)
    assertEquals(0.16 * 0.61 + 4, alphaPriorLctxCounts(catIndexer(A))(catIndexer(STA)), 1e-9)
    assertEquals(0.16 * 0.62 + 1, alphaPriorLctxCounts(catIndexer(A))(catIndexer(A)), 1e-9)
    assertEquals(0.16 * 0.63 + 2, alphaPriorLctxCounts(catIndexer(A))(catIndexer(B)), 1e-9)
    assertEquals(0.16 * 0.64 + 1, alphaPriorLctxCounts(catIndexer(A))(catIndexer(C)), 1e-9)
    assertEquals(7, /*         */ alphaPriorLctxCounts(catIndexer(B)).length)
    assertEquals(4, /*         */ alphaPriorLctxCounts(catIndexer(B)).activeCount)
    assertEquals(0.16 * 0.66 + 4, alphaPriorLctxCounts(catIndexer(B))(catIndexer(STA)), 1e-9)
    assertEquals(0.16 * 0.67 + 1, alphaPriorLctxCounts(catIndexer(B))(catIndexer(A)), 1e-9)
    assertEquals(0.16 * 0.68 + 1, alphaPriorLctxCounts(catIndexer(B))(catIndexer(B)), 1e-9)
    assertEquals(0.16 * 0.69 + 2, alphaPriorLctxCounts(catIndexer(B))(catIndexer(C)), 1e-9)
    assertEquals(7, /*         */ alphaPriorLctxCounts(catIndexer(C)).length)
    assertEquals(3, /*         */ alphaPriorLctxCounts(catIndexer(C)).activeCount)
    assertEquals(0.16 * 0.72 + 0, alphaPriorLctxCounts(catIndexer(C))(catIndexer(STA)), 1e-9)
    assertEquals(0.16 * 0.73 + 2, alphaPriorLctxCounts(catIndexer(C))(catIndexer(B)), 1e-9)
    assertEquals(0.16 * 0.74 + 3, alphaPriorLctxCounts(catIndexer(C))(catIndexer(C)), 1e-9)
    assertEquals(7, /*         */ alphaPriorLctxCounts(catIndexer(D)).length)
    assertEquals(2, /*         */ alphaPriorLctxCounts(catIndexer(D)).activeCount)
    assertEquals(0.16 * 0.75 + 0, alphaPriorLctxCounts(catIndexer(D))(catIndexer(A)), 1e-9)
    assertEquals(0.16 * 0.76 + 0, alphaPriorLctxCounts(catIndexer(D))(catIndexer(B)), 1e-9)


    assertEquals(7, /*         */ alphaPriorRctxCounts.length)
    assertEquals(4, /*         */ alphaPriorRctxCounts.count(_ != null))
    assertEquals(7, /*         */ alphaPriorRctxCounts(catIndexer(A)).length)
    assertEquals(4, /*         */ alphaPriorRctxCounts(catIndexer(A)).activeCount)
    assertEquals(0.17 * 0.81 + 2, alphaPriorRctxCounts(catIndexer(A))(catIndexer(A)), 1e-9)
    assertEquals(0.17 * 0.82 + 2, alphaPriorRctxCounts(catIndexer(A))(catIndexer(B)), 1e-9)
    assertEquals(0.17 * 0.83 + 0, alphaPriorRctxCounts(catIndexer(A))(catIndexer(C)), 1e-9)
    assertEquals(0.17 * 0.85 + 4, alphaPriorRctxCounts(catIndexer(A))(catIndexer(END)), 1e-9)
    assertEquals(7, /*         */ alphaPriorRctxCounts(catIndexer(B)).length)
    assertEquals(4, /*         */ alphaPriorRctxCounts(catIndexer(B)).activeCount)
    assertEquals(0.17 * 0.86 + 2, alphaPriorRctxCounts(catIndexer(B))(catIndexer(A)), 1e-9)
    assertEquals(0.17 * 0.87 + 2, alphaPriorRctxCounts(catIndexer(B))(catIndexer(B)), 1e-9)
    assertEquals(0.17 * 0.88 + 2, alphaPriorRctxCounts(catIndexer(B))(catIndexer(C)), 1e-9)
    assertEquals(0.17 * 0.89 + 2, alphaPriorRctxCounts(catIndexer(B))(catIndexer(END)), 1e-9)
    assertEquals(7, /*         */ alphaPriorRctxCounts(catIndexer(C)).length)
    assertEquals(3, /*         */ alphaPriorRctxCounts(catIndexer(C)).activeCount)
    assertEquals(0.17 * 0.92 + 1, alphaPriorRctxCounts(catIndexer(C))(catIndexer(A)), 1e-9)
    assertEquals(0.17 * 0.93 + 2, alphaPriorRctxCounts(catIndexer(C))(catIndexer(B)), 1e-9)
    assertEquals(0.17 * 0.94 + 2, alphaPriorRctxCounts(catIndexer(C))(catIndexer(END)), 1e-9)
    assertEquals(7, /*         */ alphaPriorRctxCounts(catIndexer(D)).length)
    assertEquals(2, /*         */ alphaPriorRctxCounts(catIndexer(D)).activeCount)
    assertEquals(0.17 * 0.95 + 0, alphaPriorRctxCounts(catIndexer(D))(catIndexer(B)), 1e-9)
    assertEquals(0.17 * 0.96 + 0, alphaPriorRctxCounts(catIndexer(D))(catIndexer(C)), 1e-9)
  }

}

