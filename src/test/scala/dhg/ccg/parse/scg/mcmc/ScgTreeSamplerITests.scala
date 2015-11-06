package dhg.ccg.parse.scg.mcmc

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
import dhg.ccg.parse.pcfg.mcmc.PcfgTreeSamplerI
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.random.AbstractRandomGenerator

class ScgTreeSamplerITests {

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
  def i_test_MetHastScgTreeSamplerI_resample {
    type Word = String

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

    val mockLogRootDist = IndirectSparseVec(Map(
      catIndexer(A) -> LogDouble(0.02).logValue,
      catIndexer(B) -> LogDouble(0.03).logValue,
      catIndexer(C) -> LogDouble(0.04).logValue),
      numCats)
    val mockLogBinyDist = DenseVec(Map(
      catIndexer(A) -> IndirectSparseVec(Map(
        catIndexer(A) -> IndirectSparseVec(Map(
          catIndexer(B) -> LogDouble(0.10).logValue), numCats),
        catIndexer(B) -> IndirectSparseVec(Map(
          catIndexer(A) -> LogDouble(0.11).logValue,
          catIndexer(B) -> LogDouble(0.12).logValue,
          catIndexer(C) -> LogDouble(0.13).logValue), numCats)),
        numCats),
      catIndexer(B) -> IndirectSparseVec(Map(
        catIndexer(A) -> IndirectSparseVec(Map(
          catIndexer(B) -> LogDouble(0.14).logValue), numCats)),
        numCats),
      catIndexer(C) -> IndirectSparseVec(Map(
        catIndexer(A) -> IndirectSparseVec(Map(
          catIndexer(A) -> LogDouble(0.15).logValue,
          catIndexer(B) -> LogDouble(0.16).logValue), numCats),
        catIndexer(B) -> IndirectSparseVec(Map(
          catIndexer(A) -> LogDouble(0.17).logValue), numCats),
        catIndexer(C) -> IndirectSparseVec(Map(
          catIndexer(B) -> LogDouble(0.18).logValue), numCats)),
        numCats),
      catIndexer(D) -> IndirectSparseVec(Map(
        catIndexer(A) -> IndirectSparseVec(Map(
          catIndexer(B) -> LogDouble(0.19).logValue), numCats)),
        numCats)),
      numCats).values
    val mockLogUnryDist = IndirectSparseVec(Map(
      catIndexer(B) -> IndirectSparseVec(Map(
        catIndexer(A) -> LogDouble(0.21).logValue,
        catIndexer(B) -> LogDouble(0.22).logValue), numCats),
      catIndexer(C) -> IndirectSparseVec(Map(
        catIndexer(A) -> LogDouble(0.23).logValue,
        catIndexer(B) -> LogDouble(0.24).logValue), numCats),
      catIndexer(D) -> IndirectSparseVec(Map(
        catIndexer(A) -> LogDouble(0.25).logValue), numCats)),
      numCats)
    val mockLogTermDist = DenseVec(Map(
      catIndexer(A) -> SparseVec(Map(
        wordIndexer("a1") -> LogDouble(0.31).logValue,
        wordIndexer("a2") -> LogDouble(0.32).logValue,
        wordIndexer("a3") -> LogDouble(0.33).logValue), numWords),
      catIndexer(B) -> SparseVec(Map(
        wordIndexer("b1") -> LogDouble(0.34).logValue,
        wordIndexer("b2") -> LogDouble(0.35).logValue), numWords),
      catIndexer(C) -> SparseVec(Map(
        wordIndexer("c1") -> LogDouble(0.36).logValue), numWords),
      catIndexer(D) -> SparseVec(Map(
        wordIndexer("d1") -> LogDouble(0.37).logValue), numWords)),
      numCats).values
    val mockLogPmixDist = DenseVec(Map(
      catIndexer(STA) -> Array(0.0, 0.0, 0.0),
      catIndexer(END) -> Array(0.0, 0.0, 0.0),
      catIndexer(A) -> Array(LogDouble(0.41).logValue, LogDouble(0.42).logValue, LogDouble(0.43).logValue),
      catIndexer(B) -> Array(LogDouble(0.44).logValue, LogDouble(0.45).logValue, LogDouble(0.46).logValue),
      catIndexer(C) -> Array(LogDouble(0.47).logValue, LogDouble(0.48).logValue, LogDouble(0.49).logValue),
      catIndexer(D) -> Array(LogDouble(0.51).logValue, LogDouble(0.52).logValue, LogDouble(0.53).logValue),
      catIndexer(E) -> Array(LogDouble(0.54).logValue, LogDouble(0.55).logValue, LogDouble(0.56).logValue)),
      numCats).values
    val mockLogLctxDist = DenseVec(Map(
      catIndexer(A) -> IndirectSparseVec(Map(
        catIndexer(STA) -> LogDouble(0.61).logValue,
        catIndexer(A) -> LogDouble(0.62).logValue,
        catIndexer(B) -> LogDouble(0.63).logValue,
        catIndexer(C) -> LogDouble(0.64).logValue,
        catIndexer(D) -> LogDouble(0.65).logValue), numCats),
      catIndexer(B) -> IndirectSparseVec(Map(
        catIndexer(STA) -> LogDouble(0.66).logValue,
        catIndexer(A) -> LogDouble(0.67).logValue,
        catIndexer(B) -> LogDouble(0.68).logValue,
        catIndexer(C) -> LogDouble(0.69).logValue,
        catIndexer(D) -> LogDouble(0.71).logValue), numCats),
      catIndexer(C) -> IndirectSparseVec(Map(
        catIndexer(STA) -> LogDouble(0.72).logValue,
        catIndexer(B) -> LogDouble(0.73).logValue,
        catIndexer(C) -> LogDouble(0.74).logValue), numCats),
      catIndexer(D) -> IndirectSparseVec(Map(
        catIndexer(A) -> LogDouble(0.75).logValue,
        catIndexer(B) -> LogDouble(0.76).logValue), numCats),
      catIndexer(E) -> IndirectSparseVec(Map(
        catIndexer(A) -> LogDouble(0.77).logValue), numCats)),
      numCats).values
    val mockLogRctxDist = DenseVec(Map(
      catIndexer(A) -> IndirectSparseVec(Map(
        catIndexer(A) -> LogDouble(0.81).logValue,
        catIndexer(B) -> LogDouble(0.82).logValue,
        catIndexer(C) -> LogDouble(0.83).logValue,
        catIndexer(D) -> LogDouble(0.84).logValue,
        catIndexer(END) -> LogDouble(0.85).logValue), numCats),
      catIndexer(B) -> IndirectSparseVec(Map(
        catIndexer(A) -> LogDouble(0.86).logValue,
        catIndexer(B) -> LogDouble(0.87).logValue,
        catIndexer(C) -> LogDouble(0.88).logValue,
        catIndexer(END) -> LogDouble(0.89).logValue,
        catIndexer(D) -> LogDouble(0.91).logValue), numCats),
      catIndexer(C) -> IndirectSparseVec(Map(
        catIndexer(A) -> LogDouble(0.92).logValue,
        catIndexer(B) -> LogDouble(0.93).logValue,
        catIndexer(END) -> LogDouble(0.94).logValue), numCats),
      catIndexer(D) -> IndirectSparseVec(Map(
        catIndexer(B) -> LogDouble(0.95).logValue,
        catIndexer(C) -> LogDouble(0.96).logValue), numCats)),
      numCats).values

    val mockGuideChart: CfgGuideChartI = CfgGuideChartI(Array.empty, new Chart(Array.empty, 0))

    /*
     * t1           A                     <s> <- A  2 * 0.61      A -> A     2 * 0.81
     *            /   \                     A <- A  1 * 0.62      A -> B     1 * 0.82
     *         B         C                  B <- A  1 * 0.63      A -> <e>   2 * 0.85
     *         |       /   \                C <- A  1 * 0.64      B -> A     1 * 0.86
     *         A      A     B             <s> <- B  2 * 0.66      B -> B     1 * 0.87
     *        / \    / \    |               A <- B  1 * 0.67      B -> C     1 * 0.88
     * <s>   B   C  B   A   A  <e>          C <- B  1 * 0.69      B -> <e>   1 * 0.89
     *       b1  c1 b1  a1  a2              B <- C  1 * 0.73      C -> B     1 * 0.93
     *                                      C <- C  1 * 0.74      C -> <e>   1 * 0.94
     *                                                  ----                     ----
     *                                                  0.010119014712045694   + 0.19912368757431365 = 0.00201493552408127
     *                                              
     * t2           A
     *            /   \  
     *         B         C
     *         |       /   \
     *         A      C     B
     *        / \     |     |
     * <s>   B   C    B     A  <e>
     *       b1  c1   b1   a2            0.005347774789430972
     *       
     * t3           B
     *            /   \  
     *         B         C
     *         |       /   \
     *         A      C     B
     *        / \     |     |
     * <s>   B   C    B     A  <e>
     *       b1  c1   b1   a2            0.006058404843417074
     */

    val t1 = CcgTreeI.to(
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
            CcgLeaf(A, "a2", "FAKEPOS")))),
      catIndexer, wordIndexer)
    val t2 = CcgTreeI.to(
      CcgBinode(A,
        CcgUnode(B,
          CcgBinode(A,
            CcgLeaf(B, "b1", "FAKEPOS"),
            CcgLeaf(C, "c1", "FAKEPOS"))),
        CcgBinode(C,
          CcgUnode(C,
            CcgLeaf(B, "b1", "FAKEPOS")),
          CcgUnode(B,
            CcgLeaf(A, "a2", "FAKEPOS")))),
      catIndexer, wordIndexer)
    val t3 = CcgTreeI.to(
      CcgBinode(B,
        CcgUnode(B,
          CcgBinode(A,
            CcgLeaf(B, "b1", "FAKEPOS"),
            CcgLeaf(C, "c1", "FAKEPOS"))),
        CcgBinode(C,
          CcgUnode(C,
            CcgLeaf(B, "b1", "FAKEPOS")),
          CcgUnode(B,
            CcgLeaf(A, "a2", "FAKEPOS")))),
      catIndexer, wordIndexer)

    def pcfgTreeSampler(ts: Array[CcgTreeI]): PcfgTreeSamplerI = new PcfgTreeSamplerI {
      def sample(
        guideChart: CfgGuideChartI,
        logRootDist: IndirectSparseVec[Double], //                                 t -> p
        logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
        logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
        logTermDist: Array[Vec[Double]], //                                        t -> w -> p
        logPmixDist: Array[Array[Double]], //                                      t -> p
        numCats: Int): CcgTreeI = ???

      def samples(
        guideChart: CfgGuideChartI,
        logRootDist: IndirectSparseVec[Double], //                                 t -> p
        logBinyDist: Array[IndirectSparseVec[IndirectSparseVec[Double]]], //       t -> u -> v -> p
        logUnryDist: IndirectSparseVec[IndirectSparseVec[Double]], //              t -> u -> p
        logTermDist: Array[Vec[Double]], //                                        t -> w -> p
        logPmixDist: Array[Array[Double]], //                                      t -> p
        numCats: Int,
        k: Int): Array[CcgTreeI] = {

        assertSame(mockGuideChart, guideChart)
        assertSame(mockLogRootDist, logRootDist)
        assertSame(mockLogBinyDist, logBinyDist)
        assertSame(mockLogUnryDist, logUnryDist)
        assertSame(mockLogTermDist, logTermDist)
        assertSame(mockLogPmixDist, logPmixDist)
        assertEquals(7, numCats)
        assertEquals(3, k)

        ts
      }
    }

    def rand(vs: Double*): RandomGenerator = new AbstractRandomGenerator {
      val vi = vs.iterator
      def nextDouble() = vi.next()
      def setSeed(seed: Long) = ???
    }

    def go(ts: Array[CcgTreeI], r: RandomGenerator, t: CcgTreeI, a: Boolean) = {

      val scgTreeSampler = new MetHastScgTreeSamplerI(pcfgTreeSampler(ts),
        treesPerIteration = 3, r)(catIndexer, wordIndexer)
      val (newTree, accept) = scgTreeSampler.resample(mockGuideChart, existingTree = t1, mockLogRootDist, mockLogBinyDist, mockLogUnryDist, mockLogTermDist, mockLogPmixDist, mockLogLctxDist, mockLogRctxDist, numCats, startCat = 0, endCat = 1)
      assertSame(t, newTree)
      assertEquals(a, accept)
    }

    //go(Array(t2, t2, t3), rand(), t3, true)
    go(Array(t2, t3, t2), rand(0.9), t3, false)
    go(Array(t2, t3, t2), rand(0.8), t2, true)

  }

  private[this] def assertEqualsLogMap[T](e: Map[T, LogDouble], a: Map[T, LogDouble], r: Double) = {
    assertEquals(e.keySet, a.keySet)
    for ((k, ev) <- e) assertEquals(ev.toDouble, a(k).toDouble, r)
  }
}
