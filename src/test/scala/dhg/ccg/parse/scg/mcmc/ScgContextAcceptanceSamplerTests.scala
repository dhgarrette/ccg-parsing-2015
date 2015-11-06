package dhg.ccg.parse.scg.mcmc

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.scg._
import dhg.ccg.tagdict.StartEndTags
import dhg.util._
import dhg.ccg.tagdict._
import dhg.ccg.test.TestUtil.DoubleIteratorRandomGenerator
import org.apache.commons.math3.random.MersenneTwister

class ScgAcceptanceSamplerTests {

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
  val G = cat"G".asInstanceOf[AtomCat]
  val H = cat"H".asInstanceOf[AtomCat]

  @Test
  def test_FullScgAcceptanceSampler_accept {
    type Word = String

    val mockRootDist = new LogProbabilityDistribution[Cat] {
      def apply(x: Cat): LogDouble = ???
      def sample(): Cat = ???
      def defaultProb = ???
    }
    val mockProdDist = new ConditionalLogProbabilityDistribution[Cat, Prod] {
      def apply(right: Prod, given: Cat): LogDouble = ???
      def sample(given: Cat): Prod = ???
    }

    val mockLctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
      def apply(left: Cat, given: Cat): LogDouble = ???
      def sample(given: Cat): Cat = ???
    }
    val mockRctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
      def apply(right: Cat, given: Cat): LogDouble = ???
      def sample(given: Cat): Cat = ???
    }

    val T1: CcgTree = CcgLeaf(A, "a", "FAKEPOS")
    val T2: CcgTree = CcgLeaf(B, "b", "FAKEPOS")
    val T3: CcgTree = CcgLeaf(C, "c", "FAKEPOS")
    val T4: CcgTree = CcgLeaf(D, "d", "FAKEPOS")
    val T5: CcgTree = CcgLeaf(E, "e", "FAKEPOS")

    val mockWeighter = new ScgWeighter {
      def weight(tree: CcgTree,
        rootDist: LogProbabilityDistribution[Cat],
        prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
        lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
        rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]): LogDouble = {
        assertSame(mockRootDist, rootDist)
        assertSame(mockProdDist, prodDist)
        assertSame(mockLctxDist, lctxDist)
        assertSame(mockRctxDist, rctxDist)
        LogDouble(tree match {
          case T1 => 0.11
          case T2 => 0.21
          case T3 => 0.31
          case T4 => 0.41
          case T5 => 0.51
          case _ => fail(f"no match for $tree"); ???
        })
      }

      def pcfgWeight(tree: CcgTree,
        rootDist: LogProbabilityDistribution[Cat],
        prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): LogDouble = {
        assertSame(mockRootDist, rootDist)
        assertSame(mockProdDist, prodDist)
        LogDouble(tree match {
          case T1 => 0.33
          case T2 => 0.13
          case T3 => 0.23
          case T4 => 0.53
          case T5 => 0.43
          case _ => fail(f"no match for $tree"); ???
        })
      }

      def ctxWeight(tree: CcgTree,
        lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
        rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]): LogDouble = ???
    }

    val se = SimpleStartEndTags[Cat](STA, END)
    def r(d: Double) = DoubleIteratorRandomGenerator(Iterator(d))

    val cas = new FullScgAcceptanceSampler(mockWeighter)

    val (a1, r1, ag1) = cas.accept(newTree = T1, curTree = T4, mockRootDist, mockProdDist, mockLctxDist, mockRctxDist, r(0.10))(se) // 0.11/0.41 = 0.2682926829268293
    assertTrue(a1)
    assertEquals(0.2682926829268293, r1, 1e-9)
    assertTrue(ag1)

    val (a2, r2, ag2) = cas.accept(newTree = T1, curTree = T4, mockRootDist, mockProdDist, mockLctxDist, mockRctxDist, r(0.25))(se) // 0.11/0.41 = 0.2682926829268293
    assertTrue(a2)
    assertEquals(0.2682926829268293, r2, 1e-9)
    assertTrue(ag2)

    val (a3, r3, ag3) = cas.accept(newTree = T1, curTree = T4, mockRootDist, mockProdDist, mockLctxDist, mockRctxDist, r(0.50))(se) // 0.11/0.41 = 0.2682926829268293
    assertFalse(a3)
    assertEquals(0.2682926829268293, r3, 1e-9)
    assertTrue(ag3)

    val (a4, r4, ag4) = cas.accept(newTree = T1, curTree = T4, mockRootDist, mockProdDist, mockLctxDist, mockRctxDist, r(0.80))(se) // 0.11/0.41 = 0.2682926829268293
    assertFalse(a4)
    assertEquals(0.2682926829268293, r4, 1e-9)
    assertTrue(ag4)

    val n1 = Vector.fill(1000000)(cas.accept(newTree = T1, curTree = T4, mockRootDist, mockProdDist, mockLctxDist, mockRctxDist, new MersenneTwister)(se)._1).count(identity) // 0.11/0.41 = 0.2682926829268293
    assertEquals(0.2682926829268293, n1 / 1000000.0, 1e-3)

    // new is better than current: unconditionally accept
    val (a5, r5, ag5) = cas.accept(newTree = T2, curTree = T1, mockRootDist, mockProdDist, mockLctxDist, mockRctxDist, DoubleIteratorRandomGenerator(Iterator()))(se) // 0.21/0.11 = 1.909090909090909
    assertTrue(a5)
    assertEquals(1.909090909090909, r5, 1e-9)
    assertFalse(ag5)
  }

  @Test
  def test_ContextScgAcceptanceSampler_accept {
    type Word = String

    val mockRootDist = new LogProbabilityDistribution[Cat] {
      def apply(x: Cat): LogDouble = ???
      def sample(): Cat = ???
      def defaultProb = ???
    }
    val mockProdDist = new ConditionalLogProbabilityDistribution[Cat, Prod] {
      def apply(right: Prod, given: Cat): LogDouble = ???
      def sample(given: Cat): Prod = ???
    }

    val mockLctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
      def apply(left: Cat, given: Cat): LogDouble = ???
      def sample(given: Cat): Cat = ???
    }
    val mockRctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
      def apply(right: Cat, given: Cat): LogDouble = ???
      def sample(given: Cat): Cat = ???
    }

    val T1: CcgTree = CcgLeaf(A, "a", "FAKEPOS")
    val T2: CcgTree = CcgLeaf(B, "b", "FAKEPOS")
    val T3: CcgTree = CcgLeaf(C, "c", "FAKEPOS")
    val T4: CcgTree = CcgLeaf(D, "d", "FAKEPOS")
    val T5: CcgTree = CcgLeaf(E, "e", "FAKEPOS")

    val mockWeighter = new ScgWeighter {
      def weight(tree: CcgTree,
        rootDist: LogProbabilityDistribution[Cat],
        prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
        lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
        rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]): LogDouble = ???

      def pcfgWeight(tree: CcgTree,
        rootDist: LogProbabilityDistribution[Cat],
        prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): LogDouble = {
        assertSame(mockRootDist, rootDist)
        assertSame(mockProdDist, prodDist)
        LogDouble(tree match {
          case T1 => 0.33
          case T2 => 0.13
          case T3 => 0.23
          case T4 => 0.53
          case T5 => 0.43
          case _ => fail(f"no match for $tree"); ???
        })
      }

      def ctxWeight(tree: CcgTree,
        lctxDist: ConditionalLogProbabilityDistribution[Cat, Cat],
        rctxDist: ConditionalLogProbabilityDistribution[Cat, Cat])(se: StartEndTags[Cat]): LogDouble = {
        assertSame(mockLctxDist, lctxDist)
        assertSame(mockRctxDist, rctxDist)
        LogDouble(tree match {
          case T1 => 0.11
          case T2 => 0.21
          case T3 => 0.31
          case T4 => 0.41
          case T5 => 0.51
          case _ => fail(f"no match for $tree"); ???
        })
      }
    }

    val se = SimpleStartEndTags[Cat](STA, END)
    def r(d: Double) = DoubleIteratorRandomGenerator(Iterator(d))

    val cas = new ContextScgAcceptanceSampler(mockWeighter)

    val (a1, r1, ag1) = cas.accept(newTree = T1, curTree = T4, mockRootDist, mockProdDist, mockLctxDist, mockRctxDist, r(0.10))(se) // 0.11/0.41 = 0.2682926829268293
    assertTrue(a1)
    assertEquals(0.2682926829268293, r1, 1e-9)
    assertTrue(ag1)

    val (a2, r2, ag2) = cas.accept(newTree = T1, curTree = T4, mockRootDist, mockProdDist, mockLctxDist, mockRctxDist, r(0.25))(se) // 0.11/0.41 = 0.2682926829268293
    assertTrue(a2)
    assertEquals(0.2682926829268293, r2, 1e-9)
    assertTrue(ag2)

    val (a3, r3, ag3) = cas.accept(newTree = T1, curTree = T4, mockRootDist, mockProdDist, mockLctxDist, mockRctxDist, r(0.50))(se) // 0.11/0.41 = 0.2682926829268293
    assertFalse(a3)
    assertEquals(0.2682926829268293, r3, 1e-9)
    assertTrue(ag3)

    val (a4, r4, ag4) = cas.accept(newTree = T1, curTree = T4, mockRootDist, mockProdDist, mockLctxDist, mockRctxDist, r(0.80))(se) // 0.11/0.41 = 0.2682926829268293
    assertFalse(a4)
    assertEquals(0.2682926829268293, r4, 1e-9)
    assertTrue(ag4)

    val n1 = Vector.fill(1000000)(cas.accept(newTree = T1, curTree = T4, mockRootDist, mockProdDist, mockLctxDist, mockRctxDist, new MersenneTwister)(se)._1).count(identity) // 0.11/0.41 = 0.2682926829268293
    assertEquals(0.2682926829268293, n1 / 1000000.0, 1e-3)

    // new is better than current: unconditionally accept
    val (a5, r5, ag5) = cas.accept(newTree = T2, curTree = T1, mockRootDist, mockProdDist, mockLctxDist, mockRctxDist, DoubleIteratorRandomGenerator(Iterator()))(se) // 0.21/0.11 = 1.909090909090909
    assertTrue(a5)
    assertEquals(1.909090909090909, r5, 1e-9)
    assertFalse(ag5)
  }

}
