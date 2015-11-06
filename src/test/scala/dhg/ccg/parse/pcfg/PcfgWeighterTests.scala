package dhg.ccg.parse.pcfg

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.parse.pcfg._
import dhg.util._
import scala.collection.immutable.ListMap
import dhg.ccg.tagdict.SimpleStartEndTags

class PcfgWeighterTests {

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
  def test_SimplePcfgWeighter_weight {
    type Word = Symbol
    val weighter = new SimplePcfgWeighter()
    val rootDist = new LogProbabilityDistribution[Cat] {
      def apply(b: Cat): LogDouble = LogDouble(b match {
        case A => 0.11
      })
      def sample(): Cat = ???
      def defaultProb: LogDouble = ???
    }
    val prodDist = new ConditionalLogProbabilityDistribution[Cat, Prod] {
      def apply(x: Prod, given: Cat): LogDouble = LogDouble((given, x) match {
        case (A, BinaryProd(B, C)) => 0.21
        case (B, TermProd("b")) => 0.22
        case (C, BinaryProd(D, E)) => 0.23
        case (D, TermProd("d")) => 0.24
        case (E, TermProd("e")) => 0.25
      })
      def sample(given: Cat): Prod = ???
    }
    val lctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
      def apply(left: Cat, given: Cat): LogDouble = ???
      def sample(given: Cat): Cat = ???
    }
    val rctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
      def apply(right: Cat, given: Cat): LogDouble = ???
      def sample(given: Cat): Cat = ???
    }
    val se = SimpleStartEndTags[Cat](STA, END)

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
    assertEqualsLog(expected, weighter.weight(tree, rootDist, prodDist), 1e-9)
  }

  @Test
  def test_SimplePcfgWeighter_weight_withUnary {
    type Word = Symbol
    val weighter = new SimplePcfgWeighter()
    val rootDist = new LogProbabilityDistribution[Cat] {
      def apply(b: Cat): LogDouble = LogDouble(b match {
        case A => 0.11
      })
      def sample(): Cat = ???
      def defaultProb: LogDouble = ???
    }
    val prodDist = new ConditionalLogProbabilityDistribution[Cat, Prod] {
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
    }
    val lctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
      def apply(left: Cat, given: Cat): LogDouble = ???
      def sample(given: Cat): Cat = ???
    }
    val rctxDist = new ConditionalLogProbabilityDistribution[Cat, Cat] {
      def apply(right: Cat, given: Cat): LogDouble = ???
      def sample(given: Cat): Cat = ???
    }
    val se = SimpleStartEndTags[Cat](STA, END)

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
    assertEqualsLog(expected, weighter.weight(tree, rootDist, prodDist), 1e-9)
  }

  def assertEqualsLog(a: LogDouble, b: LogDouble, e: Double) {
    assertEquals(a.toDouble, b.toDouble, e)
  }

}
