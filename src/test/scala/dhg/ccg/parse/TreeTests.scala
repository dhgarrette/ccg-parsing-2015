package dhg.ccg.parse

import org.junit.Test
import dhg.util._
import org.junit.Assert._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.util._
import dhg.ccg.parse.pcfg._

class TreeTests {

  val A = cat"A"
  val B = cat"B"
  val C = cat"C"
  val D = cat"D"
  val E = cat"E"
  val F = cat"F"
  val G = cat"G"
  val H = cat"H"

  @Test
  def test_CcgTree {

    val b = CcgLeaf(B, "b", "FAKEPOS")
    val h = CcgUnode(H, b)
    val d = CcgLeaf(D, "d", "FAKEPOS")
    val e = CcgLeaf(E, "e", "FAKEPOS")
    val f = CcgUnode(F, e)
    val c = CcgBinode(C, d, f)
    val g = CcgUnode(G, c)
    val a = CcgBinode(A, h, g)
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

    assertEquals(A, a.cat)
    assertEquals(D, d.cat)
    assertEquals(G, g.cat)

    assertEquals(Vector(b), b.leaves)
    assertEquals(Vector(b), h.leaves)
    assertEquals(Vector(d), d.leaves)
    assertEquals(Vector(e), e.leaves)
    assertEquals(Vector(e), f.leaves)
    assertEquals(Vector(d, e), c.leaves)
    assertEquals(Vector(d, e), g.leaves)
    assertEquals(Vector(b, d, e), a.leaves)

    assertEquals(Vector("b"), b.words)
    assertEquals(Vector("b"), h.words)
    assertEquals(Vector("d"), d.words)
    assertEquals(Vector("e"), e.words)
    assertEquals(Vector("e"), f.words)
    assertEquals(Vector("d", "e"), c.words)
    assertEquals(Vector("d", "e"), g.words)
    assertEquals(Vector("b", "d", "e"), a.words)

    assertEquals(Vector(B), b.supertags)
    assertEquals(Vector(B), h.supertags)
    assertEquals(Vector(D), d.supertags)
    assertEquals(Vector(E), e.supertags)
    assertEquals(Vector(E), f.supertags)
    assertEquals(Vector(D, E), c.supertags)
    assertEquals(Vector(D, E), g.supertags)
    assertEquals(Vector(B, D, E), a.supertags)

    assertEquals(Vector(("b", B)), b.tagged)
    assertEquals(Vector(("b", B)), h.tagged)
    assertEquals(Vector(("d", D)), d.tagged)
    assertEquals(Vector(("e", E)), e.tagged)
    assertEquals(Vector(("e", E)), f.tagged)
    assertEquals(Vector(("d", D), ("e", E)), c.tagged)
    assertEquals(Vector(("d", D), ("e", E)), g.tagged)
    assertEquals(Vector(("b", B), ("d", D), ("e", E)), a.tagged)

    assertEquals(1, b.length)
    assertEquals(1, h.length)
    assertEquals(1, d.length)
    assertEquals(1, e.length)
    assertEquals(1, f.length)
    assertEquals(2, c.length)
    assertEquals(2, g.length)
    assertEquals(3, a.length)
  }

  @Test
  def test_CcgTree_static_unapply {
    val b = CcgLeaf(B, "b", "FAKEPOS")
    val h = CcgUnode(H, b)
    val d = CcgLeaf(D, "d", "FAKEPOS")
    val e = CcgLeaf(E, "e", "FAKEPOS")
    val f = CcgUnode(F, e)
    val c = CcgBinode(C, d, f)
    val g = CcgUnode(G, c)
    val a = CcgBinode(A, h, g)

    val CcgTree(ac) = a
    assertEquals(A, ac)
    val CcgTree(bc) = b
    assertEquals(B, bc)
    val CcgTree(cc) = c
    assertEquals(C, cc)
    val CcgTree(dc) = d
    assertEquals(D, dc)
    val CcgTree(ec) = e
    assertEquals(E, ec)
    val CcgTree(fc) = f
    assertEquals(F, fc)
    val CcgTree(gc) = g
    assertEquals(G, gc)
    val CcgTree(hc) = h
    assertEquals(H, hc)
  }

  @Test
  def test_SimpleRuleViolationFinder_violations {
    val s = cat"S".asInstanceOf[AtomCat]
    val np = cat"NP".asInstanceOf[AtomCat]
    val n = cat"N".asInstanceOf[AtomCat]

    val vf = new SimpleRuleViolationFinder(Vector(FA, BA, N2NP))

    assertEquals(Vector(), vf.violations(
      CcgBinode(s,
        CcgBinode(np,
          CcgLeaf(np / n, "the", "FAKEPOS"),
          CcgLeaf(n, "dog", "FAKEPOS")),
        CcgLeaf(s \ np, "walks", "FAKEPOS"))))

    assertEquals(Vector(), vf.violations(
      CcgBinode(s,
        CcgUnode(np,
          CcgLeaf(n, "dogs", "FAKEPOS")),
        CcgLeaf(s \ np, "bark", "FAKEPOS"))))

    assertEquals(Vector((np, UnaryProd(s))), vf.violations(
      CcgBinode(s,
        CcgUnode(np,
          CcgLeaf(s, "dogs", "FAKEPOS")),
        CcgLeaf(s \ np, "bark", "FAKEPOS"))))

    assertEquals(Vector((s, BinaryProd(n, s \ np))), vf.violations(
      CcgBinode(s,
        CcgLeaf(n, "dogs", "FAKEPOS"),
        CcgLeaf(s \ np, "bark", "FAKEPOS"))))

    assertEquals(Vector(), vf.violations(
      CcgBinode(np,
        CcgLeaf(cat"NP[xb]".asInstanceOf[AtomCat] / n, "the", "FAKEPOS"),
        CcgLeaf(n, "dog", "FAKEPOS"))))

    assertEquals(Vector(), vf.violations(
      CcgBinode(s,
        CcgBinode(np,
          CcgLeaf(cat"NP[xb]".asInstanceOf[AtomCat] / n, "the", "FAKEPOS"),
          CcgLeaf(n, "dog", "FAKEPOS")),
        CcgLeaf(s \ np, "walks", "FAKEPOS"))))

    assertEquals(Vector((cat"NP[xb]".asInstanceOf[AtomCat], BinaryProd(cat"NP[x]".asInstanceOf[AtomCat] / n, n))), vf.violations(
      CcgBinode(s,
        CcgBinode(cat"NP[xb]".asInstanceOf[AtomCat],
          CcgLeaf(cat"NP[x]".asInstanceOf[AtomCat] / n, "the", "FAKEPOS"),
          CcgLeaf(n, "dog", "FAKEPOS")),
        CcgLeaf(s \ np, "walks", "FAKEPOS"))))
  }

}
