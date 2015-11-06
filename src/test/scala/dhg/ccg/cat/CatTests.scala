package dhg.ccg.cat

import org.junit.Test
import dhg.util._
import org.junit.Assert._

class CatTests {

  @Test
  def test_equals {
    assertTrue(cat"A" eq cat"A")
    assertTrue(cat"A".hashCode == cat"A".hashCode)
    assertTrue(cat"A" == cat"A")
  }

  @Test
  def test_noFeat {
    implicit val ci = NonRemovingCcgBankCatInterner

    assertEquals(AtomCat("NP", None, Some(3)),      cat"NP[xb]_3".asInstanceOf[AtomCat].noFeat)
    assertEquals(AtomCat("NP", None, None),         cat"NP[xb]".asInstanceOf[AtomCat].noFeat)
    assertEquals(AtomCat("NP", None, Some(3)), AtomCat("NP_3", None).noFeat)
    assertEquals(AtomCat("NP", None, None),    AtomCat("NP", None).noFeat)
  }

  @Test
  def test_noIndices {
    implicit val ci = NonRemovingCcgBankCatInterner

    assertEquals(AtomCat("NP", Some("xb"), None), cat"NP[xb]_3".asInstanceOf[AtomCat].noIndices)
    assertEquals(AtomCat("NP", Some("xb"), None), cat"NP[xb]".asInstanceOf[AtomCat].noIndices)
    assertEquals(AtomCat("NP", None, None),  AtomCat("NP_3", None).noIndices)
    assertEquals(AtomCat("NP", None, None),  AtomCat("NP", None).noIndices)
  }

  @Test
  def test_operators {
    implicit val ci = NonRemovingCcgBankCatInterner

    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    val x1 = cat"NP[xb]"
    val x2 = cat"NP[xb]"
    val x3 = AtomCat("NP", Some("xb"))
    val x4 = AtomCat("NP", Some("xb"))
    assertEquals(x1, x2)
    assertEquals(x3, x4)
    assertEquals(x2, x3)

    val a = BCat(AtomCat("S", None), AtomCat("NP", None))
    val b = s \ np
    val c = FCat(AtomCat("S", None), AtomCat("NP", None))
    val d = s / np
    println(a.hashCode)
    println(b.hashCode)
    println(c.hashCode)
    println(d.hashCode)

    assertEquals(BCat(AtomCat("S", None), AtomCat("NP", None)), s \ np)
    assertEquals(FCat(AtomCat("S", None), AtomCat("NP", None)), s / np)
    assertEquals(FCat(cat"NP[xb]".asInstanceOf[AtomCat], AtomCat("N", None)), AtomCat("NP", Some("xb")) / n)
    assertEquals(BCat(AtomCat("NP", None), cat"NP[xb]".asInstanceOf[AtomCat]), np \ AtomCat("NP", Some("xb")))

    assertEquals(BCat(FCat(AtomCat("S", None), cat"NP[xb]".asInstanceOf[AtomCat]), AtomCat("S", None)), (s / AtomCat("NP", Some("xb"))) \ s)
    assertEquals(FCat(BCat(AtomCat("S", None), AtomCat("NP", None)), cat"NP[xb]".asInstanceOf[AtomCat]), (s \ np) / AtomCat("NP", Some("xb")))
    assertEquals(FCat(BCat(AtomCat("S", Some("dcl")), AtomCat("NP", None)), AtomCat("NP", None)), (cat"S[dcl]".asInstanceOf[AtomCat] \ np) / np)
    assertEquals(FCat(BCat(BCat(AtomCat("S", None), AtomCat("NP", None)), BCat(AtomCat("S", None), AtomCat("NP", None))), AtomCat("NP", None)), ((s \ np) \ (s \ np)) / np)
  }

  @Test
  def test_unapply {
    implicit val ci = NonRemovingCcgBankCatInterner
    val NPxb = AtomCat("NP", Some("xb"))

    val NPxb2 = cat"NP[xb]"
    assertEquals(NPxb2, NPxb)

    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    val (a / b) = s / np
    assertEquals(s, a)
    assertEquals(np, b)

    val (c \ d) = (s / NPxb) \ np
    assertEquals(s / NPxb, c)
    assertEquals(np, d)

    val (e || f) = s / NPxb
    assertEquals(s, e)
    assertEquals(NPxb, f)

    val (g || h) = cat"S[xb]".asInstanceOf[NonPuncCat] \ (s / np)
    assertEquals(cat"S[xb]", g)
    assertEquals((s / np), h)

    assertTrue(np match { case (a / b) => false; case _ => true })
    assertTrue(s \ np match { case (a / b) => false; case _ => true })
    assertTrue(np match { case (a \ b) => false; case _ => true })
    assertTrue(s / np match { case (a \ b) => false; case _ => true })
    assertTrue(np match { case (a || b) => false; case _ => true })
  }

  @Test
  def test_unify {
    implicit val ci = NonRemovingCcgBankCatInterner

    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    assertTrue(s u s)
    assertTrue(cat"S[xb]".asInstanceOf[AtomCat] u s)
    assertTrue(s u cat"S[xb]".asInstanceOf[AtomCat])
    assertTrue(cat"S[xb]".asInstanceOf[AtomCat] u cat"S[xb]".asInstanceOf[AtomCat])

    assertFalse(s u n)
    assertFalse(cat"S[xb]".asInstanceOf[AtomCat] u n)
    assertFalse(s u AtomCat("N", Some("dcl")))
    assertFalse(cat"S[xb]".asInstanceOf[AtomCat] u AtomCat("N", Some("dcl")))

    assertTrue((s \ np) u (s \ np))
    assertTrue((cat"S[xb]".asInstanceOf[AtomCat] \ np) u (cat"S[xb]".asInstanceOf[AtomCat] \ np))
    assertTrue((cat"S[xb]".asInstanceOf[AtomCat] \ np) u (s \ np))
    assertTrue((s \ np) u (cat"S[xb]".asInstanceOf[AtomCat] \ np))
    assertTrue((s \ AtomCat("NP", Some("xb"))) u (cat"S[xb]".asInstanceOf[AtomCat] \ np))
    assertTrue((s \ np) u (cat"S[xb]".asInstanceOf[AtomCat] \ AtomCat("NP", Some("xb"))))

    assertFalse((s \ np) u (s / np))
    assertFalse((cat"S[xb]".asInstanceOf[AtomCat] \ np) u (cat"S[xb]".asInstanceOf[AtomCat] / np))
    assertFalse((cat"S[xb]".asInstanceOf[AtomCat] \ np) u (s / np))
    assertFalse((s \ np) u (cat"S[xb]".asInstanceOf[AtomCat] / np))
    assertFalse((s \ AtomCat("NP", Some("xb"))) u (cat"S[xb]".asInstanceOf[AtomCat] / np))
    assertFalse((s \ np) u (cat"S[xb]".asInstanceOf[AtomCat] / AtomCat("NP", Some("xb"))))

    assertFalse((s \ np) u (n \ np))
    assertFalse((cat"S[xb]".asInstanceOf[AtomCat] \ np) u (AtomCat("N", Some("dcl")) \ np))
    assertFalse((cat"S[xb]".asInstanceOf[AtomCat] \ np) u (n \ np))
    assertFalse((s \ np) u (AtomCat("N", Some("dcl")) \ np))
    assertFalse((s \ AtomCat("NP", Some("xb"))) u (AtomCat("N", Some("dcl")) \ np))
    assertFalse((s \ np) u (AtomCat("N", Some("dcl")) \ AtomCat("NP", Some("xb"))))

    assertFalse((s \ np) u (s \ s))
    assertFalse((cat"S[xb]".asInstanceOf[AtomCat] \ np) u (cat"S[xb]".asInstanceOf[AtomCat] \ s))
    assertFalse((cat"S[xb]".asInstanceOf[AtomCat] \ np) u (s \ s))
    assertFalse((s \ np) u (cat"S[xb]".asInstanceOf[AtomCat] \ s))
    assertFalse((s \ AtomCat("NP", Some("xb"))) u (cat"S[xb]".asInstanceOf[AtomCat] \ s))
    assertFalse((s \ np) u (cat"S[xb]".asInstanceOf[AtomCat] \ cat"S[xb]".asInstanceOf[AtomCat]))

    assertTrue((s / np) u (s / np))
    assertTrue((cat"S[xb]".asInstanceOf[AtomCat] / np) u (cat"S[xb]".asInstanceOf[AtomCat] / np))
    assertTrue((cat"S[xb]".asInstanceOf[AtomCat] / np) u (s / np))
    assertTrue((s / np) u (cat"S[xb]".asInstanceOf[AtomCat] / np))
    assertTrue((s / AtomCat("NP", Some("xb"))) u (cat"S[xb]".asInstanceOf[AtomCat] / np))
    assertTrue((s / np) u (cat"S[xb]".asInstanceOf[AtomCat] / AtomCat("NP", Some("xb"))))

    assertFalse((s / np) u (s \ np))
    assertFalse((cat"S[xb]".asInstanceOf[AtomCat] / np) u (cat"S[xb]".asInstanceOf[AtomCat] \ np))
    assertFalse((cat"S[xb]".asInstanceOf[AtomCat] / np) u (s \ np))
    assertFalse((s / np) u (cat"S[xb]".asInstanceOf[AtomCat] \ np))
    assertFalse((s / AtomCat("NP", Some("xb"))) u (cat"S[xb]".asInstanceOf[AtomCat] \ np))
    assertFalse((s / np) u (cat"S[xb]".asInstanceOf[AtomCat] \ AtomCat("NP", Some("xb"))))

    assertFalse((s / np) u (n / np))
    assertFalse((cat"S[xb]".asInstanceOf[AtomCat] / np) u (AtomCat("N", Some("dcl")) / np))
    assertFalse((cat"S[xb]".asInstanceOf[AtomCat] / np) u (n / np))
    assertFalse((s / np) u (AtomCat("N", Some("dcl")) / np))
    assertFalse((s / AtomCat("NP", Some("xb"))) u (AtomCat("N", Some("dcl")) / np))
    assertFalse((s / np) u (AtomCat("N", Some("dcl")) / AtomCat("NP", Some("xb"))))

    assertFalse((s / np) u (s / s))
    assertFalse((cat"S[xb]".asInstanceOf[AtomCat] / np) u (cat"S[xb]".asInstanceOf[AtomCat] / s))
    assertFalse((cat"S[xb]".asInstanceOf[AtomCat] / np) u (s / s))
    assertFalse((s / np) u (cat"S[xb]".asInstanceOf[AtomCat] / s))
    assertFalse((s / AtomCat("NP", Some("xb"))) u (cat"S[xb]".asInstanceOf[AtomCat] / s))
    assertFalse((s / np) u (cat"S[xb]".asInstanceOf[AtomCat] / cat"S[xb]".asInstanceOf[AtomCat]))

    assertTrue((((cat"S[xb]".asInstanceOf[AtomCat] \ np) \ (s \ np)) / np) u (((cat"S[xb]".asInstanceOf[AtomCat] \ AtomCat("NP", Some("ab"))) \ (s \ AtomCat("NP", Some("xb")))) / np))
    assertFalse((((cat"S[xb]".asInstanceOf[AtomCat] \ np) \ (s \ np)) / np) u (((AtomCat("S", Some("int")) \ AtomCat("NP", Some("ab"))) \ (s \ AtomCat("NP", Some("xb")))) / np))
  }

  @Test
  def test_toString {
    implicit val ci = NonRemovingCcgBankCatInterner

    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    assertEquals("""(((S[dcl]\NP)\(S\NP[xb]))/NP)""", (((cat"S[dcl]".asInstanceOf[AtomCat] \ np) \ (s \ AtomCat("NP", Some("xb")))) / np).toString)
  }

  @Test
  def test_size {
    implicit val ci = NonRemovingCcgBankCatInterner

    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    assertEquals(1, (n).size)
    assertEquals(1, (np).size)
    assertEquals(1, (AtomCat("NP", Some("xb"))).size)
    assertEquals(2, (s \ np).size)
    assertEquals(2, (s / np).size)
    assertEquals(2, (np / n).size)
    assertEquals(2, (np \ np).size)
    assertEquals(3, ((s / np) \ s).size)
    assertEquals(3, ((s \ np) / np).size)
    assertEquals(3, ((cat"S[xb]".asInstanceOf[AtomCat] \ np) / np).size)
    assertEquals(4, (((s \ np) \ (s \ np))).size)
    assertEquals(4, ((((s \ np) \ s) \ np)).size)
    assertEquals(5, (((s \ np) \ (s \ np)) / np).size)
  }

  @Test
  def test_complexity {
    implicit val ci = NonRemovingCcgBankCatInterner

    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    assertEquals(1, (n).complexity)
    assertEquals(1, (np).complexity)
    assertEquals(1, (AtomCat("NP", Some("xb"))).complexity)
    assertEquals(3, (s \ np).complexity)
    assertEquals(3, (s / np).complexity)
    assertEquals(3, (np / n).complexity)
    assertEquals(3, (np \ np).complexity)
    assertEquals(5, ((s / np) \ s).complexity)
    assertEquals(5, ((s \ np) / np).complexity)
    assertEquals(5, ((cat"S[xb]".asInstanceOf[AtomCat] \ np) / np).complexity)
    assertEquals(7, (((s \ np) \ (s \ np))).complexity)
    assertEquals(7, ((((s \ np) \ s) \ np)).complexity)
    assertEquals(9, (((s \ np) \ (s \ np)) / np).complexity)
  }

  @Test
  def test_isModifier {
    implicit val ci = NonRemovingCcgBankCatInterner

    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    assertFalse((s).isModifier)
    assertTrue((s / s).isModifier)
    assertTrue((s \ s).isModifier)
    assertTrue(((s / n) \ (s / n)).isModifier)
    assertFalse(((s / n) \ (s \ n)).isModifier)
    assertFalse(((s / n) \ (s / s)).isModifier)
    assertFalse(((s / s) \ (s / n)).isModifier)
    assertFalse(((s / n) / n).isModifier)
  }

}
