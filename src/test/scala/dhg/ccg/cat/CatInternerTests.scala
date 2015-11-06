package dhg.ccg.cat

import org.junit.Test
import org.junit.Assert._
import dhg.util.TestUtil._
import dhg.util._

class CatInternerTests {

  @Test
  def test_featured_ccgbank {

    implicit val ci = new StandardCatInterner(CatInterner.CcgBankPuncRe, CatInterner.CcgBankAtomRe, removeFeatures = false)

    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    ci("""(s\np)""")
    ci("""s""")
    ci("""((s\np)/np)""")
    ci("""((s[dcl]\np)/(s\np))""")

    val `i s` = ci("s")
    assertEquals(s, `i s`)
    assertTrue(`i s` eq ci("s"))

    val `(s\np)` = ci("""(s\np)""")
    assertEquals((s \ np), `(s\np)`)
    assertTrue((s \ np) eq ci("""(s\np)"""))
    assertTrue(`(s\np)` eq ci("""(s\np)"""))
    assertTrue(`i s` eq `(s\np)`.asInstanceOf[BCat].left)

    val `i np` = ci("np")
    assertEquals(np, `i np`)
    assertTrue(`i np` eq ci("np"))
    assertTrue(`i np` eq `(s\np)`.asInstanceOf[BCat].right)

    val `(s[dcl]\np)` = ci("""(s[dcl]\np)""")
    assertEquals((cat"S[dcl]".asInstanceOf[NonPuncCat] \ np), `(s[dcl]\np)`)
    assertTrue(`(s[dcl]\np)` eq ci("""(s[dcl]\np)"""))
    assertTrue(ci("""s[dcl]""") eq `(s[dcl]\np)`.asInstanceOf[BCat].left)

    val `((s[dcl]\np)/(s\np))` = ci("""((s[dcl]\np)/(s\np))""")
    assertEquals(((cat"S[dcl]".asInstanceOf[NonPuncCat] \ np) / (s \ np)), `((s[dcl]\np)/(s\np))`)
    assertTrue(`((s[dcl]\np)/(s\np))` eq ci("""((s[dcl]\np)/(s\np))"""))
    assertTrue(ci("""(s[dcl]\np)""") eq `(s[dcl]\np)`)
    assertTrue(ci("""(s[dcl]\np)""") eq `((s[dcl]\np)/(s\np))`.asInstanceOf[FCat].left)

    val `((s\np)/(s\np))` = ci("""((s\np)/(s\np))""")
    assertEquals(((s \ np) / (s \ np)), `((s\np)/(s\np))`)
    assertTrue(((s \ np) / (s \ np)) eq ci("""((s\np)/(s\np))"""))
    assertTrue(`((s\np)/(s\np))` eq ci("""((s\np)/(s\np))"""))
    assertTrue(ci("""(s\np)""") eq `(s\np)`)
    assertTrue(ci("""(s\np)""") eq `((s\np)/(s\np))`.asInstanceOf[FCat].left)
    assertTrue(ci("""(s\np)""") eq `((s\np)/(s\np))`.asInstanceOf[FCat].right)

    assertException(ci("""(s\np)/""")) { case e: Exception => } // assertEquals("""failed to find main op in (s\np)/""", e.getMessage) }
    assertException(ci("""/(s\np)""")) { case e: Exception => } // assertEquals("""failed to find main op in /(s\np)""", e.getMessage) }
    assertException(ci("""s\np""")) { case e: Exception => } // assertEquals("""failed to find main op in s\np""", e.getMessage) }
    assertException(ci("""(s\np""")) { case e: Exception => } // assertEquals("""failed to find main op in (s\np""", e.getMessage) }
    assertException(ci("""np)""")) { case e: Exception => } // assertEquals("""failed to find main op in np)""", e.getMessage) }
    assertException(ci("""(n/n)/n)""")) { case e: Exception => } // assertEquals("""failed to find main op in np)""", e.getMessage) }
  }

  @Test
  def test_featured_tut {

    implicit val ci = new StandardCatInterner(CatInterner.CcgBankPuncRe, CatInterner.TutBankAtomRe, removeFeatures = false)

    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    ci("""(s\np)""")
    ci("""s""")
    ci("""((s\np)/np)""")
    ci("""((s:dcl\np)/(s\np))""")

    val `i s` = ci("s")
    assertEquals(s, `i s`)
    assertTrue(`i s` eq ci("s"))

    val `(s\np)` = ci("""(s\np)""")
    assertEquals((s \ np), `(s\np)`)
    assertTrue(`(s\np)` eq ci("""(s\np)"""))
    assertTrue(`i s` eq `(s\np)`.asInstanceOf[BCat].left)

    val `i np` = ci("np")
    assertEquals(np, `i np`)
    assertTrue(`i np` eq ci("np"))
    assertTrue(`i np` eq `(s\np)`.asInstanceOf[BCat].right)

    val `(s:dcl\np)` = ci("""(s:dcl\np)""")
    assertEquals((cat"S[dcl]".asInstanceOf[AtomCat] \ np), `(s:dcl\np)`)
    assertTrue(`(s:dcl\np)` eq ci("""(s:dcl\np)"""))
    assertTrue(ci("""s:dcl""") eq `(s:dcl\np)`.asInstanceOf[BCat].left)

    val `((s:dcl\np)/(s\np))` = ci("""((s:dcl\np)/(s\np))""")
    assertEquals(((cat"S[dcl]".asInstanceOf[AtomCat] \ np) / (s \ np)), `((s:dcl\np)/(s\np))`)
    assertTrue(`((s:dcl\np)/(s\np))` eq ci("""((s:dcl\np)/(s\np))"""))
    assertTrue(ci("""(s:dcl\np)""") eq `(s:dcl\np)`)
    assertTrue(ci("""(s:dcl\np)""") eq `((s:dcl\np)/(s\np))`.asInstanceOf[FCat].left)

    val `((s\np)/(s\np))` = ci("""((s\np)/(s\np))""")
    assertEquals(((s \ np) / (s \ np)), `((s\np)/(s\np))`)
    assertTrue(`((s\np)/(s\np))` eq ci("""((s\np)/(s\np))"""))
    assertTrue(ci("""(s\np)""") eq `(s\np)`)
    assertTrue(ci("""(s\np)""") eq `((s\np)/(s\np))`.asInstanceOf[FCat].left)
    assertTrue(ci("""(s\np)""") eq `((s\np)/(s\np))`.asInstanceOf[FCat].right)

    assertException(ci("""(s\np)/""")) { case e: Exception => } // assertEquals("""failed to find main op in (s\np)/""", e.getMessage) }
    assertException(ci("""/(s\np)""")) { case e: Exception => } // assertEquals("""failed to find main op in /(s\np)""", e.getMessage) }
    assertException(ci("""s\np""")) { case e: Exception => } // assertEquals("""failed to find main op in s\np""", e.getMessage) }
    assertException(ci("""(s\np""")) { case e: Exception => } // assertEquals("""failed to find main op in (s\np""", e.getMessage) }
    assertException(ci("""np)""")) { case e: Exception => } // assertEquals("""failed to find main op in np)""", e.getMessage) }
    assertException(ci("""(n/n)/n)""")) { case e: Exception => } // assertEquals("""failed to find main op in np)""", e.getMessage) }
  }

  @Test
  def test_featureless_ccgbank {

    implicit val ci = new StandardCatInterner(CatInterner.CcgBankPuncRe, CatInterner.CcgBankAtomRe, removeFeatures = true)

    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    ci("""(s\np)""")
    ci("""s""")
    ci("""((s\np)/np)""")
    ci("""((s[dcl]\np)/(s\np))""")

    val `i s` = ci("s")
    assertEquals(s, `i s`)
    assertTrue(`i s` eq ci("s"))

    val `(s\np)` = ci("""(s\np)""")
    assertEquals((s \ np), `(s\np)`)
    assertTrue(`(s\np)` eq ci("""(s\np)"""))
    assertTrue(`i s` eq `(s\np)`.asInstanceOf[BCat].left)

    val `i np` = ci("np")
    assertEquals(np, `i np`)
    assertTrue(`i np` eq ci("np"))
    assertTrue(`i np` eq `(s\np)`.asInstanceOf[BCat].right)

    assertTrue(ci("""(s[dcl]\np)""") eq `(s\np)`)
    assertTrue(ci("""(s[dcl]\np)""") eq ci("""(s\np)"""))
    assertTrue(ci("""s[dcl]""") eq `i s`)
    assertTrue(ci("""s[dcl]""") eq ci("""s"""))

    val `((s\np)/(s\np))` = ci("""((s\np)/(s\np))""")
    assertEquals(((s \ np) / (s \ np)), `((s\np)/(s\np))`)
    assertTrue(`((s\np)/(s\np))` eq ci("""((s\np)/(s\np))"""))
    assertTrue(ci("""(s\np)""") eq `(s\np)`)
    assertTrue(ci("""(s\np)""") eq `((s\np)/(s\np))`.asInstanceOf[FCat].left)
    assertTrue(ci("""(s\np)""") eq `((s\np)/(s\np))`.asInstanceOf[FCat].right)

    assertTrue(`((s\np)/(s\np))` eq ci("""((s[dcl]\np)/(s\np))"""))

    assertException(ci("""(s\np)/""")) { case e: Exception => } // assertEquals("""failed to find main op in (s\np)/""", e.getMessage) }
    assertException(ci("""/(s\np)""")) { case e: Exception => } // assertEquals("""failed to find main op in /(s\np)""", e.getMessage) }
    assertException(ci("""s\np""")) { case e: Exception => } // assertEquals("""failed to find main op in s\np""", e.getMessage) }
    assertException(ci("""(s\np""")) { case e: Exception => } // assertEquals("""failed to find main op in (s\np""", e.getMessage) }
    assertException(ci("""np)""")) { case e: Exception => } // assertEquals("""failed to find main op in np)""", e.getMessage) }
    assertException(ci("""(n/n)/n)""")) { case e: Exception => } // assertEquals("""failed to find main op in np)""", e.getMessage) }
  }

  @Test
  def test_featureless_tut {

    implicit val ci = new StandardCatInterner(CatInterner.CcgBankPuncRe, CatInterner.TutBankAtomRe, removeFeatures = true)

    val s = AtomCat("S")
    val np = AtomCat("NP")
    val n = AtomCat("N")

    ci("""(s\np)""")
    ci("""s""")
    ci("""((s\np)/np)""")
    ci("""((s:dcl\np)/(s\np))""")

    val `i s` = ci("s")
    assertEquals(s, `i s`)
    assertTrue(`i s` eq ci("s"))

    val `(s\np)` = ci("""(s\np)""")
    assertEquals((s \ np), `(s\np)`)
    assertTrue(`(s\np)` eq ci("""(s\np)"""))
    assertTrue(`i s` eq `(s\np)`.asInstanceOf[BCat].left)

    val `i np` = ci("np")
    assertEquals(np, `i np`)
    assertTrue(`i np` eq ci("np"))
    assertTrue(`i np` eq `(s\np)`.asInstanceOf[BCat].right)

    assertTrue(ci("""(s:dcl\np)""") eq `(s\np)`)
    assertTrue(ci("""(s:dcl\np)""") eq ci("""(s\np)"""))
    assertTrue(ci("""s:dcl""") eq `i s`)
    assertTrue(ci("""s:dcl""") eq ci("""s"""))

    val `((s\np)/(s\np))` = ci("""((s\np)/(s\np))""")
    assertEquals(((s \ np) / (s \ np)), `((s\np)/(s\np))`)
    assertTrue(`((s\np)/(s\np))` eq ci("""((s\np)/(s\np))"""))
    assertTrue(ci("""(s\np)""") eq `(s\np)`)
    assertTrue(ci("""(s\np)""") eq `((s\np)/(s\np))`.asInstanceOf[FCat].left)
    assertTrue(ci("""(s\np)""") eq `((s\np)/(s\np))`.asInstanceOf[FCat].right)

    assertTrue(`((s\np)/(s\np))` eq ci("""((s:dcl\np)/(s\np))"""))

    assertException(ci("""(s\np)/""")) { case e: Exception => } // assertEquals("""failed to find main op in (s\np)/""", e.getMessage) }
    assertException(ci("""/(s\np)""")) { case e: Exception => } // assertEquals("""failed to find main op in /(s\np)""", e.getMessage) }
    assertException(ci("""s\np""")) { case e: Exception => } // assertEquals("""failed to find main op in s\np""", e.getMessage) }
    assertException(ci("""(s\np""")) { case e: Exception => } // assertEquals("""failed to find main op in (s\np""", e.getMessage) }
    assertException(ci("""np)""")) { case e: Exception => } // assertEquals("""failed to find main op in np)""", e.getMessage) }
    assertException(ci("""(n/n)/n)""")) { case e: Exception => } // assertEquals("""failed to find main op in np)""", e.getMessage) }
  }

}
