package dhg.ccg.parse.pcfg

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.cat._

class ProductionTests {

  @Test
  def test_Prod_hierarchy {
    val s = cat"S".asInstanceOf[AtomCat]
    val np = cat"NP".asInstanceOf[AtomCat]
    val n = cat"N".asInstanceOf[AtomCat]

    val nt = BinaryProd((s \ np) / np, np)
    assertTrue(nt.isNt)
    assertFalse(nt.isTerm)
    assertEquals(raw"[((S\NP)/NP) NP]", nt.toString)

    val un = UnaryProd(s \ np)
    assertTrue(un.isNt)
    assertFalse(un.isTerm)
    assertEquals(raw"[(S\NP)]", un.toString)

    val tm = TermProd("something")
    assertFalse(tm.isNt)
    assertTrue(tm.isTerm)
    assertEquals(raw"something", tm.toString)
  }

}
