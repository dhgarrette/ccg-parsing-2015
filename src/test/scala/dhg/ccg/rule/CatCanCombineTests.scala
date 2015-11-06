package dhg.ccg.rule

import org.junit.Test
import dhg.util._
import org.junit.Assert._
import dhg.ccg.cat._

class CatCanCombineTests {

  implicit val ci = NonRemovingCcgBankCatInterner

  @Test
  def test_canCombine {
    val s = cat"S".asInstanceOf[NonPuncCat]
    val np = cat"NP".asInstanceOf[NonPuncCat]
    val n = cat"N".asInstanceOf[NonPuncCat]

    /*
     * Combinability should work the same way forward and backward, so 
     * always check with the same conditions and expected results
     */
    def testset(cc: (Cat, Cat) => Boolean) {
      assertTrue(cc(np, s \ np))
      assertTrue(cc(s / np, np / n))
      assertTrue(cc((s \ np) / np, ((s \ np) \ (s \ np))))
      assertFalse(cc(s / np, np \ np))
      assertTrue(cc((s / np) \ s, np / n))
      assertTrue(cc(np, (s \ np) / np))
      assertTrue(cc(cat"np[nb]", s \ np))
      assertTrue(cc(n, s \ np))
      assertFalse(cc(np / n, np))
      assertTrue(cc(n, (cat"S[xb]".asInstanceOf[NonPuncCat] \ np) / np))
      assertFalse(cc(np, np))

      assertTrue(cc(Cat.startCat, s / np))
      assertFalse(cc(Cat.startCat, s \ np))
      assertFalse(cc(s / np, Cat.endCat))
      assertTrue(cc(s \ np, Cat.endCat))
    }

    val fcc = new SimpleCatCanCombine(Vector(
      FA, FAn2np, BA, BAn2np, FC, BC, /*FX,*/ BX),
      Cat.startCat, Cat.endCat)
    testset((constituentCat: Cat, rightContextCat: Cat) => fcc(constituentCat, rightContextCat))

    val bcc = new BackwardCatCanCombine(fcc)
    testset((leftContextCat: Cat, constituentCat: Cat) => bcc(constituentCat, leftContextCat))
  }

}
