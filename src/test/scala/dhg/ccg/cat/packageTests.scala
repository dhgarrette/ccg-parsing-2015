package dhg.ccg.cat

import org.junit.Test
import org.junit.Assert._
import dhg.util.TestUtil._
import dhg.util._
import dhg.ccg.tagdict.StartEndTags

class packageTests {

  implicit val ci = NonRemovingCcgBankCatInterner
  
  val s = AtomCat("S")
  val np = AtomCat("NP")
  val n = AtomCat("N")
  val Sdcl = AtomCat("S", Some("dcl"))

  @Test
  def test_CatInternerStringInterpolationHelper {
    assertEquals(((Sdcl \ np) / (s \ np)), cat"""((s[dcl]\np)/(s\np))""")
  }

  @Test
  def test_CatStartEndTags {
    def addSE(v: Vector[Cat])(implicit se: StartEndTags[Cat]) = se.startTag +: v :+ se.endTag
    assertEquals(Vector(Cat.startCat, np / n, n, s \ np, Cat.endCat), addSE(Vector(np / n, n, s \ np)))
  }

}
