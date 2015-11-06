package dhg.ccg.tagdict

import org.junit.Test
import org.junit.Assert._
import dhg.util._

class StartEndTagsTests {

  @Test
  def test_StartEndTags {
    def addSE[A](v: Vector[A])(implicit se: StartEndTags[A]) = se.startTag +: v :+ se.endTag

    implicit object IntStartEndTags extends StartEndTags[Int] { def startTag = 0; def endTag = 9 }
    object AltIntStartEndTags extends StartEndTags[Int] { def startTag = 10; def endTag = 19 }
    assertEquals(Vector(0, 1, 2, 3, 4, 9), addSE(Vector(1, 2, 3, 4)))
    assertEquals(Vector(10, 1, 2, 3, 4, 19), addSE(Vector(1, 2, 3, 4))(AltIntStartEndTags))

    implicit object StringStartEndTags extends StartEndTags[String] { def startTag = "START"; def endTag = "END" }
    assertEquals(Vector("START", "A", "B", "END"), addSE(Vector("A", "B")))
  }

  @Test
  def test_SimpleStartEndTags {
    def addSE[A](v: Vector[A])(implicit se: StartEndTags[A]) = se.startTag +: v :+ se.endTag

    val iset = SimpleStartEndTags(20, 29)
    assertEquals(Vector(20, 1, 2, 3, 4, 29), addSE(Vector(1, 2, 3, 4))(iset))
    implicit val isetb = SimpleStartEndTags(202, 292)
    assertEquals(Vector(202, 5, 6, 292), addSE(Vector(5, 6)))

    val sset = SimpleStartEndTags("S2", "E2")
    assertEquals(Vector("S2", "A", "B", "E2"), addSE(Vector("A", "B"))(sset))
    implicit val ssetb = SimpleStartEndTags("S2b", "E2b")
    assertEquals(Vector("S2b", "C", "D", "E", "E2b"), addSE(Vector("C", "D", "E")))
  }

  @Test
  def test_StartEndTags_swap {
    def addSE[A](v: Vector[A])(implicit se: StartEndTags[A]) = se.startTag +: v :+ se.endTag

    val iset = SimpleStartEndTags(30, 39)
    assertEquals(Vector(39, 1, 2, 3, 4, 30), addSE(Vector(1, 2, 3, 4))(iset.swap))

    val sset = SimpleStartEndTags("S3", "E3")
    assertEquals(Vector("E3", "A", "B", "S3"), addSE(Vector("A", "B"))(sset.swap))
  }

}
