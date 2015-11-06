package dhg.ccg.parse.pcfg

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.util._
import dhg.util.TestUtil._

class ChartTests {

  @Test
  def test_Chart_companion_fillVal {
    val itr = Iterator("no")
    val c = Chart.fillVal(3)(itr.next)
    assertEquals("no", c(0, 1))
    assertEquals("no", c(0, 2))
    assertEquals("no", c(0, 3))
    assertEquals("no", c(1, 2))
    assertEquals("no", c(1, 3))
    assertEquals("no", c(2, 3))

    c(0, 1) = "0-1"
    c(0, 2) = "0-2"
    c(0, 3) = "0-3"
    c(1, 2) = "1-2"
    c(1, 3) = "1-3"
    c(2, 3) = "2-3"

    assertEquals("0-1", c(0, 1))
    assertEquals("0-2", c(0, 2))
    assertEquals("0-3", c(0, 3))
    assertEquals("1-2", c(1, 2))
    assertEquals("1-3", c(1, 3))
    assertEquals("2-3", c(2, 3))

    assertExceptionAny(c(0, 0))
    assertExceptionAny(c(1, 1))
    assertExceptionAny(c(2, 2))
    assertExceptionAny(c(3, 3))
    assertExceptionAny(c(2, 4))
    assertExceptionAny(c(2, 1))
  }

  @Test
  def test_Chart_companion_fill {
    var itr = for { i <- (0 until 3).iterator; j <- (i + 1 to 3) } yield (i, j)
    val c = Chart.fill(3)("no-" + itr.next)
    assertEquals("no-(0,1)", c(0, 1))
    assertEquals("no-(0,2)", c(0, 2))
    assertEquals("no-(0,3)", c(0, 3))
    assertEquals("no-(1,2)", c(1, 2))
    assertEquals("no-(1,3)", c(1, 3))
    assertEquals("no-(2,3)", c(2, 3))

    c(0, 1) = "0-1"
    c(0, 2) = "0-2"
    c(0, 3) = "0-3"
    c(1, 2) = "1-2"
    c(1, 3) = "1-3"
    c(2, 3) = "2-3"

    assertEquals("0-1", c(0, 1))
    assertEquals("0-2", c(0, 2))
    assertEquals("0-3", c(0, 3))
    assertEquals("1-2", c(1, 2))
    assertEquals("1-3", c(1, 3))
    assertEquals("2-3", c(2, 3))

    assertExceptionAny(c(0, 0))
    assertExceptionAny(c(1, 1))
    assertExceptionAny(c(2, 2))
    assertExceptionAny(c(3, 3))
    assertExceptionAny(c(2, 4))
    assertExceptionAny(c(2, 1))
  }

}
