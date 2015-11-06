package dhg.ccg.math

import org.junit.Test
import dhg.util.TestUtil._
import dhg.util._
import math.{ log, exp, abs, pow }
import org.junit.Assert._
import Double.NaN
import scala.util.Random
import dhg.util.FastMathUtil._
import dhg.ccg.math.Stirling._

class StirlingTests {

  @Test
  def test_stirling {
    val sb = new StringBuilder
    sb ++= "n\\k".padRight(8) + (0 to 9).map(_.toString.padRight(8)).mkString + "\n"
    for (n <- 0 to 9) {
      sb ++= n.toString.padRight(8)
      for (k <- 0 to n) {
        sb ++= stirling1(n, k).toString.padRight(8)
      }
      sb ++= "\n"
    }
    val result = sb.toString.trim.splitlines.map(_.trim).mkString("\n")

    val expected = """
		n\k     0       1       2       3       4       5       6       7       8       9
		0       1
		1       0       1
		2       0       1       1
		3       0       2       3       1
		4       0       6       11      6       1
		5       0       24      50      35      10      1
		6       0       120     274     225     85      15      1
		7       0       720     1764    1624    735     175     21      1
		8       0       5040    13068   13132   6769    1960    322     28      1
		9       0       40320   109584  118124  67284   22449   4536    546     36      1
		""".trim.splitlines.map(_.trim).mkString("\n")

    assertEquals(expected, result)
    
    println(stirling1(9, 4))
  }

}
