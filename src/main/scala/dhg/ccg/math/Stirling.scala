package dhg.ccg.math

import dhg.util._

object Stirling {

  /**
   * Computes the Unsigned Stirling Number of the First Kind
   *
   * http://en.wikipedia.org/wiki/Stirling_numbers_of_the_first_kind
   *
   * s(n,k) = (n-1)*s(n-1,k) + s(n-1,k-1)
   *
   * println(stirling1_recursive(9, 4)) // 20 operations
   */
  def stirling1(n: Int, k: Int) = {
    if (k == n) 1
    else if (k == 0) 0
    else {
      val `s(n-1)` = new Array[Int](k + 1)
      `s(n-1)`(0) = 0
      `s(n-1)`(1) = 1
      var i = 2; while (i <= k) { `s(n-1)`(i) = 1; i += 1 }
      val `s(n)` = `s(n-1)`

      var n1 = 2
      while (n1 <= n) {
        val mink = 1 max (k - (n - n1))
        val maxk = (n1 - 1) min k
        var k1 = maxk
        while (k1 >= mink) {
          `s(n)`(k1) = (n1 - 1) * `s(n-1)`(k1) + `s(n-1)`(k1 - 1)
          k1 -= 1
        }
        n1 += 1
      }

      `s(n)`(k)
    }
  }

  //  /**
  //   * println(stirling1_recursive(9, 4)) // 251 operations
  //   */
  //  def stirling1_recursive(n: Int, k: Int): Int = {
  //    if (k == n) 1
  //    else if (k == 0) 0
  //    else (n - 1) * stirling1_recursive(n - 1, k) + stirling1_recursive(n - 1, k - 1)
  //  }

  def main(args: Array[String]) {
    println("n\\k\t" + (0 to 9).mkString("\t"))
    for (n <- 0 to 9) {
      println(n + "\t" + (0 to n).map(k => stirling1(n, k)).mkString("\t"))
    }

    println(stirling1(9, 4))
  }

}
