package dhg.ccg.parse.pcfg

import org.junit.Test
import org.junit.Assert._
import dhg.util.TestUtil._
import dhg.util._

class KBestPriorityQueueTests {

  @Test
  def test_FastKMaxPriorityQueue {
    assertExceptionMsg("assertion failed: k must be a positive number (was 0)")(FastKMaxPriorityQueue.empty[String](0))
    assertExceptionMsg("assertion failed: k must be a positive number (was -1)")(FastKMaxPriorityQueue.empty[String](-1))

    val q1 = FastKMaxPriorityQueue.empty[String](1)
    assertEquals(Vector("f" -> LogDouble(6)), q1.add("f", LogDouble(6)).toVector)
    assertEquals(Vector("f" -> LogDouble(6)), q1.add("d", LogDouble(4)).toVector)
    assertEquals(Vector("h" -> LogDouble(8)), q1.add("h", LogDouble(8)).toVector)

    val q4 = FastKMaxPriorityQueue.empty[String](4)
    assertEquals(Vector("f" -> LogDouble(6)), q4.add("f", LogDouble(6)).toVector)
    assertEquals(Vector("f" -> LogDouble(6), "d" -> LogDouble(4)), q4.add("d", LogDouble(4)).toVector)

    val q3 = FastKMaxPriorityQueue.empty[String](3)
    assertEquals(Vector[(String, LogDouble)](), q3.toVector)
    assertEquals(Vector("4" -> LogDouble(4)), q3.add("4", LogDouble(4)).toVector)
    assertEquals(Vector("6" -> LogDouble(6), "4" -> LogDouble(4)), q3.add("6", LogDouble(6)).toVector)
    assertEquals(Vector("8" -> LogDouble(8), "6" -> LogDouble(6), "4" -> LogDouble(4)), q3.add("8", LogDouble(8)).toVector)
    assertEquals(Vector("8" -> LogDouble(8), "7" -> LogDouble(7), "6" -> LogDouble(6)), q3.add("7", LogDouble(7)).toVector)
    assertEquals(Vector("8" -> LogDouble(8), "7" -> LogDouble(7), "6" -> LogDouble(6)), q3.add("2", LogDouble(2)).toVector)
    assertEquals(Vector("10" -> LogDouble(10), "8" -> LogDouble(8), "7" -> LogDouble(7)), q3.add("10", LogDouble(10)).toVector)
  }

}
