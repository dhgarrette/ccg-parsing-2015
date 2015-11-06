package dhg.ccg.parse

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.parse.pcfg._
import dhg.util._
import scala.Vector

class RerankParserTests {

  @Test
  def test_RerankParser {
    type Word = Symbol

    val mockGuideChart = CfgGuideChart(Vector.empty, Vector(Vector(Map(cat"some junk" -> Set.empty))))
    val T1 = new CcgLeaf(cat"A", "a", "FAKEPOS")
    val T2 = new CcgLeaf(cat"B", "b", "FAKEPOS")
    val T3 = new CcgLeaf(cat"C", "c", "FAKEPOS")
    val T4 = new CcgLeaf(cat"D", "d", "FAKEPOS")
    val T5 = new CcgLeaf(cat"E", "e", "FAKEPOS")

    def mockReranker = new TreeWeighter {
      def weight(t: CcgTree): LogDouble = t match {
        case T1 => LogDouble(1.1)
        case T2 => LogDouble(5.1)
        case T3 => LogDouble(3.1)
        case T4 => LogDouble(2.1)
        case T5 => LogDouble(4.1)
      }
    }

    def mockDelegateParser = new AbstractKBestGuideChartParser {
      def parseAndProbKBestFromGuideChart(guideChart: CfgGuideChart, k: Int): Vector[(CcgTree, LogDouble)] = {
        assertSame(mockGuideChart, guideChart)
        assertEquals(5, k)
        Vector(
          T1 -> LogDouble(5.0),
          T2 -> LogDouble(4.0),
          T3 -> LogDouble(3.0),
          T4 -> LogDouble(2.0),
          T5 -> LogDouble(1.0))
      }
    }

    val rerankParser = new RerankParser(mockReranker, mockDelegateParser, 5)
    val out = rerankParser.parseAndProbKBestFromGuideChart(mockGuideChart, 3)
    assertEquals(Vector(
      T2 -> LogDouble(5.1),
      T5 -> LogDouble(4.1),
      T3 -> LogDouble(3.1)), out)
  }

}
