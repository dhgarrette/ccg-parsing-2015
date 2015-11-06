//package dhg.ccg.parse
//
//import org.junit.Test
//import dhg.util._
//import org.junit.Assert._
//import dhg.ccg.cat._
//import dhg.ccg.tagdict.SimpleTagDictionary
//import scala.math.{ log, exp, pow }
//import dhg.ccg.parse.pcfg.OldToRemoveCfgGuideChartBuilder.GuideTable
//import dhg.ccg.parse.pcfg.OldToRemoveCfgGuideChartBuilder
//
//class ParserEvaluatorTests {
//
//  @Test
//  def evaluate() {
//    val s = AtomCat("S")
//    val np = AtomCat("NP")
//    val n = AtomCat("N")
//    val pp = AtomCat("PP")
//
//    val s1 = "John saw Mary with a telescope".splitWhitespace.toVector
//
//    val model = new Parser {
//      def parseAndProb(sentence: Vector[String], brackets: Vector[(Int, Int)]): Option[(CcgTree, LogDouble)] = {
//        if (sentence == s1) {
//          //	            s
//          //	            |
//          //	  +---------+------- s\np
//          //	  |                   |
//          //	  |         +----------------- np
//          //	  |         |                  |
//          //	  |         |            +-----+----- pp
//          //	  |         |            |            |
//          //	  |         |            |      +-----+--- np
//          //	  |         |            |      |          |
//          //	  |         |            |      |      +---+---+
//          //	  |         |            |      |      |       |
//          //	 np     (s\np)/np      np/pp   pp/np  np/n     n
//          //	John       saw          Mary   with    a    telescope
//          Some(
//            CcgNode(s,
//              CcgLeaf(np, "John"),
//              CcgNode(s \ np,
//                CcgLeaf((s \ np) / np, "saw"),
//                CcgNode(np,
//                  CcgLeaf(np / pp, "Mary"),
//                  CcgNode(pp,
//                    CcgLeaf(pp / np, "with"),
//                    CcgNode(np,
//                      CcgLeaf(np / n, "a"),
//                      CcgLeaf(n, "telescope")))))),
//            LogDouble(0.12))
//        }
//        else sys.error("no match")
//      }
//      def parseAndProbFromGuideChart(guideChart: GuideTable[String]): Option[(CcgTree, LogDouble)] = ???
//    }
//
//    val goldTrees = Vector(
//      //	              s
//      //	              |
//      //	  +-----------+------------s\np
//      //	  |                         |
//      //	  |            (s\np)/pp ---+-------- pp
//      //	  |                |                  |
//      //	  |                |            +-----+--- np
//      //	  |                |            |          |
//      //	  |         +------+-----+      |      +---+---+
//      //	  |         |            |      |      |       |
//      //	 np   ((s\np)/pp)/np     np   pp/np   np/n     n
//      //	John       saw          Mary   with    a    telescope
//      CcgNode(s,
//        CcgLeaf(np, "John"),
//        CcgNode(s \ np,
//          CcgNode((s \ np) / pp,
//            CcgLeaf(((s \ np) / pp) / np, "saw"),
//            CcgLeaf(np, "Mary")),
//          CcgNode(pp,
//            CcgLeaf(pp / np, "with"),
//            CcgNode(np,
//              CcgLeaf(np / n, "a"),
//              CcgLeaf(n, "telescope"))))))
//
//    val guideChartBuilder = new OldToRemoveCfgGuideChartBuilder(Vector(FA,BA))
//    val evaluator = new ParserEvaluator(guideChartBuilder)
//    evaluator.evaluate(model, goldTrees, SimpleTagDictionary.empty("<S>", cat"<S>", "<E>", cat"<E>"))
//
//  }
//
//}
