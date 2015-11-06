package dhg.ccg.parse.dep

import org.junit.Test
import dhg.util._
import org.junit.Assert._
import dhg.ccg.cat._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.parse._

class DepSpanParserEvaluatorTests {

  @Test
  def test_DepSpanParserEvaluator_evaluate {
    throw new NotImplementedError("Test not written")

    //    val s = AtomCat("S")
    //    val np = AtomCat("NP")
    //    val n = AtomCat("N")
    //    val pp = AtomCat("PP")
    //
    //    //    val modelTrees = Vector(
    //    //      DepTree("walks", Vector(
    //    //        DepTree("man", Vector(
    //    //          DepTree("a", Vector.empty),
    //    //          DepTree("tall", Vector.empty))),
    //    //        DepTree("dog", Vector(
    //    //          DepTree("a", Vector.empty))))))
    //    //
    //    //    val goldTrees = Vector(
    //    //      DepTree("walks", Vector(
    //    //        DepTree("man", Vector(
    //    //          DepTree("a", Vector.empty))),
    //    //        DepTree("tall", Vector.empty),
    //    //        DepTree("dog", Vector(
    //    //          DepTree("a", Vector.empty))))))
    //
    //    val s1 = "a dog barks".splitWhitespace
    //    val model = new Parser {
    //      def parseAndProb(sentence: Vector[String], brackets: Vector[(Int, Int)]): Option[(CcgTree, LogDouble)] = {
    //        if (sentence == s1) {
    //          Some(
    //            CcgNode(s,
    //              CcgNode(np,
    //                CcgLeaf(np / n, "a"),
    //                CcgLeaf(n, "dog")),
    //              CcgLeaf(s \ np, "barks")),
    //            LogDouble(0.12))
    //        }
    //        else sys.error("no match")
    //      }
    //      def parseAndProbFromGuideChart(guideChart: GuideTable[String]): Option[(CcgTree, LogDouble)] = ???
    //    }
    //
    //    val goldTrees = Vector(
    //      CcgNode(np,
    //        CcgNode(np,
    //          CcgLeaf(np / n, "a"),
    //          CcgLeaf(n, "dog")),
    //        CcgLeaf(np \ np, "barks")))
    //
    //    val evaluator = new DepSpanParserEvaluator[String](None)
    //    evaluator.evaluate(model, goldTrees, SimpleTagDictionary.empty("<S>", cat"<S>", "<E>", cat"<E>"))
    //
    //  }
    //
    //  @Test
    //  def evaluate2() {
    //    val s = AtomCat("S")
    //    val np = AtomCat("NP")
    //    val n = AtomCat("N")
    //    val pp = AtomCat("PP")
    //
    //    //    val modelTrees = Vector(
    //    //      DepTree("walks", Vector(
    //    //        DepTree("man", Vector(
    //    //          DepTree("a", Vector.empty),
    //    //          DepTree("tall", Vector.empty))),
    //    //        DepTree("dog", Vector(
    //    //          DepTree("a", Vector.empty))))))
    //    //
    //    //    val goldTrees = Vector(
    //    //      DepTree("walks", Vector(
    //    //        DepTree("man", Vector(
    //    //          DepTree("a", Vector.empty))),
    //    //        DepTree("tall", Vector.empty),
    //    //        DepTree("dog", Vector(
    //    //          DepTree("a", Vector.empty))))))
    //
    //    val s1 = "John saw Mary with a telescope".splitWhitespace
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
    //    val evaluator = new DepSpanParserEvaluator[String](None)
    //    evaluator.evaluate(model, goldTrees, SimpleTagDictionary.empty("<S>", cat"<S>", "<E>", cat"<E>"))

  }

}
