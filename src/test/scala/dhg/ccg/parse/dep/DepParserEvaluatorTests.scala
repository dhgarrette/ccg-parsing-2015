package dhg.ccg.parse.dep

import org.junit.Test
import dhg.util._
import org.junit.Assert._
import dhg.ccg.cat._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._

class DepParserEvaluatorTests {

  @Test
  def evaluate1() {
    val s = cat"S".asInstanceOf[AtomCat]
    val np = cat"NP".asInstanceOf[AtomCat]
    val n = cat"N".asInstanceOf[AtomCat]
    val pp = cat"PP".asInstanceOf[AtomCat]

    //    val modelTrees = Vector(
    //      DepTree("walks", Vector(
    //        DepTree("man", Vector(
    //          DepTree("a", Vector.empty),
    //          DepTree("tall", Vector.empty))),
    //        DepTree("dog", Vector(
    //          DepTree("a", Vector.empty))))))
    //
    //    val goldTrees = Vector(
    //      DepTree("walks", Vector(
    //        DepTree("man", Vector(
    //          DepTree("a", Vector.empty))),
    //        DepTree("tall", Vector.empty),
    //        DepTree("dog", Vector(
    //          DepTree("a", Vector.empty))))))

    val GC1 = CfgGuideChart(Vector.empty, Vector(Vector(Map[Cat, Set[GuideChartEntry]](cat"GC1" -> Set()))))
    val model = new GuideChartParser {
      def parseAndProbFromGuideChart(guideChart: CfgGuideChart): Option[(CcgTree, LogDouble)] = {
        if (guideChart == GC1) {
          Some((
            CcgBinode(s,
              CcgBinode(np,
                CcgLeaf(np / n, "a", "FAKEPOS"),
                CcgLeaf(n, "dog", "FAKEPOS")),
              CcgLeaf(s \ np, "barks", "FAKEPOS")),
              LogDouble(0.12)))
        }
        else sys.error("no match")
      }
    }

    val goldTrees = Vector[(Option[CfgGuideChart], CcgTree)](
      (None,
        CcgBinode(np,
          CcgBinode(np,
            CcgLeaf(np / n, "a", "FAKEPOS"),
            CcgLeaf(n, "dog", "FAKEPOS")),
          CcgLeaf(np \ np, "barks", "FAKEPOS"))))

    val evaluator = new DepParserEvaluator(None)
    evaluator.evaluate(model, goldTrees, SimpleTagDictionary.empty("<S>", cat"<S>", "<E>", cat"<E>"))

  }

  @Test
  def evaluate2() {
    val s = cat"S".asInstanceOf[AtomCat]
    val np = cat"NP".asInstanceOf[AtomCat]
    val n = cat"N".asInstanceOf[AtomCat]
    val pp = cat"PP".asInstanceOf[AtomCat]

    //    val modelTrees = Vector(
    //      DepTree("walks", Vector(
    //        DepTree("man", Vector(
    //          DepTree("a", Vector.empty),
    //          DepTree("tall", Vector.empty))),
    //        DepTree("dog", Vector(
    //          DepTree("a", Vector.empty))))))
    //
    //    val goldTrees = Vector(
    //      DepTree("walks", Vector(
    //        DepTree("man", Vector(
    //          DepTree("a", Vector.empty))),
    //        DepTree("tall", Vector.empty),
    //        DepTree("dog", Vector(
    //          DepTree("a", Vector.empty))))))

    val GC1 = CfgGuideChart(Vector.empty, Vector(Vector(Map[Cat, Set[GuideChartEntry]](cat"GC1" -> Set()))))
    val model = new GuideChartParser {
      def parseAndProbFromGuideChart(guideChart: CfgGuideChart): Option[(CcgTree, LogDouble)] = {
        if (guideChart == GC1) {
          //	            s
          //	            |
          //	  +---------+------- s\np
          //	  |                   |
          //	  |         +----------------- np
          //	  |         |                  |
          //	  |         |            +-----+----- pp
          //	  |         |            |            |
          //	  |         |            |      +-----+--- np
          //	  |         |            |      |          |
          //	  |         |            |      |      +---+---+
          //	  |         |            |      |      |       |
          //	 np     (s\np)/np      np/pp   pp/np  np/n     n
          //	John       saw          Mary   with    a    telescope
          Some((
            CcgBinode(s,
              CcgLeaf(np, "John", "FAKEPOS"),
              CcgBinode(s \ np,
                CcgLeaf((s \ np) / np, "saw", "FAKEPOS"),
                CcgBinode(np,
                  CcgLeaf(np / pp, "Mary", "FAKEPOS"),
                  CcgBinode(pp,
                    CcgLeaf(pp / np, "with", "FAKEPOS"),
                    CcgBinode(np,
                      CcgLeaf(np / n, "a", "FAKEPOS"),
                      CcgLeaf(n, "telescope", "FAKEPOS")))))),
              LogDouble(0.12)))
        }
        else sys.error("no match")
      }
    }

    val goldTrees = Vector[(Option[CfgGuideChart], CcgTree)](
      (None,
        //	              s
        //	              |
        //	  +-----------+------------s\np
        //	  |                         |
        //	  |            (s\np)/pp ---+-------- pp
        //	  |                |                  |
        //	  |                |            +-----+--- np
        //	  |                |            |          |
        //	  |         +------+-----+      |      +---+---+
        //	  |         |            |      |      |       |
        //	 np   ((s\np)/pp)/np     np   pp/np   np/n     n
        //	John       saw          Mary   with    a    telescope
        CcgBinode(s,
          CcgLeaf(np, "John", "FAKEPOS"),
          CcgBinode(s \ np,
            CcgBinode((s \ np) / pp,
              CcgLeaf(((s \ np) / pp) / np, "saw", "FAKEPOS"),
              CcgLeaf(np, "Mary", "FAKEPOS")),
            CcgBinode(pp,
              CcgLeaf(pp / np, "with", "FAKEPOS"),
              CcgBinode(np,
                CcgLeaf(np / n, "a", "FAKEPOS"),
                CcgLeaf(n, "telescope", "FAKEPOS")))))))

    val evaluator = new DepParserEvaluator(None)
    evaluator.evaluate(model, goldTrees, SimpleTagDictionary.empty("<S>", cat"<S>", "<E>", cat"<E>"))

  }

  @Test
  def evaluate3_withUnary() {
    throw new NotImplementedError("Test not written")
  }

}
