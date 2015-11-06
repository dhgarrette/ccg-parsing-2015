package dhg.ccg.parse.dep

import org.junit.Test
import dhg.util._
import org.junit.Assert._
import dhg.ccg.cat._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.util.viz.TreeViz
import dhg.ccg.parse._

class DepGraphTests {

  val s = cat"S".asInstanceOf[NonPuncCat]
  val np = cat"NP".asInstanceOf[NonPuncCat]
  val n = cat"N".asInstanceOf[NonPuncCat]
  val pp = cat"PP".asInstanceOf[NonPuncCat]

  val S = cat"S".asInstanceOf[NonPuncCat]
  val NP = cat"NP".asInstanceOf[NonPuncCat]
  val N = cat"N".asInstanceOf[NonPuncCat]
  val PP = cat"PP".asInstanceOf[NonPuncCat]

  @Test
  def test_DepGraph_fromCcgTree() {

    //    /*
    //     *          S                     S                       S
    //     *        /   \                 /   \                   /   \
    //     *      NP     S\NP           NP     S\NP              NP    S\NP
    //     *     /  \                  /  \                     /  \
    //     *   NP   NP[conj]         NP   NP[conj]            NP   NP\NP  
    //     *       /   \                 /   \                    /     \
    //     *      ,     NP[conj]        ,   NP\NP     (NP\NP)/(NP\NP)    NP\NP
    //     *           /   \               /    \                       /    \
    //     *        conj    NP      (NP\NP)/NP   NP               (NP\NP)/NP  NP
    //     */
    //    val t12 =
    //      CcgBinode(np,
    //        CcgLeaf(np, "A"),
    //        CcgBinode(np \ np,
    //          CcgLeaf((np \ np) / (np \ np), ","),
    //          CcgBinode(np \ np,
    //            CcgLeaf((np \ np) / np, "and"),
    //            CcgLeaf(np, "B"))))
    //    TreeViz.drawTree(t12)
    //    val d12 = DepGraph.fromCcgTree(t12)
    //    TreeViz.drawTree(d12)
    //    assertEquals(
    //      DepTree("sees", 2, Vector(
    //        DepTree("A", 1, Vector()),
    //        DepTree("quickly", 4, Vector(
    //          DepTree("really", 3, Vector()))),
    //        DepTree("B", 5, Vector()))),
    //      d12)

    //    /*
    //     *      NP
    //     *     /  \
    //     *  NP/N    N
    //     *        /   \
    //     *     N/N     N
    //     *           /   \
    //     *        conj    N
    //     *              /   \
    //     *           N/N     N         
    //     */
    //    val t11 =
    //      CcgBinode(np,
    //        CcgLeaf(np / n, "the"),
    //        CcgBinode(n,
    //          CcgLeaf(n / n, "loud"),
    //          CcgBinode(n,
    //            CcgLeaf(n / n, "and"),
    //            CcgBinode(n,
    //              CcgLeaf(n / n, "aggressive"),
    //              CcgLeaf(n, "dog")))))
    //    TreeViz.drawTree(t11)
    //    val d11 = DepGraph.fromCcgTree(t11)
    //    TreeViz.drawTree(d11)
    //    assertEquals(
    //      DepTree("sees", 2, Vector(
    //        DepTree("A", 1, Vector()),
    //        DepTree("quickly", 4, Vector(
    //          DepTree("really", 3, Vector()))),
    //        DepTree("B", 5, Vector()))),
    //      d11)

    //    /*
    //     *             np
    //     *     +-------+-------+                          and
    //     *     np             np\np                      /   \
    //     *    cats           /     \          =>     cats    dogs
    //     *       x:(np\np)/np      np                         |
    //     *            and        /    \                      big
    //     *                  np/np       np                    |
    //     *            +-------+---+    dogs                 really
    //     *     (np/np)/(np/np)  np/np
    //     *          really       big
    //     */
    //    val t9 = {
    //      CcgBinode(np,
    //        CcgLeaf(np, "A"),
    //        CcgBinode(np \ np,
    //          CcgLeaf((np \ np) / np, "and"),
    //          CcgBinode(np,
    //            CcgBinode(np / np,
    //              CcgLeaf((np / np) / (np / np), "really"),
    //              CcgLeaf(np / np, "big")),
    //            CcgLeaf(NP, "dogs"))))
    //    }
    //    TreeViz.drawTree(t9)
    //    val d9 = DepGraph.fromCcgTree(t9)
    //    TreeViz.drawTree(d9)
    //    assertEquals(
    //      DepTree("and", 2, Vector(
    //        DepTree("A", 1, Vector()),
    //        DepTree("B", 5, Vector(
    //          DepTree("big", 4, Vector(
    //            DepTree("really", 3, Vector()))))))),
    //      d9)

    /*
     *          s
     *        /   \                   barks
     *       np    s\np                 |
     *      /  \   barks      =>       dog
     *   np/n    n                      |
     *     a    dog                     a
     */
    val t1 =
      CcgBinode(s,
        CcgBinode(np,
          CcgLeaf(np / n, "a", "FAKEPOS"),
          CcgLeaf(n, "dog", "FAKEPOS")),
        CcgLeaf(s \ np, "barks", "FAKEPOS"))
    val d1 = DepGraph.fromCcgTree(t1)
    println(d1)
    //VizGraph.drawGraph(d1)
    assertEquals(
      DepTree("barks", 3, s, Vector(
        DepTree("a", 1, np, Vector(
          DepTree("dog", 2, n, Vector()))))),
      d1)

    /*
     *          np
     *        /    \                    a
     *       np     np\np             /   \ 
     *      /  \    barks     =>    dog   barks
     *   np/n   n                      
     *     a   dog                     
     */
    val t2 =
      CcgBinode(np,
        CcgBinode(np,
          CcgLeaf(np / n, "a", "FAKEPOS"),
          CcgLeaf(n, "dog", "FAKEPOS")),
        CcgLeaf(np \ np, "barks", "FAKEPOS"))
    val d2 = DepGraph.fromCcgTree(t2)
    //TreeViz.drawTree(d2)
    assertEquals(
      DepTree("a", 1, np, Vector(
        DepTree("dog", 2, n, Vector()),
        DepTree("barks", 3, np \ np, Vector()))),
      d2)

    /*
     *           s
     *        /     \                               saw
     *      np       s\np                          /   \ 
     *     John     /    \                 =>   John    Mary
     *       (s\np)/np    np                             |
     *          saw     /    \                          with
     *               np/pp    pp                         |
     *               Mary   /    \                       a
     *                   pp/np    np                     |
     *                   with    /  \                telescope
     *                        np/n   n
     *                          a  telescope 
     */
    val t3 =
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
                CcgLeaf(n, "telescope", "FAKEPOS"))))))
    //TreeViz.drawTree(t3)
    val d3 = DepGraph.fromCcgTree(t3)
    //TreeViz.drawTree(d3)
    assertEquals(
      DepTree("saw", 2, s, Vector(
        DepTree("John", 1, np, Vector()),
        DepTree("Mary", 3, np, Vector(
          DepTree("with", 4, pp, Vector(
            DepTree("a", 5, np, Vector(
              DepTree("telescope", 6, n, Vector()))))))))),
      d3)

    /*
     *              s
     *       +------+------+                                    saw
     *      np            s\np                               /   |   \ 
     *     John     +------+-------+              =>      John  Mary  with
     *          (s\np)/pp          pp                                  |
     *          /      \         /    \                                a
     * ((s\np)/pp)/np   np    pp/np    np                              |
     *       saw       Mary   with    /  \                         telescope
     *                             np/n   n
     *                               a  telescope 
     */
    val t4 =
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
              CcgLeaf(n, "telescope", "FAKEPOS")))))
    //TreeViz.drawTree(t4)
    val d4 = DepGraph.fromCcgTree(t4)
    //TreeViz.drawTree(d4)
    assertEquals(
      DepTree("saw", 2, s, Vector(
        DepTree("John", 1, np, Vector()),
        DepTree("Mary", 3, np, Vector()),
        DepTree("with", 4, pp, Vector(
          DepTree("a", 5, np, Vector(
            DepTree("telescope", 6, n, Vector()))))))),
      d4)

    /*
     *             s
     *       +-----+--------+                                         walks
     *       np          (s\np)                                       /   \
     *      a man     +-----+----------+                 =>         a      a 
     *             (s\np)       ((s\np)\(s\np))                     |      |
     *             walks     +---------+---------+                 man    dog
     *              (((s\np)\(s\np)))/n)         n
     *                       a                  dog
     */
    val t5 =
      CcgBinode(cat"""s""",
        CcgBinode(cat"""np""",
          CcgLeaf(cat"""(np/n)""", "a", "FAKEPOS"),
          CcgLeaf(cat"""n""", "man", "FAKEPOS")),
        CcgBinode(cat"""(s\np)""",
          CcgLeaf(cat"""(s\np)""", "walks", "FAKEPOS"),
          CcgBinode(cat"""((s\np)\(s\np))""",
            CcgLeaf(cat"""(((s\np)\(s\np))/n)""", "a", "FAKEPOS"),
            CcgLeaf(cat"""n""", "dog", "FAKEPOS"))))
    val d5 = DepGraph.fromCcgTree(t5)
    //TreeViz.drawTree(d5)
    def rCheck(a: DepTree, b: Graph) {
      a match {
        case DepTree(aw, ai, ac, av) =>
          fail()
//          assertEquals(aw, b.word)
//          assertEquals(ai, b.index)
//          assertEquals(ac, b.cat)
//          assertEquals(av.size, b.children.size)
//          (av zip b.children).foreach { case (x, y) => rCheck(x, y) }
      }
    }

    rCheck( //assertEquals(
      DepTree("walks", 3, cat"""s""", Vector(
        DepTree("a", 1, cat"""np""", Vector(
          DepTree("man", 2, cat"""n""", Vector()))),
        DepTree("a", 4, cat"""((s\np)\(s\np))""", Vector(
          DepTree("dog", 5, cat"""n""", Vector()))))),
      d5)

    /* a tall man quickly gives the dog a bone
     * 
     *                  s
     *    +-------------+-------------+              
     *    np                         s\np
     *                     +----------+--------+
     *                  s\np/np                np
     *              +------+-----+
     *         s\np/np/np        np
     *               
     *               
     *                     gives
     *           +-----+-----+-----+-----+
     *           a  quickly       the    a
     *           |                 |     |
     *          man               dog   bone
     *           |
     *         tall    
     *                   
     */
    val t6 =
      CcgBinode(cat"""s""",
        CcgBinode(cat"""np""",
          CcgLeaf(cat"""(np/n)""", "a", "FAKEPOS"),
          CcgBinode(cat"""n""",
            CcgLeaf(cat"""(n/n)""", "tall", "FAKEPOS"),
            CcgLeaf(cat"""n""", "man", "FAKEPOS"))),
        CcgBinode(cat"""(s\np)""",
          CcgBinode(cat"""((s\np)/np)""",
            CcgBinode(cat"""(((s\np)/np)/np)""",
              CcgLeaf(cat"""((((s\np)/np)/np)/(((s\np)/np)/np))""", "quickly", "FAKEPOS"),
              CcgLeaf(cat"""(((s\np)/np)/np)""", "gives", "FAKEPOS")),
            CcgBinode(cat"""np""",
              CcgLeaf(cat"""(np/n)""", "the", "FAKEPOS"),
              CcgLeaf(cat"""n""", "dog", "FAKEPOS"))),
          CcgBinode(cat"""np""",
            CcgLeaf(cat"""(np/n)""", "a", "FAKEPOS"),
            CcgLeaf(cat"""n""", "bone", "FAKEPOS"))))
    //TreeViz.drawTree(t6)
    val d6 = DepGraph.fromCcgTree(t6)
    //TreeViz.drawTree(d6)
    assertEquals(
      DepTree("gives", 5, cat"""s""", Vector(
        DepTree("a", 1, cat"""np""", Vector(
          DepTree("man", 3, cat"""n""", Vector(
            DepTree("tall", 2, cat"""(n/n)""", Vector()))))),
        DepTree("quickly", 4, cat"""((((s\np)/np)/np)/(((s\np)/np)/np))""", Vector()),
        DepTree("the", 6, cat"""np""", Vector(
          DepTree("dog", 7, cat"""n""", Vector()))),
        DepTree("a", 8, cat"""np""", Vector(
          DepTree("bone", 9, cat"""n""", Vector()))))),
      d6)

    /*
     *              s
     *       +------+-----+                          sees
     *       np          s\np                      /  |  \
     *       A         /     \          =>        A   q  B
     *         x:(s\np)/np    np                      |
     *            /       \   B                       re
     *          x/x        x
     *       +---+-----+  sees
     *  (x/x)/(x/x)   x/x
     *      really   quickly
     */
    {
      val x = (s \ np) / np
      val t8 = {
        CcgBinode(s,
          CcgLeaf(np, "A", "FAKEPOS"),
          CcgBinode(s \ np,
            CcgBinode(x,
              CcgBinode(x / x,
                CcgLeaf((x / x) / (x / x), "really", "FAKEPOS"),
                CcgLeaf(x / x, "quickly", "FAKEPOS")),
              CcgLeaf(x, "sees", "FAKEPOS")),
            CcgLeaf(NP, "B", "FAKEPOS")))
      }
      //TreeViz.drawTree(t8)
      val d8 = DepGraph.fromCcgTree(t8)
      //TreeViz.drawTree(d8)
      assertEquals(
        DepTree("sees", 4, s, Vector(
          DepTree("A", 1, np, Vector()),
          DepTree("quickly", 3, x / x, Vector(
            DepTree("really", 2, (x / x) / (x / x), Vector()))),
          DepTree("B", 5, np, Vector()))),
        d8)
    }

    /*
     *              s
     *       +------+-----+                          sees
     *       np          s\np                      /  |  \
     *       A         /     \          =>        A   q  B
     *         x:(s\np)/np    np                      |
     *            /       \   B                       re
     *     (s\np)/np       x\x         
     *       sees       +---+-----+    
     *             (x/x)/(x/x)   x/x
     *                really   quickly
     */
    {
      val x = (s \ np) / np
      val t10 = {
        CcgBinode(s,
          CcgLeaf(np, "A", "FAKEPOS"),
          CcgBinode(s \ np,
            CcgBinode(x,
              CcgLeaf(x, "sees", "FAKEPOS"),
              CcgBinode(x \ x,
                CcgLeaf((x \ x) / (x \ x), "really", "FAKEPOS"),
                CcgLeaf(x \ x, "quickly", "FAKEPOS"))),
            CcgLeaf(np, "B", "FAKEPOS")))
      }
      //TreeViz.drawTree(t10)
      val d10 = DepGraph.fromCcgTree(t10)
      //TreeViz.drawTree(d10)
      assertEquals(
        DepTree("sees", 2, s, Vector(
          DepTree("A", 1, np, Vector()),
          DepTree("quickly", 4, x \ x, Vector(
            DepTree("really", 3, (x \ x) / (x \ x), Vector()))),
          DepTree("B", 5, np, Vector()))),
        d10)
    }

    //    /*
    //     *              s
    //     *       +------+-----+                         and
    //     *       np         np\np                      /   \
    //     *       A         /     \          =>        A     B
    //     *           (np\np)/np   np 
    //     *              and       B
    //     */
    //    val t7 = CcgBinode(NP, CcgLeaf(NP, "A"), CcgBinode((NP \ NP), CcgLeaf(((NP \ NP) / NP), "and"), CcgLeaf(NP, "B")))
    //    //TreeViz.drawTree(t7)
    //    val d7 = DepGraph.fromCcgTree(t7)
    //    TreeViz.drawTree(d7)
    //    assertEquals(
    //      DepTree("and", 2, Vector(
    //        DepTree("A", 1, Vector()),
    //        DepTree("B", 3, Vector()))),
    //      d7)
  }

  @Test
  def test_DepGraph_fromCcgTree_withUnary() {

    //    /*
    //     *              s
    //     *       +------+-----+                         and
    //     *       np         np\np                      /   \
    //     *       |           and            =>    cancer   asbestosis
    //     *       n         /     \
    //     *   cancer  (np\np)/np   np
    //     *              and       |
    //     *                        n
    //     *                    asbestosis  
    //     */
    //    val t3 = CcgBinode(NP, CcgUnode(NP, CcgLeaf(N, "cancer")), CcgBinode((NP \ NP), CcgLeaf(((NP \ NP) / NP), "and"), CcgUnode(NP, CcgLeaf(N, "asbestosis"))))
    //    //TreeViz.drawTree(t3)
    //    val d3 = DepGraph.fromCcgTree(t3)
    //    //TreeViz.drawTree(d3)
    //    ???

    /*
     *          s
     *        /   \                   barks
     *       np    s\np                 |
     *       |     bark       =>       dog
     *       n
     *      dogs
     */
    val t1 =
      CcgBinode(s,
        CcgUnode(np,
          CcgLeaf(n, "dogs", "FAKEPOS")),
        CcgLeaf(s \ np, "bark", "FAKEPOS"))
    //TreeViz.drawTree(t1)
    val d1 = DepGraph.fromCcgTree(t1)
    //TreeViz.drawTree(d1)
    assertEquals(
      DepTree("bark", 1, s, Vector(
        DepTree("dogs", 0, np, Vector()))),
      d1)

    /*
     *          s
     *        /   \                bark
     *       np    s\np             |
     *       |     bark     =>     dogs
     *       n                      |
     *      / \                    big  
     *    n/n  n
     *    big dogs
     */
    val t2 =
      CcgBinode(s,
        CcgUnode(np,
          CcgBinode(n,
            CcgLeaf(n / n, "big", "FAKEPOS"),
            CcgLeaf(n, "dogs", "FAKEPOS"))),
        CcgLeaf(s \ np, "bark", "FAKEPOS"))
    val d2 = DepGraph.fromCcgTree(t2)
    //TreeViz.drawTree(d2)
    assertEquals(
      DepTree("bark", 3, s, Vector(
        DepTree("dogs", 2, np, Vector(
          DepTree("big", 1, n / n, Vector()))))),
      d2)

    /*
     *          s
     *        /   \                   grow
     *       np    s\np                 |
     *       |     grow       =>      trees
     *       n                          |
     *     /   \                      pine
     *    n     n
     *   pine trees
     */
    val t3 =
      CcgBinode(s,
        CcgBinode(s,
          CcgUnode(np,
            CcgBinode(n, CcgLeaf(n, "pine", "FAKEPOS"), CcgLeaf(n, "trees", "FAKEPOS"))),
          CcgLeaf(s \ np, "grow", "FAKEPOS")),
        CcgLeaf(cat".", ".", "FAKEPOS"))
    //TreeViz.drawTree(t3)
    val d3 = DepGraph.fromCcgTree(t3)
    //TreeViz.drawTree(d3)
    assertEquals(
      DepTree("grow", 3, s, Vector(
        DepTree("trees", 2, np, Vector(
          DepTree("pine", 1, n, Vector()))),
        DepTree(".", 4, cat".", Vector()))),
      d3)

    /*
     *          s
     *        /   \                   grow
     *       np    s\np                 |
     *       |     grow       =>      trees
     *       n                          |
     *     /   \                      pine
     *    n     n
     *   pine trees
     */
    val t4 =
      CcgBinode(s,
        CcgBinode(s,
          CcgUnode(np,
            CcgBinode(n,
              CcgBinode(n, CcgLeaf(cat"LRB", "(", "FAKEPOS"),
                CcgBinode(n, CcgLeaf(n, "pine", "FAKEPOS"), CcgLeaf(n, "trees", "FAKEPOS"))),
              CcgLeaf(cat"RRB", ")", "FAKEPOS"))),
          CcgLeaf(s \ np, "grow", "FAKEPOS")),
        CcgLeaf(cat".", ".", "FAKEPOS"))
    //TreeViz.drawTree(t4)
    val d4 = DepGraph.fromCcgTree(t4)
    //TreeViz.drawTree(d4)
    assertEquals(
      DepTree("grow", 5, s, Vector(
        DepTree("trees", 3, np, Vector(
          DepTree("(", 1, cat"LRB", Vector()),
          DepTree("pine", 2, n, Vector()),
          DepTree(")", 4, cat"RRB", Vector()))),
        DepTree(".", 6, cat".", Vector()))),
      d4)

  }

}
