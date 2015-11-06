package dhg.ccg.tag.learn

import org.junit.Test
import dhg.util._
import org.junit.Assert._
import dhg.ccg.tag._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tagdict.SimpleTagDictionary

/*
 * TODO: Test with unrestricted tag dictionary
 */
class SupHmmDistributionTests {

  @Test
  def test_tr_unsmoothed() {
    val sentences = Vector(
      "the|D dog|N walks|V",
      "the|D man|N walks|V the|D dog|N",
      "the|D man|N runs|V")
      .map(_.lsplit(" ").map(_.split('|').toTuple2))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set("D"),
      "man" -> Set("N", "V"),
      "walks" -> Set("V")),
      "<S>", "<S>", "<E>", "<E>")

    /* C(t1,t2)
     *    S  D  N  V  E    total
     * S     3           |   3
     * D        4        |   4
     * N           3  1  |   4
     * V     1        2  |   3
     * E
     */

    val tr = new UnsmoothedTransitionDistributioner()(sentences, tagdict)
    assertEquals(0.0, tr("<S>", "<S>").toDouble, 1e-5)
    assertEquals(3 / 3.0, tr("D", "<S>").toDouble, 1e-5)
    assertEquals(0.0, tr("N", "<S>").toDouble, 1e-5)
    assertEquals(0.0, tr("V", "<S>").toDouble, 1e-5)
    assertEquals(0.0, tr("default", "<S>").toDouble, 1e-5)
    assertEquals(0.0, tr("<E>", "<S>").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "D").toDouble, 1e-5)
    assertEquals(0.0, tr("D", "D").toDouble, 1e-5)
    assertEquals(4 / 4.0, tr("N", "D").toDouble, 1e-5)
    assertEquals(0.0, tr("V", "D").toDouble, 1e-5)
    assertEquals(0.0, tr("default", "D").toDouble, 1e-5)
    assertEquals(0.0, tr("<E>", "D").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "N").toDouble, 1e-5)
    assertEquals(0.0, tr("D", "N").toDouble, 1e-5)
    assertEquals(0.0, tr("N", "N").toDouble, 1e-5)
    assertEquals(3 / 4.0, tr("V", "N").toDouble, 1e-5)
    assertEquals(0.0, tr("default", "N").toDouble, 1e-5)
    assertEquals(1 / 4.0, tr("<E>", "N").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "V").toDouble, 1e-5)
    assertEquals(1 / 3.0, tr("D", "V").toDouble, 1e-5)
    assertEquals(0.0, tr("N", "V").toDouble, 1e-5)
    assertEquals(0.0, tr("V", "V").toDouble, 1e-5)
    assertEquals(0.0, tr("default", "V").toDouble, 1e-5)
    assertEquals(2 / 3.0, tr("<E>", "V").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "default").toDouble, 1e-5)
    assertEquals(0.0, tr("D", "default").toDouble, 1e-5)
    assertEquals(0.0, tr("N", "default").toDouble, 1e-5)
    assertEquals(0.0, tr("V", "default").toDouble, 1e-5)
    assertEquals(0.0, tr("default", "default").toDouble, 1e-5)
    assertEquals(0.0, tr("default", "<E>").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("D", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("N", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("V", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("default", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("<E>", "<E>").toDouble, 1e-5)
  }

  @Test
  def test_em_unsmoothed() {
    val sentences = Vector(
      "a|D cat|N chases|V the|D walks|N",
      "the|D dog|N walks|V",
      "the|D man|N walks|V the|D dog|N",
      "the|D man|N runs|V",
      "the|N bird|V walks|D")
      .map(_.lsplit(" ").map(_.split('|').toTuple2))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set("D"),
      "a" -> Set("D"),
      "every" -> Set("D"),
      "some" -> Set("D"),
      "man" -> Set("N", "V"),
      "cat" -> Set("N"),
      "bird" -> Set("N"),
      "fox" -> Set("N"),
      "walks" -> Set("N", "V"),
      "flies" -> Set("N", "V")),
      "<S>", "<S>", "<E>", "<E>")

    /* Words not in TD:
     *   chases
     *   dog
     *   runs
     */

    /* C(t,w)
     * 			D  N  V
     * a		1
     * cat		   1
     * chases	      1
     * the		5  
     * dog		   2  
     * walks	   1  2
     * man		   2
     * runs		      1
     *          -  -  -
     * total	6  6  4
     */

    val em = new UnsmoothedEmissionDistributioner()(sentences, tagdict)
    assertEquals(1.0, em("<S>", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("the", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("a", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("man", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("walks", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("dog", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("runs", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("chases", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("default", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "<S>").toDouble, 1e-5)

    assertEquals(0.0, em("<S>", "D").toDouble, 1e-5)
    assertEquals(5 / 6.0, em("the", "D").toDouble, 1e-5)
    assertEquals(1 / 6.0, em("a", "D").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "D").toDouble, 1e-5)
    assertEquals(0.0, em("man", "D").toDouble, 1e-5)
    assertEquals(0.0, em("walks", "D").toDouble, 1e-5)
    assertEquals(0.0, em("dog", "D").toDouble, 1e-5)
    assertEquals(0.0, em("runs", "D").toDouble, 1e-5)
    assertEquals(0.0, em("chases", "D").toDouble, 1e-5)
    assertEquals(0.0, em("default", "D").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "D").toDouble, 1e-5)

    assertEquals(0.0, em("<S>", "N").toDouble, 1e-5)
    assertEquals(0.0, em("the", "N").toDouble, 1e-5)
    assertEquals(0.0, em("a", "N").toDouble, 1e-5)
    assertEquals(1 / 6.0, em("cat", "N").toDouble, 1e-5)
    assertEquals(2 / 6.0, em("man", "N").toDouble, 1e-5)
    assertEquals(1 / 6.0, em("walks", "N").toDouble, 1e-5)
    assertEquals(2 / 6.0, em("dog", "N").toDouble, 1e-5)
    assertEquals(0.0, em("runs", "N").toDouble, 1e-5)
    assertEquals(0.0, em("chases", "N").toDouble, 1e-5)
    assertEquals(0.0, em("default", "N").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "N").toDouble, 1e-5)

    assertEquals(0.0, em("<S>", "V").toDouble, 1e-5)
    assertEquals(0.0, em("the", "V").toDouble, 1e-5)
    assertEquals(0.0, em("a", "V").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "V").toDouble, 1e-5)
    assertEquals(0.0, em("man", "V").toDouble, 1e-5)
    assertEquals(2 / 4.0, em("walks", "V").toDouble, 1e-5)
    assertEquals(0.0, em("dog", "V").toDouble, 1e-5)
    assertEquals(1 / 4.0, em("runs", "V").toDouble, 1e-5)
    assertEquals(1 / 4.0, em("chases", "V").toDouble, 1e-5)
    assertEquals(0.0, em("default", "V").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "V").toDouble, 1e-5)

    assertEquals(0.0, em("<S>", "default").toDouble, 1e-5)
    assertEquals(0.0, em("the", "default").toDouble, 1e-5)
    assertEquals(0.0, em("a", "default").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "default").toDouble, 1e-5)
    assertEquals(0.0, em("man", "default").toDouble, 1e-5)
    assertEquals(0.0, em("walks", "default").toDouble, 1e-5)
    assertEquals(0.0, em("dog", "default").toDouble, 1e-5)
    assertEquals(0.0, em("runs", "default").toDouble, 1e-5)
    assertEquals(0.0, em("chases", "default").toDouble, 1e-5)
    assertEquals(0.0, em("default", "default").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "default").toDouble, 1e-5)

    assertEquals(0.0, em("<S>", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("the", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("a", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("man", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("walks", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("dog", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("runs", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("chases", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("default", "<E>").toDouble, 1e-5)
    assertEquals(1.0, em("<E>", "<E>").toDouble, 1e-5)
  }

  @Test
  def test_tr_addLambda() {
    val sentences = Vector(
      "the|D dog|N walks|V",
      "the|D man|N walks|V the|D dog|N",
      "the|D man|N runs|V")
      .map(_.lsplit(" ").map(_.split('|').toTuple2))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set("D"),
      "man" -> Set("N", "V"),
      "walks" -> Set("V")),
      "<S>", "<S>", "<E>", "<E>")

    /* C(t1,t2)
     *    S  D  N  V  E    total
     * S     3           |   3
     * D        4        |   4
     * N           3  1  |   4
     * V     1        2  |   3
     * E
     */

    val tr = new AddLambdaTransitionDistributioner(0.2)(sentences, tagdict)
    assertEquals(0.0, tr("<S>", "<S>").toDouble, 1e-5)
    assertEquals((3 + 0.2) / (3 + 3 * 0.2), tr("D", "<S>").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (3 + 3 * 0.2), tr("N", "<S>").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (3 + 3 * 0.2), tr("V", "<S>").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (3 + 3 * 0.2), tr("default", "<S>").toDouble, 1e-5)
    assertEquals(0.0, tr("<E>", "<S>").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "D").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tr("D", "D").toDouble, 1e-5)
    assertEquals((4 + 0.2) / (4 + 4 * 0.2), tr("N", "D").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tr("V", "D").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tr("default", "D").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tr("<E>", "D").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "N").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tr("D", "N").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tr("N", "N").toDouble, 1e-5)
    assertEquals((3 + 0.2) / (4 + 4 * 0.2), tr("V", "N").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (4 + 4 * 0.2), tr("default", "N").toDouble, 1e-5)
    assertEquals((1 + 0.2) / (4 + 4 * 0.2), tr("<E>", "N").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "V").toDouble, 1e-5)
    assertEquals((1 + 0.2) / (3 + 4 * 0.2), tr("D", "V").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (3 + 4 * 0.2), tr("N", "V").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (3 + 4 * 0.2), tr("V", "V").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (3 + 4 * 0.2), tr("default", "V").toDouble, 1e-5)
    assertEquals((2 + 0.2) / (3 + 4 * 0.2), tr("<E>", "V").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "default").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (0 + 4 * 0.2), tr("D", "default").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (0 + 4 * 0.2), tr("N", "default").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (0 + 4 * 0.2), tr("V", "default").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (0 + 4 * 0.2), tr("default", "default").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (0 + 4 * 0.2), tr("<E>", "default").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("D", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("N", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("V", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("default", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("<E>", "<E>").toDouble, 1e-5)
  }

  @Test
  def test_tr_oneCount() {
    val sentences = Vector(
      "the|D dog|N walks|V",
      "dogs|N walk|V",
      "the|D sheep|N dogs|N walk|V",
      "the|D man|N walks|V the|D dog|N",
      "the|D man|N runs|V")
      .map(_.lsplit(" ").map(_.split('|').toTuple2))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set("D"),
      "man" -> Set("N", "V"),
      "walks" -> Set("V")),
      "<S>", "<S>", "<E>", "<E>")

    /* C(t1,t2)
     *    S  D  N  V  E    total    a          p
     * S     4  1        |   5   |  1  |  
     * D        5        |   5   |     |  (5+1)/(22+4)
     * N        1  5  1  |   7   |  2  |  (7+1)/(22+4)
     * V     1        4  |   5   |  1  |  (5+1)/(22+4)
     * E                     5         |  (5+1)/(22+4)
     */

    /* P(t2 | t1)
     *       S     D                                     N                                     V                                     E    total
     * S           4 + (1+0.2) * ((5+0.1)/(17+3*0.1))    1 + (1+0.2) * ((7+0.1)/(17+3*0.1))    0 + (1+0.2) * ((5+0.1)/(17+3*0.1))    0    5 + (1+0.2)
     * D          
     * N     
     * V     
     * E     
     * d
     *      
     */

    val tr = new OneCountTransitionDistributioner(0.2, 0.1)(sentences, tagdict)
    assertEquals(0.0, tr("<S>", "<S>").toDouble, 1e-5)
    assertEquals((4 + (1 + 0.2) * ((5 + 0.1) / (17 + 3 * 0.1))) / (5 + (1 + 0.2)), tr("D", "<S>").toDouble, 1e-5) // 0.70
    assertEquals((1 + (1 + 0.2) * ((7 + 0.1) / (17 + 3 * 0.1))) / (5 + (1 + 0.2)), tr("N", "<S>").toDouble, 1e-5) // 0.24
    assertEquals((0 + (1 + 0.2) * ((5 + 0.1) / (17 + 3 * 0.1))) / (5 + (1 + 0.2)), tr("V", "<S>").toDouble, 1e-5) // 0.06
    assertEquals((0 + (1 + 0.2) * ((0 + 0.1) / (17 + 3 * 0.1))) / (5 + (1 + 0.2)), tr("default", "<S>").toDouble, 1e-5) // 0.001
    assertEquals(0.0, tr("<E>", "<S>").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "D").toDouble, 1e-5)
    assertEquals((0 + (0 + 0.2) * ((5 + 0.1) / (22 + 4 * 0.1))) / (5 + (0 + 0.2)), tr("D", "D").toDouble, 1e-5) // 0.0087
    assertEquals((5 + (0 + 0.2) * ((7 + 0.1) / (22 + 4 * 0.1))) / (5 + (0 + 0.2)), tr("N", "D").toDouble, 1e-5) // 0.9737
    assertEquals((0 + (0 + 0.2) * ((5 + 0.1) / (22 + 4 * 0.1))) / (5 + (0 + 0.2)), tr("V", "D").toDouble, 1e-5) // 0.0087
    assertEquals((0 + (0 + 0.2) * ((0 + 0.1) / (22 + 4 * 0.1))) / (5 + (0 + 0.2)), tr("default", "D").toDouble, 1e-5) // 0.00001
    assertEquals((0 + (0 + 0.2) * ((5 + 0.1) / (22 + 4 * 0.1))) / (5 + (0 + 0.2)), tr("<E>", "D").toDouble, 1e-5) // 0.0087

    assertEquals(0.0, tr("<S>", "N").toDouble, 1e-5)
    assertEquals((0 + (2 + 0.2) * ((5 + 0.1) / (22 + 4 * 0.1))) / (7 + (2 + 0.2)), tr("D", "N").toDouble, 1e-5) // 0.05
    assertEquals((1 + (2 + 0.2) * ((7 + 0.1) / (22 + 4 * 0.1))) / (7 + (2 + 0.2)), tr("N", "N").toDouble, 1e-5) // 0.18
    assertEquals((5 + (2 + 0.2) * ((5 + 0.1) / (22 + 4 * 0.1))) / (7 + (2 + 0.2)), tr("V", "N").toDouble, 1e-5) // 0.60
    assertEquals((0 + (2 + 0.2) * ((0 + 0.1) / (22 + 4 * 0.1))) / (7 + (2 + 0.2)), tr("default", "N").toDouble, 1e-5) // 0.001
    assertEquals((1 + (2 + 0.2) * ((5 + 0.1) / (22 + 4 * 0.1))) / (7 + (2 + 0.2)), tr("<E>", "N").toDouble, 1e-5) // 0.16

    assertEquals(0.0, tr("<S>", "V").toDouble, 1e-5)
    assertEquals((1 + (1 + 0.2) * ((5 + 0.1) / (22 + 4 * 0.1))) / (5 + (1 + 0.2)), tr("D", "V").toDouble, 1e-5) // 0.21
    assertEquals((0 + (1 + 0.2) * ((7 + 0.1) / (22 + 4 * 0.1))) / (5 + (1 + 0.2)), tr("N", "V").toDouble, 1e-5) // 0.06
    assertEquals((0 + (1 + 0.2) * ((5 + 0.1) / (22 + 4 * 0.1))) / (5 + (1 + 0.2)), tr("V", "V").toDouble, 1e-5) // 0.04
    assertEquals((0 + (1 + 0.2) * ((0 + 0.1) / (22 + 4 * 0.1))) / (5 + (1 + 0.2)), tr("default", "V").toDouble, 1e-5) // 0.00008
    assertEquals((4 + (1 + 0.2) * ((5 + 0.1) / (22 + 4 * 0.1))) / (5 + (1 + 0.2)), tr("<E>", "V").toDouble, 1e-5) // 0.69

    assertEquals(0.0, tr("<S>", "default").toDouble, 1e-5)
    assertEquals((0 + (0 + 0.2) * ((5 + 0.1) / (22 + 4 * 0.1))) / (0 + (0 + 0.2)), tr("D", "default").toDouble, 1e-5) // 0.23
    assertEquals((0 + (0 + 0.2) * ((7 + 0.1) / (22 + 4 * 0.1))) / (0 + (0 + 0.2)), tr("N", "default").toDouble, 1e-5) // 0.32
    assertEquals((0 + (0 + 0.2) * ((5 + 0.1) / (22 + 4 * 0.1))) / (0 + (0 + 0.2)), tr("V", "default").toDouble, 1e-5) // 0.23
    assertEquals((0 + (0 + 0.2) * ((0 + 0.1) / (22 + 4 * 0.1))) / (0 + (0 + 0.2)), tr("default", "default").toDouble, 1e-5) // 0.004
    assertEquals((0 + (0 + 0.2) * ((5 + 0.1) / (22 + 4 * 0.1))) / (0 + (0 + 0.2)), tr("<E>", "default").toDouble, 1e-5) // 0.23

    assertEquals(0.0, tr("<S>", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("D", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("N", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("V", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("default", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("<E>", "<E>").toDouble, 1e-5)
  }

  @Test
  def test_em_addLambda() {
    val sentences = Vector(
      "a|D cat|N chases|V the|D walks|N",
      "the|D dog|N walks|V",
      "the|D man|N walks|V the|D dog|N",
      "the|D man|N runs|V",
      "the|N bird|V walks|D")
      .map(_.lsplit(" ").map(_.split('|').toTuple2))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set("D"),
      "a" -> Set("D"),
      "big" -> Set("A"),
      "good" -> Set("A", "N"),
      "every" -> Set("D"),
      "some" -> Set("D"),
      "man" -> Set("N", "V"),
      "cat" -> Set("N"),
      "bird" -> Set("N"),
      "fox" -> Set("N"),
      "walks" -> Set("N", "V"),
      "flies" -> Set("N", "V")),
      "<S>", "<S>", "<E>", "<E>")

    /* Words not in TD:
     *   chases
     *   dog
     *   runs
     */

    /* C(t,w)
     * 			D  N  V
     * a		1
     * cat		   1
     * chases	      1
     * the		5  
     * dog		   2  
     * walks	   1  2
     * man		   2
     * runs		      1
     *          -  -  -
     * total	6  6  4
     */

    val em = new AddLambdaEmissionDistributioner(0.2)(sentences, tagdict)
    assertEquals(1.0, em("<S>", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("a", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("bird", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("chases", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("dog", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("every", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("flies", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("fox", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("man", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("runs", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("some", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("the", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("walks", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("big", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("good", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("default", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "<S>").toDouble, 1e-5)

    assertEquals(0.0, em("<S>", "D").toDouble, 1e-5)
    assertEquals((1 + 0.2) / (6 + 7 * 0.2), em("a", "D").toDouble, 1e-5)
    assertEquals(0.0, em("bird", "D").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "D").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (6 + 7 * 0.2), em("chases", "D").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (6 + 7 * 0.2), em("dog", "D").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (6 + 7 * 0.2), em("every", "D").toDouble, 1e-5)
    assertEquals(0.0, em("flies", "D").toDouble, 1e-5)
    assertEquals(0.0, em("fox", "D").toDouble, 1e-5)
    assertEquals(0.0, em("man", "D").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (6 + 7 * 0.2), em("runs", "D").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (6 + 7 * 0.2), em("some", "D").toDouble, 1e-5)
    assertEquals((5 + 0.2) / (6 + 7 * 0.2), em("the", "D").toDouble, 1e-5)
    assertEquals(0.0, em("walks", "D").toDouble, 1e-5)
    assertEquals(0.0, em("big", "D").toDouble, 1e-5)
    assertEquals(0.0, em("good", "D").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (6 + 7 * 0.2), em("default", "D").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "D").toDouble, 1e-5)

    assertEquals(0.0, em("<S>", "N").toDouble, 1e-5)
    assertEquals(0.0, em("a", "N").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (6 + 10 * 0.2), em("bird", "N").toDouble, 1e-5)
    assertEquals((1 + 0.2) / (6 + 10 * 0.2), em("cat", "N").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (6 + 10 * 0.2), em("chases", "N").toDouble, 1e-5)
    assertEquals((2 + 0.2) / (6 + 10 * 0.2), em("dog", "N").toDouble, 1e-5)
    assertEquals(0.0, em("every", "N").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (6 + 10 * 0.2), em("flies", "N").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (6 + 10 * 0.2), em("fox", "N").toDouble, 1e-5)
    assertEquals((2 + 0.2) / (6 + 10 * 0.2), em("man", "N").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (6 + 10 * 0.2), em("runs", "N").toDouble, 1e-5)
    assertEquals(0.0, em("some", "N").toDouble, 1e-5)
    assertEquals(0.0, em("the", "N").toDouble, 1e-5)
    assertEquals((1 + 0.2) / (6 + 10 * 0.2), em("walks", "N").toDouble, 1e-5)
    assertEquals(0.0, em("big", "N").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (6 + 10 * 0.2), em("good", "N").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (6 + 10 * 0.2), em("default", "N").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "N").toDouble, 1e-5)

    assertEquals(0.0, em("<S>", "V").toDouble, 1e-5)
    assertEquals(0.0, em("a", "V").toDouble, 1e-5)
    assertEquals(0.0, em("bird", "V").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "V").toDouble, 1e-5)
    assertEquals((1 + 0.2) / (4 + 6 * 0.2), em("chases", "V").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (4 + 6 * 0.2), em("dog", "V").toDouble, 1e-5)
    assertEquals(0.0, em("every", "V").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (4 + 6 * 0.2), em("flies", "V").toDouble, 1e-5)
    assertEquals(0.0, em("fox", "V").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (4 + 6 * 0.2), em("man", "V").toDouble, 1e-5)
    assertEquals((1 + 0.2) / (4 + 6 * 0.2), em("runs", "V").toDouble, 1e-5)
    assertEquals(0.0, em("some", "V").toDouble, 1e-5)
    assertEquals(0.0, em("the", "V").toDouble, 1e-5)
    assertEquals((2 + 0.2) / (4 + 6 * 0.2), em("walks", "V").toDouble, 1e-5)
    assertEquals(0.0, em("big", "V").toDouble, 1e-5)
    assertEquals(0.0, em("good", "V").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (4 + 6 * 0.2), em("default", "V").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "V").toDouble, 1e-5)

    assertEquals(0.0, em("<S>", "A").toDouble, 1e-5)
    assertEquals(0.0, em("a", "A").toDouble, 1e-5)
    assertEquals(0.0, em("bird", "A").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "A").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (0 + 5 * 0.2), em("chases", "A").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (0 + 5 * 0.2), em("dog", "A").toDouble, 1e-5)
    assertEquals(0.0, em("every", "A").toDouble, 1e-5)
    assertEquals(0.0, em("flies", "A").toDouble, 1e-5)
    assertEquals(0.0, em("fox", "A").toDouble, 1e-5)
    assertEquals(0.0, em("man", "A").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (0 + 5 * 0.2), em("runs", "A").toDouble, 1e-5)
    assertEquals(0.0, em("some", "A").toDouble, 1e-5)
    assertEquals(0.0, em("the", "A").toDouble, 1e-5)
    assertEquals(0.0, em("walks", "A").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (0 + 5 * 0.2), em("big", "A").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (0 + 5 * 0.2), em("good", "A").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (0 + 5 * 0.2), em("default", "A").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "A").toDouble, 1e-5)

    assertEquals(0.0, em("<S>", "default").toDouble, 1e-5)
    assertEquals(0.0, em("a", "default").toDouble, 1e-5)
    assertEquals(0.0, em("bird", "default").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "default").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (0 + 3 * 0.2), em("chases", "default").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (0 + 3 * 0.2), em("dog", "default").toDouble, 1e-5)
    assertEquals(0.0, em("every", "default").toDouble, 1e-5)
    assertEquals(0.0, em("flies", "default").toDouble, 1e-5)
    assertEquals(0.0, em("fox", "default").toDouble, 1e-5)
    assertEquals(0.0, em("man", "default").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (0 + 3 * 0.2), em("runs", "default").toDouble, 1e-5)
    assertEquals(0.0, em("some", "default").toDouble, 1e-5)
    assertEquals(0.0, em("the", "default").toDouble, 1e-5)
    assertEquals(0.0, em("walks", "default").toDouble, 1e-5)
    assertEquals(0.0, em("big", "default").toDouble, 1e-5)
    assertEquals(0.0, em("good", "default").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (0 + 3 * 0.2), em("default", "default").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "default").toDouble, 1e-5)

    assertEquals(0.0, em("<S>", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("a", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("bird", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("chases", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("dog", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("every", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("flies", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("fox", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("man", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("runs", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("some", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("the", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("walks", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("big", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("good", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("default", "<E>").toDouble, 1e-5)
    assertEquals(1.0, em("<E>", "<E>").toDouble, 1e-5)
  }

  @Test
  def test_em_onecount() {
    val sentences = Vector(
      "a|D cat|N chases|V the|D walks|N",
      "the|D dog|N walks|V",
      "the|D man|N walks|V the|D dog|N",
      "the|D man|N runs|V",
      "the|N bird|V walks|D")
      .map(_.lsplit(" ").map(_.split('|').toTuple2))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set("D"),
      "a" -> Set("D"),
      "every" -> Set("D"),
      "some" -> Set("D"),
      "man" -> Set("N", "V"),
      "cat" -> Set("N"),
      "bird" -> Set("N"),
      "fox" -> Set("N"),
      "walks" -> Set("N", "V"),
      "flies" -> Set("N", "V")),
      "<S>", "<S>", "<E>", "<E>")

    /* Words not in TD:
     *   chases
     *   dog
     *   runs
     */

    /* C(t,w)
     * 			D  N  V     C
     * a		1        |  1
	 * bird              |  
     * cat		   1     |  1
     * chases	      1  |  1
     * dog		   2     |  2
	 * every             |
	 * flies             |
	 * fox               |
     * man		   2     |  2
     * runs		      1  |  1
	 * some              |
     * the		5        |  5
     * walks	   1  2  |  3
     *          -  -  -  
     * total	6  6  4     16 + (13 * 0.1)
     * 
     * b        1  2  2
     */

    /**
     *            C(t,w) + b(t) * p(w)
     * p(w | t) = --------------------
     *                C(t) + b(t)
     *     where b(t) = |w : C(t,w) = 1| + \epsilon
     */

    /*
     * 
     */

    val em = new OneCountEmissionDistributioner(0.2, 0.1)(sentences, tagdict)
    assertEquals(1.0, em("<S>", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("a", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("bird", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("chases", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("dog", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("every", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("flies", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("fox", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("man", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("runs", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("some", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("the", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("walks", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("default", "<S>").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "<S>").toDouble, 1e-5)

    val zD = (6 + (1 + 0.2) * ((10 + 7 * 0.1) / (16 + (13 * 0.1))))
    assertEquals(0.0, em("<S>", "D").toDouble, 1e-5)
    assertEquals((1 + (1 + 0.2) * ((1 + 0.1) / (16 + (13 * 0.1)))) / zD, em("a", "D").toDouble, 1e-5)
    assertEquals(0.0, em("bird", "D").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "D").toDouble, 1e-5)
    assertEquals((0 + (1 + 0.2) * ((1 + 0.1) / (16 + (13 * 0.1)))) / zD, em("chases", "D").toDouble, 1e-5)
    assertEquals((0 + (1 + 0.2) * ((2 + 0.1) / (16 + (13 * 0.1)))) / zD, em("dog", "D").toDouble, 1e-5)
    assertEquals((0 + (1 + 0.2) * ((0 + 0.1) / (16 + (13 * 0.1)))) / zD, em("every", "D").toDouble, 1e-5)
    assertEquals(0.0, em("flies", "D").toDouble, 1e-5)
    assertEquals(0.0, em("fox", "D").toDouble, 1e-5)
    assertEquals(0.0, em("man", "D").toDouble, 1e-5)
    assertEquals((0 + (1 + 0.2) * ((1 + 0.1) / (16 + (13 * 0.1)))) / zD, em("runs", "D").toDouble, 1e-5)
    assertEquals((0 + (1 + 0.2) * ((0 + 0.1) / (16 + (13 * 0.1)))) / zD, em("some", "D").toDouble, 1e-5)
    assertEquals((5 + (1 + 0.2) * ((5 + 0.1) / (16 + (13 * 0.1)))) / zD, em("the", "D").toDouble, 1e-5)
    assertEquals(0.0, em("walks", "D").toDouble, 1e-5)
    assertEquals((0 + (1 + 0.2) * ((0 + 0.1) / (16 + (13 * 0.1)))) / zD, em("default", "D").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "D").toDouble, 1e-5)

    val zN = (6 + (2 + 0.2) * ((10 + 9 * 0.1) / (16 + (13 * 0.1))))
    assertEquals(0.0, em("<S>", "N").toDouble, 1e-5)
    assertEquals(0.0, em("a", "N").toDouble, 1e-5)
    assertEquals((0 + (2 + 0.2) * ((0 + 0.1) / (16 + (13 * 0.1)))) / zN, em("bird", "N").toDouble, 1e-5)
    assertEquals((1 + (2 + 0.2) * ((1 + 0.1) / (16 + (13 * 0.1)))) / zN, em("cat", "N").toDouble, 1e-5)
    assertEquals((0 + (2 + 0.2) * ((1 + 0.1) / (16 + (13 * 0.1)))) / zN, em("chases", "N").toDouble, 1e-5)
    assertEquals((2 + (2 + 0.2) * ((2 + 0.1) / (16 + (13 * 0.1)))) / zN, em("dog", "N").toDouble, 1e-5)
    assertEquals(0.0, em("every", "N").toDouble, 1e-5)
    assertEquals((0 + (2 + 0.2) * ((0 + 0.1) / (16 + (13 * 0.1)))) / zN, em("flies", "N").toDouble, 1e-5)
    assertEquals((0 + (2 + 0.2) * ((0 + 0.1) / (16 + (13 * 0.1)))) / zN, em("fox", "N").toDouble, 1e-5)
    assertEquals((2 + (2 + 0.2) * ((2 + 0.1) / (16 + (13 * 0.1)))) / zN, em("man", "N").toDouble, 1e-5)
    assertEquals((0 + (2 + 0.2) * ((1 + 0.1) / (16 + (13 * 0.1)))) / zN, em("runs", "N").toDouble, 1e-5)
    assertEquals(0.0, em("some", "N").toDouble, 1e-5)
    assertEquals(0.0, em("the", "N").toDouble, 1e-5)
    assertEquals((1 + (2 + 0.2) * ((3 + 0.1) / (16 + (13 * 0.1)))) / zN, em("walks", "N").toDouble, 1e-5)
    assertEquals((0 + (2 + 0.2) * ((0 + 0.1) / (16 + (13 * 0.1)))) / zN, em("default", "N").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "N").toDouble, 1e-5)

    val zV = (4 + (2 + 0.2) * ((9 + 6 * 0.1) / (16 + (13 * 0.1))))
    assertEquals(0.0, em("<S>", "V").toDouble, 1e-5)
    assertEquals(0.0, em("a", "V").toDouble, 1e-5)
    assertEquals(0.0, em("bird", "V").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "V").toDouble, 1e-5)
    assertEquals((1 + (2 + 0.2) * ((1 + 0.1) / (16 + (13 * 0.1)))) / zV, em("chases", "V").toDouble, 1e-5)
    assertEquals((0 + (2 + 0.2) * ((2 + 0.1) / (16 + (13 * 0.1)))) / zV, em("dog", "V").toDouble, 1e-5)
    assertEquals(0.0, em("every", "V").toDouble, 1e-5)
    assertEquals((0 + (2 + 0.2) * ((0 + 0.1) / (16 + (13 * 0.1)))) / zV, em("flies", "V").toDouble, 1e-5)
    assertEquals(0.0, em("fox", "V").toDouble, 1e-5)
    assertEquals((0 + (2 + 0.2) * ((2 + 0.1) / (16 + (13 * 0.1)))) / zV, em("man", "V").toDouble, 1e-5)
    assertEquals((1 + (2 + 0.2) * ((1 + 0.1) / (16 + (13 * 0.1)))) / zV, em("runs", "V").toDouble, 1e-5)
    assertEquals(0.0, em("some", "V").toDouble, 1e-5)
    assertEquals(0.0, em("the", "V").toDouble, 1e-5)
    assertEquals((2 + (2 + 0.2) * ((3 + 0.1) / (16 + (13 * 0.1)))) / zV, em("walks", "V").toDouble, 1e-5)
    assertEquals((0 + (2 + 0.2) * ((0 + 0.1) / (16 + (13 * 0.1)))) / zV, em("default", "V").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "V").toDouble, 1e-5)

    val zDefault = (0 + (0 + 0.2) * ((16 + 13 * 0.1) / (16 + (13 * 0.1))))
    assertEquals(0.0, em("<S>", "default").toDouble, 1e-5)
    assertEquals((0 + (0 + 0.2) * ((1 + 0.1) / (16 + (13 * 0.1)))) / zDefault, em("a", "default").toDouble, 1e-5)
    assertEquals((0 + (0 + 0.2) * ((0 + 0.1) / (16 + (13 * 0.1)))) / zDefault, em("bird", "default").toDouble, 1e-5)
    assertEquals((0 + (0 + 0.2) * ((1 + 0.1) / (16 + (13 * 0.1)))) / zDefault, em("cat", "default").toDouble, 1e-5)
    assertEquals((0 + (0 + 0.2) * ((1 + 0.1) / (16 + (13 * 0.1)))) / zDefault, em("chases", "default").toDouble, 1e-5)
    assertEquals((0 + (0 + 0.2) * ((2 + 0.1) / (16 + (13 * 0.1)))) / zDefault, em("dog", "default").toDouble, 1e-5)
    assertEquals((0 + (0 + 0.2) * ((0 + 0.1) / (16 + (13 * 0.1)))) / zDefault, em("every", "default").toDouble, 1e-5)
    assertEquals((0 + (0 + 0.2) * ((0 + 0.1) / (16 + (13 * 0.1)))) / zDefault, em("flies", "default").toDouble, 1e-5)
    assertEquals((0 + (0 + 0.2) * ((0 + 0.1) / (16 + (13 * 0.1)))) / zDefault, em("fox", "default").toDouble, 1e-5)
    assertEquals((0 + (0 + 0.2) * ((2 + 0.1) / (16 + (13 * 0.1)))) / zDefault, em("man", "default").toDouble, 1e-5)
    assertEquals((0 + (0 + 0.2) * ((1 + 0.1) / (16 + (13 * 0.1)))) / zDefault, em("runs", "default").toDouble, 1e-5)
    assertEquals((0 + (0 + 0.2) * ((0 + 0.1) / (16 + (13 * 0.1)))) / zDefault, em("some", "default").toDouble, 1e-5)
    assertEquals((0 + (0 + 0.2) * ((5 + 0.1) / (16 + (13 * 0.1)))) / zDefault, em("the", "default").toDouble, 1e-5)
    assertEquals((0 + (0 + 0.2) * ((3 + 0.1) / (16 + (13 * 0.1)))) / zDefault, em("walks", "default").toDouble, 1e-5)
    assertEquals((0 + (0 + 0.2) * ((0 + 0.1) / (16 + (13 * 0.1)))) / zDefault, em("default", "default").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "default").toDouble, 1e-5)

    assertEquals(0.0, em("<S>", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("a", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("bird", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("chases", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("dog", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("every", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("flies", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("fox", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("man", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("runs", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("some", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("the", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("walks", "<E>").toDouble, 1e-5)
    assertEquals(0.0, em("default", "<E>").toDouble, 1e-5)
    assertEquals(1.0, em("<E>", "<E>").toDouble, 1e-5)
  }

}
