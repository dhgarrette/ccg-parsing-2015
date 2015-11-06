package dhg.ccg.tag.learn

import org.junit.Test
import dhg.util._
import org.junit.Assert._
import dhg.ccg.tag._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tagdict.SimpleTagDictionary

class TypesupHmmInitializationTests {

  @Test
  def test_TrUniform() {
    val sentences = Vector(
      "the dog walks",
      "the man walks the dog",
      "the man runs").map(_.lsplit(" "))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set("D"),
      "man" -> Set("N", "V"),
      "walks" -> Set("V")),
      "<S>", "<S>", "<E>", "<E>")

    val tr = new TrUniform().fromRaw(sentences, tagdict)
    assertEquals(0.0, tr("<S>", "<S>").toDouble, 1e-5)
    assertEquals(1.0 / 3, tr("D", "<S>").toDouble, 1e-5)
    assertEquals(1.0 / 3, tr("N", "<S>").toDouble, 1e-5)
    assertEquals(1.0 / 3, tr("V", "<S>").toDouble, 1e-5)
    assertEquals(1.0 / 3, tr("default", "<S>").toDouble, 1e-5)
    assertEquals(0.0, tr("<E>", "<S>").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "D").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("D", "D").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("N", "D").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("V", "D").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("default", "D").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("<E>", "D").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "N").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("D", "N").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("N", "N").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("V", "N").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("default", "N").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("<E>", "N").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "V").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("D", "V").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("N", "V").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("V", "V").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("default", "V").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("<E>", "V").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "default").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("D", "default").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("N", "default").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("V", "default").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("default", "default").toDouble, 1e-5)
    assertEquals(1.0 / 4, tr("<E>", "default").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("D", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("N", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("V", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("default", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("<E>", "<E>").toDouble, 1e-5)
  }

  @Test
  def test_em_uniform() {
    val sentences = Vector(
      "a cat chases the dog",
      "the dog walks",
      "the man walks the dog",
      "the man runs").map(_.lsplit(" "))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set("D"),
      "a" -> Set("D"),
      "every" -> Set("D"),
      "some" -> Set("D"),
      "man" -> Set("N", "V"),
      "cat" -> Set("N"),
      "bird" -> Set("N"),
      "fox" -> Set("N"),
      "walks" -> Set("V"),
      "flies" -> Set("N", "V")),
      "<S>", "<S>", "<E>", "<E>")

    /* Words not in TD:
     *   chases
     *   dog
     *   runs
     */

    val em = new EmUniform().fromRaw(sentences, tagdict)
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
    assertEquals(1.0 / (4 + 3), em("the", "D").toDouble, 1e-5)
    assertEquals(1.0 / (4 + 3), em("a", "D").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "D").toDouble, 1e-5)
    assertEquals(0.0, em("man", "D").toDouble, 1e-5)
    assertEquals(0.0, em("walks", "D").toDouble, 1e-5)
    assertEquals(1.0 / (4 + 3), em("dog", "D").toDouble, 1e-5)
    assertEquals(1.0 / (4 + 3), em("runs", "D").toDouble, 1e-5)
    assertEquals(1.0 / (4 + 3), em("chases", "D").toDouble, 1e-5)
    assertEquals(1.0 / (4 + 3), em("default", "D").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "D").toDouble, 1e-5)

    assertEquals(0.0, em("<S>", "N").toDouble, 1e-5)
    assertEquals(0.0, em("the", "N").toDouble, 1e-5)
    assertEquals(0.0, em("a", "N").toDouble, 1e-5)
    assertEquals(1.0 / (5 + 3), em("cat", "N").toDouble, 1e-5)
    assertEquals(1.0 / (5 + 3), em("man", "N").toDouble, 1e-5)
    assertEquals(0.0, em("walks", "N").toDouble, 1e-5)
    assertEquals(1.0 / (5 + 3), em("dog", "N").toDouble, 1e-5)
    assertEquals(1.0 / (5 + 3), em("runs", "N").toDouble, 1e-5)
    assertEquals(1.0 / (5 + 3), em("chases", "N").toDouble, 1e-5)
    assertEquals(1.0 / (5 + 3), em("default", "N").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "N").toDouble, 1e-5)

    assertEquals(0.0, em("<S>", "V").toDouble, 1e-5)
    assertEquals(0.0, em("the", "V").toDouble, 1e-5)
    assertEquals(0.0, em("a", "V").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "V").toDouble, 1e-5)
    assertEquals(1.0 / (3 + 3), em("man", "V").toDouble, 1e-5)
    assertEquals(1.0 / (3 + 3), em("walks", "V").toDouble, 1e-5)
    assertEquals(1.0 / (3 + 3), em("dog", "V").toDouble, 1e-5)
    assertEquals(1.0 / (3 + 3), em("runs", "V").toDouble, 1e-5)
    assertEquals(1.0 / (3 + 3), em("chases", "V").toDouble, 1e-5)
    assertEquals(1.0 / (3 + 3), em("default", "V").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "V").toDouble, 1e-5)

    assertEquals(0.0, em("<S>", "default").toDouble, 1e-5)
    assertEquals(0.0, em("the", "default").toDouble, 1e-5)
    assertEquals(0.0, em("a", "default").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "default").toDouble, 1e-5)
    assertEquals(0.0, em("man", "default").toDouble, 1e-5)
    assertEquals(0.0, em("walks", "default").toDouble, 1e-5)
    assertEquals(1.0 / (0 + 3), em("dog", "default").toDouble, 1e-5)
    assertEquals(1.0 / (0 + 3), em("runs", "default").toDouble, 1e-5)
    assertEquals(1.0 / (0 + 3), em("chases", "default").toDouble, 1e-5)
    assertEquals(1.0 / (0 + 3), em("default", "default").toDouble, 1e-5)
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
  def test_TrTagDictEntriesPossibilities() {
    val sentences = Vector(
      "the dog walks", //         {S} -> {D} -> { }  -> {V} -> {E}
      "the man walks the dog", // {S} -> {D} -> {NV} -> {V} -> {D} -> { } -> {E}
      "the man runs") //          {S} -> {D} -> {NV} -> { } -> {E}
      .map(_.lsplit(" "))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set("D"),
      "man" -> Set("N", "V"),
      "walks" -> Set("V")),
      "<S>", "<S>", "<E>", "<E>")

    /*    S   D   N   V   E
     * S      3              /3
     * D          1   1      /2
     * N             0.5     /0.5
     * V      1      0.5  1  /2.5
     * E     
     */

    val tr = new TrTagDictEntriesPossibilities(new AddLambdaTransitionDistributioner(0.2)).fromRaw(sentences, tagdict)
    assertEquals(0.0, tr("<S>", "<S>").toDouble, 1e-5)
    assertEquals((3 + 0.2) / (3 + 3 * 0.2), tr("D", "<S>").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (3 + 3 * 0.2), tr("N", "<S>").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (3 + 3 * 0.2), tr("V", "<S>").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (3 + 3 * 0.2), tr("default", "<S>").toDouble, 1e-5)
    assertEquals(0.0, tr("<E>", "<S>").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "D").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (2 + 4 * 0.2), tr("D", "D").toDouble, 1e-5)
    assertEquals((1 + 0.2) / (2 + 4 * 0.2), tr("N", "D").toDouble, 1e-5)
    assertEquals((1 + 0.2) / (2 + 4 * 0.2), tr("V", "D").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (2 + 4 * 0.2), tr("default", "D").toDouble, 1e-5)
    assertEquals((0 + 0.2) / (2 + 4 * 0.2), tr("<E>", "D").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "N").toDouble, 1e-5)
    assertEquals((0.0 + 0.2) / (0.5 + 4 * 0.2), tr("D", "N").toDouble, 1e-5)
    assertEquals((0.0 + 0.2) / (0.5 + 4 * 0.2), tr("N", "N").toDouble, 1e-5)
    assertEquals((0.5 + 0.2) / (0.5 + 4 * 0.2), tr("V", "N").toDouble, 1e-5)
    assertEquals((0.0 + 0.2) / (0.5 + 4 * 0.2), tr("default", "N").toDouble, 1e-5)
    assertEquals((0.0 + 0.2) / (0.5 + 4 * 0.2), tr("<E>", "N").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "V").toDouble, 1e-5)
    assertEquals((1.0 + 0.2) / (2.5 + 4 * 0.2), tr("D", "V").toDouble, 1e-5)
    assertEquals((0.0 + 0.2) / (2.5 + 4 * 0.2), tr("N", "V").toDouble, 1e-5)
    assertEquals((0.5 + 0.2) / (2.5 + 4 * 0.2), tr("V", "V").toDouble, 1e-5)
    assertEquals((0.0 + 0.2) / (2.5 + 4 * 0.2), tr("default", "V").toDouble, 1e-5)
    assertEquals((1.0 + 0.2) / (2.5 + 4 * 0.2), tr("<E>", "V").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "default").toDouble, 1e-5)
    assertEquals((0 / 1.0 + 0.2) / (0 + 4 * 0.2), tr("D", "default").toDouble, 1e-5)
    assertEquals((0 / 1.0 + 0.2) / (0 + 4 * 0.2), tr("N", "default").toDouble, 1e-5)
    assertEquals((0 / 1.0 + 0.2) / (0 + 4 * 0.2), tr("V", "default").toDouble, 1e-5)
    assertEquals((0 / 1.0 + 0.2) / (0 + 4 * 0.2), tr("default", "default").toDouble, 1e-5)
    assertEquals((0 / 1.0 + 0.2) / (0 + 4 * 0.2), tr("<E>", "default").toDouble, 1e-5)

    assertEquals(0.0, tr("<S>", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("D", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("N", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("V", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("default", "<E>").toDouble, 1e-5)
    assertEquals(0.0, tr("<E>", "<E>").toDouble, 1e-5)
  }

  @Test
  def test_InterpolatingTransitionInitializer {
    val s: AtomCat = cat"S".asInstanceOf[AtomCat]
    val n: AtomCat = cat"N".asInstanceOf[AtomCat]

    val sentences1 = Vector[Vector[(String, Set[Cat])]](Vector("junk" -> Set.empty))
    val td1 = SimpleTagDictionary[Cat](Map("whatever" -> Set(cat"X")), "<S>", cat"<S>", "<E>", cat"<E>", Set(),
      Set(n, s / s, s \ s, n / n, n \ n, s \ n, (s \ n) / n))

    val init = new InterpolatingTransitionInitializer(Vector(
      new TransitionInitializer[Cat] {
        def fromKnownSupertagSets(sentences: Vector[Vector[(String, Set[Cat])]], initialTagdict: TagDictionary[Cat]) = {
          assertSame(sentences, sentences1)
          assertEquals(initialTagdict.entries, td1.entries)
          new ConditionalLogProbabilityDistribution[Cat, Cat] {
            def apply(x: Cat, given: Cat): LogDouble = {
              Map[(Cat, Cat), LogDouble](
                (n, s) -> LogDouble(0.11),
                (s, n) -> LogDouble(0.21),
                (s, n / n) -> LogDouble(0.31))(x -> given)
            }
            def sample(given: Cat): Cat = ???
          }
        }
      } -> 0.6,
      new TransitionInitializer[Cat] {
        def fromKnownSupertagSets(sentences: Vector[Vector[(String, Set[Cat])]], initialTagdict: TagDictionary[Cat]) = {
          assertSame(sentences, sentences1)
          assertEquals(initialTagdict.entries, td1.entries)
          new ConditionalLogProbabilityDistribution[Cat, Cat] {
            def apply(x: Cat, given: Cat): LogDouble = {
              Map[(Cat, Cat), LogDouble](
                (n, s) -> LogDouble(0.41),
                (s, n) -> LogDouble(0.51),
                (s, n / n) -> LogDouble(0.61))(x -> given)
            }
            def sample(given: Cat): Cat = ???
          }
        }
      } -> 0.4))

    val tr = init.fromKnownSupertagSets(sentences1, td1)
    assertEqualsLog(LogDouble(0.6 * 0.11 + 0.4 * 0.41), tr(n, s), 1e-9)
    assertEqualsLog(LogDouble(0.6 * 0.31 + 0.4 * 0.61), tr(s, n / n), 1e-9)
  }

  @Test
  def test_em_tde() {
    val sentences = Vector(
      "a cat chases the dog",
      "the dog walks",
      "the man walks the dog",
      "the man runs").map(_.lsplit(" "))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set("D"),
      "a" -> Set("D"),
      "every" -> Set("D"),
      "some" -> Set("D"),
      "man" -> Set("N", "V"),
      "cat" -> Set("N"),
      "bird" -> Set("N"),
      "fox" -> Set("N"),
      "walks" -> Set("V"),
      "flies" -> Set("N", "V")),
      "<S>", "<S>", "<E>", "<E>")

    /* Words not in TD:
     *   chases	
     *   dog	
     *   runs	
     */

    /* C
     * 
     * a		1
     * cat		1
     * chases	1
     * dog		3
     * man		2
     * runs		1
     * the		5
     * walks	2
     */

    /* C_k
     *           D   N   V
     * a		 1
     * cat		     1
     * man		     1   1
     * the		 5
     * walks	         2
     */

    /* p(t|unk)
     * 
     *  	[sum_w' C_k(t,w')]  p(t)  |  |TD(t)|  p(unk|t)  |          p(t|unk)
     * D	6					6/11  |     4       4/12    |  24/(11*12) / (43/(11*12)) = 24/43  
     * N	2					2/11  |     5       5/12    |  10/(11*12) / (43/(11*12)) = 10/43
     * V	3					3/11  |     3       3/12    |   9/(11*12) / (43/(11*12)) =  9/43
     * 
     */

    /* C_u		  
     *          C(w)  |    D       N       V
     * chases	 1    |  24/43   10/43    9/43
     * dog		 3    |  72/43   30/43   27/43
     * runs		 1    |  24/43   10/43    9/43
     */

    /* C_ku totals
     *                    D       N       V
     *                  378/43  136/43  174/43
     */

    val em = new EmTagDictionaryEstimate(
      new TagDictionaryEstimateTagPriorInitializer,
      0.2, 1.0, false).fromRaw(sentences, tagdict)
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
    assertEquals(5.0 / (378 / 43.0), em("the", "D").toDouble, 1e-5)
    assertEquals(1.0 / (378 / 43.0), em("a", "D").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "D").toDouble, 1e-5)
    assertEquals(0.0, em("man", "D").toDouble, 1e-5)
    assertEquals(0.0, em("walks", "D").toDouble, 1e-5)
    assertEquals((72 / 43.0) / (378 / 43.0), em("dog", "D").toDouble, 1e-5)
    assertEquals((24 / 43.0) / (378 / 43.0), em("runs", "D").toDouble, 1e-5)
    assertEquals((24 / 43.0) / (378 / 43.0), em("chases", "D").toDouble, 1e-5)
    assertEquals(0.0 / (378 / 43.0), em("default", "D").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "D").toDouble, 1e-5)

    assertEquals(0.0, em("<S>", "N").toDouble, 1e-5)
    assertEquals(0.0, em("the", "N").toDouble, 1e-5)
    assertEquals(0.0, em("a", "N").toDouble, 1e-5)
    assertEquals(1.0 / (136 / 43.0), em("cat", "N").toDouble, 1e-5)
    assertEquals(1.0 / (136 / 43.0), em("man", "N").toDouble, 1e-5)
    assertEquals(0.0, em("walks", "N").toDouble, 1e-5)
    assertEquals((30 / 43.0) / (136 / 43.0), em("dog", "N").toDouble, 1e-5)
    assertEquals((10 / 43.0) / (136 / 43.0), em("runs", "N").toDouble, 1e-5)
    assertEquals((10 / 43.0) / (136 / 43.0), em("chases", "N").toDouble, 1e-5)
    assertEquals(0.0 / (136 / 43.0), em("default", "N").toDouble, 1e-5)
    assertEquals(0.0, em("<E>", "N").toDouble, 1e-5)

    assertEquals(0.0, em("<S>", "V").toDouble, 1e-5)
    assertEquals(0.0, em("the", "V").toDouble, 1e-5)
    assertEquals(0.0, em("a", "V").toDouble, 1e-5)
    assertEquals(0.0, em("cat", "V").toDouble, 1e-5)
    assertEquals(1.0 / (174 / 43.0), em("man", "V").toDouble, 1e-5)
    assertEquals(2.0 / (174 / 43.0), em("walks", "V").toDouble, 1e-5)
    assertEquals((27 / 43.0) / (174 / 43.0), em("dog", "V").toDouble, 1e-5)
    assertEquals((9 / 43.0) / (174 / 43.0), em("runs", "V").toDouble, 1e-5)
    assertEquals((9 / 43.0) / (174 / 43.0), em("chases", "V").toDouble, 1e-5)
    assertEquals(0.0 / (174 / 43.0), em("default", "V").toDouble, 1e-5)
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
  def test_em_tde_manualTagPrior() {
    throw new NotImplementedError("Test not written")
  }

  def assertEqualsLog(a: LogDouble, b: LogDouble, c: Double) {
    assertEquals(a.toDouble, b.toDouble, c)
  }
}
