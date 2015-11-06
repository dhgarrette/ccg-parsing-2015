package dhg.ccg.tag.learn

import org.junit.Test
import dhg.util._
import math.{ abs, pow }
import org.junit.Assert._
import Double.NaN
import dhg.ccg.tag.learn._
import dhg.ccg.tag._
import dhg.ccg.prob._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tagdict.SimpleTagDictionary

class EmHmmTests {

  @Test
  def test_SoftEmHmm_toy_train_2iterations {
    val scale = 10

    val sentences = Vector(
      "a cat chases the dog",
      "the dog walks",
      "the man walks the dog",
      "the man runs").map(_.lsplit(" "))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set('D),
      "a" -> Set('D),
      "every" -> Set('D),
      "some" -> Set('D),
      "man" -> Set('N, 'V),
      "cat" -> Set('N),
      "bird" -> Set('N),
      "fox" -> Set('N),
      "walks" -> Set('V),
      "flies" -> Set('N, 'V)),
      "<S>", 'A, "<E>", 'Z)
      .withWords(sentences.flatten.toSet)

    val trInit =
      new SimpleConditionalLogProbabilityDistribution[Symbol, Symbol](
        Map(
          tagdict.startTag -> new SimpleLogProbabilityDistribution(Map('D -> 0.7, 'N -> 0.2, 'V -> 0.1).mapVals(LogDouble(_))),
          'D -> new SimpleLogProbabilityDistribution(Map('D -> 0.1, 'N -> 0.7, 'V -> 0.1, tagdict.endTag -> 0.1).mapVals(LogDouble(_))),
          'N -> new SimpleLogProbabilityDistribution(Map('D -> 0.1, 'N -> 0.3, 'V -> 0.4, tagdict.endTag -> 0.2).mapVals(LogDouble(_))),
          'V -> new SimpleLogProbabilityDistribution(Map('D -> 0.3, 'N -> 0.2, 'V -> 0.1, tagdict.endTag -> 0.4).mapVals(LogDouble(_))),
          tagdict.endTag -> LogProbabilityDistribution.empty))
    val emInit =
      new SimpleConditionalLogProbabilityDistribution[Symbol, String](
        Map(
          'D -> new SimpleLogProbabilityDistribution(Map("the" -> 0.4, "a" -> 0.3, "dog" -> 0.1, "chases" -> 0.1, "runs" -> 0.1).mapVals(LogDouble(_))),
          'N -> new SimpleLogProbabilityDistribution(Map("cat" -> 0.2, "man" -> 0.3, "dog" -> 0.2, "chases" -> 0.2, "runs" -> 0.1).mapVals(LogDouble(_))),
          'V -> new SimpleLogProbabilityDistribution(Map("man" -> 0.2, "dog" -> 0.1, "walks" -> 0.2, "chases" -> 0.3, "runs" -> 0.2).mapVals(LogDouble(_))),
          tagdict.startTag -> new SimpleLogProbabilityDistribution(Map(tagdict.startWord -> LogDouble.one)),
          tagdict.endTag -> new SimpleLogProbabilityDistribution(Map(tagdict.endWord -> LogDouble.one))))

    //    val emt = new SimpleTypeSupervisedTrainer(new EmHmmTrainer(2, 1e-10), trInitializer, emInitializer)
    //    val hmm = emt.train(sentences, tagdict).asInstanceOf[HmmTagger[String, Symbol]]

    val emt = new SoftEmHmmTaggerTrainer[Symbol](2, new UnsmoothedTransitionDistributioner[Symbol](), new UnsmoothedEmissionDistributioner[Symbol](), alphaT = 0.0, alphaE = 0.0, 1e-10)
    val hmm = emt.train(sentences, tagdict, trInit, emInit).asInstanceOf[HmmTagger[Symbol]]

    val tr = hmm.transitions
    val em = hmm.emissions

    //    for (t1 <- tagdict.allTags.toVector.sortBy(_.toString))
    //      println(f"$t1 -> ${tagdict.allTags.toVector.sortBy(_.toString).mapTo(t2 => f"${tr(t2, t1)}%.2f").mkString(" ")}")
    //    for (t <- tagdict.allTags.toVector.sortBy(_.toString))
    //      println(f"$t -> ${tagdict.allWords.toVector.sortBy(_.toString).mapTo(w => f"${em(w, t)}%.2f").mkString(" ")}")

    assertEquals(0.0, tr('A, 'A).toDouble, 1e-9)
    assertEquals(1.0, tr('D, 'A).toDouble, 1e-9)
    assertEquals(0.0, tr('N, 'A).toDouble, 1e-9)
    assertEquals(0.0, tr('V, 'A).toDouble, 1e-9)
    assertEquals(0.0, tr('Z, 'A).toDouble, 1e-9)

    assertEquals(0.0, tr('A, 'D).toDouble, 1e-9)
    assertEquals(0.0000212112, tr('D, 'D).toDouble, 1e-9)
    assertEquals(0.9951962019, tr('N, 'D).toDouble, 1e-9)
    assertEquals(0.0047679144, tr('V, 'D).toDouble, 1e-9)
    assertEquals(0.0000146724, tr('Z, 'D).toDouble, 1e-9)

    assertEquals(0.0, tr('A, 'N).toDouble, 1e-9)
    assertEquals(0.0001151986, tr('D, 'N).toDouble, 1e-9)
    assertEquals(0.0011682076, tr('N, 'N).toDouble, 1e-9)
    assertEquals(0.6678369319, tr('V, 'N).toDouble, 1e-9)
    assertEquals(0.3308796618, tr('Z, 'N).toDouble, 1e-9)

    assertEquals(0.0, tr('A, 'V).toDouble, 1e-9)
    assertEquals(0.4971650599, tr('D, 'V).toDouble, 1e-9)
    assertEquals(0.0000002073, tr('N, 'V).toDouble, 1e-9)
    assertEquals(0.0000863669, tr('V, 'V).toDouble, 1e-9)
    assertEquals(0.5027483659, tr('Z, 'V).toDouble, 1e-9)

    assertEquals(0.0, tr('A, 'Z).toDouble, 1e-9)
    assertEquals(0.0, tr('D, 'Z).toDouble, 1e-9)
    assertEquals(0.0, tr('N, 'Z).toDouble, 1e-9)
    assertEquals(0.0, tr('V, 'Z).toDouble, 1e-9)
    assertEquals(0.0, tr('Z, 'Z).toDouble, 1e-9)

    assertEquals(1.0, em("<S>", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("a", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("cat", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("chases", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("the", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("dog", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("walks", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("man", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("runs", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("bird", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("<E>", 'A).toDouble, 1e-9)

    assertEquals(0.0, em("<S>", 'D).toDouble, 1e-9)
    assertEquals(0.1666620188, em("a", 'D).toDouble, 1e-9)
    assertEquals(0.0, em("cat", 'D).toDouble, 1e-9)
    assertEquals(0.0000055693, em("chases", 'D).toDouble, 1e-9)
    assertEquals(0.8333100939, em("the", 'D).toDouble, 1e-9)
    assertEquals(0.0000156419, em("dog", 'D).toDouble, 1e-9)
    assertEquals(0.0, em("walks", 'D).toDouble, 1e-9)
    assertEquals(0.0, em("man", 'D).toDouble, 1e-9)
    assertEquals(0.0000066761, em("runs", 'D).toDouble, 1e-9)
    assertEquals(0.0, em("bird", 'D).toDouble, 1e-9)
    assertEquals(0.0, em("<E>", 'D).toDouble, 1e-9)

    assertEquals(0.0, em("<S>", 'N).toDouble, 1e-9)
    assertEquals(0.0, em("a", 'N).toDouble, 1e-9)
    assertEquals(0.1672708350, em("cat", 'N).toDouble, 1e-9)
    assertEquals(0.0001031622, em("chases", 'N).toDouble, 1e-9)
    assertEquals(0.0, em("the", 'N).toDouble, 1e-9)
    assertEquals(0.4970417101, em("dog", 'N).toDouble, 1e-9)
    assertEquals(0.0, em("walks", 'N).toDouble, 1e-9)
    assertEquals(0.3345191078, em("man", 'N).toDouble, 1e-9)
    assertEquals(0.0010651849, em("runs", 'N).toDouble, 1e-9)
    assertEquals(0.0, em("bird", 'N).toDouble, 1e-9)
    assertEquals(0.0, em("<E>", 'N).toDouble, 1e-9)

    assertEquals(0.0, em("<S>", 'V).toDouble, 1e-9)
    assertEquals(0.0, em("a", 'V).toDouble, 1e-9)
    assertEquals(0.0, em("cat", 'V).toDouble, 1e-9)
    assertEquals(0.2485015068, em("chases", 'V).toDouble, 1e-9)
    assertEquals(0.0, em("the", 'V).toDouble, 1e-9)
    assertEquals(0.0070688787, em("dog", 'V).toDouble, 1e-9)
    assertEquals(0.4973263522, em("walks", 'V).toDouble, 1e-9)
    assertEquals(0.0000335407, em("man", 'V).toDouble, 1e-9)
    assertEquals(0.2470697217, em("runs", 'V).toDouble, 1e-9)
    assertEquals(0.0, em("bird", 'V).toDouble, 1e-9)
    assertEquals(0.0, em("<E>", 'V).toDouble, 1e-9)

    assertEquals(0.0, em("<S>", 'Z).toDouble, 1e-9)
    assertEquals(0.0, em("a", 'Z).toDouble, 1e-9)
    assertEquals(0.0, em("cat", 'Z).toDouble, 1e-9)
    assertEquals(0.0, em("chases", 'Z).toDouble, 1e-9)
    assertEquals(0.0, em("the", 'Z).toDouble, 1e-9)
    assertEquals(0.0, em("dog", 'Z).toDouble, 1e-9)
    assertEquals(0.0, em("walks", 'Z).toDouble, 1e-9)
    assertEquals(0.0, em("man", 'Z).toDouble, 1e-9)
    assertEquals(0.0, em("runs", 'Z).toDouble, 1e-9)
    assertEquals(0.0, em("bird", 'Z).toDouble, 1e-9)
    assertEquals(1.0, em("<E>", 'Z).toDouble, 1e-9)

  }

  @Test
  def test_SoftEmHmm_icecream_doTrain_2iterations {
    val scale = 10

    val s1 = Array(-1, 2, 3, 3, 2, 3, 2, 3, 2, 2, 3, 1, 3, 3, 1, 1, 1, 2, 1, 1, 1, 3, 1, 2, 1, 1, 1, 2, 3, 3, 2, 3, 2, 2, 0)
    val s2 = Array(-1, 2, 3, 3, 2, 3, 2, 3, 2, 2, 3, 1, 3, 3, 1, 1, 1, 2, 1, 1, 1, 3, 1, 2, 1, 1, 1, 2, 3, 3, 2, 3, 2, 2, 0)
    val td = Array(0) +: Array(1) +: Array.fill(3)(Array(2, 3))

    val emt = new SoftEmHmmTaggerTrainer(2, new UnsmoothedTransitionDistributioner[String](), new UnsmoothedEmissionDistributioner[String](), alphaT = 0.0, alphaE = 0.0, 1e-10)
    val (expectedTrLogCounts, expectedEmLogCounts) =
      emt.doTrain(
        Vector(s1, s2).map(_.map(_ + 1)).map(s => (s, s.map(td))),
        numWords = 5, numTags = 4,
        rtd = Array(0) +: Array(1) +: Array.fill(2)(Array(2, 3, 4)),
        alphaPriorTr = Array(
          Array(0.0, 0.0, 0.0, 0.0), //  C_t(.|START)
          Array(0.0, 0.0, 0.0, 0.0), //  C_t(.|END) 
          Array(0.0, 0.0, 0.0, 0.0), //  C_t(.|C)
          Array(0.0, 0.0, 0.0, 0.0) //   C_t(.|H)
          ).map(_.map(identity)),
        alphaPriorEm = Array(
          Array(0.0, 0.0, 0.0, 0.0, 0.0), //  C_e(.|START)
          Array(0.0, 0.0, 0.0, 0.0, 0.0), //  C_e(.|END)
          Array(0.0, 0.0, 0.0, 0.0, 0.0), //  C_e(.|C)
          Array(0.0, 0.0, 0.0, 0.0, 0.0) //   C_e(.|H)
          ).map(_.map(identity)),
        logInitialTr = Array(
          Array(0.0, 0.0, 0.5, 0.5), //  p_t(.|START)
          Array(0.0, 0.0, 0.0, 0.0), //  p_t(.|END) 
          Array(0.0, 0.1, 0.7, 0.2), //  p_t(.|C)
          Array(0.0, 0.1, 0.1, 0.8) //   p_t(.|H)
          ).map(_.map(math.log)),
        logInitialEm = Array(
          Array(1.0, 0.0, 0.0, 0.0, 0.0), //  p_e(.|START)
          Array(0.0, 1.0, 0.0, 0.0, 0.0), //  p_e(.|END)
          Array(0.0, 0.0, 0.7, 0.2, 0.1), //  p_e(.|C)
          Array(0.0, 0.0, 0.1, 0.3, 0.6) //   p_e(.|H)
          ).map(_.map(math.log)))
    assertEquals2dArray(Array(
      Array(0.0, 0.0, 0.019187795922706, 0.980812204077294), //  p_t(.|START)
      Array(NaN, NaN, NaN, NaN), //  p_t(.|END) 
      Array(0.0, 0.001218263806694, 0.885892498643987, 0.112889237549318), //  p_t(.|C)
      Array(0.0, 0.052351859830148, 0.085481356704799, 0.862166783465053)), // p_t(.|H)
      expectedTrLogCounts.map(_.map { case Double.NaN => 0.0; case x => math.exp(x) }.toVector.normalize.toArray), scale)
    assertEquals2dArray(Array(
      Array(NaN, NaN, NaN, NaN, NaN), //  p_e(.|START)
      Array(NaN, NaN, NaN, NaN, NaN), //  p_e(.|END)
      Array(NaN, NaN, 0.714543877350896, 0.168586118793123, 0.116870003855980), //  p_e(.|C)
      Array(NaN, NaN, 0.044341970791651, 0.458226316363542, 0.497431712844807)), // p_e(.|H)
      expectedEmLogCounts.map(_.map { case Double.NaN => 0.0; case x => math.exp(x) }.toVector.normalize.toArray), scale)
  }

  @Test
  def test_SoftEmHmm_icecream_doTrain_11iterations {
    val scale = 3

    val s1 = Array(-1, 2, 3, 3, 2, 3, 2, 3, 2, 2, 3, 1, 3, 3, 1, 1, 1, 2, 1, 1, 1, 3, 1, 2, 1, 1, 1, 2, 3, 3, 2, 3, 2, 2, 0)
    val s2 = Array(-1, 2, 3, 3, 2, 3, 2, 3, 2, 2, 3, 1, 3, 3, 1, 1, 1, 2, 1, 1, 1, 3, 1, 2, 1, 1, 1, 2, 3, 3, 2, 3, 2, 2, 0)
    val td = Array(0) +: Array(1) +: Array.fill(3)(Array(2, 3))

    val emt = new SoftEmHmmTaggerTrainer(11, new UnsmoothedTransitionDistributioner[String](), new UnsmoothedEmissionDistributioner[String](), alphaT = 0.0, alphaE = 0.0, 1e-10)
    val (expectedTrLogCounts, expectedEmLogCounts) =
      emt.doTrain(
        Vector(s1, s2).map(_.map(_ + 1)).map(s => (s, s.map(td))),
        numWords = 5, numTags = 4,
        rtd = Array(0) +: Array(1) +: Array.fill(2)(Array(2, 3, 4)),
        alphaPriorTr = Array(
          Array(0.0, 0.0, 0.0, 0.0), //  C_t(.|START)
          Array(0.0, 0.0, 0.0, 0.0), //  C_t(.|END) 
          Array(0.0, 0.0, 0.0, 0.0), //  C_t(.|C)
          Array(0.0, 0.0, 0.0, 0.0) //   C_t(.|H)
          ).map(_.map(identity)),
        alphaPriorEm = Array(
          Array(0.0, 0.0, 0.0, 0.0, 0.0), //  C_e(.|START)
          Array(0.0, 0.0, 0.0, 0.0, 0.0), //  C_e(.|END)
          Array(0.0, 0.0, 0.0, 0.0, 0.0), //  C_e(.|C)
          Array(0.0, 0.0, 0.0, 0.0, 0.0) //   C_e(.|H)
          ).map(_.map(identity)),
        Array(
          Array(0.0, 0.0, 0.5, 0.5), //  p_t(.|START)
          Array(0.0, 0.0, 0.0, 0.0), //  p_t(.|END) 
          Array(0.0, 0.1, 0.7, 0.2), //  p_t(.|C)
          Array(0.0, 0.1, 0.1, 0.8) //   p_t(.|H)
          ).map(_.map(math.log)),
        Array(
          Array(1.0, 0.0, 0.0, 0.0, 0.0), //  p_e(.|START)
          Array(0.0, 1.0, 0.0, 0.0, 0.0), //  p_e(.|END)
          Array(0.0, 0.0, 0.7, 0.2, 0.1), //  p_e(.|C)
          Array(0.0, 0.0, 0.1, 0.3, 0.6) //   p_e(.|H)
          ).map(_.map(math.log)))
    assertEquals2dArray(Array(
      Array(NaN, NaN, 2.741248327961E-13, 0.999999999999726), //  p_t(.|START)
      Array(NaN, NaN, NaN, NaN), //  p_t(.|END) 
      Array(0.0, 1.824185761141E-14, 0.933701716226397, 0.066298283773585), //  p_t(.|C)
      Array(0.0, 0.063135967746579, 0.071833324323886, 0.865030707929535)), // p_t(.|H)
      expectedTrLogCounts.map(_.map { case Double.NaN => 0.0; case x => math.exp(x) }.toVector.normalize.toArray), scale)
    assertEquals2dArray(Array(
      Array(NaN, NaN, NaN, NaN, NaN), //  p_e(.|START)
      Array(NaN, NaN, NaN, NaN, NaN), //  p_e(.|END)
      Array(NaN, NaN, 0.640784802222534, 0.148092649969018, 0.211122547808448), //  p_e(.|C)
      Array(NaN, NaN, 2.136834492453E-4, 0.534039193707180, 0.465747122843575)), // p_e(.|H)
      expectedEmLogCounts.map(_.map { case Double.NaN => 0.0; case x => math.exp(x) }.toVector.normalize.toArray), scale)
  }

  @Test
  def test_HardEmHmm_toy_train_1iteration {
    val scale = 10

    val sentences = Vector(
      "the dog runs").map(_.lsplit(" "))
    val trainData = Vector(
      "the|D man|N walks|V the|D dog|N",
      "the|D dog|N runs|V",
      "the|D dog|N walks|V",
      "the|D man|N walks|V",
      "a|D man|N saw|V the|D dog|N",
      "the|D cat|N walks|V").map(_.splitWhitespace.map(_.split("\\|").toTuple2))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set("D", "N", "V"),
      "dog" -> Set("D", "N", "V"),
      "runs" -> Set("D", "N", "V")),
      "<S>", "A", "<E>", "Z")
      .withWords(sentences.flatten.toSet ++ trainData.flatten.map(_._1).toSet + "x1")

    val trInit = new AddLambdaTransitionDistributioner[String](lambda = 1.0)(trainData, tagdict)
    val emInit = new AddLambdaEmissionDistributioner[String](lambda = 1.0)(trainData, tagdict)

    val x =
      (Vector(tagdict.startTag, "D", "N", "V", tagdict.endTag).sliding2.mapt((a, b) => trInit(b, a).logValue)) ++
        Vector(("the", "D"), ("dog", "N"), ("runs", "V")).mapt((w, t) => emInit(w, t).logValue).toVector
    println(x.sum)

    val emt = new HardEmHmmTaggerTrainer[String](2, new UnsmoothedTransitionDistributioner[String](), new UnsmoothedEmissionDistributioner[String](), alphaT = 0.0, alphaE = 0.0, 1e-10)
    val hmm = emt.train(sentences, tagdict, trInit, emInit).asInstanceOf[HmmTagger[String]]

    val tr = hmm.transitions
    val em = hmm.emissions

    //    for (t1 <- tagdict.allTags.toVector.sortBy(_.toString))
    //      println(f"$t1 -> ${tagdict.allTags.toVector.sortBy(_.toString).mapTo(t2 => f"${tr(t2, t1)}%.2f").mkString(" ")}")
    //    for (t <- tagdict.allTags.toVector.sortBy(_.toString))
    //      println(f"$t -> ${tagdict.allWords.toVector.sortBy(_.toString).mapTo(w => f"${em(w, t)}%.2f").mkString(" ")}")

    assertEquals(0.0, tr("A", "A").toDouble, 1e-9)
    assertEquals(1.0, tr("D", "A").toDouble, 1e-9)
    assertEquals(0.0, tr("N", "A").toDouble, 1e-9)
    assertEquals(0.0, tr("V", "A").toDouble, 1e-9)
    assertEquals(0.0, tr("Z", "A").toDouble, 1e-9)

    assertEquals(0.0, tr("A", "D").toDouble, 1e-9)
    assertEquals(0.0, tr("D", "D").toDouble, 1e-9)
    assertEquals(1.0, tr("N", "D").toDouble, 1e-9)
    assertEquals(0.0, tr("V", "D").toDouble, 1e-9)
    assertEquals(0.0, tr("Z", "D").toDouble, 1e-9)

    assertEquals(0.0, tr("A", "N").toDouble, 1e-9)
    assertEquals(0.0, tr("D", "N").toDouble, 1e-9)
    assertEquals(0.0, tr("N", "N").toDouble, 1e-9)
    assertEquals(1.0, tr("V", "N").toDouble, 1e-9)
    assertEquals(0.0, tr("Z", "N").toDouble, 1e-9)

    assertEquals(0.0, tr("A", "V").toDouble, 1e-9)
    assertEquals(0.0, tr("D", "V").toDouble, 1e-9)
    assertEquals(0.0, tr("N", "V").toDouble, 1e-9)
    assertEquals(0.0, tr("V", "V").toDouble, 1e-9)
    assertEquals(1.0, tr("Z", "V").toDouble, 1e-9)

    assertEquals(0.0, tr("A", "Z").toDouble, 1e-9)
    assertEquals(0.0, tr("D", "Z").toDouble, 1e-9)
    assertEquals(0.0, tr("N", "Z").toDouble, 1e-9)
    assertEquals(0.0, tr("V", "Z").toDouble, 1e-9)
    assertEquals(0.0, tr("Z", "Z").toDouble, 1e-9)

    assertEquals(1.0, em("<S>", "A").toDouble, 1e-9)
    assertEquals(0.0, em("a", "A").toDouble, 1e-9)
    assertEquals(0.0, em("cat", "A").toDouble, 1e-9)
    assertEquals(0.0, em("chases", "A").toDouble, 1e-9)
    assertEquals(0.0, em("the", "A").toDouble, 1e-9)
    assertEquals(0.0, em("dog", "A").toDouble, 1e-9)
    assertEquals(0.0, em("walks", "A").toDouble, 1e-9)
    assertEquals(0.0, em("man", "A").toDouble, 1e-9)
    assertEquals(0.0, em("runs", "A").toDouble, 1e-9)
    assertEquals(0.0, em("bird", "A").toDouble, 1e-9)
    assertEquals(0.0, em("<E>", "A").toDouble, 1e-9)

    assertEquals(0.0, em("<S>", "D").toDouble, 1e-9)
    assertEquals(0.0, em("a", "D").toDouble, 1e-9)
    assertEquals(0.0, em("cat", "D").toDouble, 1e-9)
    assertEquals(0.0, em("chases", "D").toDouble, 1e-9)
    assertEquals(1.0, em("the", "D").toDouble, 1e-9)
    assertEquals(0.0, em("dog", "D").toDouble, 1e-9)
    assertEquals(0.0, em("walks", "D").toDouble, 1e-9)
    assertEquals(0.0, em("man", "D").toDouble, 1e-9)
    assertEquals(0.0, em("runs", "D").toDouble, 1e-9)
    assertEquals(0.0, em("bird", "D").toDouble, 1e-9)
    assertEquals(0.0, em("<E>", "D").toDouble, 1e-9)

    assertEquals(0.0, em("<S>", "N").toDouble, 1e-9)
    assertEquals(0.0, em("a", "N").toDouble, 1e-9)
    assertEquals(0.0, em("cat", "N").toDouble, 1e-9)
    assertEquals(0.0, em("chases", "N").toDouble, 1e-9)
    assertEquals(0.0, em("the", "N").toDouble, 1e-9)
    assertEquals(1.0, em("dog", "N").toDouble, 1e-9)
    assertEquals(0.0, em("walks", "N").toDouble, 1e-9)
    assertEquals(0.0, em("man", "N").toDouble, 1e-9)
    assertEquals(0.0, em("runs", "N").toDouble, 1e-9)
    assertEquals(0.0, em("bird", "N").toDouble, 1e-9)
    assertEquals(0.0, em("<E>", "N").toDouble, 1e-9)

    assertEquals(0.0, em("<S>", "V").toDouble, 1e-9)
    assertEquals(0.0, em("a", "V").toDouble, 1e-9)
    assertEquals(0.0, em("cat", "V").toDouble, 1e-9)
    assertEquals(0.0, em("chases", "V").toDouble, 1e-9)
    assertEquals(0.0, em("the", "V").toDouble, 1e-9)
    assertEquals(0.0, em("dog", "V").toDouble, 1e-9)
    assertEquals(0.0, em("walks", "V").toDouble, 1e-9)
    assertEquals(0.0, em("man", "V").toDouble, 1e-9)
    assertEquals(1.0, em("runs", "V").toDouble, 1e-9)
    assertEquals(0.0, em("bird", "V").toDouble, 1e-9)
    assertEquals(0.0, em("<E>", "V").toDouble, 1e-9)

    assertEquals(0.0, em("<S>", "Z").toDouble, 1e-9)
    assertEquals(0.0, em("a", "Z").toDouble, 1e-9)
    assertEquals(0.0, em("cat", "Z").toDouble, 1e-9)
    assertEquals(0.0, em("chases", "Z").toDouble, 1e-9)
    assertEquals(0.0, em("the", "Z").toDouble, 1e-9)
    assertEquals(0.0, em("dog", "Z").toDouble, 1e-9)
    assertEquals(0.0, em("walks", "Z").toDouble, 1e-9)
    assertEquals(0.0, em("man", "Z").toDouble, 1e-9)
    assertEquals(0.0, em("runs", "Z").toDouble, 1e-9)
    assertEquals(0.0, em("bird", "Z").toDouble, 1e-9)
    assertEquals(1.0, em("<E>", "Z").toDouble, 1e-9)

  }

  @Test
  def test_HardEmHmm_toy_train_2iterations {
    val scale = 10

    val sentences = Vector(
      "a cat chases the dog",
      "the dog walks",
      "the man walks the dog",
      "the man runs").map(_.lsplit(" "))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set('D),
      "a" -> Set('D),
      "every" -> Set('D),
      "some" -> Set('D),
      "man" -> Set('N, 'V),
      "cat" -> Set('N),
      "bird" -> Set('N),
      "fox" -> Set('N),
      "walks" -> Set('V),
      "flies" -> Set('N, 'V)),
      "<S>", 'A, "<E>", 'Z)
      .withWords(sentences.flatten.toSet)

    val trInit =
      new SimpleConditionalLogProbabilityDistribution[Symbol, Symbol](
        Map(
          tagdict.startTag -> new SimpleLogProbabilityDistribution(Map('D -> 0.7, 'N -> 0.2, 'V -> 0.1).mapVals(LogDouble(_))),
          'D -> new SimpleLogProbabilityDistribution(Map('D -> 0.1, 'N -> 0.7, 'V -> 0.1, tagdict.endTag -> 0.1).mapVals(LogDouble(_))),
          'N -> new SimpleLogProbabilityDistribution(Map('D -> 0.1, 'N -> 0.3, 'V -> 0.4, tagdict.endTag -> 0.2).mapVals(LogDouble(_))),
          'V -> new SimpleLogProbabilityDistribution(Map('D -> 0.3, 'N -> 0.2, 'V -> 0.1, tagdict.endTag -> 0.4).mapVals(LogDouble(_))),
          tagdict.endTag -> LogProbabilityDistribution.empty))
    val emInit =
      new SimpleConditionalLogProbabilityDistribution[Symbol, String](
        Map(
          'D -> new SimpleLogProbabilityDistribution(Map("the" -> 0.4, "a" -> 0.3, "dog" -> 0.1, "chases" -> 0.1, "runs" -> 0.1).mapVals(LogDouble(_))),
          'N -> new SimpleLogProbabilityDistribution(Map("cat" -> 0.2, "man" -> 0.3, "dog" -> 0.2, "chases" -> 0.2, "runs" -> 0.1).mapVals(LogDouble(_))),
          'V -> new SimpleLogProbabilityDistribution(Map("man" -> 0.2, "dog" -> 0.1, "walks" -> 0.2, "chases" -> 0.3, "runs" -> 0.2).mapVals(LogDouble(_))),
          tagdict.startTag -> new SimpleLogProbabilityDistribution(Map(tagdict.startWord -> LogDouble.one)),
          tagdict.endTag -> new SimpleLogProbabilityDistribution(Map(tagdict.endWord -> LogDouble.one))))

    //    val emt = new SimpleTypeSupervisedTrainer(new EmHmmTrainer(2, 1e-10), trInitializer, emInitializer)
    //    val hmm = emt.train(sentences, tagdict).asInstanceOf[HmmTagger[String, Symbol]]

    val emt = new HardEmHmmTaggerTrainer[Symbol](2, new UnsmoothedTransitionDistributioner[Symbol](), new UnsmoothedEmissionDistributioner[Symbol](), alphaT = 0.0, alphaE = 0.0, 1e-10)
    val hmm = emt.train(sentences, tagdict, trInit, emInit).asInstanceOf[HmmTagger[Symbol]]

    val tr = hmm.transitions
    val em = hmm.emissions

    //    for (t1 <- tagdict.allTags.toVector.sortBy(_.toString))
    //      println(f"$t1 -> ${tagdict.allTags.toVector.sortBy(_.toString).mapTo(t2 => f"${tr(t2, t1)}%.2f").mkString(" ")}")
    //    for (t <- tagdict.allTags.toVector.sortBy(_.toString))
    //      println(f"$t -> ${tagdict.allWords.toVector.sortBy(_.toString).mapTo(w => f"${em(w, t)}%.2f").mkString(" ")}")

    assertEquals(0.0, tr('A, 'A).toDouble, 1e-9)
    assertEquals(1.0, tr('D, 'A).toDouble, 1e-9)
    assertEquals(0.0, tr('N, 'A).toDouble, 1e-9)
    assertEquals(0.0, tr('V, 'A).toDouble, 1e-9)
    assertEquals(0.0, tr('Z, 'A).toDouble, 1e-9)

    assertEquals(0.0, tr('A, 'D).toDouble, 1e-9)
    assertEquals(0.0, tr('D, 'D).toDouble, 1e-9)
    assertEquals(1.0, tr('N, 'D).toDouble, 1e-9)
    assertEquals(0.0, tr('V, 'D).toDouble, 1e-9)
    assertEquals(0.0, tr('Z, 'D).toDouble, 1e-9)

    assertEquals(0.0, tr('A, 'N).toDouble, 1e-9)
    assertEquals(0.0, tr('D, 'N).toDouble, 1e-9)
    assertEquals(0.0, tr('N, 'N).toDouble, 1e-9)
    assertEquals(0.6666666667, tr('V, 'N).toDouble, 1e-9)
    assertEquals(0.3333333333, tr('Z, 'N).toDouble, 1e-9)

    assertEquals(0.0, tr('A, 'V).toDouble, 1e-9)
    assertEquals(0.5, tr('D, 'V).toDouble, 1e-9)
    assertEquals(0.0, tr('N, 'V).toDouble, 1e-9)
    assertEquals(0.0, tr('V, 'V).toDouble, 1e-9)
    assertEquals(0.5, tr('Z, 'V).toDouble, 1e-9)

    assertEquals(0.0, tr('A, 'Z).toDouble, 1e-9)
    assertEquals(0.0, tr('D, 'Z).toDouble, 1e-9)
    assertEquals(0.0, tr('N, 'Z).toDouble, 1e-9)
    assertEquals(0.0, tr('V, 'Z).toDouble, 1e-9)
    assertEquals(0.0, tr('Z, 'Z).toDouble, 1e-9)

    assertEquals(1.0, em("<S>", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("a", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("cat", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("chases", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("the", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("dog", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("walks", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("man", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("runs", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("bird", 'A).toDouble, 1e-9)
    assertEquals(0.0, em("<E>", 'A).toDouble, 1e-9)

    assertEquals(0.0, em("<S>", 'D).toDouble, 1e-9)
    assertEquals(0.1666666667, em("a", 'D).toDouble, 1e-9)
    assertEquals(0.0, em("cat", 'D).toDouble, 1e-9)
    assertEquals(0.0, em("chases", 'D).toDouble, 1e-9)
    assertEquals(0.8333333333, em("the", 'D).toDouble, 1e-9)
    assertEquals(0.0, em("dog", 'D).toDouble, 1e-9)
    assertEquals(0.0, em("walks", 'D).toDouble, 1e-9)
    assertEquals(0.0, em("man", 'D).toDouble, 1e-9)
    assertEquals(0.0, em("runs", 'D).toDouble, 1e-9)
    assertEquals(0.0, em("bird", 'D).toDouble, 1e-9)
    assertEquals(0.0, em("<E>", 'D).toDouble, 1e-9)

    assertEquals(0.0, em("<S>", 'N).toDouble, 1e-9)
    assertEquals(0.0, em("a", 'N).toDouble, 1e-9)
    assertEquals(0.1666666667, em("cat", 'N).toDouble, 1e-9)
    assertEquals(0.0, em("chases", 'N).toDouble, 1e-9)
    assertEquals(0.0, em("the", 'N).toDouble, 1e-9)
    assertEquals(0.5, em("dog", 'N).toDouble, 1e-9)
    assertEquals(0.0, em("walks", 'N).toDouble, 1e-9)
    assertEquals(0.3333333333, em("man", 'N).toDouble, 1e-9)
    assertEquals(0.0, em("runs", 'N).toDouble, 1e-9)
    assertEquals(0.0, em("bird", 'N).toDouble, 1e-9)
    assertEquals(0.0, em("<E>", 'N).toDouble, 1e-9)

    assertEquals(0.0, em("<S>", 'V).toDouble, 1e-9)
    assertEquals(0.0, em("a", 'V).toDouble, 1e-9)
    assertEquals(0.0, em("cat", 'V).toDouble, 1e-9)
    assertEquals(0.25, em("chases", 'V).toDouble, 1e-9)
    assertEquals(0.0, em("the", 'V).toDouble, 1e-9)
    assertEquals(0.0, em("dog", 'V).toDouble, 1e-9)
    assertEquals(0.5, em("walks", 'V).toDouble, 1e-9)
    assertEquals(0.0, em("man", 'V).toDouble, 1e-9)
    assertEquals(0.25, em("runs", 'V).toDouble, 1e-9)
    assertEquals(0.0, em("bird", 'V).toDouble, 1e-9)
    assertEquals(0.0, em("<E>", 'V).toDouble, 1e-9)

    assertEquals(0.0, em("<S>", 'Z).toDouble, 1e-9)
    assertEquals(0.0, em("a", 'Z).toDouble, 1e-9)
    assertEquals(0.0, em("cat", 'Z).toDouble, 1e-9)
    assertEquals(0.0, em("chases", 'Z).toDouble, 1e-9)
    assertEquals(0.0, em("the", 'Z).toDouble, 1e-9)
    assertEquals(0.0, em("dog", 'Z).toDouble, 1e-9)
    assertEquals(0.0, em("walks", 'Z).toDouble, 1e-9)
    assertEquals(0.0, em("man", 'Z).toDouble, 1e-9)
    assertEquals(0.0, em("runs", 'Z).toDouble, 1e-9)
    assertEquals(0.0, em("bird", 'Z).toDouble, 1e-9)
    assertEquals(1.0, em("<E>", 'Z).toDouble, 1e-9)

  }

  def assertEquals2dArray(expected: Array[Array[Double]], result: Array[Array[Double]], scale: Int) {
    //    println("\n" + g(expected, result))
    if (expected.size != result.size)
      fail("\n" + g(expected, result, scale))
    for (((a, b), i) <- (expected zip result).zipWithIndex) {
      if (a.size != b.size)
        fail("\n" + g(expected, result, scale))
      for (((x, y), j) <- (a zip b).zipWithIndex) {
        assertTrue(f"[$i,$j]: $x != $y\n" + g(expected, result, scale), x.isNaN || abs(x - y) < pow(0.1, scale))
      }
    }
  }

  def f(a: Array[Array[Double]], scale: Int) = {
    val m1 = a.map(_.map(v => if (v == NaN) "" else f"%%.${scale}f".format(v)))
    val maxl = if (scale > 0) scale + 2 else m1.flatten.map(_.length).max
    m1.map(_.map(_.padRight(maxl)).mkString("[", ", ", "]")).mkString("\n")
  }
  def g(ex: Array[Array[Double]], re: Array[Array[Double]], scale: Int) =
    sideBySideStrings(1, "expected:", f(ex, scale), "   ", "result:", f(re, scale))

}
