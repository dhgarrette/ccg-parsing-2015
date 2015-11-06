package dhg.ccg.tag.learn

import org.junit.Test
import dhg.util._
import org.junit.Assert._
import dhg.ccg.tag.learn._
import dhg.ccg.prob._
import dhg.ccg.tag._
import org.apache.commons.math3.random.{ MersenneTwister, RandomGenerator }
import dhg.ccg.test.TestUtil.MockableRandomGenerator
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tagdict.SimpleTagDictionary

class FfbsHmmTests {

  @Test
  def test_toy_train_5iterations {
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

    val trInitializer = new TransitionInitializer[Symbol] {
      def fromKnownSupertagSets(sentences: Vector[Vector[(String, Set[Symbol])]], itd: TagDictionary[Symbol]) = {
        new Double2LogConditionalProbabilityDistributionAdapter(new SimpleConditionalExpProbabilityDistribution(
          Map(
            itd.startTag -> new SimpleExpProbabilityDistribution(Map('D -> 0.7, 'N -> 0.2, 'V -> 0.1)),
            'D -> new SimpleExpProbabilityDistribution(Map('D -> 0.1, 'N -> 0.7, 'V -> 0.1, itd.endTag -> 0.1)),
            'N -> new SimpleExpProbabilityDistribution(Map('D -> 0.1, 'N -> 0.3, 'V -> 0.4, itd.endTag -> 0.2)),
            'V -> new SimpleExpProbabilityDistribution(Map('D -> 0.3, 'N -> 0.2, 'V -> 0.1, itd.endTag -> 0.4)),
            itd.endTag -> ExpProbabilityDistribution.empty)))
      }
    }
    val emInitializer = new EmissionInitializer[Symbol] {
      def fromKnownSupertagSets(sentences: Vector[Vector[(String, Set[Symbol])]], initialTagdict: TagDictionary[Symbol]) = {
        new Double2LogConditionalProbabilityDistributionAdapter(new SimpleConditionalExpProbabilityDistribution(
          Map(
            'D -> new SimpleExpProbabilityDistribution(Map("the" -> 0.4, "a" -> 0.3, "dog" -> 0.1, "chases" -> 0.1, "runs" -> 0.1)),
            'N -> new SimpleExpProbabilityDistribution(Map("cat" -> 0.2, "man" -> 0.3, "dog" -> 0.2, "chases" -> 0.2, "runs" -> 0.1)),
            'V -> new SimpleExpProbabilityDistribution(Map("man" -> 0.2, "dog" -> 0.1, "walks" -> 0.2, "chases" -> 0.3, "runs" -> 0.2)),
            tagdict.startTag -> new SimpleExpProbabilityDistribution(Map(tagdict.startWord -> 1.0)),
            tagdict.endTag -> new SimpleExpProbabilityDistribution(Map(tagdict.endWord -> 1.0)))))
      }
    }

    class RandomGeneratorDoubleDisplay(rg: RandomGenerator) extends MockableRandomGenerator {
      var d = rg.nextDouble
      override def nextDouble = { val temp = d; d = rg.nextDouble; temp }
      override def toString = f"RG(next=$d%.2f)"
    }

    val rand = new RandomGeneratorDoubleDisplay(new MersenneTwister(0))

    val emt = new SimpleTypeSupervisedTaggerTrainer(new FfbsHmmTaggerTrainer(3, 2, 1, 1, new UnsmoothedTransitionDistributioner[Symbol](), new UnsmoothedEmissionDistributioner[Symbol](), rand), trInitializer, emInitializer)
    val hmm = emt.typesupTrain(sentences, tagdict).asInstanceOf[HmmTagger[Symbol]]

    val tr = hmm.transitions
    val em = hmm.emissions

    for (t1 <- tagdict.allTags.toVector.sortBy(_.toString))
      println(f"$t1 -> ${tagdict.allTags.toVector.sortBy(_.toString).mapTo(t2 => f"${tr(t2, t1).toDouble}%.2f").mkString(" ")}")
    for (t <- tagdict.allTags.toVector.sortBy(_.toString))
      println(f"$t -> ${tagdict.allWords.toVector.sortBy(_.toString).mapTo(w => f"${em(w, t).toDouble}%.2f").mkString(" ")}")

    ???

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

}
