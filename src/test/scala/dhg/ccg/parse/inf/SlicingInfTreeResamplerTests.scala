package dhg.ccg.parse.inf

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.test.TestUtil._
import dhg.ccg.parse.pcfg._
import dhg.util._
import org.apache.commons.math3.random.MersenneTwister
import dhg.ccg.parse.scg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.tagdict.SimpleStartEndTags
import org.apache.commons.math3.random.RandomGenerator
import dhg.ccg.tagdict.StartEndTags
import dhg.ccg.parse.scg.mcmc._
import scala.collection.parallel.immutable.ParVector
import dhg.ccg.math._
import dhg.ccg.parse.scg._
import scalaz._
import Scalaz._
import org.apache.commons.math3.random.SynchronizedRandomGenerator

class SlicingInfTreeResamplerTests {

  @Test
  def test {

    val A = cat"A".asInstanceOf[AtomCat]
    val B = cat"B".asInstanceOf[AtomCat]
    val C = cat"C".asInstanceOf[AtomCat]
    val D = cat"D".asInstanceOf[AtomCat]
    val E = cat"E".asInstanceOf[AtomCat]
    val F = cat"F".asInstanceOf[AtomCat]
    val X = cat"X".asInstanceOf[AtomCat]

    val S = cat"S".asInstanceOf[AtomCat]
    val NP = cat"NP".asInstanceOf[AtomCat]
    val N = cat"N".asInstanceOf[AtomCat]
    val PP = cat"PP".asInstanceOf[AtomCat]
    val STA = cat"<S>"
    val END = cat"<E>"
    val SE = new SimpleStartEndTags(STA, END)

    val s = S.asInstanceOf[AtomCat]
    val np = NP.asInstanceOf[AtomCat]
    val n = N.asInstanceOf[AtomCat]
    val pp = PP.asInstanceOf[AtomCat]

    val startWord = "<S>"
    val startTag = STA
    val endWord = "<E>"
    val endTag = END

    def main(args: Array[String]): Unit = {
      type Word = String
      type Tag = Cat

      val Det: Set[Cat] = Set(
        np / n)
      val Adj: Set[Cat] = Set(
        n / n)
      val IV: Set[Cat] = Set(
        s \ np,
        (s \ np) / pp)
      val TV: Set[Cat] = Set(
        (s \ np) / np,
        ((s \ np) / pp) / np,
        (((s \ np) / pp) / pp) / np)
      val N: Set[Cat] = Set(
        n)
      val NNP: Set[Cat] = Set(
        np,
        np / pp,
        (np / pp) / pp)
      val Prep: Set[Cat] = Set(
        pp / np)

      val tagdict = SimpleTagDictionary.apply(
        Map[Word, Set[Tag]](
          "the" -> Det,
          "a" -> Det,
          "big" -> Adj,
          "man" -> N,
          "dog" -> N,
          "dogs" -> N,
          "cat" -> N,
          "cats" -> N,
          "telescope" -> N,
          "saw" -> (IV | TV),
          "walked" -> (IV | TV),
          "chase" -> TV,
          "run" -> IV,
          "ran" -> IV,
          "John" -> NNP,
          "Mary" -> NNP,
          "with" -> Prep,
          "nnp" -> Set(n, np)),
        "<S>", cat"<S>", "<E>", cat"<E>")

      val B1 = (B / C) \ A
      val B2 = (B \ A) / C
      val X = C / D
      val XS: Vector[(Word, Set[Cat])] = Vector(("s", Set(S / E)), ("x", Set(B / C)), ("y", Set(X)), ("z", Set((E \ (B / C)) \ X)))
      val sentences: Vector[Vector[(Word, Set[Cat])]] = Vector[Vector[(Word, Set[Cat])]](
        XS, XS, XS, XS, XS, XS, XS,
        Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B1)), ("c", Set(C / D)), ("d", Set(D))),
        Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B2)), ("c", Set(C / D)), ("d", Set(D))),
        Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B1, B2)), ("c", Set(C / D)), ("d", Set(D))),
        Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B1)), ("c", Set(C / D)), ("d", Set(D))))

      //    val B1 = (B / C) \ A
      //    val B2 = (B \ A) / C
      //    val X = F//C / D
      //    val XS: Vector[(Word, Set[Cat])] = Vector(("s", Set(S/E)), ("x", Set(B / C)), ("y", Set(X)), ("z", Set((E \ (B / C)) \ X)))
      //    val sentences: Vector[Vector[(Word, Set[Cat])]] = Vector(
      //      XS,XS,XS,XS,XS,XS,XS,
      //      Vector(("s", Set(S/B)), ("a", Set(A)), ("b", Set(B1)), ("c", Set(C / D)), ("d", Set(D))),
      //      Vector(("s", Set(S/B)), ("a", Set(A)), ("b", Set(B2)), ("c", Set(C / D)), ("d", Set(D))),
      //      Vector(("s", Set(S/B)), ("a", Set(A)), ("b", Set(B1, B2)), ("c", Set(C / D)), ("d", Set(D))),
      //      Vector(("s", Set(S/B)), ("a", Set(A)), ("b", Set(B1)), ("c", Set(C / D)), ("d", Set(D))))

      val rootSet: Set[Cat] = Set(S)

      val infCatPrior = new SimpleInfCatPrior(
        allPunc = Set(cat".", cat",", cat";", cat":", cat"LRB", cat"RRB").map(_.asInstanceOf[PuncCat]), puncDist = new SimpleLogProbabilityDistribution(Vector(cat".", cat",", cat";", cat":", cat"LRB", cat"RRB").zipWithIndex.mapt((p, i) => p.asInstanceOf[PuncCat] -> math.pow(0.5, i)).toMap.normalizeValues.mapVals(LogDouble(_))), pPunc = 0.1,
        Set(S, A, B, C, D, E),
        new SimpleLogProbabilityDistribution(Map(S -> LogDouble(1 / 6.0), A -> LogDouble(1 / 6.0), B -> LogDouble(1 / 6.0), C -> LogDouble(1 / 6.0), D -> LogDouble(1 / 6.0), E -> LogDouble(1 / 6.0)).normalizeValues),
        pTerm = 0.8, pMod = 0.1, pFwd = 0.5,
        tagdict.excludedTags)

      val pcfgTreeSampler = new SimplePcfgTreeSampler(new SimplePcfgInsideChartBuilder())

      val rules = Vector(FA, BA, N2NP)
      val rand = new SynchronizedRandomGenerator(new MersenneTwister)

      //      val infTreeResampler = new SlicingInfTreeResampler(rules, pcfgTreeSampler, infCatPrior, rootSet, DirSampler, qBetaA = LogDouble(0.1), qBetaB = LogDouble(0.1), rand)

      //      infTreeResampler.resampleTrees(currentTrees, knownRootProds, knownBinyProds, knownUnryProds, rootDist, prodDist, prodMixes, infCatPriorsDesc, binyScaleFactors, unryScaleFactors, sentences, annotations)

    }
  }

}
