package dhg.ccg.parse.inf

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
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
import scalaz.{ Ordering => _, _ }
import Scalaz._

class InfCatPriorTests {

  val s = cat"s".asInstanceOf[AtomCat]
  val np = cat"np".asInstanceOf[AtomCat]
  val n = cat"n".asInstanceOf[AtomCat]

  @Test
  def test_apply {

    val catprior = new SimpleInfCatPrior(
      allPunc = Set(cat".", cat",", cat";", cat":", cat"LRB", cat"RRB").map(_.asInstanceOf[PuncCat]), puncDist = new SimpleLogProbabilityDistribution(Map(
        cat"." -> 0.4,
        cat"," -> 0.3,
        cat";" -> 0.15,
        cat":" -> 0.10,
        cat"LRB" -> 0.03,
        cat"RRB" -> 0.02)
        .mapt((c, p) => c.asInstanceOf[PuncCat] -> LogDouble(p))
        .normalizeValues.mapVals(LogDouble(_))),
      pPunc = 0.12,
      allAtoms = Set(s, np, n),
      pAtom = new SimpleLogProbabilityDistribution[AtomCat](Map(
        s -> 0.5,
        np -> 0.2,
        n -> 0.3).mapVals(LogDouble(_))),
      pTerm = 0.8,
      pMod = 0.23,
      pFwd = 0.55)

    assertLogEquals(LogDouble(0.12 * 0.4), catprior(cat"."))
    val `p S` = 0.8 * 0.5
    val `p NP` = 0.8 * 0.2
    val `p N` = 0.8 * 0.3
    val `p //` = (1 - 0.8) * (1 - 0.23) * 0.55
    val `p \\` = (1 - 0.8) * (1 - 0.23) * (1 - 0.55)
    val `p m//` = (1 - 0.8) * 0.23 * 0.55
    val `p m\\` = (1 - 0.8) * 0.23 * (1 - 0.55)
    assertLogEquals(LogDouble((1 - 0.12) * `p N`), catprior(n))
    assertLogEquals(LogDouble((1 - 0.12) * `p S`), catprior(s))
    val `p (S \\ N)` = `p S` * `p \\` * `p N`
    assertLogEquals(LogDouble((1 - 0.12) * `p (S \\ N)`), catprior(s \ n))
    val `p (N \\ N)` = `p N` * `p m\\` + `p N` * `p \\` * `p N`
    assertLogEquals(LogDouble((1 - 0.12) * `p (S \\ N)`), catprior(s \ n))
    val `p (S \\ N) // NP` = `p (S \\ N)` * `p //` * `p NP`
    assertLogEquals(LogDouble((1 - 0.12) * `p (S \\ N) // NP`), catprior((s \ n) / np))
    val `p (S \\ N) m// (S \\ N)` = `p (S \\ N)` * `p m//` + `p (S \\ N)` * `p //` * `p (S \\ N)`
    assertLogEquals(LogDouble((1 - 0.12) * `p (S \\ N) m// (S \\ N)`), catprior((s \ n) / (s \ n)))
    val `p ((S \\ N) // NP) m\\ ((S \\ N) // NP)` = `p (S \\ N) // NP` * `p m\\` + `p (S \\ N) // NP` * `p \\` * `p (S \\ N) // NP`
    assertLogEquals(LogDouble((1 - 0.12) * `p ((S \\ N) // NP) m\\ ((S \\ N) // NP)`), catprior(((s \ n) / np) \ ((s \ n) / np)))
  }

  @Test
  def test_allAbove_1 {

    val catprior = new SimpleInfCatPrior(
      allPunc = Set(cat".", cat",", cat";", cat":", cat"LRB", cat"RRB").map(_.asInstanceOf[PuncCat]),
      puncDist = new SimpleLogProbabilityDistribution(Map(
        cat"." -> 0.4,
        cat"," -> 0.3,
        cat";" -> 0.15,
        cat":" -> 0.10,
        cat"LRB" -> 0.03,
        cat"RRB" -> 0.02)
        .mapt((c, p) => c.asInstanceOf[PuncCat] -> LogDouble(p)).normalizeValues),
      pPunc = 0.12,
      allAtoms = Set(s, np, n),
      pAtom = new SimpleLogProbabilityDistribution[AtomCat](Map(
        s -> 0.5,
        np -> 0.2,
        n -> 0.3).mapVals(LogDouble(_))),
      pTerm = 0.8,
      pMod = 0.23,
      pFwd = 0.55)

    val sortedCats = catprior.allAbove(LogDouble(1.866849345858984E-6), 6)
    //sortedCats foreach println
    //val sortedCats = catprior.allAbove(LogDouble(1.5E-6), 6)
    sortedCats.sliding(2).foreach { case Vector((_, a), (_, b)) => assert(a >= b) }

    println(f"number of cats above: ${sortedCats.size}")

    val catProbs = sortedCats.toMap

    assertLogEquals(LogDouble(0.12 * 0.4), catProbs(cat"."))
    val `p S` = 0.8 * 0.5
    val `p NP` = 0.8 * 0.2
    val `p N` = 0.8 * 0.3
    val `p //` = (1 - 0.8) * (1 - 0.23) * 0.55
    val `p \\` = (1 - 0.8) * (1 - 0.23) * (1 - 0.55)
    val `p m//` = (1 - 0.8) * 0.23 * 0.55
    val `p m\\` = (1 - 0.8) * 0.23 * (1 - 0.55)
    assertLogEquals(LogDouble((1 - 0.12) * `p N`), catProbs(n))
    assertLogEquals(LogDouble((1 - 0.12) * `p S`), catProbs(s))
    val `p (S \\ N)` = `p S` * `p \\` * `p N`
    assertLogEquals(LogDouble((1 - 0.12) * `p (S \\ N)`), catProbs(s \ n))
    val `p (N \\ N)` = `p N` * `p m\\` + `p N` * `p \\` * `p N`
    assertLogEquals(LogDouble((1 - 0.12) * `p (S \\ N)`), catProbs(s \ n))
    val `p (S \\ N) // NP` = `p (S \\ N)` * `p //` * `p NP`
    assertLogEquals(LogDouble((1 - 0.12) * `p (S \\ N) // NP`), catProbs((s \ n) / np))
    val `p (S \\ N) m// (S \\ N)` = `p (S \\ N)` * `p m//` + `p (S \\ N)` * `p //` * `p (S \\ N)`
    assertLogEquals(LogDouble((1 - 0.12) * `p (S \\ N) m// (S \\ N)`), catProbs((s \ n) / (s \ n)))
    val `p ((S \\ N) // NP) m\\ ((S \\ N) // NP)` = `p (S \\ N) // NP` * `p m\\` + `p (S \\ N) // NP` * `p \\` * `p (S \\ N) // NP`
    assertLogEquals(LogDouble((1 - 0.12) * `p ((S \\ N) // NP) m\\ ((S \\ N) // NP)`), catProbs(((s \ n) / np) \ ((s \ n) / np)))

    println(" ----->>>> " + catProbs(((s \ n) / np) \ ((s \ n) / np)))
    println(" ----->>>> " + catprior(cat"""(((S\N)/NP)\((S\N)/NP))"""))
    println(" ----->>>> " + ((1 - 0.12) * `p ((S \\ N) // NP) m\\ ((S \\ N) // NP)`))

    val x1 = catprior.allAbove(LogDouble(1E-1), 6)
    val x2 = catprior.allAbove(LogDouble(1E-2), 6)
    val x3 = catprior.allAbove(LogDouble(1E-3), 6)
    val x4 = catprior.allAbove(LogDouble(1E-4), 6)
    val x5 = catprior.allAbove(LogDouble(1E-5), 6)
    val x6 = catprior.allAbove(LogDouble(1E-6), 6)
    val x7 = catprior.allAbove(LogDouble(1E-7), 6)
    val x8 = catprior.allAbove(LogDouble(1E-8), 6)
    val x18 = Vector(x1, x2, x3, x4, x5, x6, x7, x8)
    for ((y, i) <- x18.zipWithIndex) {
      y.sliding(2).foreach { case Vector((_, a), (_, b)) => assert(a >= b) }
      val xs = x18.take(i + 1)
      for (x <- xs) assert(y.take(x.size).toSet == x.toSet)
    }
  }

  @Test
  def test_aboveAll {

    val catprior = new SimpleInfCatPrior(
      allPunc = Set(cat".", cat",", cat";", cat":", cat"LRB", cat"RRB").map(_.asInstanceOf[PuncCat]), puncDist = new SimpleLogProbabilityDistribution(Vector(cat".", cat",", cat";", cat":", cat"LRB", cat"RRB").zipWithIndex.mapt((p, i) => p.asInstanceOf[PuncCat] -> math.pow(0.5, i)).toMap.normalizeValues.mapVals(LogDouble(_))),
      pPunc = 0.0,
      allAtoms = Set(s, np, n),
      pAtom = new SimpleLogProbabilityDistribution[AtomCat](Map(s -> 0.5, np -> 0.2, n -> 0.3).mapVals(LogDouble(_))),
      pTerm = 0.8,
      pMod = 0.1,
      pFwd = 0.55)

    def f(maxSize: Int, expectedNumDistinctCats: Int, expectedMaxCatSize: Int) = {
      val t = 1e-6
      val x2 = catprior.allAbove(LogDouble(t), maxSize).mapt((c, cp) => ((c, cp), catprior(c)))
      assertEquals(expectedNumDistinctCats, x2.map(_._1._1).distinct.size)
      assertEquals(expectedMaxCatSize, x2.toVector.sortBy(_._1).last._1._1.size)

      def allSubExpressions(c: Cat): Vector[Cat] = (c match {
        case a || b => allSubExpressions(a) ++ allSubExpressions(b)
        case a: AtomCat => Vector(a)
      }) :+ c
      val allsubs = x2.map(_._1._1).flatMap(allSubExpressions)
      val subcodegroups = allsubs.groupBy(_.toString).mapValues(_.map(c => Integer.toHexString(System.identityHashCode(c))))
      assertFalse(subcodegroups.exists(_._2.distinct.size > 1))
    }

    f(9, 3992, 8)
    f(8, 3992, 8)
    f(7, 3975, 6)
    f(6, 3975, 6)
    f(5, 3685, 5)
    f(4, 3289, 4)
    f(3, 237, 3)
    f(2, 21, 2)
    f(1, 3, 1)
  }

  def assertLogEquals(expected: LogDouble, actual: LogDouble) = {
    assertEquals(expected.toDouble, actual.toDouble, 1e-9)
  }

}
