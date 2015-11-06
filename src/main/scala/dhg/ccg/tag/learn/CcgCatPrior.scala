package dhg.ccg.tag.learn

import dhg.util._
import math.pow
import scalaz._
import scalaz.Scalaz._
import dhg.ccg.cat._
import dhg.ccg.prob._
import dhg.ccg.tagdict.TagDictionary

trait TagPriorInitializer[Tag] {
  type Word = String
  final def fromRaw(sentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Tag]): LogProbabilityDistribution[Tag] = {
    val tagdict = initialTagdict.withWords(sentences.flatten.toSet)
    fromKnownSupertagSets(sentences.map(_.mapTo(tagdict.entries.getOrElse(_, Set.empty))), tagdict)
  }

  /**
   * Each token associated with its set of possible supertags, if such a set is KNOWN; if the set is unknown, it will be EMPTY.
   */
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]): LogProbabilityDistribution[Tag]
}

class UniformTagPriorInitializer[Tag] extends TagPriorInitializer[Tag] {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]) = {
    new LaplaceLogProbabilityDistribution(Map(), Some(initialTagdict.allTagsSE), Some(initialTagdict.excludedTags), LogDouble(1.0), totalAddition = LogDouble(0.0))
  }
  override def toString = f"UniformTagPriorInitializer()"
}

class CatComplexityInitializer extends TagPriorInitializer[Cat] {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Cat])]], initialTagdict: TagDictionary[Cat]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.map(_._1).toSet).withTags(sentences.flatten.flatMap(_._2).toSet)
    new CatComplexityLogProbabilityDistribution(tagdict)
  }

  override def toString = f"CatComplexityInitializer()"
}

/**
 * Baldridge (2008)
 *
 * Only used by above initializer
 */
class CatComplexityLogProbabilityDistribution(
  tagdict: TagDictionary[Cat])
    extends LogProbabilityDistribution[Cat] {

  private[this] val z = tagdict.allTagsSE.sumBy(1.0 / _.complexity)

  def apply(c: Cat): LogDouble = if (tagdict.excludedTags(c)) LogDouble.zero else LogDouble((1.0 / c.complexity) / z)
  def sample(): Cat = ???
  def defaultProb: LogDouble = LogDouble.zero
}

class CheatingTagPriorInitializer[Tag](supervisedCorpus: Vector[Vector[(String, Tag)]], lambda: LogDouble) extends TagPriorInitializer[Tag] {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]) = {
    val supvTagCounts = supervisedCorpus.flatten.map(_._2).counts +
      (initialTagdict.startTag -> supervisedCorpus.size) +
      (initialTagdict.endTag -> supervisedCorpus.size)
    new LaplaceLogProbabilityDistribution(supvTagCounts.mapVals(LogDouble(_)), Some(initialTagdict.allTagsSE ++ supvTagCounts.keys), Some(initialTagdict.excludedTags), lambda)
  }
  override def toString = f"CheatingTagPriorInitializer()"
}

//

trait AtomCatDistInitializer {
  type Word = String
  final def fromRaw(sentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Cat]): LogProbabilityDistribution[AtomCat] = {
    val tagdict = initialTagdict.withWords(sentences.flatten.toSet)
    fromKnownSupertagSets(sentences.map(_.mapTo(tagdict.entries.getOrElse(_, Set.empty))), tagdict)
  }

  /**
   * Each token associated with its set of possible supertags, if such a set is KNOWN; if the set is unknown, it will be EMPTY.
   */
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Cat])]], initialTagdict: TagDictionary[Cat]): LogProbabilityDistribution[AtomCat]
}

class UniformAtomCatDistInitializer extends AtomCatDistInitializer {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Cat])]], initialTagdict: TagDictionary[Cat]) = {
    def atoms(c: Cat): Set[AtomCat] = c match { case a: AtomCat => Set(a); case l || r => atoms(l) | atoms(r) }
    new LaplaceLogProbabilityDistribution[AtomCat](Map(), Some(initialTagdict.allTagsSE.flatMap(atoms)), Some(initialTagdict.excludedTags), LogDouble(1.0))
  }
  override def toString = f"UniformAtomCatDistInitializer()"
}

class TagdictInformedAtomCatDistInitializer(atomLambda: Double) extends AtomCatDistInitializer {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Cat])]], initialTagdict: TagDictionary[Cat]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.map(_._1).toSet).withTags(sentences.flatten.flatMap(_._2).toSet)

    def atomCounts(c: Cat): Map[AtomCat, Int] = c match { case a: AtomCat => Map(a -> 1); case l || r => atomCounts(l) |+| atomCounts(r); case _ => Map.empty }

    val C_k = sentences.flatten.flatMap {
      case (w, ts) =>
        val partialCounts = 1.0 / ts.size
        ts.mapTo(t => partialCounts)
    }.groupByKey.mapVals(_.sum) +
      (tagdict.startTag -> sentences.size.toDouble) +
      (tagdict.endTag -> sentences.size.toDouble)

    //    val C = sentences.flatten.map(_._1).counts // C(w)
    //    val C_k = tagdict.entries.toVector.flatMap {
    //      case (w, ts) =>
    //        val partialCounts = C.getOrElse(w, 0) / ts.size.toDouble
    //        ts.mapTo(t => partialCounts)
    //    }.groupByKey.mapVals(_.sum) +
    //      (tagdict.startTag -> sentences.size.toDouble) +
    //      (tagdict.endTag -> sentences.size.toDouble)

    val estAtomCounts = C_k.toVector.flatMap { case (t, c) => atomCounts(t).mapVals(_ * c) }.groupByKey.mapVals(xs => LogDouble(xs.sum))

    new LaplaceLogProbabilityDistribution[AtomCat](estAtomCounts, Some(tagdict.allTagsSE.flatMap(t => atomCounts(t).keys)), Some(tagdict.excludedTags), LogDouble(atomLambda))
  }
  override def toString = f"TagdictInformedAtomCatDistInitializer(atomLambda=${atomLambda.toDouble})"
}

/**
 * For use when pAtom is given
 */
class KnownAtomCatDistInitializer(atomDist: LogProbabilityDistribution[AtomCat]) extends AtomCatDistInitializer {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Cat])]], initialTagdict: TagDictionary[Cat]) = {
    atomDist
  }
  override def toString = f"KnownAtomCatDistInitializer($atomDist)"
}

@deprecated("Use CatgramInfCatPriorInitializer. This one doesn't handle punctuation categories.","")
class CatgramCatPriorInitializer(
  pAtomInit: AtomCatDistInitializer,
  pTerm: Double, // must be > 0.5 to ensure finiteness
  pMod: Double, //  probability mass devoted to modifier categories
  pFwd: Double)
    extends TagPriorInitializer[Cat] {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Cat])]], initialTagdict: TagDictionary[Cat]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.map(_._1).toSet).withTags(sentences.flatten.flatMap(_._2).toSet)
    val pAtom = pAtomInit.fromKnownSupertagSets(sentences, tagdict)
    new CatgramCatPriorLogProbabilityDistribution(pAtom, pTerm, pMod, pFwd, tagdict.allTagsSE, if (tagdict.excludedTags.nonEmpty) Some(tagdict.excludedTags) else None)
  }

  override def toString = f"CatgramCatPriorInitializer($pAtomInit, pTerm=$pTerm%.3f, pMod=$pMod%.3f, pFwd=$pFwd%.3f)"
}

/**
 * Used only by the above initializer
 */
@deprecated("Use SimpleInfCatPrior. This one doesn't handle punctuation categories.","")
class CatgramCatPriorLogProbabilityDistribution(
  pAtom: LogProbabilityDistribution[AtomCat],
  pTerm: Double,
  pMod: Double,
  pFwd: Double,
  knownCats: Set[Cat],
  excludedCats: Option[Cat => Boolean] = None)
    extends LogProbabilityDistribution[Cat] {

  import CatgramCatPriorLogProbabilityDistribution._

  private[this] val pCache = knownCats.filterNot(excludedCats.getOrElse(Set.empty)).mapTo(g).toMap

  def apply(c: Cat): LogDouble = {
    if (excludedCats.exists(_.apply(c))) LogDouble.zero
    else pCache.getOrElse(c, g(c))
  }

  private[this] def g(c: Cat): LogDouble = {
    c match {
      case atom: AtomCat => LogDouble(+pTerm) * pAtom(atom)
      case FMod(x) /* */ => (LogDouble(~pTerm) * LogDouble(~pMod) * LogDouble(+pFwd) * g(x) * g(x)) + (LogDouble(~pTerm) * LogDouble(+pMod) * LogDouble(+pFwd) * g(x))
      case l / r /*   */ => (LogDouble(~pTerm) * LogDouble(~pMod) * LogDouble(+pFwd) * g(l) * g(r))
      case BMod(x) /* */ => (LogDouble(~pTerm) * LogDouble(~pMod) * LogDouble(~pFwd) * g(x) * g(x)) + (LogDouble(~pTerm) * LogDouble(+pMod) * LogDouble(~pFwd) * g(x))
      case l \ r /*   */ => (LogDouble(~pTerm) * LogDouble(~pMod) * LogDouble(~pFwd) * g(l) * g(r))
    }
  }

  def sample(): Cat = ???
  def defaultProb: LogDouble = sys.error("`defaultProb` not available on CatgramCatPriorLogProbabilityDistribution")
}

class NormalizingCatgramCatPriorInitializer(
  pAtomInit: AtomCatDistInitializer,
  pTerm: Double, // must be > 0.5 to ensure finiteness
  pMod: Double, //  probability mass devoted to modifier categories
  pFwd: Double)
    extends TagPriorInitializer[Cat] {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Cat])]], initialTagdict: TagDictionary[Cat]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.map(_._1).toSet).withTags(sentences.flatten.flatMap(_._2).toSet)
    val pAtom = pAtomInit.fromKnownSupertagSets(sentences, tagdict)
    new NormalizingCatgramCatPriorLogProbabilityDistribution(tagdict, pAtom, pTerm, pMod, pFwd)
  }

  override def toString = f"NormalizingCatgramCatPriorInitializer($pAtomInit, pTerm=$pTerm%.3f, pMod=$pMod%.3f, pFwd=$pFwd%.3f)"
}

/**
 * Used only by the above initializer
 */
class NormalizingCatgramCatPriorLogProbabilityDistribution(
  tagdict: TagDictionary[Cat],
  pAtom: LogProbabilityDistribution[AtomCat],
  pTerm: Double,
  pMod: Double,
  pFwd: Double)
    extends LogProbabilityDistribution[Cat] {

  import CatgramCatPriorLogProbabilityDistribution._

  private[this] val gCache = tagdict.allTagsSE.mapTo(g).toMap
  private[this] val z = gCache.values.sum
  private[this] val pCache = gCache.mapVals(_ / z)

  def apply(c: Cat): LogDouble = {
    if (tagdict.excludedTags(c)) LogDouble.zero
    else pCache.getOrElse(c, g(c) / z)
  }

  private[this] def g(c: Cat): LogDouble = {
    c match {
      case atom: AtomCat => LogDouble(+pTerm) * pAtom(atom)
      case FMod(x) /* */ => (LogDouble(~pTerm) * LogDouble(~pMod) * LogDouble(+pFwd) * g(x) * g(x)) + (LogDouble(~pTerm) * LogDouble(+pMod) * LogDouble(+pFwd) * g(x))
      case l / r /*   */ => (LogDouble(~pTerm) * LogDouble(~pMod) * LogDouble(+pFwd) * g(l) * g(r))
      case BMod(x) /* */ => (LogDouble(~pTerm) * LogDouble(~pMod) * LogDouble(~pFwd) * g(x) * g(x)) + (LogDouble(~pTerm) * LogDouble(+pMod) * LogDouble(~pFwd) * g(x))
      case l \ r /*   */ => (LogDouble(~pTerm) * LogDouble(~pMod) * LogDouble(~pFwd) * g(l) * g(r))
    }
  }

  def sample(): Cat = ???
  def defaultProb: LogDouble = LogDouble.zero
}

object CatgramCatPriorLogProbabilityDistribution {
  implicit final class DoubleProb(val v: Double) extends AnyVal { def unary_~ = 1 - v }
  object FMod { def unapply(c: NonPuncCat): Option[NonPuncCat] = c match { case l / r if l == r => Some(l); case _ => None } }
  object BMod { def unapply(c: NonPuncCat): Option[NonPuncCat] = c match { case l \ r if l == r => Some(l); case _ => None } }
}
