package dhg.ccg.parse.inf

import dhg.util._
import dhg.ccg.tag._
import dhg.ccg.prob._
import dhg.ccg.cat._
import scala.collection.breakOut
import scala.collection.mutable
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tagdict.SimpleTagDictionary
import scalaz.{ Ordering => _, _ }
import Scalaz._
import scala.annotation.tailrec
import dhg.ccg.tag.learn.AtomCatDistInitializer
import dhg.ccg.tag.learn.TagPriorInitializer
import dhg.ccg.tag.learn.CatgramCatPriorLogProbabilityDistribution
import scala.util.control.Breaks._
import scala.collection.immutable.VectorBuilder

trait InfCatPrior extends LogProbabilityDistribution[Cat] {
  /**
   * Return all categories with probabilities above the threshold, sorted
   * highest-to-lowest probability.
   */
  def allAbove(threshold: LogDouble, maxSize: Int = Int.MaxValue): Vector[(Cat, LogDouble)]
}

/**
 *
 * Punctuation:
 *   Punctuation cannot combine with other categories.
 *
 *      ,    59995
 *      .    48453
 *      :     2412
 *      RRB   1667
 *      ;     1459
 *      LRB    553
 *
 *   114541.0/1148426 = 0.0997    (punct rate)
 *   
 *  
 *  
 *  
 *  def decompose(c: NonPuncCat): Vector[String] = {
 *      c match {
 *        case c: AtomCat   => Vector("t")
 *        case a||b if a==b => Vector("m") ++ decompose(a) ++ decompose(b)
 *        case a||b         => Vector("n") ++ decompose(a) ++ decompose(b)
 *        case _: ConjCat   => Vector()
 *      }
 *  }
 *  
 *  val en = EnglishCcgTreeBankReader()
 *  val cats = en.fullCorpusDONTUSE.flatMap(_.allCatsVec).toVector
 *  val numCats = cats.size
 *  val nonPunc = cats.collect{case c: NonPuncCat => c}
 *  val numPunc = numCats - nonPunc.size
 *  val decomp = nonPunc.flatMap(decompose).counts  
 *  
 *  English:
 *  
 *  cats:          2411565
 *  punc:           114542     0.05
 *  nonpunc:
 *    decomp:      6168507     
 *      atoms:     4211986     0.68
 *      nonterm:   1956521
 *        mod:      613155     0.31 of nonterm;  0.10 of decomp
 *        nonmod:  1343366     
 *  
 *  
 *  
 *  def r(x: Double, m: Double, t: Double, a: Double): Stream[Double] = {val y = a+(x*(1-t)*m); y #:: r(x*(1-t)*(1-m), m,t,y)}
 *  r(1,0.20,0.7,0)(50)  = 0.07894736842105265
 *  r(1,0.25,0.7,0)(50)  = 0.09677419354838711
 *  r(1,0.26,0.7,0)(50)  = 0.10025706940874035
 *  r(1,0.30,0.7,0)(50)  = 0.11392405063291139
 *  
 *  val m = 0.2
 *  val t = 0.7
 *  
 *  x=1
 *  |--------------term--------------------|-----mod-----|------------nonmod-----------|
 *                  0.7                     x*(1-0.7)*0.2     x2=x*(1-0.7)*(1-0.2)
 *                                                       |---term---|-mod-|--nonmod--|
 *  
 *  
 *
 */
class SimpleInfCatPrior(
  allPunc: Set[PuncCat], puncDist: LogProbabilityDistribution[PuncCat], pPunc: Double,
  allAtoms: Set[AtomCat], pAtom: LogProbabilityDistribution[AtomCat],
  pTerm: Double, pMod: Double, pFwd: Double,
  excludedTags: Set[Cat] = Set.empty,
  deletionProb: Double = 0.0,
  startEndProb: Double = 0.0)
    extends InfCatPrior {

  import dhg.ccg.tag.learn.CatgramCatPriorLogProbabilityDistribution._

  val logStartEndProb = LogDouble(startEndProb)
  val logDeletionProb = LogDouble(deletionProb)
  val pNormalCat = ~(2 * startEndProb + 2 * deletionProb)
  val logPPunc = LogDouble(pPunc * pNormalCat)
  val pNonPunc = LogDouble(~pPunc * pNormalCat)
  val logPTerm = LogDouble(pTerm)
  val pFwdNtNmod = LogDouble(~pTerm * ~pMod * +pFwd)
  val pBkdNtNmod = LogDouble(~pTerm * ~pMod * ~pFwd)
  val pFwdNtMod = LogDouble(~pTerm * +pMod * +pFwd)
  val pBkdNtMod = LogDouble(~pTerm * +pMod * ~pFwd)

  
  def apply(c: Cat): LogDouble = {
    c match {
      case StartCat | EndCat => logStartEndProb
      case DeleteFromLeftCat | DeleteFromRightCat => logDeletionProb
      case c: PuncCat if (allPunc(c)) => logPPunc * puncDist(c)
      case c: NonPuncCat => pNonPunc * p(c)
      case BadCat => sys.error("uhhh.... no")
    }
  }

  private[this] def p(c: NonPuncCat): LogDouble = c match {
    case _ if excludedTags(c) => LogDouble.zero
    case atom: AtomCat => logPTerm * pAtom(atom)
    case FMod(x) =>
      val px = p(x)
      (pFwdNtNmod * px * px) + (pFwdNtMod * px)
    case l / r =>
      pFwdNtNmod * p(l) * p(r)
    case BMod(x) =>
      val px = p(x)
      (pBkdNtNmod * px * px) + (pBkdNtMod * px)
    case l \ r =>
      pBkdNtNmod * p(l) * p(r)
  }

  /**
   * Return all categories with probabilities above the threshold, sorted
   * highest-to-lowest probability.
   *
   * Process in layers.  Keep lists ordered largest-to-smallest so that when
   * the probability drops below the threshold, we can break to avoid
   * pointless computation.
   */
  def allAbove(threshold: LogDouble, maxSize: Int): Vector[(Cat, LogDouble)] = {
    val startTime = System.currentTimeMillis()
    @tailrec def f(existingBySize: Vector[(Int, Vector[(NonPuncCat, LogDouble)])], recentBySize: Vector[(Int, Vector[(NonPuncCat, LogDouble)])], scaledThreshold: LogDouble, fi: Int): Vector[(NonPuncCat, LogDouble)] = {
      println(f"$fi: recent.size=${recentBySize.sumBy(_._2.size)}")

      val stuff =
        existingBySize.flatMap(_._2).mapToVal(recentBySize) ++
          recentBySize.flatMap(_._2).mapToVal(recentBySize) ++
          recentBySize.flatMap(_._2).mapToVal(existingBySize)
      val newStuffFBs: Vector[(NonPuncCat, LogDouble)] = stuff.zipWithIndex.flatMap {
        case (((x, px), ys), xi) =>

          /**
           * ySizeItr, sorted from smallest to largest size;
           *   yItr, sorted from highest to lowest prob
           */
          @tailrec def g(ySizeItr: PeekableIterator[(Int, Iterator[(NonPuncCat, LogDouble)])], pMult: LogDouble, op: (NonPuncCat, NonPuncCat) => NonPuncCat, accum: VectorBuilder[(NonPuncCat, LogDouble)], gi: Int): (Vector[(NonPuncCat, LogDouble)], Int) = {
            if (ySizeItr.hasNext) {
              val Some((ySize, yItr)) = ySizeItr.peek
              if (x.size + ySize <= maxSize) {
                if (yItr.hasNext) {
                  val (y, py) = yItr.next()
                  if (x ne y) {
                    val p = pMult * py
                    if (p.logValue >= scaledThreshold.logValue) g(ySizeItr, pMult, op, accum += (op(x, y) -> p), gi + 1)
                    else { ySizeItr.next(); g(ySizeItr, pMult, op, accum, gi + 1) } // y has gone under the threshold, advance to the next yItr (and size)
                  }
                  else g(ySizeItr, pMult, op, accum, gi + 1) // x==y, skip this y
                }
                else { ySizeItr.next(); g(ySizeItr, pMult, op, accum, gi + 1) } // yItr has run out of elements, advance to the next yItr (and size)
              }
              else (accum.result(), gi) // ySize has gotten too big, we're done
            }
            else (accum.result(), gi) // ySizeItr is empty, we're done
          }

          val (fwds, fchecked) = g(new PeekableIterator(ys.iterator.mapVals(_.iterator)), pFwdNtNmod * px, _ / _, new VectorBuilder, 0)
          val (bkds, bchecked) = g(new PeekableIterator(ys.iterator.mapVals(_.iterator)), pBkdNtNmod * px, _ \ _, new VectorBuilder, 0)
          //println(f"  $xi/${stuff.size}: fchecked=$fchecked; bchecked=$bchecked    ${x}%-30s  ${px.toDouble}  (${px.logValue})")
          fwds ++ bkds
      }
      //      val newStuffMods = recentBySize.collect {
      //        case (ySize, ys) if ySize * 2 <= maxSize => ys.flatMap {
      //          case (x, px) =>
      //            val fMod: Vector[(Cat, LogDouble)] = { val p = pFwdNtNmod * px * px + LogDouble(~pTerm) * LogDouble(pMod) * LogDouble(+pFwd) * px; if (p.logValue >= threshold.logValue) Vector((x / x) -> p) else Vector.empty }
      //            val bMod: Vector[(Cat, LogDouble)] = { val p = pBkdNtNmod * px * px + LogDouble(~pTerm) * LogDouble(pMod) * LogDouble(~pFwd) * px; if (p.logValue >= threshold.logValue) Vector((x \ x) -> p) else Vector.empty }
      //            fMod ++ bMod
      //        }
      //      }.flatten

      val newStuffMods = recentBySize
        .takeWhile { case (ySize, ys) => ySize * 2 <= maxSize }
        .flatMap {
          case (ySize, ys) =>
            def h(yItr: Iterator[(NonPuncCat, LogDouble)], nonmodP: LogDouble, modP: LogDouble, op: (NonPuncCat, NonPuncCat) => NonPuncCat, accum: VectorBuilder[(NonPuncCat, LogDouble)]): Vector[(NonPuncCat, LogDouble)] = {
              if (yItr.hasNext) {
                val (y, py) = yItr.next()
                val p = nonmodP * py * py + modP * py
                if (p.logValue >= scaledThreshold.logValue) h(yItr, nonmodP, modP, op, accum += (op(y, y) -> p))
                else accum.result()
              }
              else accum.result()
            }
            val fMods = h(ys.iterator, pFwdNtNmod, pFwdNtMod, _ / _, new VectorBuilder)
            val bMods = h(ys.iterator, pBkdNtNmod, pBkdNtMod, _ \ _, new VectorBuilder)
            fMods ++ bMods
        }

      val newStuffComb: Vector[(NonPuncCat, LogDouble)] = (newStuffFBs ++ newStuffMods).toVector
      val newStuff =
        newStuffComb
          .groupBy(_._1.size)
          .toVector.sortBy(_._1)
          .mapVals(_.sorted(implicitly[Ordering[LogDouble]].on[(NonPuncCat, LogDouble)](_._2).reverse)) // MergeSortedIterator(newStuffComb).toVector

      println((System.currentTimeMillis() - startTime) / 1000.0)

      if (newStuff.nonEmpty) {
        val existingAndRecent = (existingBySize ++ recentBySize).groupByKey.mapVals(_.flatten.sorted(implicitly[Ordering[LogDouble]].on[(NonPuncCat, LogDouble)](_._2).reverse)).toVector.sortBy(_._1)
        f(existingAndRecent, newStuff, scaledThreshold, fi + 1)
      }
      else {
        (existingBySize ++ recentBySize).flatMap(_._2).sorted(implicitly[Ordering[LogDouble]].on[(NonPuncCat, LogDouble)](_._2).reverse)
      }
    }
    val atoms: Vector[(NonPuncCat, LogDouble)] =
      allAtoms.mapTo(pAtom(_) * logPTerm)
        .filter(_._2 >= threshold)
        .toVector.sorted(implicitly[Ordering[LogDouble]].on[(NonPuncCat, LogDouble)](_._2).reverse)
    val nonPunc = f(Vector.empty, Vector((1, atoms)), threshold * pNonPunc, 1).mapt((c, p) => (c, pNonPunc * p))
    val puncts = allPunc.mapTo(puncDist(_) * logPPunc).filter(_._2 >= threshold)
    val other = Vector(StartCat -> startEndProb, EndCat -> startEndProb, DeleteFromLeftCat -> deletionProb, DeleteFromRightCat -> deletionProb).filter(_._2 >= threshold.toDouble)
    (nonPunc ++ puncts).sorted(implicitly[Ordering[LogDouble]].on[(Cat, LogDouble)](_._2).reverse)
    //println("done")
  }

  private[this] class PeekableIterator[A](itr: Iterator[A]) extends Iterator[A] {
    private[this] var x: Option[A] = null
    def advance() = { x = (if (itr.hasNext) Some(itr.next()) else None); this }
    advance()
    def peek = x
    def next() = { assert(hasNext); val y = x.get; advance(); y }
    def hasNext() = x.isDefined
  }

  private[this] object PeekableIterator {
    def apply[A](v: Seq[A]) = new PeekableIterator(v.iterator)
  }

  private[this] class MergeSortedIterator(sortedLists: Vector[Seq[(NonPuncCat, LogDouble)]]) extends Iterator[(NonPuncCat, LogDouble)] {
    private[this] val sis = sortedLists.map(PeekableIterator(_))
    private[this] val sisLen = sis.size
    def next() = {
      //sis.flatMap(i => i.peek.map(x => (i, x._2))).maxBy(_._2)._1.next()
      var i = 0
      var bestI = -1
      var bestP = LogDouble.zero
      while (i < sisLen) {
        val x = sis(i)
        val y = x.peek
        if (y.isDefined) {
          val p = y.get._2
          if (p.logValue > bestP.logValue) {
            bestI = i
            bestP = p
          }
        }
        i += 1
      }
      sis(bestI).next()
    }
    def hasNext() = sis.exists(_.hasNext)
  }

  private[this] object MergeSortedIterator {
    def apply(sortedLists: Vector[Seq[(NonPuncCat, LogDouble)]]) = sortedLists match {
      case Vector(l) => l.iterator
      case _ => new MergeSortedIterator(sortedLists)
      //case _ =>  sortedLists.flatten.sorted(implicitly[Ordering[LogDouble]].on[(Cat, LogDouble)](_._2).reverse).iterator
      //case _ =>  sortedLists.flatten.sortWith(_._2.logValue > _._2.logValue).iterator
    }
  }

  /**
   * Return all categories with probabilities above the threshold
   */
  def allAbove_old(threshold: LogDouble): Map[NonPuncCat, LogDouble] = {
    val startTime = System.currentTimeMillis()
    @tailrec def f(existing: Map[NonPuncCat, LogDouble], recent: Map[NonPuncCat, LogDouble]): Map[NonPuncCat, LogDouble] = {
      //      val newStuff = for {
      //        (x, px) <- existing
      //        (y, py) <- existing
      //        c <- Vector(x / y, x \ y)
      //        if !existing.contains(c)
      //        p = apply(c) if p >= threshold
      //      } yield (c, p)

      println(recent.size)

      val existingAndRecent = existing ++ recent

      val newStuff: Map[NonPuncCat, LogDouble] = (for {
        ((x, y), (px, py)) <- {
          val a = for ((x, px) <- existing.iterator; (y, py) <- recent) yield ((x, y), (px, py))
          val b = for ((x, px) <- recent.iterator; (y, py) <- existing) yield ((x, y), (px, py))
          val c = for ((x, px) <- recent.iterator; (y, py) <- recent) yield ((x, y), (px, py))
          (a ++ b ++ c)
        }
        //        (x, px) <- existingAndRecent
        //        (y, py) <- existingAndRecent

        f = { // Forward
          val c = x / y
          //          if (existingAndRecent.contains(c)) None
          //          else {
          var p = pFwdNtNmod * px * py
          if (x == y) {
            p += (pFwdNtMod * px)
          }
          (p >= threshold).option((c, p))
          //          }
        }
        b = { // Backward
          val c = x \ y
          //          if (existingAndRecent.contains(c)) None
          //          else {
          var p = pBkdNtNmod * px * py
          if (x eq y) {
            p += (pBkdNtMod * px)
          }
          (p >= threshold).option((c, p))
          //          }
        }
        cp <- Vector(f, b).flatten
      } yield cp).toMap

      println((System.currentTimeMillis() - startTime) / 1000.0)
      if (newStuff.nonEmpty)
        f(existingAndRecent, newStuff)
      else
        existingAndRecent
    }
    val atoms: Map[NonPuncCat, LogDouble] = allAtoms.mapTo(pAtom(_) * logPTerm).filter(_._2 >= threshold).toMap
    f(atoms, atoms)
    //println("done")
  }

  def sample(): Cat = ???
  def defaultProb: LogDouble = LogDouble.zero
}

//

trait InfTagPriorInitializer[Tag] {
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

class CatgramInfCatPriorInitializer(
  pAtomInit: AtomCatDistInitializer,
  allPunc: Set[PuncCat], puncDist: LogProbabilityDistribution[PuncCat], pPunc: Double,
  pTerm: Double, // must be > 0.5 to ensure finiteness
  pMod: Double, //  probability mass devoted to modifier categories
  pFwd: Double)
    extends TagPriorInitializer[Cat] {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Cat])]], initialTagdict: TagDictionary[Cat]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.map(_._1).toSet).withTags(sentences.flatten.flatMap(_._2).toSet)
    val pAtom = pAtomInit.fromKnownSupertagSets(sentences, tagdict)
    new SimpleInfCatPrior(
      allPunc, // ++ tagdict.allTags.collect { case c: PuncCat => c }, 
      puncDist, pPunc,
      allAtoms = tagdict.allTags.collect { case a: AtomCat => a }.toSet,
      pAtom, pTerm, pMod, pFwd,
      tagdict.excludedTags)
  }

  override def toString = f"CatgramInfCatPriorInitializer($pAtomInit, pTerm=$pTerm%.3f, pMod=$pMod%.3f, pFwd=$pFwd%.3f)"
}

class MemoizingInfCatPrior(delegate: InfCatPrior) extends InfCatPrior {
  private[this] var cache = mutable.Map.empty[Cat, LogDouble]
  private[this] var storedVector: Option[Vector[(Cat, LogDouble)]] = None
  private[this] var cachedThreshold = LogDouble.one

  private[this] val monitor = new AnyRef
  def apply(c: Cat): LogDouble = monitor.synchronized {
    cache.getOrElseUpdate(c, {
      val p = delegate(c)
      //println(f" -> MemoizingInfCatPrior($c [h=${c.hashCode}]) = $p")
      if (cache.size % 10000 == 0) println(f" -> MemoizingInfCatPrior.size   = ${cache.size}      ::   ${cache.keySet.map(_.toString).size}     ::   $c")
      p
    })
  }

  def sample(): Cat = ???
  def defaultProb: LogDouble = LogDouble.zero

  def allAbove(threshold: LogDouble, maxSize: Int = Int.MaxValue): Vector[(Cat, LogDouble)] = monitor.synchronized {
    if (storedVector.isEmpty || threshold < cachedThreshold) {
      val all = delegate.allAbove(threshold, maxSize)
      cache ++= all
      storedVector = Some(all)
    }
    storedVector.get
  }

}

//

object InfCatPriorMain {

  def main(args: Array[String]): Unit = {

    val s = cat"s".asInstanceOf[AtomCat]
    val np = cat"np".asInstanceOf[AtomCat]
    val n = cat"n".asInstanceOf[AtomCat]

    val catprior = new SimpleInfCatPrior(
      allPunc = Set(cat".", cat",", cat";", cat":", cat"LRB", cat"RRB").map(_.asInstanceOf[PuncCat]), puncDist = new SimpleLogProbabilityDistribution(Vector(cat".", cat",", cat";", cat":", cat"LRB", cat"RRB").zipWithIndex.mapt((p, i) => p.asInstanceOf[PuncCat] -> math.pow(0.5, i)).toMap.normalizeValues.mapVals(LogDouble(_))), pPunc = 0.1,
      allAtoms = Set(s, np, n), pAtom = new SimpleLogProbabilityDistribution[AtomCat](Map(s -> 0.5, np -> 0.2, n -> 0.3).mapVals(LogDouble(_))),
      pTerm = 0.8,
      pMod = 0.1,
      pFwd = 0.55)

    //    def f(x: Cat, y: Cat) {
    //      println(f"${catprior(x)}  ${catprior(y)}  ${catprior(x)/catprior(y)}")
    //    }
    //    f((s\np)/(s\np), (s\np)/(s\n))
    //    f(n/n, np/n)

    val t = 1e-10 // 0.00000001
    val x2 = time("allAbove", {
      val x2 = catprior.allAbove(LogDouble(t), maxSize = 8).mapt((c, cp) => ((c, cp), catprior(c)))
      for (((c, cp), cp2) <- x2) {
        //println(f"$c%-50s ${cp.toDouble}%.6f ${cp2.toDouble}%.6f  ${if (cp != cp2) "**" else ""}")
      }
      x2
    })
    //    val x1 = time("allAbove_old", {
    //      val x1 = catprior.allAbove_old(LogDouble(t)).mapt((c, cp) => ((c, cp), catprior(c))).desc
    //      for (((c, cp), cp2) <- x1) {
    //        //        println(f"$c%-50s ${cp.toDouble}%.6f ${cp2.toDouble}%.6f  ${if (cp != cp2) "**" else ""}")
    //      }
    //      x1
    //    })
    //    assert(x1.size == x2.size, f"x1.size = ${x1.size}; x2.size = ${x2.size}")
    //    var mismatch = false
    //    for ((((a, ap), ap2), ((b, bp), bp2)) <- x1.sortBy(_._1) zipSafe x2.sortBy(_._1)) {
    //      if (!(ap approx bp)) mismatch = true
    //      //println(f"$a%-50s $b%-50s ${ap.toDouble}%.6f ${bp.toDouble}%.6f ${ap2.toDouble}%.6f ${bp2.toDouble}%.6f  ${if (!(ap approx ap2) || !(bp approx bp2) || !(ap approx bp)) "**" else ""}")
    //      //assert(a == b, f"a=$a; b=$b"); assert(ap approx bp, f"ap=$ap; bp=$bp") 
    //    }
    //    println(if (!mismatch) "\nall good\n\n" else "\nOH NO!\n\n")
    x2.toVector.sortBy(_._1).takeRight(100).foreach { case ((c, cp), _) => println(f"$c%-60s $cp") }
    println(f"Total number of distinct categories: ${x2.map(_._1._1).distinct.size}")
    println(f"Size of last category: ${x2.toVector.sortBy(_._1).last._1._1.size}")

    def allSubExpressions(c: Cat): Vector[Cat] = (c match {
      case c: PuncCat => Vector(c)
      case a: AtomCat => Vector(a)
      case a || b => allSubExpressions(a) ++ allSubExpressions(b)
    }) :+ c
    val allsubs = x2.map(_._1._1).flatMap(allSubExpressions)
    val subcodegroups = allsubs.groupBy(_.toString).mapValues(_.map(c => Integer.toHexString(System.identityHashCode(c))))
    //    println("\n\n"); subcodegroups.toVector.sortBy(_._1).foreach{case (c,ids) => println(f"${c}%-30s  =>  ${ids.toSet}%-15s (${ids.size})  ${if(ids.distinct.size > 1) "**" else ""}")}
    println(if (subcodegroups.exists(_._2.distinct.size > 1)) "duplicate" else "no duplicates")

  }

}
