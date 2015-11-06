package dhg.ccg.cat

import dhg.util._
import scalaz.Scalaz._
import scala.collection.mutable
import scala.util.matching.Regex

trait CatInterner // extends (Cat => Cat)
{
  def cachedVersion(key: Cat): Cat
  def fromString(s: String): Cat
  def conj(c: Cat): ConjCat
  final def apply(s: String): Cat = fromString(s)
  final def unapply(s: String): Option[Cat] = Some(fromString(s))
}

/**
 * Parse category strings to create Cat objects, but create a pool of
 * Cat objects that can be reused (including as sub-expressions) to
 * save lots of memory.
 */
final class StandardCatInterner private[cat] (
  PuncRe: Regex,
  AtomRe: Regex,
  removeFeatures: Boolean,
  uppercaseAtoms: Boolean = true) //
    extends CatInterner {

  private[this] val IndexRe = """(\d+)(:(B|U))?""".r // TODO: what are the B and U ??

  //private[this] val cache = mutable.HashMap[String, Cat]()
  private[this] val cache = mutable.HashMap[Cat, Cat]()

  //final def apply(key: Cat): Cat = cachedVersion(key)
  private[this] val monitor = new AnyRef
  final def cachedVersion(key: Cat): Cat = monitor.synchronized {
    //val keyString = key.toString
    cache.get(key) match {
      case Some(v) =>
        v // found in the cache
      case None =>
        val cat = key match {
          case c: PuncCat => new PuncCat(PuncCat.PuncCatInner(c.punc), this)
          case a: AtomCat => new AtomCat(AtomCat.AtomCatInner(a.atom, a.feature, a.index), this)
          case FCat(l, r, i) => new FCat(FCat.FCatInner(cachedVersion(l).asInstanceOf[NonPuncCat], cachedVersion(r).asInstanceOf[NonPuncCat], i), this)
          case BCat(l, r, i) => new BCat(BCat.BCatInner(cachedVersion(l).asInstanceOf[NonPuncCat], cachedVersion(r).asInstanceOf[NonPuncCat], i), this)
          case ConjCat(c, i) => new ConjCat(ConjCat.ConjCatInner(cachedVersion(c).asInstanceOf[NonPuncCat], i), this)
          case StartCat | EndCat | DeleteFromLeftCat | DeleteFromRightCat => key
          case _ => sys.error(s"uhhh.... $key")
        }
        cache(cat) = cat
        cat
    }
  }

  final def fromString(s: String): Cat = {
    try {
      val s2 = s.replaceAll("\\s+", "")
      val s3 = if (removeFeatures) AtomRe.replaceAllIn(s2, "$1") else s2
      val cat = tokensToCat(s3
        .replaceAll("([\\\\/()_])", " $1 ")
        .lsplit("\\s+")
        .filter(_.nonEmpty))
      cat
    }
    catch {
      case e: RuntimeException =>
        throw new RuntimeException(f"failed to parse cat: `$s`  ${if (s.isEmpty) "(empty)" else ""}", e)
    }
  }

  private[this] final def tokensToCat(tokens: Vector[String]): Cat = tokens match {
    case _ if tokens.size <= 3 =>
      val (s, i) = tokens match {
        case Vector(s, "_", IndexRe(UInt(i), _, _)) => (s, Some(i))
        case Vector(s) => (s, None)
      }
      val cat: Cat =
        s match {
          case _ if s == StartCat.toString => StartCat
          case _ if s == EndCat.toString => EndCat
          case _ if s == DeleteFromLeftCat.toString => DeleteFromLeftCat
          case _ if s == DeleteFromRightCat.toString => DeleteFromRightCat

          case PuncRe(punc) =>
            new PuncCat(PuncCat.PuncCatInner(punc), JunkCatInterner)

          case AtomRe(atom, _, feature) =>
            val casedAtom = if (uppercaseAtoms) atom.toUpperCase else atom
            val f = Option(feature).filter(_ != "nb") // ignore, following Lewis&Steedman2014
            new AtomCat(AtomCat.AtomCatInner(casedAtom, f, i), JunkCatInterner)

          case _ => sys.error(f"Bad atom: $s, ${tokens.mkString("[", ",", "]")}")
        }
      cachedVersion(cat)

    case _ if tokens.size >= 5 =>
      val (v, i) = if (tokens(tokens.length - 2) == "_") (tokens.dropRight(2), Some(tokens.last.toInt)) else (tokens, None)
      val opIdx = getMainOpIdx(v)
      val left = tokensToCat(v.slice(1, opIdx))
      val right = tokensToCat(v.slyce(opIdx + 1, -1))
      val cat = (left, v(opIdx), right) match {
        case (l: NonPuncCat, """/""", r: NonPuncCat) => new FCat(FCat.FCatInner(l, r, i), JunkCatInterner)
        case (l: NonPuncCat, """\""", r: NonPuncCat) => new BCat(BCat.BCatInner(l, r, i), JunkCatInterner)
        case (l: PuncCat, """\""", _) => throw new ComplexPuncCatException(v.mkString)
        case (_, """\""", r: PuncCat) => throw new ComplexPuncCatException(v.mkString)
        case _ => throw new RuntimeException("no match")
      }
      cachedVersion(cat)
  }

  private[this] final def getMainOpIdx(v: Vector[String]) = {
    v.zipWithIndex.tail.foldLeftWhile((0, none[Int]))((z, _) => z._2.isEmpty) {
      case ((pc, None), (tok, i)) =>
        tok match {
          case "(" => (pc + 1, none)
          case ")" => (pc - 1, none)
          case "/" | "\\" if pc == 0 => (0, Some(i))
          case _ => (pc, none)
        }
    } match {
      case (_, Some(opIdx)) => opIdx
      case _ => sys.error(s"failed to find main op in ${v.mkString}")
    }
  }

  def conj(c: Cat): ConjCat = { new ConjCat(ConjCat.ConjCatInner(c.asInstanceOf[NonPuncCat], None), JunkCatInterner) }

}

class SeparateCacheCatInterner(parser: CatInterner, cacher: CatInterner) extends CatInterner {
  def cachedVersion(key: Cat): Cat = cacher.cachedVersion(key)
  def fromString(s: String): Cat = cacher.cachedVersion(parser.fromString(s))
  def conj(c: Cat): ConjCat = cacher.cachedVersion(parser.conj(c)).asInstanceOf[ConjCat]
}

object JunkCatInterner extends CatInterner {
  def cachedVersion(key: Cat): Cat = key
  def fromString(s: String): Cat = sys.error("JunkCatInterner")
  def conj(c: Cat): ConjCat = sys.error("JunkCatInterner")
  override def toString = "JunkCatInterner"
}

object CatInterner {

  val CcgBankPuncRe = """([.,:;]|RRB|LRB)""".r
  val CcgBankAtomRe = """([^/\\\[\]\(\)_]+)(\[([^/\\\[\]\(\)_]+)\])?""".r
  val TutBankAtomRe = """([^/\\\[\]\(\):]+)(:([^/\\\[\]\(\):]+))?""".r

}

class ComplexPuncCatException(msg: String) extends RuntimeException(msg)

