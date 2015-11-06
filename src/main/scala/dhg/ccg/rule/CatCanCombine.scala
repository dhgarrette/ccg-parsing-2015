package dhg.ccg.rule

import scala.annotation.tailrec
import dhg.ccg.cat._
import dhg.ccg.rule._

trait CatCanCombine {
  def apply(a: Cat, b: Cat): Boolean

  def rules: Vector[CcgRule]
  def startCat: Cat
  def endCat: Cat
}

final class SimpleCatCanCombine(val rules: Vector[CcgRule], val startCat: Cat, val endCat: Cat) extends CatCanCombine {
  private[this] val unaryRules = rules.collect { case r: UnaryCcgRule => r }
  private[this] val binaryRules = rules.collect { case r: BinaryCcgRule => r }

  override def apply(a: Cat, b: Cat): Boolean = {
    if (a == startCat)
      return !removeAllFollowing(b).isInstanceOf[BCat]
    if (b == endCat)
      return !removeAllPreceding(a).isInstanceOf[FCat]
    for {
      ax <- removePreceding(a)
      bx <- removeFollowing(b)
      rule <- binaryRules
      if rule(ax, bx).isDefined
    } return true
    return false
  }

  private[this] def removePreceding(a: Cat): List[Cat] = {
    @tailrec def f(a: Cat, accum: List[Cat]): List[Cat] =
      a match {
        case l \ r => f(l, l :: accum)
        case _ => accum
      }
    f(a, List(a))
  }

  private[this] def removeFollowing(b: Cat): List[Cat] = {
    @tailrec def f(b: Cat, accum: List[Cat]): List[Cat] =
      b match {
        case l / r => f(l, l :: accum)
        case _ => accum
      }
    f(b, List(b))
  }

  @tailrec
  private[this] def removeAllPreceding(a: Cat): Cat = {
    a match {
      case l \ r => removeAllPreceding(l)
      case _ => a
    }
  }

  @tailrec
  private[this] def removeAllFollowing(b: Cat): Cat = {
    b match {
      case l / r => removeAllFollowing(l)
      case _ => b
    }
  }

  override def toString = f"CatCanCombine(${rules.mkString(", ")})"
}

final class BackwardCatCanCombine(val rules: Vector[CcgRule], val startCat: Cat, val endCat: Cat) extends CatCanCombine {
  def this(cc: CatCanCombine) = this(cc.rules, cc.startCat, cc.endCat)
  private[this] val cc: CatCanCombine = new SimpleCatCanCombine(rules, startCat, endCat)
  override def apply(a: Cat, b: Cat): Boolean = cc(b, a)
  override def toString = f"BackwardCatCanCombine(${rules.mkString(", ")})"
}
