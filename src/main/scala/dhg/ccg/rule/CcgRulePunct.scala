package dhg.ccg.rule

import scala.Iterator

import dhg.ccg.cat._
import scalaz._
import Scalaz._

/**
 * (NP,1887460)
 * (S,1141900)
 * (N,1076352)
 * (PP,80060)
 * (,,59995)
 * (.,48453)
 * (CONJ,26211)
 * (:,2422)
 * (RRB,1667)
 * (;,1459)
 * (LRB,553)
 *
 *
 */

/**
 *  Punctuation removal
 *  X  .  =>  X
 *  .  X  =>  X
 */
trait PunctRight extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat) = if (right == punctCat) Some(left) else None
  def inferRight(parent: Cat, left: Cat): Option[Cat] = (parent == left).option(punctCat)
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = (right == punctCat).option(parent)
  def punctCat: PuncCat
}
case object LeftBracketPunctRight /* */ extends PunctRight { val punctCat = cat"LRB".asInstanceOf[PuncCat] }
case object RightBracketPunctRight /**/ extends PunctRight { val punctCat = cat"RRB".asInstanceOf[PuncCat] }
case object CommaPunctRight /*       */ extends PunctRight { val punctCat = cat",".asInstanceOf[PuncCat] }
case object SemicolonPunctRight /*   */ extends PunctRight { val punctCat = cat";".asInstanceOf[PuncCat] }
case object ColonPunctRight /*       */ extends PunctRight { val punctCat = cat":".asInstanceOf[PuncCat] }
case object FullstopPunctRight /*    */ extends PunctRight { val punctCat = cat".".asInstanceOf[PuncCat] }
case object AnyPunctRight extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat) = right match { case r: PuncCat => Some(left); case _ => None }
  def inferRight(parent: Cat, left: Cat): Option[Cat] = ???
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = ???
}
//object PunctRight {
//  def apply(left: Cat, right: Cat) = {
//    Iterator(
//      LeftBracketPunctRight,
//      RightBracketPunctRight,
//      CommaPunctRight,
//      SemicolonPunctRight,
//      ColonPunctRight,
//      FullstopPunctRight)
//      .map { r => r(left, right) }
//      .collectFirst { case Some(p) => p }
//  }
//}

trait PunctLeft extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat) = if (left == punctCat) Some(right) else None
  def inferRight(parent: Cat, left: Cat): Option[Cat] = (left == punctCat).option(parent)
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = (parent == right).option(punctCat)
  def punctCat: PuncCat
}
case object LeftBracketPunctLeft extends PunctLeft { val punctCat = cat"LRB".asInstanceOf[PuncCat] }
case object AnyPunctLeft extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat) = left match { case l: PuncCat => Some(right); case _ => None }
  def inferRight(parent: Cat, left: Cat): Option[Cat] = ???
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = ???
}
//object PunctLeft {
//  def apply(left: Cat, right: Cat) = {
//    Iterator(
//      LeftBracketPunctLeft)
//      .map { r => r(left, right) }
//      .collectFirst { case Some(p) => p }
//  }
//}


//
//
//


///**
// *  Punctuation removal
// *  X  .  =>  X
// *  .  X  =>  X
// */
//case object PunctRight extends BinaryCcgRule {
//  private[this] val Punct = """[^A-Za-z0-9]+|LRB|RRB|LQU|RQU""".r
//  override def apply(left: Cat, right: Cat) = (left, right) match {
//    case (x, AtomCat(Punct(), None)) => Some(x)
//    case _ => None
//  }
//  def inferRight(parent: Cat, left: Cat): Option[Cat] = ???
//  def inferLeft(parent: Cat, right: Cat): Option[Cat] = ???
//}
//
////
//
//case object PunctLeft extends BinaryCcgRule {
//  private[this] val LhsPunct = """\[|\(|LRB|`|``|LQU|,|;""".r
//  override def apply(left: Cat, right: Cat) = (left, right) match {
//    case (AtomCat(LhsPunct(), None), x) => Some(x)
//    case _ => None
//  }
//  def inferRight(parent: Cat, left: Cat): Option[Cat] = ???
//  def inferLeft(parent: Cat, right: Cat): Option[Cat] = ???
//}
