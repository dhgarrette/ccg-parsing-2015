package dhg.ccg.rule

import dhg.ccg.cat._
import scalaz._
import Scalaz._

case object DeleteRight extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat) = (right eq DeleteFromLeftCat).option(left)
  def inferRight(parent: Cat, left: Cat): Option[Cat] = Some(DeleteFromLeftCat)
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = Some(parent)
}

case object DeleteLeft extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat) = (left eq DeleteFromRightCat).option(right)
  def inferRight(parent: Cat, left: Cat): Option[Cat] = Some(parent)
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = Some(DeleteFromRightCat)
}
