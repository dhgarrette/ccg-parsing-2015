package dhg.ccg.data

import dhg.util._
import scala.util.Try
import dhg.ccg.cat._
import scala.util.parsing.combinator.RegexParsers
import dhg.ccg.parse._
import scala.annotation.tailrec
import scalaz.{ Tree => _, _ }
import Scalaz._
import dhg.util.viz._

case class ChineseCcgTreeBankReader(usePos: Boolean = false) extends CcgbankReader(usePos) {
  override protected[this] val Loc = "data/ccgbank-chinese/AUTO"
  override protected[this] final val catmap = NonRemovingCcgBankCatInterner

  override final def tdData = readTrees(0 to 11)
  override final def rawDataDONTUSE = readTrees(20 to 24)
  override final def devData = readTrees(25 to 27)
  override final def testData = readTrees(28 to 31)

  override final val startTag = Cat.startCat(catmap)
  override final val endTag = Cat.endCat(catmap)
  override final val tagToString: (Cat => String) = _.toString
  override final lazy val tagFromString: (String => Cat) = catmap.fromString _

  override final def filename(fileNumber: Int) = f"chtb_${f"$fileNumber%04d"}.fid"

}
