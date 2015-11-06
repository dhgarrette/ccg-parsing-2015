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

/**
 * EnglishCcgTreeBankReader().fullCorpusDONTUSE
 */
case class EnglishCcgTreeBankReader(catmap: CatInterner = NonRemovingCcgBankCatInterner, usePos: Boolean = false) extends CcgbankReader(usePos) {
  override protected[this] val Loc = "data/ccgbank/AUTO"

  override final def tdData = readTrees(0 to 15)
  override final def rawDataDONTUSE = readTrees(16 to 18)
  override final def devData = readTrees(19 to 21)
  override final def testData = readTrees(22 to 24)

  override final val startTag = Cat.startCat(catmap)
  override final val endTag = Cat.endCat(catmap)
  override final val tagToString: (Cat => String) = _.toString
  override final lazy val tagFromString: (String => Cat) = catmap.fromString _

  override final def filename(fileNumber: Int) = f"wsj_${f"$fileNumber%04d"}.auto"

}
