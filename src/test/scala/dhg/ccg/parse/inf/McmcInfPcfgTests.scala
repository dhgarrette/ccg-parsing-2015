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
import scalaz._
import Scalaz._

class McmcInfPcfgTests {

  val A: AtomCat = cat"A".asInstanceOf[AtomCat]
  val B: AtomCat = cat"B".asInstanceOf[AtomCat]
  val C: AtomCat = cat"C".asInstanceOf[AtomCat]
  val D: AtomCat = cat"D".asInstanceOf[AtomCat]
  val E: AtomCat = cat"E".asInstanceOf[AtomCat]
  val F: AtomCat = cat"F".asInstanceOf[AtomCat]
  val X: Cat = cat"X".asInstanceOf[AtomCat]

  val S: AtomCat = cat"S".asInstanceOf[AtomCat]
  val NP: Cat = cat"NP".asInstanceOf[AtomCat]
  val N: Cat = cat"N".asInstanceOf[AtomCat]
  val PP: Cat = cat"PP".asInstanceOf[AtomCat]
  val STA: Cat = cat"<S>"
  val END: Cat = cat"<E>"
  val SE = new SimpleStartEndTags(STA, END)

  val s: Cat = cat"S".asInstanceOf[AtomCat]
  val np: Cat = cat"NP".asInstanceOf[AtomCat]
  val n: Cat = cat"N".asInstanceOf[AtomCat]
  val pp: Cat = cat"PP".asInstanceOf[AtomCat]

  val startWord = "<S>"
  val startTag = STA
  val endWord = "<E>"
  val endTag = END

  @Test
  def integration_againstEM {

    ???
  }

}
