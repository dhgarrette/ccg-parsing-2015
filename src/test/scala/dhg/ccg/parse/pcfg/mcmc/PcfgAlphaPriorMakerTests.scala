package dhg.ccg.parse.pcfg.mcmc

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.scg._
import dhg.ccg.tagdict.StartEndTags
import dhg.util._
import dhg.ccg.tagdict._
import dhg.ccg.test.TestUtil.DoubleIteratorRandomGenerator
import org.apache.commons.math3.random.MersenneTwister
import scala.collection.immutable.ListMap
import scalaz._
import Scalaz._

class PcfgAlphaPriorMakerTests {

  val A: AtomCat = cat"A".asInstanceOf[AtomCat]
  val B: AtomCat = cat"B".asInstanceOf[AtomCat]
  val C: AtomCat = cat"C".asInstanceOf[AtomCat]
  val D: AtomCat = cat"D".asInstanceOf[AtomCat]
  val E: AtomCat = cat"E".asInstanceOf[AtomCat]
  val F: AtomCat = cat"F".asInstanceOf[AtomCat]
  val G: AtomCat = cat"G".asInstanceOf[AtomCat]
  val H: AtomCat = cat"H".asInstanceOf[AtomCat]

  val S: AtomCat = cat"S".asInstanceOf[AtomCat]
  val NP: AtomCat = cat"NP".asInstanceOf[AtomCat]
  val N: AtomCat = cat"N".asInstanceOf[AtomCat]
  val PP: AtomCat = cat"PP".asInstanceOf[AtomCat]
  val STA: Cat = cat"<S>"
  val END: Cat = cat"<E>"

  @Test
  def test_TrainDataPcfgAlphaPriorMaker {
    throw new NotImplementedError("Test not written")
  }

}
