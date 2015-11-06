package dhg.ccg.parse.pcfg.typesup

import dhg.util._
import scalaz._
import scalaz.Scalaz._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tag.learn.EmissionInitializer // interface
import dhg.ccg.tag.learn.TagPriorInitializer // interface
import dhg.ccg.parse.pcfg._

trait TermDistInitializer {
  def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, TermProd]
}

class UniformTermDistInitializer() extends TermDistInitializer {
  def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, TermProd] = {
    new UnconditionalWrappingConditionalLogProbabilityDistribution(
      new LaplaceLogProbabilityDistribution(Map(), None, None, LogDouble(1.0), LogDouble(1000.0)))
  }
  override def toString = f"UniformTermDistInitializer()"
}
