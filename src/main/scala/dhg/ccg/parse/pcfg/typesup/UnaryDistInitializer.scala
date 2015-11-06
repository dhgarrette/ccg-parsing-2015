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

trait UnaryDistInitializer {
  def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, UnaryProd]
}

class UniformUnaryDistInitializer() extends UnaryDistInitializer {
  def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, UnaryProd] = {
    new UnconditionalWrappingConditionalLogProbabilityDistribution(
      new LaplaceLogProbabilityDistribution(Map(), None, None, LogDouble(1.0), LogDouble(1000.0)))
  }
  override def toString = f"UniformUnaryDistInitializer()"
}

/**
 * Just delegate to the tag prior
 */
class CatpriorUnaryDistInitializer(tagPriorInitializer: TagPriorInitializer[Cat]) extends UnaryDistInitializer {
  def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, UnaryProd] = {
    val tagprior = tagPriorInitializer.fromKnownSupertagSets(sentences.map(s => s.words zipSafe s.supertagSets), initialTagdict)
    new UnaryCondPriorDist(tagprior)
  }
  override def toString = f"CatpriorUnaryDistInitializer($tagPriorInitializer)"
}

class UnaryPriorDist(tagprior: LogProbabilityDistribution[Cat]) extends LogProbabilityDistribution[UnaryProd] {
  def apply(x: UnaryProd): LogDouble = tagprior(x.sub)
  def sample(): UnaryProd = ???
  def defaultProb: LogDouble = ???
  override def toString = f"UnaryPriorDist()"
}

class UnaryCondPriorDist(tagprior: LogProbabilityDistribution[Cat]) extends ConditionalLogProbabilityDistribution[Cat, UnaryProd] {
  def apply(x: UnaryProd, given: Cat): LogDouble = tagprior(x.sub)
  def sample(given: Cat): UnaryProd = ???
  override def toString = f"UnaryCondPriorDist()"
}

