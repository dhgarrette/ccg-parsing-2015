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

trait BinaryDistInitializer {
  def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, BinaryProd]
}

class UniformBinaryDistInitializer() extends BinaryDistInitializer {
  def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, BinaryProd] = {
    new UnconditionalWrappingConditionalLogProbabilityDistribution(
      new LaplaceLogProbabilityDistribution(Map(), None, None, LogDouble(1.0), LogDouble(1000.0)))
  }
  override def toString = f"UniformBinaryDistInitializer()"
}

/**
 * Just delegate to the tag prior
 */
class CatpriorBinaryDistInitializer(tagPriorInitializer: TagPriorInitializer[Cat]) extends BinaryDistInitializer {
  def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, BinaryProd] = {
    val tagprior = tagPriorInitializer.fromKnownSupertagSets(sentences.map(s => s.words zipSafe s.supertagSets), initialTagdict)
    new BinaryCondPriorDist(tagprior)
  }
  override def toString = f"CatpriorBinaryDistInitializer($tagPriorInitializer)"
}

class BinaryPriorDist(tagprior: LogProbabilityDistribution[Cat]) extends LogProbabilityDistribution[BinaryProd] {
  def apply(x: BinaryProd): LogDouble = tagprior(x.left) * tagprior(x.right)
  def sample(): BinaryProd = ???
  def defaultProb: LogDouble = ???
  override def toString = f"BinaryPriorDist()"
}

class BinaryCondPriorDist(tagprior: LogProbabilityDistribution[Cat]) extends ConditionalLogProbabilityDistribution[Cat, BinaryProd] {
  def apply(x: BinaryProd, given: Cat): LogDouble = tagprior(x.left) * tagprior(x.right)
  def sample(given: Cat): BinaryProd = ???
  override def toString = f"BinaryCondPriorDist()"
}

