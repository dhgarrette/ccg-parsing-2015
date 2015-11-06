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

trait RootDistInitializer {
  def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): LogProbabilityDistribution[Cat]
}

class UniformRootDistInitializer() extends RootDistInitializer {
  def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): LogProbabilityDistribution[Cat] = {
    val possibleRoots = sentences.flatMap(_.root.keySet).toSet
    new LaplaceLogProbabilityDistribution(Map(), None, None, LogDouble(1.0), LogDouble(possibleRoots.size))
  }
  override def toString = f"UniformRootDistInitializer()"
}

/**
 * Just delegate to the tag prior
 */
class CatpriorRootDistInitializer(tagPriorInitializer: TagPriorInitializer[Cat]) extends RootDistInitializer {
  def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): LogProbabilityDistribution[Cat] = {
    tagPriorInitializer.fromKnownSupertagSets(sentences.map(s => s.words zipSafe s.supertagSets), initialTagdict)
  }
  override def toString = f"CatpriorRootDistInitializer($tagPriorInitializer)"
}
