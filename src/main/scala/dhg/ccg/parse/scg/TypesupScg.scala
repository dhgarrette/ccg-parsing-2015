package dhg.ccg.parse.scg

import dhg.util._
import scalaz._
import scalaz.Scalaz._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.tagdict.StartEndSwappedTagDictionary
import dhg.ccg.parse.pcfg._
import dhg.ccg.tag.learn.TransitionInitializer
import dhg.ccg.tag.learn.TagPriorInitializer
import dhg.ccg.tag.learn.CcgCombinabilityTrInitializer
import dhg.ccg.tag.learn.TagPriorTrInitializer
import dhg.ccg.tag.learn.InterpolatingTransitionInitializer // tested

class ReversingTrInit[Tag](trInit: TransitionInitializer[Tag]) extends TransitionInitializer[Tag] {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]) = {
    val reversedSentences = sentences.map(_.reverse)
    val reversedInitialTagdict = new StartEndSwappedTagDictionary(initialTagdict)
    trInit.fromKnownSupertagSets(reversedSentences, reversedInitialTagdict)
  }
  
  override def toString() = f"ReversingTrInit($trInit)" 
}
