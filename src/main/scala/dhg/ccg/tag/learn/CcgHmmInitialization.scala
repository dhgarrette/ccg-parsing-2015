package dhg.ccg.tag.learn

import dhg.util._
import math.pow
import scalaz.{ \/ => _, _ }
import scalaz.Scalaz._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.prob._
import dhg.ccg.tagdict.TagDictionary

/**
 * Assign `combinableTransitionMass` proportion of the probability mass of
 * the delegate TransitionInitializer to combining transitions, and the rest
 * to non-combining transitions.
 *
 * For example,
 *   If combinableTransitionMass = 0.95
 *   and the can-combine total mass is 0.6 and the can't-combine total mass is 0.4,
 *   Then:
 *   	 1. A transition X that can combine and has probability 0.2 will take one-third
 *        of the can-combine total mass (0.2/0.6), meaning that in the final result
 *        it will need to take one-third of the allocated can-combine mass (0.95),
 *        meaning that p(X) = (0.2/0.6)*0.95 = 0.2*(0.95/0.6)
 *   	 2. A transition X that can't combine and has probability 0.2 will take one-half
 *        of the can't-combine total mass (0.2/0.4), meaning that in the final result
 *        it will need to take one-half of the allocated can't-combine mass (0.05),
 *        meaning that p(X) = (0.2/0.4)*0.05 = 0.2*(0.05/0.4)
 */
class CcgCombinabilityTrInitializer(
  delegateInitializer: TransitionInitializer[Cat],
  canCombine: CatCanCombine,
  combinableTransitionMass: Double = 0.95, // amount of probability mass reserved for combinable transitions
  totalSmoothing: LogDouble) extends TransitionInitializer[Cat] {

  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Cat])]], initialTagdict: TagDictionary[Cat]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.map(_._1).toSet).withTags(sentences.flatten.flatMap(_._2).toSet)
    val delegate = delegateInitializer.fromKnownSupertagSets(sentences, tagdict)
    new CcgCombinabilityTransitionConditionalLogProbabilityDistribution(delegate, tagdict, canCombine, combinableTransitionMass, totalSmoothing)
  }

  override def toString = f"CcgCombinabilityTrInitializer($delegateInitializer, $canCombine, combinableTransitionMass=$combinableTransitionMass, totalSmoothing=$totalSmoothing)"
}

/**
 * This is only used by the above CcgCombinabilityTrInitializer
 */
class CcgCombinabilityTransitionConditionalLogProbabilityDistribution(
  delegate: ConditionalLogProbabilityDistribution[Cat, Cat],
  tagdict: TagDictionary[Cat],
  canCombine: CatCanCombine,
  combinableTransitionMass: Double,
  totalSmoothing: LogDouble)
  extends ConditionalLogProbabilityDistribution[Cat, Cat] {

  private[this] def getCombinableSplitSums(t1: Cat): (LogDouble, LogDouble) = {
    val (can, cant) =
      (tagdict.allTags + tagdict.endTag).mapTo { t2 =>
        delegate(t2, t1)
      }.toMap.partition { case (t2, p) => canCombine(t1, t2) }

    val canTotal = can.values.sum + totalSmoothing
    val cantTotal = cant.values.sum + totalSmoothing

    //    if (Set("NP")(t1.toString)) {
    //      println(f"CcgCombinabilityTransitionConditionalLogProbabilityDistribution.getCombinableSplitSums: for t1=$t1,")
    //      println(f"     can=$can")
    //      println(f"     cant=$cant")
    //      println(f"     canTotal=$canTotal")
    //      println(f"     cantTotal=$cantTotal")
    //    }

    //    println(f"CcgCombinabilityTransitionConditionalLogProbabilityDistribution.getCombinableSplitSums: for t1=$t1, canTotal > combinableTransitionMass (${canTotal.toDouble}%.4f > $combinableTransitionMass%.4f)")
    //    println(f"CcgCombinabilityTransitionConditionalLogProbabilityDistribution.getCombinableSplitSums: for t1=$t1, cantTotal < 1-combinableTransitionMass (${cantTotal.toDouble}%.4f < ${1 - combinableTransitionMass}%.4f)")

    val (canZ, cantZ) =
      if (canTotal.toDouble > combinableTransitionMass || cantTotal.toDouble < (1 - combinableTransitionMass)) {
        //        if (canTotal.toDouble > combinableTransitionMass) println(f"CcgCombinabilityTransitionConditionalLogProbabilityDistribution.getCombinableSplitSums: for t1=$t1, canTotal > combinableTransitionMass (${canTotal.toDouble}%.4f > $combinableTransitionMass%.4f)")
        //        if (cantTotal.toDouble < (1 - combinableTransitionMass)) println(f"CcgCombinabilityTransitionConditionalLogProbabilityDistribution.getCombinableSplitSums: for t1=$t1, cantTotal < 1-combinableTransitionMass (${cantTotal.toDouble}%.4f < ${1 - combinableTransitionMass}%.4f)")
        (LogDouble.one, LogDouble.one) // TODO: THIS IS A TERRIBLE HACK
      }
      else {
        val canZ =
          if (canTotal.isZero) {
            //            println(f"CcgCombinabilityTransitionConditionalLogProbabilityDistribution.getCombinableSplitSums: for t1=$t1, canTotal is zero")
            LogDouble.one // TODO: THIS IS A TERRIBLE HACK
          }
          else {
            assert(canTotal.nonZero, f"canTotal is zero for t1=$t1")
            canTotal / LogDouble(combinableTransitionMass)
          }

        val cantZ =
          if (cantTotal.isZero) {
            //            println(f"CcgCombinabilityTransitionConditionalLogProbabilityDistribution.getCombinableSplitSums: for t1=$t1, cantTotal is zero")
            LogDouble.one // TODO: THIS IS A TERRIBLE HACK
          }
          else {
            assert(cantTotal.nonZero, f"cantTotal is zero for t1=$t1")
            cantTotal / LogDouble(1.0 - combinableTransitionMass)
          }

        (canZ, cantZ)
      }

    (canZ, cantZ)
  }

  //private[this] 
  val combinableSplitSums: Map[Cat, (LogDouble, LogDouble)] = (tagdict.allTags + tagdict.startTag).mapTo(getCombinableSplitSums).toMap

  def apply(x: Cat, given: Cat): LogDouble = {
    val (canZ, cantZ) = combinableSplitSums.getOrElse(given, getCombinableSplitSums(given))
    val z = if (canCombine(given, x)) canZ else cantZ
    val p = delegate(x, given)
    p / z
  }

  def sample(given: Cat): Cat = ???
}

//
//
//

/**
 * P(Tag1->Tag2) = P(Tag2), where P(Tag2) is initialized by tagPriorInitializer
 */
class TagPriorTrInitializer[Tag](tagPriorInitializer: TagPriorInitializer[Tag]) extends TransitionInitializer[Tag] {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]) = {
    val tagdict = initialTagdict.withWords(sentences.flatten.map(_._1).toSet).withTags(sentences.flatten.flatMap(_._2).toSet)
    new SimpleConditionalLogProbabilityDistribution[Tag, Tag](Map.empty[Tag, LogProbabilityDistribution[Tag]], tagPriorInitializer.fromKnownSupertagSets(sentences, tagdict), Some(tagdict.excludedTags + tagdict.endTag))
  }

  override def toString = f"TagPriorTrInitializer($tagPriorInitializer)"
}
