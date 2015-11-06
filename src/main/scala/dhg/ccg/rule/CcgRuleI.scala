package dhg.ccg.rule

import dhg.util._
import dhg.ccg.util.Indexer
import dhg.ccg.util.IndirectSparseVec
import dhg.ccg.cat._
import dhg.ccg.rule._
import scala.collection.immutable.BitSet
import scalaz._
import Scalaz._

object CcgRuleI {
  private[this]type CatSet = BitSet

  def makeIndexedRules(rules: Set[CcgRule], catIndexer: Indexer[Cat]): ( //
  Array[IndirectSparseVec[Int]], // Map[LeftCat, Map[RightCat, ParentCat]]
  IndirectSparseVec[BitSet]) // Map[ChildCat, Set[ParentCat]]
  = {
    val numCats = catIndexer.size

    val bRules = rules.collect { case r: BinaryCcgRule => r }
    val uRules = rules.collect { case r: UnaryCcgRule => r }

    val binaryRulesI: Array[IndirectSparseVec[Int]] = { // u -> v -> t
      catIndexer.objects.toArray.map { u =>
        IndirectSparseVec(catIndexer.objects.zipWithIndex.flatMap {
          case (v, i) =>
            val ts = bRules.flatMap { r => r(u, v) }
            ts.nonEmpty.option(i -> catIndexer(ts.only))
        }, numCats)
      }
    }

    val unaryRulesI: IndirectSparseVec[BitSet] = { // u -> ts
      IndirectSparseVec(catIndexer.objects.zipWithIndex.flatMap {
        case (u, i) =>
          val ts = uRules.flatMap { r => r(u) }
          ts.nonEmpty.option(i -> ts.map(catIndexer).toBitSet)
      }, numCats)
    }

    (binaryRulesI, unaryRulesI)
  }

}
