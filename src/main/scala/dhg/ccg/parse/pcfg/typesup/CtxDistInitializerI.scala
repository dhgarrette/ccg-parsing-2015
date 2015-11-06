package dhg.ccg.parse.pcfg.typesup

import dhg.util._
import scalaz._
import scalaz.Scalaz._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tag.learn.EmissionInitializer
import dhg.ccg.tag.learn.TagPriorInitializer
import dhg.ccg.parse.pcfg._
import dhg.ccg.util._
import dhg.ccg.rule._

trait LctxDistInitializerI {
  type Word = Int
  type Tag = Int

  def apply(gcs: Vector[CfgGuideChartI],
    knownLctxs: Array[Array[Int]], //                 t -> ls
    combinableMultiplier: Double = 10.0,
    numCats: Int) //
    : Array[IndirectSparseVec[LogDouble]] //                  t -> w -> p
}

class UniformCombLctxDistInitializerI(catIndexer: Indexer[Cat], wordIndexer: Indexer[String])
    extends LctxDistInitializerI {
  val canCombine = new SimpleCatCanCombine(Vector(FA, FAn2np, BA, BAn2np, FC, FC2, BC, /*FX,*/ BX, BX2), StartCat, EndCat)

  def apply(gcs: Vector[CfgGuideChartI],
    knownLctxs: Array[Array[Int]], //                 t -> ls
    combinableMultiplier: Double = 10.0,
    numCats: Int) //
    : Array[IndirectSparseVec[LogDouble]] //                  t -> w -> p
    = {

    knownLctxs.zipWithIndex.map {
      case (knownLs, t) =>
        if (knownLs != null) {
          val activeCount = knownLs.length
          val activeProbs = new Array[LogDouble](activeCount)
          var li = 0
          while (li < activeCount) {
            val l = knownLs(li)
            activeProbs(li) = LogDouble((
              if (canCombine(catIndexer.obj(l), catIndexer.obj(t)))
                combinableMultiplier
              else
                1.0) / (numCats - 1))
            //println(f"nsiod::  ${catIndexer.obj(t)} -> ${catIndexer.obj(l)} -> ${canCombine(catIndexer.obj(t), catIndexer.obj(l))} => ${activeProbs(li).toDouble}%.2f")
            li += 1
          }
          new IndirectSparseVec(knownLs, activeProbs, activeCount, numCats)
        }
        else null
    }
  }
}

class CatpriorCombLctxDistInitializerI(catIndexer: Indexer[Cat], wordIndexer: Indexer[String], catPriorI: Array[LogDouble])
    extends LctxDistInitializerI {
  val canCombine = new SimpleCatCanCombine(Vector(FA, FAn2np, BA, BAn2np, FC, FC2, BC, /*FX,*/ BX, BX2), StartCat, EndCat)

  private[this] val totalCatPrior = LogDouble.sum(catPriorI)

  def apply(gcs: Vector[CfgGuideChartI],
    knownLctxs: Array[Array[Int]], //                 t -> ls
    combinableMultiplier: Double = 10.0,
    numCats: Int) //
    : Array[IndirectSparseVec[LogDouble]] //                  t -> w -> p
    = {

    knownLctxs.zipWithIndex.map {
      case (knownLs, t) =>
        if (knownLs != null) {
          val activeCount = knownLs.length
          val activeProbs = new Array[LogDouble](activeCount)
          var li = 0
          while (li < activeCount) {
            val l = knownLs(li)
            activeProbs(li) = LogDouble(
              if (canCombine(catIndexer.obj(l), catIndexer.obj(t)))
                combinableMultiplier
              else
                1.0) * (catPriorI(l) * totalCatPrior)
            //println(f"nsiod::  ${catIndexer.obj(t)} -> ${catIndexer.obj(l)} -> ${canCombine(catIndexer.obj(t), catIndexer.obj(l))} => ${activeProbs(li).toDouble}%.2f")
            li += 1
          }
          new IndirectSparseVec(knownLs, activeProbs, activeCount, numCats)
        }
        else null
    }
  }
}

//
//
//

trait RctxDistInitializerI {
  type Word = Int
  type Tag = Int

  def apply(gcs: Vector[CfgGuideChartI],
    knownRctxs: Array[Array[Int]], //                 t -> rs
    combinableMultiplier: Double = 10.0,
    numCats: Int) //
    : Array[IndirectSparseVec[LogDouble]] //                  t -> w -> p
}

class UniformCombRctxDistInitializerI(catIndexer: Indexer[Cat], wordIndexer: Indexer[String])
    extends RctxDistInitializerI {
  val canCombine = new SimpleCatCanCombine(Vector(FA, FAn2np, BA, BAn2np, FC, FC2, BC, /*FX,*/ BX, BX2), StartCat, EndCat)

  def apply(gcs: Vector[CfgGuideChartI],
    knownRctxs: Array[Array[Int]], //                 t -> rs
    combinableMultiplier: Double = 10.0,
    numCats: Int) //
    : Array[IndirectSparseVec[LogDouble]] //                  t -> w -> p
    = {

    knownRctxs.zipWithIndex.map {
      case (knownRs, t) =>
        if (knownRs != null) {
          val activeCount = knownRs.length
          val activeProbs = new Array[LogDouble](activeCount)
          var ri = 0
          while (ri < activeCount) {
            val r = knownRs(ri)
            activeProbs(ri) = LogDouble((
              if (canCombine(catIndexer.obj(t), catIndexer.obj(r)))
                combinableMultiplier
              else
                1.0) / (numCats - 1))
            //println(f"miovios::  ${catIndexer.obj(t)} -> ${catIndexer.obj(r)} -> ${canCombine(catIndexer.obj(t), catIndexer.obj(r))} => ${activeProbs(ri).toDouble}%.2f")
            ri += 1
          }
          new IndirectSparseVec(knownRs, activeProbs, activeCount, numCats)
        }
        else null
    }
  }
}

class CatpriorCombRctxDistInitializerI(catIndexer: Indexer[Cat], wordIndexer: Indexer[String], catPriorI: Array[LogDouble])
    extends RctxDistInitializerI {
  val canCombine = new SimpleCatCanCombine(Vector(FA, FAn2np, BA, BAn2np, FC, FC2, BC, /*FX,*/ BX, BX2), StartCat, EndCat)

  private[this] val totalCatPrior = LogDouble.sum(catPriorI)

  def apply(gcs: Vector[CfgGuideChartI],
    knownRctxs: Array[Array[Int]], //                 t -> rs
    combinableMultiplier: Double = 10.0,
    numCats: Int) //
    : Array[IndirectSparseVec[LogDouble]] //                  t -> w -> p
    = {

    knownRctxs.zipWithIndex.map {
      case (knownRs, t) =>
        if (knownRs != null) {
          val activeCount = knownRs.length
          val activeProbs = new Array[LogDouble](activeCount)
          var ri = 0
          while (ri < activeCount) {
            val r = knownRs(ri)
            activeProbs(ri) = LogDouble(
              if (canCombine(catIndexer.obj(t), catIndexer.obj(r)))
                combinableMultiplier
              else
                1.0) * (catPriorI(r) * totalCatPrior)
            //println(f"miovios::  ${catIndexer.obj(t)} -> ${catIndexer.obj(r)} -> ${canCombine(catIndexer.obj(t), catIndexer.obj(r))} => ${activeProbs(ri).toDouble}%.2f")
            ri += 1
          }
          new IndirectSparseVec(knownRs, activeProbs, activeCount, numCats)
        }
        else null
    }
  }

}




  //class TagdictCtxDistInitializerI(
  //    catIndexer: Indexer[Cat],
  //    wordIndexer: Indexer[String]) {
  //
  //  def apply(gcs: Vector[CfgGuideChartI],
  //    knownRctxs: Array[Array[Int]], //                 t -> rs
  //    catIndexer: SimpleIndexer[dhg.ccg.cat.Cat],
  //    wordIndexer: SimpleIndexer[String],
  //    combinableTransitionMass: Double = 0.85,
  //    numCats: Int) //
  //    : Array[IndirectSparseVec[LogDouble]] //                  t -> w -> p
  //    = {
  //
  //    val canCombine = new SimpleCatCanCombine(Vector(FA, FAn2np, BA, BAn2np, FC, FC2, BC, /*FX,*/ BX, BX2), StartCat, EndCat)
  //
  //    //
  //
  //    val countsTable = 
  //      knownRctxs.zipWithIndex.map {
  //        case (knownRs, t) =>
  //          if (knownRs != null)
  //            new IndirectSparseVec(knownRs, new Array[Int](knownRs.length), knownRs.length, numCats)
  //          else null
  //      }
  //
  //      for {
  //        gc <- gcs
  //        //_ = CfgGuideChartI.from(gc,catIndexer,wordIndexer).draw()
  //        supertagSets = Array(0) +: gc.supertagSets.toArray :+ Array(1)
  //        (i, j, cell) <- gc.bottomUpNodes
  //        //_ = println(f"($i,$j): cell = ${cell.toVector.map(_._1).map(catIndexer.obj)}") 
  //        (t, _) <- cell
  //        //_ = println(f"  t=${catIndexer.obj(t)}%-10s") 
  //        r <- supertagSets(j + 1)
  //        //_ = println(f"  t=${catIndexer.obj(t)}%-10s, r=${catIndexer.obj(r)}%-10s") 
  //      } {
  //        println(f"  t=${catIndexer.obj(t)}%-10s, r=${catIndexer.obj(r)}%-10s, ${catIndexer.obj(t)}%-10s -> ${catIndexer.obj(r)}%-10s -> +1")
  //        countsTable(t)(r) += 1
  //      }
  //
  //    //
  //
  //    val potentialTransitions =
  //      for {
  //        gc <- gcs
  //        Array(currSet, nextSet) <- (Array(0) +: gc.supertagSets :+ Array(1)).sliding(2)
  //        count = 1.0 / (currSet.length * nextSet.length)
  //        c <- currSet; n <- nextSet
  //      } yield {
  //        (c, (n, count))
  //      }
  //
  ////    trCounts.foreach {
  ////      case (t, rs) =>
  ////        rs.foreach {
  ////          case (r, c) =>
  ////            println(f"${catIndexer.obj(t)}%-10s -> ${catIndexer.obj(r)}%-10s -> ${c}%.3f")
  ////        }
  ////    }
  ////
  ////    knownRctxs.zipWithIndex.map {
  ////      case (knownRs, t) =>
  ////        if (knownRs != null) {
  ////          var can_CombineSum = 0.0
  ////          var cantCombineSum = 0.0
  ////          val activeCounts = knownRs.map { r =>
  ////            val count = trCounts(t)(r) + 0.1
  ////            if (canCombine(catIndexer.obj(t), catIndexer.obj(r))) {
  ////              can_CombineSum += count
  ////            }
  ////            else {
  ////              cantCombineSum += count
  ////            }
  ////            count
  ////          }
  ////
  ////          val (can_Z, cantZ) =
  ////            if (can_CombineSum / (can_CombineSum + cantCombineSum) > combinableTransitionMass) {
  ////              (1.0, 1.0)
  ////            }
  ////            else {
  ////              (can_CombineSum / combinableTransitionMass, cantCombineSum / (1.0 - combinableTransitionMass))
  ////            }
  ////
  ////          val activeProbs = knownRs.map { r =>
  ////            val count = trCounts(t)(r) + 0.1
  ////            if (canCombine(catIndexer.obj(t), catIndexer.obj(r))) {
  ////              LogDouble(count / can_Z)
  ////            }
  ////            else {
  ////              LogDouble(count / cantZ)
  ////            }
  ////          }
  ////
  ////          //assert(activeProbs.sum approx LogDouble.one)
  ////
  ////          new IndirectSparseVec(knownRs, activeProbs, knownRs.length, numCats)
  ////        }
  ////        else null
  ////    }
  //      ???
  //
  //  }

  