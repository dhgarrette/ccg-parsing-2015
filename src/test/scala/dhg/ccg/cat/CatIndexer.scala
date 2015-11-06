package dhg.ccg.cat

import java.io.FileInputStream
import java.io.ObjectInputStream
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.util.zip._
import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.parse._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.util.DrawMatrix
import dhg.util.GraphUtil._
import dhg.gfl.FudgSentence
import dhg.util._
import dhg.util.viz.TreeViz
import scalaz._
import scalaz.Scalaz._
import scala.util.Try
import dhg.ccg.util.Indexer
import dhg.ccg.util.SimpleIndexer

object CatIndexer {

  def apply(objects: TraversableOnce[Cat], rules: Set[CcgRule]): Indexer[Cat] = {
    val objectSet = objects.toSet
    val allDerived = CcgRule.allDerivable(rules, objectSet)
    new SimpleIndexer(allDerived.toVector)
  }

}
