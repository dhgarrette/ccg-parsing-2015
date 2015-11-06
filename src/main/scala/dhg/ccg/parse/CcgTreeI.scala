package dhg.ccg.parse

import scala.Vector
import dhg.ccg.cat._
import dhg.util.viz.VizTree
import scalaz._
import Scalaz._
import dhg.ccg.parse.pcfg._
import dhg.ccg.util.Indexer

trait CcgTreeI extends VizTree {
  type Word = Int
  type Cat = Int

  def cat: Cat
  def leaves: Vector[CcgLeafI]
  final def words: Vector[Word] = leaves.map { case CcgLeafI(cat, word) => word }
  final def supertags: Vector[Cat] = leaves.map { case CcgLeafI(cat, word) => cat }
  final def tagged: Vector[(Word, Cat)] = leaves.map { case CcgLeafI(cat, word) => (word, cat) }
  def isModifier: Boolean
  def length : Int
  def repr: String
}

object CcgTreeI {
  type Word = Int
  type Cat = Int

  def unapply(t: CcgTreeI) = Some(t.cat)

  def apply(t: CcgTree, catIndexer: Indexer[dhg.ccg.cat.Cat], wordIndexer: Indexer[String]): CcgTreeI = to(t, catIndexer, wordIndexer)
  def to(t: CcgTree, catIndexer: Indexer[dhg.ccg.cat.Cat], wordIndexer: Indexer[String]): CcgTreeI = t match {
    case CcgBinode(c, l, r) => CcgBinodeI(catIndexer(c), CcgTreeI(l, catIndexer, wordIndexer), CcgTreeI(r, catIndexer, wordIndexer))
    case CcgUnode(c, s) => CcgUnodeI(catIndexer(c), CcgTreeI(s, catIndexer, wordIndexer))
    case CcgLeaf(c, w, _) => CcgLeafI(catIndexer(c), wordIndexer(w))
  }

  def toCcgTree(t: CcgTreeI, catIndexer: Indexer[dhg.ccg.cat.Cat], wordIndexer: Indexer[String]): CcgTree = from(t, catIndexer, wordIndexer)
  def from(t: CcgTreeI, catIndexer: Indexer[dhg.ccg.cat.Cat], wordIndexer: Indexer[String]): CcgTree = t match {
    case CcgBinodeI(c, l, r) => CcgBinode(catIndexer.obj(c), toCcgTree(l, catIndexer, wordIndexer), toCcgTree(r, catIndexer, wordIndexer))
    case CcgUnodeI(c, s) => CcgUnode(catIndexer.obj(c), toCcgTree(s, catIndexer, wordIndexer))
    case CcgLeafI(c, w) => CcgLeaf(catIndexer.obj(c), wordIndexer.obj(w), "FAKEPOS")
  }
}

case class CcgBinodeI(cat: Int, left: CcgTreeI, right: CcgTreeI) extends CcgTreeI {
  //assert(cat u rule(left.cat, right.cat).get, f"$rule(${left.cat}, ${right.cat}) => $cat")
  override def leaves = left.leaves ++ right.leaves
  override def toString: String = f"[$cat $left $right]"
  override def label = cat.toString
  override def children = Vector(left, right)
  override val length : Int = left.length + right.length
  override lazy val isModifier = (left == right)
  override def repr = f"CcgBinodeI($cat, ${left.repr}, ${right.repr})"
}

case class CcgUnodeI(cat: Int, sub: CcgTreeI) extends CcgTreeI {
  //assert(cat u rule(sub.cat).get, f"$rule(${sub.cat}) => $cat")
  override def leaves = sub.leaves
  override def toString: String = f"[$cat $sub]"
  override def label = cat.toString
  override def children = Vector(sub)
  override final def isModifier = false
  override val length : Int = sub.length
  override def repr = f"CcgUnodeI($cat, ${sub.repr})"
}

case class CcgLeafI(cat: Int, word: Int) extends CcgTreeI {
  override def leaves = Vector(this)
  override def toString: String = f"[$cat $word]"
  override def label = cat.toString
  override def children = Vector(new VizTree { def label = f"${word}"; def children = Vector() })
  override final def isModifier = false
  override val length : Int = 1
  override def repr = f"""CcgLeafI($cat, "$word")"""
}
