package dhg.ccg.parse

import scala.Vector
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.parse.pcfg._
import dhg.util.viz.VizTree
import scalaz._
import Scalaz._

trait CcgTree extends Serializable with VizTree {
  type Word = String

  def cat: Cat
  def allCats: Set[Cat]
  def allCatsVec: Vector[Cat]
  def leaves: Vector[CcgLeaf]
  final def words: Vector[Word] = leaves.map { case CcgLeaf(cat, word, _) => word }
  final def supertags: Vector[Cat] = leaves.map { case CcgLeaf(cat, word, _) => cat }
  final def tagged: Vector[(Word, Cat)] = leaves.map { case CcgLeaf(cat, word, _) => (word, cat) }
  final def wordposcats: Vector[(Word, String, Cat)] = leaves.map { case CcgLeaf(cat, word, pos) => (word, pos, cat) }
  def isModifier: Boolean
  final def length = leaves.size
  def repr: String
}

object CcgTree {
  def unapply(t: CcgTree) = Some(t.cat)
}

case class CcgBinode(cat: Cat, left: CcgTree, right: CcgTree) extends CcgTree {
  //assert(cat u rule(left.cat, right.cat).get, f"$rule(${left.cat}, ${right.cat}) => $cat")
  override def allCats: Set[Cat] = Set(cat) | left.allCats | right.allCats
  override def allCatsVec: Vector[Cat] = Vector(cat) ++ left.allCatsVec ++ right.allCatsVec
  override def leaves = left.leaves ++ right.leaves
  override def toString: String = f"[$cat $left $right]"
  override def label = cat.toString
  override def children = Vector(left, right)
  lazy val isModifier = (left == right)
  override def repr = f"CcgBinode($cat, ${left.repr}, ${right.repr})"
}

case class CcgUnode(cat: Cat, sub: CcgTree) extends CcgTree {
  //assert(cat u rule(sub.cat).get, f"$rule(${sub.cat}) => $cat")
  override def allCats: Set[Cat] = Set(cat) | sub.allCats
  override def allCatsVec: Vector[Cat] = Vector(cat) ++ sub.allCatsVec
  override def leaves = sub.leaves
  override def toString: String = f"[$cat $sub]"
  override def label = cat.toString
  override def children = Vector(sub)
  final def isModifier = false
  override def repr = f"CcgUnode($cat, ${sub.repr})"
}

case class CcgLeaf(cat: Cat, word: String, pos: String) extends CcgTree {
  override def allCats: Set[Cat] = Set(cat)
  override def allCatsVec: Vector[Cat] = Vector(cat)
  override def leaves = Vector(this)
  override def toString: String = f"[$cat $word]"
  override def label = cat.toString
  override def children = Vector(new VizTree { def label = f"${word}"; def children = Vector() })
  final def isModifier = false
  override def repr = f"""CcgLeaf($cat, "$word")"""
}

//

trait RuleViolationFinder {
  def violations(t: CcgTree): Vector[(Cat, Prod)]
}

class SimpleRuleViolationFinder(rules: Vector[CcgRule], allowAllUnary: Boolean = false) extends RuleViolationFinder {
  def violations(t: CcgTree): Vector[(Cat, Prod)] = {
    t match {
      case CcgBinode(cat, left, right) =>
        val sv = violations(left) ++ violations(right)
        if (rules.collect { case rule: BinaryCcgRule => rule(left.cat, right.cat) }.flatten.exists(_ u cat)) sv
        else (cat, BinaryProd(left.cat, right.cat)) +: sv

      case CcgUnode(cat, sub) =>
        val sv = violations(sub)
        if (allowAllUnary || rules.collect { case rule: UnaryCcgRule => rule(sub.cat) }.flatten.exists(_ u cat)) sv
        else (cat, UnaryProd(sub.cat)) +: sv

      case CcgLeaf(cat, word, _) => Vector.empty
    }
  }

  override def toString = f"SimpleRuleViolationFinder($rules)"
}

////
////

object CcgTreeUtil {
  //  def unapply(t: CcgTree) = Some(t.cat)
  //
  //  def traverse[R](tree: CcgTree)(mapper: CcgTree => R)(reducer: (R, R) => R): R = {
  //    tree match {
  //      case CcgBinode(cat, l, r) => reducer(mapper(tree), reducer(traverse(l)(mapper)(reducer), traverse(r)(mapper)(reducer)))
  //      case CcgLeaf(cat, w) => mapper(tree)
  //    }
  //  }
  //
  //  /**
  //   * val cats = trees.flatMap(t => traverse2(t)((c,l,r)=>Set(c))((c,w,i)=>Set(c))(_|_)).toSet
  //   * val cats = trees.flatMap(t => traverse2(t)((c,l,r)=>Vector(c))((c,w,i)=>Vector(c))(_|_))
  //   */
  //  def traverse2[R](
  //    tree: CcgTree)(
  //      ntHandle: (Cat, CcgTree, CcgTree) => R)(
  //        tHandle: (Cat, Word) => R)(
  //          reducer: (R, R) => R): R = {
  //    tree match {
  //      case CcgBinode(cat, l, r) => reducer(ntHandle(cat, l, r), reducer(traverse2(l)(ntHandle)(tHandle)(reducer), traverse2(r)(ntHandle)(tHandle)(reducer)))
  //      case CcgLeaf(cat, w) => tHandle(cat, w)
  //    }
  //  }

  def getSpans(tree: CcgTree): Set[(Int, Int)] = {
    def traverse(t: CcgTree, i: Int, j: Int): Set[(Int, Int)] = t match {
      case CcgLeaf(cat, word, _) => Set((i, j))
      case CcgUnode(cat, s) => traverse(s, i, j)
      case CcgBinode(cat, l, r) => traverse(l, i, i + l.length) | traverse(r, i + l.length, j)
    }
    traverse(tree, 0, tree.length)
  }

  def toNtTable(tree: CcgTree): Array[Array[Option[Cat]]] = {
    val n = tree.length
    val table = Array.fill(n)(Array.fill(n + 1)(none[Cat]))
    def traverse(t: CcgTree, i: Int, j: Int): Unit = {
      table(i)(j) = Some(t.cat)

      t match {
        case CcgLeaf(cat, word, _) =>
          assert(i + 1 == j)

        case CcgBinode(cat, l, r) =>
          assert(i + 1 < j)

          val li = i
          val lj = i + l.length
          val ri = i + l.length
          val rj = j

          assert(li + l.length == lj, f"i=$i, j=$j: li=$li, lj=$lj, l.length=${l.length}")
          assert(ri + r.length == rj, f"i=$i, j=$j: ri=$ri, rj=$rj, r.length=${r.length}")

          assert(rj == i + t.length, f"i=$i, j=$j: rj=$rj, t.length=${t.length}")
          assert(ri == rj - r.length, f"i=$i, j=$j: t.length=${t.length}, r.length=${r.length}")

          traverse(l, li, lj)
          traverse(r, ri, rj)
      }
    }
    traverse(tree, 0, n)
    table
  }

  //  def constituentSpans(tree: CcgTree) = {
  //    val n = tree.words.size
  //    def traverse(t: CcgTree, i: Int, j: Int): Set[(Int, Int)] = {
  //      (t match {
  //        case CcgLeaf(cat, word, _) =>
  //          assert(i + 1 == j)
  //          Set.empty[(Int, Int)]
  //
  //        case CcgBinode(cat, l, r) =>
  //          assert(i + 1 < j)
  //
  //          val li = i
  //          val lj = i + l.length
  //          val ri = i + l.length
  //          val rj = j
  //
  //          assert(li + l.length == lj, f"i=$i, j=$j: li=$li, lj=$lj, l.length=${l.length}")
  //          assert(ri + r.length == rj, f"i=$i, j=$j: ri=$ri, rj=$rj, r.length=${r.length}")
  //
  //          assert(rj == i + t.length, f"i=$i, j=$j: rj=$rj, t.length=${t.length}")
  //          assert(ri == rj - r.length, f"i=$i, j=$j: t.length=${t.length}, r.length=${r.length}")
  //
  //          traverse(l, li, lj) | traverse(r, ri, rj)
  //      }) + ((i, j))
  //    }
  //    traverse(tree, 0, n)
  //  }
  //
  //  def constituentCatSpans(tree: CcgTree) = {
  //    val n = tree.words.size
  //    def traverse(t: CcgTree, i: Int, j: Int): Set[(Cat, (Int, Int))] = {
  //      (t match {
  //        case CcgLeaf(cat, word, _) =>
  //          assert(i + 1 == j)
  //          Set.empty[(Cat, (Int, Int))]
  //
  //        case CcgBinode(cat, l, r) =>
  //          assert(i + 1 < j)
  //
  //          val li = i
  //          val lj = i + l.length
  //          val ri = i + l.length
  //          val rj = j
  //
  //          assert(li + l.length == lj, f"i=$i, j=$j: li=$li, lj=$lj, l.length=${l.length}")
  //          assert(ri + r.length == rj, f"i=$i, j=$j: ri=$ri, rj=$rj, r.length=${r.length}")
  //
  //          assert(rj == i + t.length, f"i=$i, j=$j: rj=$rj, t.length=${t.length}")
  //          assert(ri == rj - r.length, f"i=$i, j=$j: t.length=${t.length}, r.length=${r.length}")
  //
  //          traverse(l, li, lj) | traverse(r, ri, rj)
  //      }) + (t.cat -> (i, j))
  //    }
  //    traverse(tree, 0, n)
  //  }
}
