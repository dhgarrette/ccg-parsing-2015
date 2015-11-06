package dhg.ccg.parse.dep

import dhg.util._
import dhg.util.viz._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.parse._

case class DepTree(word: String, index: Int, cat: Cat, children: Vector[DepTree]) extends VizTree {
  def prependChild(t: DepTree) = DepTree(word, index, cat, t +: children)
  def appendChild(t: DepTree) = DepTree(word, index, cat, children :+ t)
  override def toString = f"""DepTree("$word", $index, ${cat}, $children)"""
  def label = f"${word}_${index}_${cat}"
  def id = index
}

object DepTree {

  def fromCcgTree(ccgTree: CcgTree): DepTree = {
    def inner(ccgTree: CcgTree, counter: Counter): DepTree = ccgTree match {
      case CcgBinode(cat, left, right) =>
        joinToBinary(cat, left.cat, right.cat, inner(left, counter), inner(right, counter))
      case CcgUnode(cat, sub) =>
        val DepTree(subWord, subIndex, _, subChildren) = inner(sub, counter)
        DepTree(subWord, subIndex, cat, subChildren)
      case CcgLeaf(cat, word, _) => DepTree(word, counter.get, cat, Vector.empty)
    }
    inner(ccgTree, new Counter(0))
  }

  def joinToBinary(cat: Cat, leftCat: Cat, rightCat: Cat, lt: DepTree, rt: DepTree): DepTree = {
    if (leftIsHead(cat, leftCat, rightCat))
      DepTree(lt.word, lt.index, cat, lt.children :+ rt)
    else
      DepTree(rt.word, rt.index, cat, lt +: rt.children)
  }

  def leftIsHead(cat: Cat, leftCat: Cat, rightCat: Cat): Boolean = {
    if ( //
    FA(leftCat, rightCat).exists(_ u cat) ||
      FC(leftCat, rightCat).exists(_ u cat) ||
      FC2(leftCat, rightCat).exists(_ u cat) ||
      FX(leftCat, rightCat).exists(_ u cat) ||
      FX2(leftCat, rightCat).exists(_ u cat))
      leftCat match {
        case ll / lr if (ll u lr) => false // left is modifier
        case _ => true
      }

    else if ( //
    BA(leftCat, rightCat).exists(_ u cat) ||
      BC(leftCat, rightCat).exists(_ u cat) ||
      BC2(leftCat, rightCat).exists(_ u cat) ||
      BX(leftCat, rightCat).exists(_ u cat) ||
      BX2(leftCat, rightCat).exists(_ u cat))
      rightCat match {
        case rl \ rr if (rl u rr) => true // right is modifier
        case _ => false
      }

    else if (AnyPunctRight(leftCat, rightCat).exists(_ u cat)) true
    else if (AnyPunctLeft(leftCat, rightCat).exists(_ u cat)) false

    else if (Merge(leftCat, rightCat).exists(_ u cat)) false

    else if (DeleteRight(leftCat, rightCat).exists(_ u cat)) true
    else if (DeleteLeft(leftCat, rightCat).exists(_ u cat)) false

    else {
      sys.error(f"couldn't figure out which side was the head (cat=$cat [${cat.getClass.getName}], leftCat=$leftCat [${leftCat.getClass.getName}], rightCat=$rightCat [${rightCat.getClass.getName}])")
    }
  }

  private[this] class Counter(start: Int = 0) {
    private[this] var i = start
    def get: Int = { val c = i; i += 1; c }
  }

}
