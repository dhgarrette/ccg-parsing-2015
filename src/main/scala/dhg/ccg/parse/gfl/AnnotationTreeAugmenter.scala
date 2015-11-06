package dhg.ccg.parse.gfl

import dhg.util._
import dhg.util.viz._
import dhg.gfl._
import dhg.ccg.parse._
import dhg.ccg.parse.dep.DepTree
import scalaz.{ Node => _, _ }
import Scalaz._
import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.random.RandomGenerator
import dhg.ccg.cat.Cat
import GoldAnnoExtractor._

trait AnnotationTreeAugmenter {
  def apply(t: AnnotationTree): AnnotationTree
}

class NoOpAnnotationTreeAugmenter extends AnnotationTreeAugmenter {
  def apply(t: AnnotationTree): AnnotationTree = t
}

class BracketAnnotationTreeAugmenter(bracketProportion: Double = 1.0, bracketHighMatchBase: Boolean = false, bracketCats: (Cat => Boolean) = UniversalSet(), baseCats: Boolean = true, highCats: Boolean = true) extends AnnotationTreeAugmenter {
  private[this] val random: RandomGenerator = new MersenneTwister

  def apply(treeWithFeNodes: AnnotationTree): AnnotationTree = {
    //TreeViz.drawTree(treeWithFeNodes)
    //println(f"base brackets = ${numBaseBrackets(treeWithFeNodes)}")
    val possiblyBaseOnlyTree = if (!bracketHighMatchBase) AnnotationTree(treeWithFeNodes.node, treeWithFeNodes.cat, treeWithFeNodes.subtrees.flatMap(basehigh)) else treeWithFeNodes
    //TreeViz.drawTree(possiblyBaseOnlyTree)
    val validCatOnlyTree = AnnotationTree(possiblyBaseOnlyTree.node, possiblyBaseOnlyTree.cat, possiblyBaseOnlyTree.subtrees.flatMap(getValidCatOnlyTree))
    //TreeViz.drawTree(validCatOnlyTree)
    val bracketPrunedTree =
      if (!bracketHighMatchBase) AnnotationTree(validCatOnlyTree.node, validCatOnlyTree.cat, validCatOnlyTree.subtrees.flatMap(removeRandomBrackets))
      else takeTopNBrackets(validCatOnlyTree, numBaseBrackets(treeWithFeNodes) + 1)
    //TreeViz.drawTree(bracketPrunedTree)
    bracketPrunedTree
  }

  private[this] def numBaseBrackets(t: AnnotationTree): Int = {
    (if (!t.isLeaf && t.subtrees.forall(_.isLeaf)) 1 else 0) + t.subtrees.sumBy(numBaseBrackets)
  }

  private[this] def basehigh(t: AnnotationTree): Vector[AnnotationTree] = {
    val keep =
      if (t.isLeaf) {
        true
      }
      else {
        val isBase = t.subtrees.forall(_.isLeaf)
        if (isBase) {
          baseCats
        }
        else { // higher level constituent
          highCats
        }
      }
    val newT = AnnotationTree(t.node, t.cat, t.subtrees.flatMap(basehigh))
    if (keep) Vector(newT) else newT.subtrees
  }

  private[this] def getValidCatOnlyTree(t: AnnotationTree): Vector[AnnotationTree] = {
    val newT = AnnotationTree(t.node, t.cat, t.subtrees.flatMap(getValidCatOnlyTree))
    //println(f"${newT.cat.noFeat}  ${bracketCats(newT.cat.noFeat)}")
    if (newT.isLeaf || bracketCats(newT.cat.noFeat)) Vector(newT) else newT.subtrees
  }

  private[this] def removeRandomBrackets(t: AnnotationTree): Vector[AnnotationTree] = {
    val newT = AnnotationTree(t.node, t.cat, t.subtrees.flatMap(removeRandomBrackets))
    if (newT.isLeaf || random.nextDouble < bracketProportion) Vector(newT) else newT.subtrees
  }

  private[this] def takeTopNBrackets(t: AnnotationTree, n: Int): AnnotationTree = {
    var bfsQueue = Vector(t)
    var bfsIndexSets = Vector.empty[Set[Int]]
    while (bfsIndexSets.size < n && bfsQueue.nonEmpty) {
      val head = bfsQueue.head; bfsQueue = bfsQueue.tail ++ head.subtrees.reverse.filter(!_.isLeaf)
      bfsIndexSets = bfsIndexSets :+ head.indicesCoveredRecursively
      //println(f"head=${head.label}  ${bfsIndexSets.map { s => RangeString(s.toSeq.sorted) }}")
    }

    val bfsIndexSetsSet = bfsIndexSets.toSet
    def r(t: AnnotationTree): Vector[AnnotationTree] = {
      val newT = AnnotationTree(t.node, t.cat, t.subtrees.map(r).flatten)
      if (newT.isLeaf || bfsIndexSetsSet(newT.indicesCoveredRecursively)) Vector(newT) else newT.subtrees
    }
    r(t).only
    //AnnotationTree(t.node, t.cat, r(t))
  }

  override def toString = f"BracketAnnotationTreeAugmenter(bracketProportion=$bracketProportion, bracketCats=$bracketCats, baseCats=$baseCats, highCats=$highCats)"
}

//

case class AnnotationTree(node: Node, cat: Cat, subtrees: Vector[AnnotationTree]) extends VizTree {
  def label = f"${node.name}_${cat}"
  def children = subtrees.sortBy(_.indicesCoveredRecursively.min)
  def isLeaf = subtrees.isEmpty
  lazy val indicesCoveredRecursively: Set[Int] = node.tokens.map(_.index).toSet ++ subtrees.flatMap(_.indicesCoveredRecursively)

  def toFudgSentence(tokens: Vector[Token], tokenNodes: Vector[WordNode]) = {
    def getEdges(t: AnnotationTree): Vector[Edge] = t.subtrees.map(s => Edge(t.node, s.node, Some("fe"))) ++ t.subtrees.flatMap(getEdges)
    val feEdges = getEdges(this)
    val feNodes = feEdges.map(_.parent)
    val nodeMap = (tokenNodes ++ feNodes).map(n => n.name -> n).toMap
    Sentence(tokens, nodeMap, feEdges)
  }

  //override def toString = f"T(${node.name}, $cat, ${subtrees.mkString("[", ", ", "]")})"
  override def toString = if(isLeaf) node.tokens.map(_.token).mkString(" ") else subtrees.mkString("(", " ", ")")
}
