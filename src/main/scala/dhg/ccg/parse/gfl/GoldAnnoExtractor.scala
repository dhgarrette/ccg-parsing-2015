package dhg.ccg.parse.gfl

import dhg.util._
import dhg.gfl._
import dhg.ccg.parse._
import dhg.ccg.parse.dep.DepTree
import scalaz.{ Node => _, _ }
import Scalaz._
import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.random.RandomGenerator
import dhg.ccg.cat.Cat
import GoldAnnoExtractor._
import dhg.util.viz.TreeViz

trait GoldAnnoExtractor { // extends (CcgTree => FudgSentence) {
  def apply(ccgTree: CcgTree): FudgSentence
}

class NoOpGoldAnnoExtractor extends GoldAnnoExtractor {
  def apply(ccgTree: CcgTree): FudgSentence = apply(DepTree.fromCcgTree(ccgTree))
  def apply(depTree: DepTree): FudgSentence = {
    val (tokens, tokenNodes) = makeTokens(depTree)
    Sentence(tokens, tokenNodes.map(n => n.name -> n).toMap, Vector.empty)
  }

  override def toString = f"NoOpGoldAnnoExtractor()"
}

/**
 * Only constituents with categories in `bracketCats` will be bracketed,
 */
class AugDelegateGoldAnnoExtractor(annotationTreeAugmenter: AnnotationTreeAugmenter) extends GoldAnnoExtractor {
  private[this] val random: RandomGenerator = new MersenneTwister

  def apply(ccgTree: CcgTree): FudgSentence = {
    val d = DepTree.fromCcgTree(ccgTree)
    //    TreeViz.drawTree(d)
    val r = apply(d)
    //    TreeViz.drawTree(r.fudgTree)
    r
  }
  def apply(depTree: DepTree): FudgSentence = {
    allBracketsInFudgSentence(depTree)
  }

  def allBracketsInFudgSentence(tx: DepTree): FudgSentence = {
    val (tokens, tokenNodes) = makeTokens(tx)

    def makeTreeWithFeNodes(subtree: DepTree): AnnotationTree = subtree match {
      case DepTree(w, i, cat, Vector()) =>
        AnnotationTree(tokenNodes(i), cat, Vector.empty)
      case DepTree(w, i, cat, cs) =>
        val indices = indicesCovered(subtree).filter(_ >= 0).toVector.sorted.mkString("_")
        val feNode = FeNode(f"FE_$indices")
        val newChildren = cs.map(makeTreeWithFeNodes) :+ AnnotationTree(tokenNodes(i), cat, Vector.empty)
        AnnotationTree(feNode, cat, newChildren)
    }

    val treeWithFeNodes = makeTreeWithFeNodes(tx)
    val annotationTree = annotationTreeAugmenter(treeWithFeNodes)
    annotationTree.toFudgSentence(tokens, tokenNodes)
  }

  override def toString = f"AugDelegateGoldAnnoExtractor($annotationTreeAugmenter)"
}

class DepAddingGoldAnnoExtractor(depProportion: Double = 1.0, delegate: GoldAnnoExtractor) extends GoldAnnoExtractor {
  private[this] val random: RandomGenerator = new MersenneTwister

  def apply(ccgTree: CcgTree): FudgSentence = {
    val fs = delegate.apply(ccgTree)
    val depTree = DepTree.fromCcgTree(ccgTree)
    addDepsToFudgSentence(depTree, fs)
  }

  def addDepsToFudgSentence(t: DepTree, fs: FudgSentence): FudgSentence = {
    allDeps(t).foldLeft(fs) {
      case (s, (p, c)) if random.nextDouble < depProportion => s.addEdge(p, c)
      case (s, _) => s
    }
  }

  def allDeps(t: DepTree): Vector[(Int, Int)] = {
    t.children.flatMap(c => (t.index -> c.index) +: allDeps(c))
  }

  override def toString = f"DepAddingGoldAnnoExtractor(depProportion=$depProportion, delegate=$delegate)"
}

object GoldAnnoExtractor {

  def makeTokens(t: DepTree) = {
    def getTokens(t: DepTree): Vector[Token] = {
      val xs: Vector[Token] = t.children.flatMap(getTokens)
      Token(t.word, t.index) +: xs
    }

    val tokens = getTokens(t).sortBy(_.index)
    assert(tokens.zipWithIndex.forall { case (tok, i) => tok.index == i }, f"${tokens}")
    val tokenNodes = tokens.map(tok => WordNode(f"${tok.token}_${tok.index}", tok))
    (tokens, tokenNodes)
  }

  def allBrackets(t: DepTree): Set[(Int, Int)] = {
    def r(t: DepTree): Set[(Int, Int)] = t match {
      case DepTree(w, i, _, Vector()) => Set.empty
      case DepTree(w, i, _, cs) =>
        val theseIndices = indicesCovered(t)
        val thisBracket = indicesAreSpan(theseIndices).option((theseIndices.min, theseIndices.max + 1))
        cs.flatMap(r).toSet ++ thisBracket
    }
    t.children.flatMap(r).toSet
  }

  def indicesCovered(t: DepTree): Set[Int] = t match {
    case DepTree(w, i, _, Vector()) => Set(i)
    case DepTree(w, i, _, cs) => Set(i) | cs.flatMap(indicesCovered).toSet
  }

  def indicesAreSpan(indicesCovered: Set[Int]): Boolean = {
    indicesCovered.size > 1 && indicesCovered.max - indicesCovered.min == indicesCovered.size - 1
  }

}
