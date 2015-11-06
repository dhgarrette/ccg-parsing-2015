package dhg.ccg.parse.dep

import dhg.util._
import dhg.ccg.rule._
import dhg.ccg.parse._
import dhg.ccg.cat._
import org.netbeans.api.visual.anchor.AnchorFactory
import org.abego.treelayout.netbeans.AbegoTreeLayoutForNetbeans
import org.netbeans.api.visual.widget.ConnectionWidget
import org.netbeans.api.visual.graph.GraphScene
import javax.swing.JScrollPane
import org.netbeans.api.visual.widget.LayerWidget
import org.netbeans.api.visual.layout.LayoutFactory
import javax.swing.JDialog
import org.netbeans.api.visual.widget.LabelWidget
import org.netbeans.api.visual.widget.Widget
import java.awt.BorderLayout
import java.awt.Container

trait Graph {
  def nodes: Vector[GraphNode]
  def edges: Vector[GraphEdge]
  def pretty: String = toString
}
case class DefaultGraph(nodes: Vector[GraphNode], edges: Vector[GraphEdge]) extends Graph {
  override def pretty = {
    val b = new StringBuilder
    for (n <- nodes.sortBy(_.id)) b ++= f"${n.id}%2d  ${n.lab}\n"
    for (e <- edges) b ++= f"${e._1.id}%2d  ${e._1.lab}%-10s  ->  ${e._2.id}%2d  ${e._2.lab}\n"
    b.dropRight(1).result
  }
}

trait GraphNode  {
  def lab: String
  def id: Int
}
case class DefaultGraphNode(lab: String, id: Int) extends GraphNode

trait GraphEdge {
  def lab: Option[String]
  def _1: GraphNode
  def _2: GraphNode
}
case class DefaultGraphEdge(_1: GraphNode, _2: GraphNode, lab: Option[String] = None) extends GraphEdge

/**
 * @author dhg
 */
object DepGraph {
  def fromCcgTree(ccgTree: CcgTree): Graph = {
    val t = inner(ccgTree, new Counter(0))

    val dgb = new GraphBuilder()
    def t2g(t: DepTree) {
      val n = DefaultGraphNode(t.word, t.id)
      dgb.nodes += n
      for (c <- t.children) {
        dgb.edges += DefaultGraphEdge(n, DefaultGraphNode(c.word, c.id))
        t2g(c)
      }
    }
    t2g(t)
    dgb.result
  }

  class GraphBuilder() {
    val nodes = collection.mutable.Buffer.empty[GraphNode]
    val edges = collection.mutable.Buffer.empty[GraphEdge]
    def result = DefaultGraph(nodes.toVector, edges.toVector)
  }

  def inner(ccgTree: CcgTree, counter: Counter): DepTree = ccgTree match {
    case CcgBinode(cat, left, right) =>
      joinToBinary(cat, left.cat, right.cat, inner(left, counter), inner(right, counter))
    case CcgUnode(cat, sub) =>
      val DepTree(subWord, subId, _, subChildren) = inner(sub, counter)
      DepTree(subWord, subId, cat, subChildren)
    case CcgLeaf(cat, word, _) => DepTree(word, counter.get, cat, Vector.empty)
  }

  def joinToBinary(cat: Cat, leftCat: Cat, rightCat: Cat, lt: DepTree, rt: DepTree): DepTree = {
    if ( //
    FA(leftCat, rightCat).exists(_ u cat) ||
      FC(leftCat, rightCat).exists(_ u cat) ||
      FC2(leftCat, rightCat).exists(_ u cat))
      leftCat match {
        case ll / lr if (ll u lr) => DepTree(rt.word, rt.id, cat, lt +: rt.children)
        case _ => DepTree(lt.word, lt.id, cat, lt.children ++ Vector(rt))
      }

    else if ( //
    BA(leftCat, rightCat).exists(_ u cat) ||
      BX(leftCat, rightCat).exists(_ u cat) ||
      BX2(leftCat, rightCat).exists(_ u cat))
      rightCat match {
        case rl \ rr if (rl u rr) => DepTree(lt.word, lt.id, cat, lt.children :+ rt)
        case _ => DepTree(rt.word, rt.id, cat, Vector(lt) ++ rt.children)
      }

    else if (AnyPunctRight(leftCat, rightCat).exists(_ u cat))
      DepTree(lt.word, lt.id, cat, lt.children ++ Vector(rt))
    else if (AnyPunctLeft(leftCat, rightCat).exists(_ u cat))
      DepTree(rt.word, rt.id, cat, Vector(lt) ++ rt.children)

    else if (Merge(leftCat, rightCat).exists(_ u cat))
      DepTree(rt.word, rt.id, cat, Vector(lt) ++ rt.children)

    else if (DeleteRight(leftCat, rightCat).exists(_ u cat))
      DepTree(lt.word, lt.id, cat, lt.children ++ Vector(rt))
    else if (DeleteLeft(leftCat, rightCat).exists(_ u cat))
      DepTree(rt.word, rt.id, cat, Vector(lt) ++ rt.children)

    else {
      //println(f"dhdsgdr1   DeleteRight($leftCat, $rightCat) => ${DeleteRight(leftCat, rightCat)} =?= ${cat}")
      //println(f"dhdsgdr2   $rightCat =?= $DeleteFromLeftCat; ${rightCat == DeleteFromLeftCat}; ${rightCat eq DeleteFromLeftCat}")
      sys.error(f"couldn't convert ($cat, $leftCat, $rightCat) into a dependency")
    }
  }

  private[this] class Counter(start: Int = 0) {
    private[this] var i = start
    def get: Int = { val c = i; i += 1; c }
  }

}
