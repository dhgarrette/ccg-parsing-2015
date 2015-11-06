package dhg.ccg.data

import dhg.util._
import dhg.util.viz._
import dhg.ccg.cat._
import dhg.ccg.parse.dep._
import scalaz._
import Scalaz._

object Conll2009Reader {

  def main(args: Array[String]): Unit = {
    val r = new Conll2009Reader("/Users/dhg/Corpora/conll-2009/CoNLL2009-ST-all-trial/CoNLL2009-ST-English-trial.txt")
    val sentences = r.read()

    sentences.next.semEdges

    for (sentence <- sentences.slice(6, 7)) {
      sentence.printGrid()
      //TreeViz.drawTree(sentence.depTree)
      //GraphViz.drawGraph(toVizGraph(sentence))
      //VizGraph.drawGraph(toVizGraph(sentence))
      println(toGraphViz(sentence.toGraph))
    }

  }

  case class ConllSentence(rows: Vector[Row]) {
    def length = rows.length
    val colNames = "ID FORM LEMMA PLEMMA POS PPOS FEAT PFEAT HEAD PHEAD DEPREL PDEPREL FILLPRED PRED".splitWhitespace ++ rows.flatMap(_.pred)
    def row(i: Int) = rows(i - 1)
    def words = rows.map(_.form)
    def pos = rows.map(_.pos)
    def preds(): Vector[Row] = rows.filter(_.pred.isDefined)
    def synDeps(): Vector[Edge] = rows.map { r => Edge(rows(r.head - 1), r, r.deprel) }
    def synTree(): ConllDepTree = synTree(rows.filter(_.head == 0).only)
    private[this] def synTree(headRow: Row): ConllDepTree = ConllDepTree(headRow, rows.filter(_.head == headRow.id).map(c => synTree(c) -> c.deprel))
    def depTree(): DepTree = toSynDepTree(synTree)
    private[this] def toSynDepTree(t: ConllDepTree): DepTree = DepTree(t.word, t.row.index, cat"X", t.children.map(toSynDepTree))
    def semEdges() = (rows.filter(_.pred.isDefined) zip 1.up).flatMap { case (head, i) => rows.flatMap(child => child.arg(i).map(semDep => Edge(head, child, semDep))) }
    def printGrid(): Unit = { def p(row: Vector[String]) = println(row.map(x => f"$x%-15s").mkString(" ")); p(colNames); rows.foreach(r => p(r.values)) }

    def toGraph() = {
      def getNodes(dt: ConllDepTree): Map[Int, Row] = {
        dt.synDeps.foldLeft(Map(dt.id -> dt.row)) { case (z, (c, dr)) => z ++ getNodes(c) }
      }
      val dt = synTree()
      val nodes = getNodes(dt)
      def getEdges(dt: ConllDepTree): Vector[Edge] = {
        dt.synDeps.flatMap {
          case (c, dr) =>
            Edge(nodes(dt.id), nodes(c.id), dr) +: getEdges(c)
        }
      }
      val synEdges = getEdges(dt)
      ConllGraph(nodes.values.toVector.sortBy(_.id), synEdges, semEdges)
    }
  }

  case class Row(values: Vector[String]) extends GraphNode {
    def id: Int = values(0).toInt
    def form: String = values(1)
    def lemma: String = values(2)
    def pos: String = values(4)
    def feat: Option[String] = Some(values(6)).filterNot(Set("_"))
    def head: Int = values(8).toInt
    def deprel: String = values(10)
    def pred: Option[String] = Some(values(13)).filterNot(Set("_"))
    def arg(i: Int): Option[String] = Some(values(13 + i)).filterNot(Set("_"))
    def lab = f"$id $form"
    def index = id - 1
  }

  case class Edge(parent: Row, child: Row, name: String) extends GraphEdge { def _1 = parent; def _2 = child; def lab = Some(name) }

  case class ConllDepTree(row: Row, synDeps: Vector[(ConllDepTree, String)]) extends VizTree {
    def word: String = row.form
    def id: Int = row.id
    def label = f"${word}_${id}"
    def children: Vector[ConllDepTree] = synDeps.map(_._1)
  }

  case class ConllGraph(nodes: Vector[Row], synEdges: Vector[Edge], semEdges: Vector[Edge]) {

  }

  def toGraphViz(g: ConllGraph): String = {
    def nodeString(n: Row): String = {
      s""""${n.id} ${n.form}""""
    }
    def edgeString(edge: Edge, color: Option[String] = None): String = {
      nodeString(edge._1) + " -> " + nodeString(edge._2) +
        f"""  [ label = "${edge.name}" ]""" +
        color.fold("") { c => f"""  [ color = "$c" ]""" } +
        """  [samehead=true, sametail=true]"""
    }
    val sb = new StringBuilder
    sb ++= "digraph {\n"
    //sb ++= "  graph [splines=ortho];\n"
    for ((n, i) <- g.nodes.zipWithIndex) { sb ++= ("  " + nodeString(n) + f"""  [shape=box] [ pos="${i * 1.5},5!" ]""" + "\n") }
    for (e <- g.synEdges) { sb ++= ("  " + edgeString(e) + "\n") }
    for (e <- g.semEdges) { sb ++= ("  " + edgeString(e, Some("red")) + "\n") }
    sb ++= "}"
    sb.toString
  }

}

class Conll2009Reader(dataFile: String) {
  import Conll2009Reader._

  def read(maxLength: Int = Int.MaxValue) = {
    File(dataFile).readLines.split("")
      .filter(_.length <= maxLength)
      .map { sentenceRows => new ConllSentence(sentenceRows.map(line => new Row(line.splitWhitespace))) }
  }

}
