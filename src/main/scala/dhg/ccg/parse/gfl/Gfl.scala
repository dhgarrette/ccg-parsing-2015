package dhg.ccg.parse.gfl

import dhg.gfl.Fudg._
import dhg.util._
import dhg.util.viz.VizTree
import scalaz._
import Scalaz._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.tagdict._
import dhg.ccg.prob.DefaultedLogProbabilityDistribution
import dhg.ccg.prob._

object Gfl {
  
  

  def main(args: Array[String]): Unit = {

    val gcb = new SimpleCfgGuideChartBuilder(Vector(FA, BA), allowTerminalDeletion = false)
    val parser = new PcfgParser(
      new UniformDefaultLogProbabilityDistribution[Cat](LogDouble(0.01)),
      new UnconditionalWrappingConditionalLogProbabilityDistribution[Cat, Prod](new UniformDefaultLogProbabilityDistribution(LogDouble(0.01))))

    val s: NonPuncCat = cat"S".asInstanceOf[NonPuncCat]
    val np: NonPuncCat = cat"NP".asInstanceOf[NonPuncCat]
    val n: NonPuncCat = cat"N".asInstanceOf[NonPuncCat]
    val pp: NonPuncCat = cat"PP".asInstanceOf[NonPuncCat]

    val Det: Set[Cat] = Set(
      np / n)
    val Adj: Set[Cat] = Set(
      n / n)
    val IV: Set[Cat] = Set(
      s \ np,
      (s \ np) / pp)
    val TV: Set[Cat] = Set(
      (s \ np) / np,
      ((s \ np) / pp) / np,
      (((s \ np) / pp) / pp) / np)
    val N: Set[Cat] = Set(
      n)
    val NNP: Set[Cat] = Set(
      np,
      np / pp,
      (np / pp) / pp)
    val Prep: Set[Cat] = Set(
      pp / np)

    val tagdict = SimpleTagDictionary.apply(
      Map[String, Set[Cat]](
        "the" -> Det,
        "big" -> Adj,
        "man" -> N,
        "dog" -> N,
        "dogs" -> N,
        "cats" -> N,
        "telescope" -> N,
        "saw" -> (IV | TV),
        "walked" -> (IV | TV),
        "chase" -> TV,
        "run" -> IV,
        "ran" -> IV,
        "John" -> NNP,
        "Mary" -> NNP,
        "with" -> Prep),
      "<S>", cat"<S>", "<E>", cat"<E>")

    //      "the dogs walked",
    //      "the man walked the dog",
    //      "dogs chase cats",
    //      "big dogs run",
    //      "the big dogs run",
    //      "John saw Mary with the dog",
    //      "John saw Mary with the telescope",
    //      "John saw Mary with the dog with the telescope")

    if (false) {
      val text = "John saw Mary with the telescope"
      val annotation = """ John > saw < (Mary < (with the telescope)) """
      val sentence = fromGfl(text, annotation).getOrElseThrow()
      dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
      val supertagSetSentence: Vector[(String, Set[Cat])] = text.splitWhitespace.mapToVal(Set.empty[Cat])
      val gc = gcb.buildFromSupertagSetSentence(supertagSetSentence, Some(sentence), tagdict)
      gc.get.draw()
      val parses = parser.parseAndProbKBestFromGuideChart(gc.get, 100).map(_._1)
      for (p <- parses) dhg.util.viz.TreeViz.drawTree(p)
    }

    {
      val text = "John saw Mary with the telescope"
      val annotation = """ John saw Mary with the telescope """
      val sentence = fromGfl(text, annotation).getOrElseThrow()
      dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
      val supertagSetSentence: Vector[(String, Set[Cat])] = text.splitWhitespace.mapToVal(Set.empty[Cat])
      val gc = gcb.buildFromSupertagSetSentence(supertagSetSentence, Some(sentence), tagdict)
      gc.get.draw()
      val parses = parser.parseAndProbKBestFromGuideChart(gc.get, 100).map(_._1)
      for (p <- parses) dhg.util.viz.TreeViz.drawTree(p)
    }

  }

}
