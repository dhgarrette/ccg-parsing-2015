package dhg.ccg.bisk

import dhg.util._
import dhg.util.viz._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.tagdict._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import scalaz.{ \/ => _, _ }
import Scalaz._
import scala.collection.parallel.immutable.ParVector
import dhg.gfl.Fudg
import dhg.gfl.FudgSentence
import dhg.gfl.{ Edge => GflEdge }
import dhg.gfl.Token
import dhg.gfl.Node
import dhg.gfl.WordNode
import dhg.gfl.Sentence
import dhg.gfl.{ Sentence => GflSentence }

/**
 * @author dhg
 */
object Bisk {

  type Word = String
  type Pos = String

  val A = cat"A".asInstanceOf[AtomCat]
  val N = cat"N".asInstanceOf[AtomCat]
  val S = cat"S".asInstanceOf[AtomCat]
  val CONJ = cat"conj".asInstanceOf[AtomCat]

  def main(args: Array[String]): Unit = {

    //    val tagdict = SimpleTagDictionary(Map(
    //      "The" -> Set(cat"(NP/N)"),
    //      "old" -> Set(cat"N", cat"(N/N)"),
    //      "man" -> Set(cat"N", cat"((S\NP)/NP)"),
    //      "ships" -> Set(cat"N", cat"(S\NP)")), "<S>", StartCat, "<E>", EndCat)
    //    parser.parse("The old man ships".splitWhitespace, tagdict) foreach TreeViz.drawTree

    //    Vector[Cat](
    //      S,
    //
    //      S / S,
    //      S \ S,
    //
    //      (S \ S) / (S \ S),
    //      (S \ S) \ (S \ S),
    //      (S / S) / (S / S),
    //      (S / S) \ (S / S),
    //
    //      (S \ S) / (S / S),
    //      (S \ S) \ (S / S),
    //      (S / S) / (S \ S),
    //      (S / S) \ (S \ S),
    //
    //      (N \ S) \ (S \ S),
    //      (S \ N) / (S \ S),
    //      (S / S) / (N / S),
    //      (S / S) \ (S / N)).foreach {
    //        case ModMod(x) => println(s"Yes: $x")
    //        case x => println(s"No:  $x")
    //      }
    //
    //    {
    //      val cats = Set[Cat](
    //        N, S \ N, S \ S, N / N, (S \ N) / N, (S \ N) / (S \ N),
    //        (N \ N) / (S / N), S / (S \ N),
    //        S / N, N \ N,
    //        N, S, CONJ, TopCat,
    //        N / N, N \ N, S / S, S \ S, S \ N,
    //        (S / S) / (S / S), (N \ N) / (N \ N), S \ N, (S \ S) / (S \ S), (S \ S) \ (S \ S),
    //        (N / N) \ (N / N), (S / S) \ (S / S), (N \ N) \ (N \ N), (S \ S) / (S \ S),
    //        N, N / N, (N / N) / N,
    //        (S / S) / N,
    //        (S \ S) / N,
    //        (N \ N) / N, (N / N) / N, (S \ N) \ S, (S \ S) / S, (S \ S) / N, S / N,
    //        (S / N) / S)
    //
    //      println(cats -- cleanCatSet(cats))
    //    }

    //

    val ss1: Vector[(Vector[(Word, Pos)], Option[FudgSentence])] = {
      val s1 = "The|DT man|NNS ate|VBD quickly|RB".splitWhitespace.map(_.rsplit("\\|")).map { case Seq(w, t) => (w: Word, t: Pos) }
      val s2 = "The|DT man|NNS eats|VBZ with|IN friends|NNS".splitWhitespace.map(_.rsplit("\\|")).map { case Seq(w, t) => (w: Word, t: Pos) }
      val s3 = "The|DT man|NNS ate|VBD chips|NNS".splitWhitespace.map(_.rsplit("\\|")).map { case Seq(w, t) => (w: Word, t: Pos) }
      Vector(s1 -> None, s2 -> None, s3 -> None)
    }

    val ss2: Vector[(Vector[(Word, Pos)], Option[FudgSentence])] = {
      //        0        1          2    3
      val s4 = "John|NNP walked|VBD a|DT dog|NN".splitWhitespace.map(_.rsplit("\\|")).map { case Seq(w, t) => (w: Word, t: Pos) }
      val e4 = Some(GflSentence.fromDepIndices(s4.map(_._1), Vector(1 -> 0, 1 -> 3)))
      Vector(s4 -> e4)
    }

    val pos2catFinal = new BiskPosCatMappingInducer().induceWithConstraints(ss2, 2)
    pos2catFinal.toVector.sortBy(_._1).mapt((p, cs) => f"$p%-3s -> ${cs.size}:  ${cs.mkString(", ")}") foreach println

  }

}

class BiskPosCatMappingInducer(
    gcb: CfgGuideChartBuilder = new DependencyTrackingCfgGuideChartBuilder(Vector[CcgRule](
      FA, BA,
      FC, BX,
      BC, FX,
      FC2, BX2,
      BC2, FX2,
      new UnaryCcgRule { val child: Cat = cat"N"; val parent: Cat = cat"(S/(S\N))" },
      new UnaryCcgRule { val child: Cat = cat"S"; val parent: Cat = TopCat },
      new UnaryCcgRule { val child: Cat = cat"N"; val parent: Cat = TopCat }),
      rootSet = Set(TopCat),
      additionalSupertagAdder = new NoOpAdditionalTagAdder),
    quick: Boolean = true) {
  type Word = String
  type Pos = String

  private[this] val N = cat"N".asInstanceOf[AtomCat]
  private[this] val S = cat"S".asInstanceOf[AtomCat]
  private[this] val CONJ = cat"conj".asInstanceOf[AtomCat]

  private[this] val initialPos2Cat: Map[Pos, Set[Cat]] = Map(
    N -> "NN NNS NNP PRP DT".splitWhitespace,
    S -> "MD VB VBZ VBG VBN VBD".splitWhitespace,
    CONJ -> "CC".splitWhitespace)
    .flatMap { case (c, ps) => ps.mapToVal(Set[Cat](c)) }

  //  def apply(posTaggedSentences: Vector[Vector[(String, String)]], iterations: Int) = {
  //    val intermediatePos2Cat: Map[String, Set[Cat]] = (1 to iterations).foldLeft(initialPos2Cat) { (pos2cat, i) =>
  //      time(f"    iteration $i    (${posTaggedSentences.size} to process)", posTaggedSentences.par.map(doIteration(_, guessCatsFromTriples, pos2cat, quick)).reduce(_ |+| _))
  //      //|+| Map("DT" -> Set(N / N), "VBD" -> Set(S \ N), "VBZ" -> Set(S \ N), "RB" -> Set(S \ S), "NNS" -> Set(S \ S), "IN" -> Set(S \ S))
  //    }
  //
  //    val finalPos2Cat: Map[String, Set[Cat]] = time("    finalize", posTaggedSentences.par.map(doIteration(_, guessCatsFromConstituents, intermediatePos2Cat, false))).reduce(_ |+| _)
  //    println("Final result of Bisk induction:  "); finalPos2Cat.toVector.sortBy(_._1).mapt((p, cs) => f"    $p%-3s -> ${cs.mkString(", ")}") foreach println
  //    finalPos2Cat
  //  }
  //
  //  def doIteration(s: Vector[(String, String)], guessFunction: ((Vector[Word], Vector[Set[Cat]]) => Vector[Set[Cat]]), pos2cat: Map[Pos, Set[Cat]], quick: Boolean): Map[String, Set[Cat]] = {
  //    val words = s.map(_._1)
  //    val pos = s.map(_._2)
  //    val initialSupertagSets: Vector[Set[Cat]] = pos.map(p => pos2cat.getOrElse(p, Set.empty[Cat]))
  //    val guessedSupertagSets: Vector[Set[Cat]] = guessFunction(words, initialSupertagSets)
  //    val usableSupertagSets: Vector[Set[Cat]] = if (quick) guessedSupertagSets else filterWithParsing(words, pos, guessedSupertagSets)
  //    val usablePosSupertagMap = (pos zipSafe usableSupertagSets).groupByKey.mapVals(_.flatten.toSet)
  //    usablePosSupertagMap
  //  }

  //|+| Map("DT" -> Set(N / N), "VBD" -> Set(S \ N), "VBZ" -> Set(S \ N), "RB" -> Set(S \ S), "NNS" -> Set(S \ S), "IN" -> Set(S \ S))

  def fudgToGraphViz(fs: FudgSentence): String = {
    def nodeString(n: Node): String = {
      s""""${n.name}""""
    }
    def edgeString(edge: GflEdge, color: Option[String] = None): String = {
      nodeString(edge.parent) + " -> " + nodeString(edge.child) +
        edge.label.fold("") { c => f"""  [ label = "${c}" ]""" } +
        color.fold("") { c => f"""  [ color = "$c" ]""" } +
        """  [samehead=true, sametail=true]"""
    }
    val edges = fs.edges //.collect { case GflEdge(parent, child, _) if false || parent != child => println(s"$parent -> $child, ${parent != child}"); GflEdge(parent, child, None) }
    //val allnodes = edges.flatMap { case GflEdge(p, c, _) => Vector(p, c) }
    val sb = new StringBuilder
    sb ++= "digraph {\n"
    //sb ++= "  graph [splines=ortho];\n"
    for ((n, i) <- fs.nodes.values.toVector.sortBy(s => s.name.rsplit("_").last.toInt).zipWithIndex) { sb ++= ("  " + nodeString(n) + f"""  [shape=box] [ pos="${i * 1.5},5!" ]""" + "\n") }
    for (e <- edges) { sb ++= ("  " + edgeString(e) + "\n") }
    sb ++= "}"
    sb.toString
  }

  def induce(posTaggedSentences: Vector[Vector[(String, String)]], iterations: Int) = {
    induceWithConstraints(posTaggedSentences.map(s => (s, none[FudgSentence])), iterations)
  }

  def induceWithConstraints(posTaggedSentencesWithConstraints: Vector[(Vector[(String, String)], Option[FudgSentence])], iterations: Int) = {
    val intermediatePos2Cat: Map[String, Set[Cat]] = (1 to iterations).foldLeft(initialPos2Cat) { (pos2cat, i) =>
      time(f"    iteration $i    (${posTaggedSentencesWithConstraints.size} to process)", posTaggedSentencesWithConstraints.par.map { s =>
        val words = s._1.map(_._1)
        val pos = s._1.map(_._2)
        //        val fudgAnnotation = s._2
        //        fudgAnnotation.foreach { a =>
        //          if (!Fudg.isSemanticallyValid(a.edges, ignoreSelfLoops = true, ignoreMultipleParents = true, throwOnFalse = false)){
        //            println(words.mkString(" "))
        //            println(fudgToGraphViz(a))
        //          }
        //          Fudg.isSemanticallyValid(a.edges, ignoreSelfLoops = true, ignoreMultipleParents = true, throwOnFalse = true)
        //        }

        //println("bisk induction, sentence: " + posTaggedSentencesWithConstraints.map{_._1})
        val initialSupertagSets: Vector[Set[Cat]] = pos.map(p => pos2cat.getOrElse(p, Set.empty[Cat]))
        //println("bisk induction, initialSupertagSets: " + initialSupertagSets)
        val guessedSupertagSets: Vector[Set[Cat]] = guessCatsFromTriples(words, initialSupertagSets)
        val usableSupertagSets: Vector[Set[Cat]] = /*if (quick)*/ guessedSupertagSets /*else filterWithParsing(words, pos, fudgAnnotation, guessedSupertagSets)*/
        val usablePosSupertagMap = (pos zipSafe usableSupertagSets).groupByKey.mapVals(_.flatten.toSet)
        //println("Intermediate result of Bisk induction:  "); usablePosSupertagMap.toVector.sortBy(_._1).mapt((p, cs) => f"    $p%-3s -> ${cs.toVector.sorted.mkString(", ")}") foreach println
        usablePosSupertagMap
      }.reduce(_ |+| _))
    }

    val finalPos2Cat: Map[String, Set[Cat]] = time("    finalize", posTaggedSentencesWithConstraints.par.map { s =>
      val words = s._1.map(_._1)
      val pos = s._1.map(_._2)
      val fudgAnnotation = s._2
      val initialSupertagSets: Vector[Set[Cat]] = pos.map(p => intermediatePos2Cat.getOrElse(p, Set.empty[Cat]))
      val guessedSupertagSets: Vector[Set[Cat]] = if (quick) initialSupertagSets else guessCatsFromConstituents(words, fudgAnnotation, initialSupertagSets)
      val usableSupertagSets: Vector[Set[Cat]] = filterWithParsing(words, pos, fudgAnnotation, guessedSupertagSets)
      val usablePosSupertagMap = (pos zipSafe usableSupertagSets).groupByKey.mapVals(_.flatten.toSet)
      usablePosSupertagMap
    }.reduce(_ |+| _))

    println("Final result of Bisk induction:  "); finalPos2Cat.toVector.sortBy(_._1).mapt((p, cs) => f"    $p%-3s -> ${cs.toVector.sorted.mkString(", ")}") foreach println
    finalPos2Cat
  }

  def filterWithParsing(words: Vector[Word], pos: Vector[Pos], fudgAnnotation: Option[FudgSentence], guessedSupertagSets: Vector[Set[Cat]]): Vector[Set[Cat]] = {
    val guideChart = time1(f"        build chart (${words.length}%3d)", {
      val gco = fudgAnnotation.flatMap(_ => gcb.buildFromSupertagSetSentence(words zipSafe guessedSupertagSets, fudgAnnotation, DummyCatTagDictionary)) // try with annotation constraints
      if(fudgAnnotation.isDefined && gco.nonEmpty) Console.err.println("failed to parse with annotation, attempting without...")
      val gco2 = gco.orElse(gcb.buildFromSupertagSetSentence(words zipSafe guessedSupertagSets, None, DummyCatTagDictionary)) // if it fails, try without constraints
      if(gco2.isEmpty) Console.err.println("failed to parse WITHOUT annotation")
      gco2
    })
    guideChart.map(_.supertagSets).getOrElse(Vector.fill(pos.length)(Set.empty))
  }

  /**
   * guessFunction for normal iterations
   */
  def guessCatsFromTriples(words: Vector[Word], initialSupertagSets: Vector[Set[Cat]]): Vector[Set[Cat]] = {
    val triples = (("" -> Set.empty[Cat]) +: (words zipSafe initialSupertagSets) :+ ("" -> Set.empty[Cat])).sliding3.toVector
    triples.mapt {
      case (aw -> as, bw -> bs, cw -> cs) => bw match {

        case "." | "?" | "!" => Set(cat"""(S\S)""")
        case "," => Set(cat"""(N\N)""")
        case ";" => Set(cat"""((S\S)/S)""")

        case _ =>
          val newCatSet = bs ++
            as.collect { case a: NonPuncCat => a \ a } ++
            cs.collect { case c: NonPuncCat => c / c } ++
            bs.flatCollect { case b: NonPuncCat if b != N => as.collect { case a: NonPuncCat if !a.isModifier => b \ a } } ++
            bs.flatCollect { case b: NonPuncCat if b != N => cs.collect { case c: NonPuncCat if !c.isModifier => b / c } }
          cleanCatSet(newCatSet)
      }
    }
  }

  /**
   * guessFunction for final iteration
   */
  def guessCatsFromConstituents(words: Vector[Word], fudgAnnotation: Option[FudgSentence], initialSupertagSets: Vector[Set[Cat]]): Vector[Set[Cat]] = {
    val gc: CfgGuideChart = gcb.buildUnpruned(words zipSafe initialSupertagSets, fudgAnnotation)
    val n = gc.length
    for (j <- (0 until n).toVector) yield {
      val thisCatSet: Set[Cat] = gc.supertagSets(j)
      val combWithRght: Iterable[Cat] = (for (k <- (j + 2) to n) yield thisCatSet.flatCollect { case b: NonPuncCat => gc(j + 1, k).keys.collect { case c: NonPuncCat => (b / c): Cat } }).flatten
      val combWithLeft: Iterable[Cat] = (for (i <- 0 to (j - 1)) yield thisCatSet.flatCollect { case b: NonPuncCat => gc(i, j).keys.collect { case a: NonPuncCat => (b \ a): Cat } }).flatten
      cleanCatSet(thisCatSet ++ combWithRght ++ combWithLeft)
    }
  }

  def cleanCatSet(newCatSet: Set[Cat]): Set[Cat] = {
    newCatSet.filter { // #1
      case N || N => true
      case (N || N) || (N || N) => true
      case N || x => false
      case _ => true
    }.filter { // #2
      case ModMod(_) => true
      case Mod(_) => true
      case ModModAndArgs(_, args) => args.forall(Set(S, N))
      case ModAndArgs(_, args) => args.forall(Set(S, N)) // blocks (N\N)/(S/N)  :-(
      case HeadAndArgs(S, args) => args.forall(Set(S, N))
      case _ => true
    }.filter { // #3
      case S || S => true
      case S || a => a == N
      case _ => true
    }.filter { // #4
      case ModMod(_) => true
      case (a || b) || (c || d) if a == b && b == c && c == d => false
      case _ => true
    }.filter { // #5
      case HeadAndArgs(_, args) => args.size <= 2
    }.flatMap { // #6
      case (S / N) \ N => Some((S \ N) / N)
      case c => Some(c)
    }

  }

  //

  object HeadAndArgs {
    def unapply(c: Cat): Option[(Cat, Vector[Cat])] = toVec(c) match {
      case Vector(h, args @ _*) => Some(h -> args.toVector)
    }
    def toVec(c: Cat): Vector[Cat] = c match {
      case x || y => toVec(x) :+ y
      case a => Vector(a)
    }
  }

  def argsOf(Head: Cat, cat: Cat): Option[Vector[Cat]] = cat match {
    case Head => Some(Vector.empty)
    case functor || arg => argsOf(Head, functor).map(_ :+ arg)
    case _ => None
  }

  object ModAndArgs {
    def unapply(cat: Cat): Option[(Cat, Vector[Cat])] = cat match {
      case Mod(mod) => Some((mod, Vector.empty))
      case functor || arg => unapply(functor).map { case (mod, args) => (mod, args :+ arg) }
      case _ => None
    }
  }

  object ModModAndArgs {
    def unapply(cat: Cat): Option[(Cat, Vector[Cat])] = cat match {
      case ModMod(mod) => Some((mod, Vector.empty))
      case functor || arg => unapply(functor).map { case (mod, args) => (mod, args :+ arg) }
      case _ => None
    }
  }

  trait ModExtractor { def unapply(cat: Cat): Option[Cat] }
  object Mod extends ModExtractor { def unapply(cat: Cat) = cat match { case a || b if a == b => Some(cat); case _ => None } }
  //object ModMod extends ModExtractor { def unapply(cat: Cat) = cat match { case (a || b) || (c || d) if a == b && b == c && c == d => Some(cat); case _ => None } }
  object ModMod extends ModExtractor { def unapply(cat: Cat) = cat match { case Mod(Mod(_) || Mod(_)) => Some(cat); case _ => None } }
}
