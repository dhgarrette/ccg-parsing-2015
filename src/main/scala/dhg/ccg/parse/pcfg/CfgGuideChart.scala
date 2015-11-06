package dhg.ccg.parse.pcfg

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
import dhg.ccg.tagdict._
import dhg.ccg.util.DrawMatrix
import dhg.util.GraphUtil._
import dhg.gfl.FudgSentence
import dhg.util._
import dhg.util.viz.TreeViz
import scalaz._
import scalaz.Scalaz._
import scala.util.Try

trait GuideChartEntry extends Serializable { def prod: Prod }
case class BinaryGuideChartEntry(k: Int, prod: BinaryProd) extends GuideChartEntry
case class UnaryGuideChartEntry(prod: UnaryProd) extends GuideChartEntry
case class TermGuideChartEntry(prod: TermProd) extends GuideChartEntry

/**
 *
 * In a given cell, if A can be rewritten as B, then we must compute A's
 * and B's independent parse counts and then add A's count to B's
 * (because for every way of getting A, we can get B via rewrite, in
 * addition to all the ways we can get B without going through A).  This
 * also means that if we can rewrite B as C, then we must add B's count
 * to C's *after* adding A's to B.  So the right answer is to turn all
 * unary rules into a DAG and then to process the categories in order.
 * The operations performed within the category can be done in any order.
 * This should be completely handled by the SortedMap in the guide chart.
 */
case class CfgGuideChart(words: Vector[String], matrix: Vector[Vector[Map[Cat, Set[GuideChartEntry]]]], rootSet: Set[Cat] = UniversalSet()) extends Serializable {
  def apply(row: Int): Vector[Map[Cat, Set[GuideChartEntry]]] = matrix(row)
  def apply(row: Int, col: Int): Map[Cat, Set[GuideChartEntry]] = matrix(row)(col)

  def length = matrix.size

  def root = this(0, length)
  def terminals = (0 until length).map(i => matrix(i)(i + 1)).toVector
  def supertagSets: Vector[Set[Cat]] = wordSupertagsets.map(_._2)
  def wordSupertagsets = (words zipSafe terminals).mapt((word, cell) => cell.to[Set].flatMap { case (t, entries) => entries.collect { case TermGuideChartEntry(prod) => prod.word -> t } }.groupByKey match {
    case Coll((w, supertags)) =>
      assert(w == word, s"w=$w, word=$word"); (word, supertags)
    case Coll() => (word, Set.empty[Cat])
  })
  def allCats = topDownNodes.flatMap(_._3.keys).toSet

  /**
   * Iterate over all cells
   */
  def bottomUpNodes: IndexedSeq[(Int, Int, Map[Cat, Set[GuideChartEntry]])] = {
    for {
      span <- 1 to length //        span size
      i <- 0 to (length - span) //  start of span
      j = i + span //               end of span
    } yield (i, j, this(i, j))
  }

  /**
   * Iterate over all cells
   */
  def topDownNodes: IndexedSeq[(Int, Int, Map[Cat, Set[GuideChartEntry]])] = {
    for {
      span <- length downto 1 //    span size
      i <- 0 to (length - span) //  start of span
      j = i + span //               end of span
    } yield (i, j, this(i, j))
  }

  lazy val numPossibleParses = {
    val table: Vector[Vector[Map[Cat, MutableNumber[BigInt]]]] =
      matrix.map { row =>
        if (row != null)
          row.map { col =>
            if (col != null)
              col.mapVals { _ =>
                new MutableNumber[BigInt]()
              }
            else null
          }
        else null
      }

    for {
      (i, j, cell) <- bottomUpNodes if cell.nonEmpty // visit all relevant cells
      (ij, entries) <- cell // cell is sorted according to unary rule dependency relationships
      entry <- entries
    } entry match {
      case BinaryGuideChartEntry(k, BinaryProd(ik, kj)) =>
        val ikCount = table(i)(k)(ik).get
        val kjCount = table(k)(j)(kj).get
        //println(f"table($i)($j)($ij)=${table(i)(j)(ij).get}  +=  table($i)($k)($ik)=${ikCount.get}  *  table($k)($j)($kj)=${kjCount.get}")
        table(i)(j)(ij) += (ikCount * kjCount)
      case UnaryGuideChartEntry(UnaryProd(sub)) =>
        val subCount = table(i)(j)(sub).get
        //println(f"table($i)($j)($ij)=${table(i)(j)(ij).get}  +=  table($i)($j)($sub)=${subCount.get}")
        table(i)(j)(ij) += subCount
      case TermGuideChartEntry(TermProd(word)) =>
        //println(f"table($i)($j)($ij)=${table(i)(j)(ij).get}  +=  1")
        table(i)(j)(ij) += 1
    }

    table(0)(length).values.sumBy(_.get)
  }

  def allParses: Set[CcgTree] = {
    val table: Vector[Vector[Map[Cat, MSet[CcgTree]]]] =
      matrix.map(_.map(_.mapVals(_ => MSet.empty[CcgTree])))

    for {
      (i, j, cell) <- bottomUpNodes if cell.nonEmpty // visit all relevant cells
      (ij, entries) <- cell // cell is sorted according to unary rule dependency relationships
      entry <- entries
    } entry match {
      case BinaryGuideChartEntry(k, BinaryProd(ik, kj)) =>
        for {
          ikTree <- table(i)(k)(ik)
          kjTree <- table(k)(j)(kj)
        } {
          //println(f"table($i)($j)($ij)=${table(i)(j)(ij).get}  +=  table($i)($k)($ik)=${ikCount.get}  *  table($k)($j)($kj)=${kjCount.get}")
          table(i)(j)(ij) += CcgBinode(ij, ikTree, kjTree)
        }
      case UnaryGuideChartEntry(UnaryProd(sub)) =>
        for { subTree <- table(i)(j)(sub) } {
          //println(f"table($i)($j)($ij)=${table(i)(j)(ij).get}  +=  table($i)($j)($sub)=${subCount.get}")
          table(i)(j)(ij) += CcgUnode(ij, subTree)
        }
      case TermGuideChartEntry(TermProd(word)) =>
        //println(f"table($i)($j)($ij)=${table(i)(j)(ij).get}  +=  1")
        table(i)(j)(ij) += CcgLeaf(ij, word, "FAKEPOS")
    }

    //DrawMatrix.drawMatrix(table.map(_.tail)) { _.map { case (c, i) => f"$c -> ${i.get}" }.mkString("\n") }

    table(0)(length).values.flatten.toSet
  }

  def draw(out: String => Unit = println): Unit = {
    DrawMatrix.drawMatrix(matrix.map(_.tail))(_.map {
      case (cat, set) =>
        val left = f"$cat -> "
        f"$left${
          set.map {
            case BinaryGuideChartEntry(k, BinaryProd(l, r)) => f"$k:[$l $r]"
            case UnaryGuideChartEntry(UnaryProd(sub)) => f"$sub"
            case TermGuideChartEntry(TermProd(word)) => f""""$word""""
          }.mkString("\n" + " " * left.length)
        }"
    }.mkString("\n"))(out)
  }

  def repr: String = {
    "new CfgGuideChart(Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](\n" + matrix.map(row => s"  ${
      row.map(m => "ListMap(" + m.map {
        case (cat, set) => f"$cat -> ${
          set.map {
            case BinaryGuideChartEntry(k, BinaryProd(l, r)) => f"BinaryGuideChartEntry($k, BinaryProd($l, $r))"
            case UnaryGuideChartEntry(UnaryProd(sub)) => f"UnaryGuideChartEntry(UnaryProd($sub))"
            case TermGuideChartEntry(TermProd(w)) => s"""TermGuideChartEntry(TermProd("$w"))"""
          }
        }"
      }.mkString(", ") + ")")
    } ").mkString(",\n") + "))"
  }

  override def toString = f"CfgGuideChart($matrix)"
}

//

trait CfgGuideChartBuilder {
  type Word = String

  final def build(sentence: Vector[Word], fudgAnnotation: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[CfgGuideChart] = buildFromSupertagSetSentence(sentence.mapToVal(Set.empty[Cat]), fudgAnnotation, tagdict)
  //  final def buildFromSupertagged(supertaggedSentence: Vector[(Word, Cat)], fudgAnnotation: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[CfgGuideChart] = buildFromSupertagSetSentence(supertaggedSentence.mapVals(Set(_)), fudgAnnotation, tagdict)
  def buildFromSupertagSetSentence(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence] = None, tagdict: TagDictionary[Cat] = DummyCatTagDictionary): Option[CfgGuideChart]
  def buildUnpruned(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence] = None, tagdict: TagDictionary[Cat] = DummyCatTagDictionary): CfgGuideChart
}

class CascadingAttemptsCfgGuideChartBuilder(orderedDelegates: Vector[CfgGuideChartBuilder]) extends CfgGuideChartBuilder {
  def buildFromSupertagSetSentence(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[CfgGuideChart] = {
    orderedDelegates.iterator.map { gcb =>
      val gc = gcb.buildFromSupertagSetSentence(supertagSetSentence, fudgAnnotation, tagdict)
      //if (gc.isEmpty) println(f"    failed guide chart building: ${gcb}")
      gc
    }.collectFirst { case Some(gc) => gc }
  }
  def buildUnpruned(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence] = None, tagdict: TagDictionary[Cat] = DummyCatTagDictionary): CfgGuideChart = ???
  override def toString() = f"CascadingAttemptsCfgGuideChartBuilder(orderedDelegates=$orderedDelegates)"
}

class DecisionCfgGuideChartBuilder(val a: CfgGuideChartBuilder, val b: CfgGuideChartBuilder, useA: ((Vector[(String, Set[Cat])], Option[FudgSentence]) => Boolean)) extends CfgGuideChartBuilder {
  def buildFromSupertagSetSentence(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[CfgGuideChart] = {
    val builder = if (useA(supertagSetSentence, fudgAnnotation)) a else b
    //println(f"      decision: $builder")
    builder.buildFromSupertagSetSentence(supertagSetSentence, fudgAnnotation, tagdict)
  }

  def buildUnpruned(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence] = None, tagdict: TagDictionary[Cat] = DummyCatTagDictionary): CfgGuideChart = ???
  override def toString() = f"DecisionCfgGuideChartBuilder(a=$a, b=$b)"
}

/**
 * Ignore any annotation except for brackets
 */
class BracketOnlyCfgGuideChartBuilder(delegate: CfgGuideChartBuilder) extends CfgGuideChartBuilder {
  def buildFromSupertagSetSentence(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[CfgGuideChart] = {
    delegate.buildFromSupertagSetSentence(supertagSetSentence, fudgAnnotation.map(_.bracketsOnly()), tagdict)
  }

  def buildUnpruned(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence] = None, tagdict: TagDictionary[Cat] = DummyCatTagDictionary): CfgGuideChart = ???
  override def toString() = f"BracketOnlyCfgGuideChartBuilder($delegate)"
}

class SimpleCfgGuideChartBuilder(
  val rules: Vector[CcgRule],
  additionalSupertagAdder: AdditionalTagAdder[Cat] = new StandardTagDictAdditionalTagAdder[Cat](),
  rootSet: Set[Cat] = UniversalSet(),
  allowTerminalDeletion: Boolean = false,
  verbose: Boolean = false)
    extends CfgGuideChartBuilder {

  private[this] val unaryRules = rules.collect { case r: UnaryCcgRule => r }
  private[this] val binaryRules = rules.collect { case r: BinaryCcgRule => r }

  import CfgGuideChartBuilderUtil._

  def buildFromSupertagSetSentence(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence] = None, tagdict: TagDictionary[Cat] = DummyCatTagDictionary): Option[CfgGuideChart] = {
    if (verbose) println(s"11111: $supertagSetSentence")
    val (sentence, sentenceExistingTags) = supertagSetSentence.unzip
    val sentenceTags = additionalSupertagAdder(sentence, sentenceExistingTags, tagdict) // get supertags set for each word
    if (verbose) println(s"2222: $sentenceTags")
    //println(f"      SimpleCfgGuideChartBuilder: brackets=${fudgAnnotation.map(_.brackets)}")
    doBuild(sentence, sentenceTags, sentence.length, fudgAnnotation)
  }

  def buildUnpruned(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence] = None, tagdict: TagDictionary[Cat] = DummyCatTagDictionary): CfgGuideChart = {
    val (sentence, sentenceExistingTags) = supertagSetSentence.unzip
    val sentenceTags = additionalSupertagAdder(sentence, sentenceExistingTags, tagdict) // get supertags set for each word

    val brackets: Vector[(Int, Int)] = makeBracketList(sentence, fudgAnnotation)
    val invalidSpans: Set[(Int, Int)] = Set.empty

    val table: MTable = fillBottomUp(sentence, sentenceTags, supertagSetSentence.length, brackets, invalidSpans)
    toCfgGuideChart(supertagSetSentence.map(_._1), table, unaryRules, rootSet)
  }

  /**
   * Go bottom-up with CKY to fill the chart with potential node entries
   *
   * Brackets are given as start/end index pairs, with indexing as:
   *         the   old   man   the   sea    .
   *       0     1     2     3     4     5     6
   * so that bracket (1,3) would cover "old man".
   */
  private[this] def doBuild(sentence: Vector[Word], sentenceTags: Vector[Set[Cat]], n: Int, fudgAnnotation: Option[FudgSentence]): Option[CfgGuideChart] = {
    //for (((w, ts), i) <- (sentence zipSafe sentenceTags).zipWithIndex) assert(ts.nonEmpty, s"In guidechartbuilder, word $i, `$w`, has no associated tags, so it will definitely fail. Check the AdditionalTagAdder.")

    val brackets: Vector[(Int, Int)] = makeBracketList(sentence, fudgAnnotation)
    val invalidSpans: Set[(Int, Int)] = Set.empty

    val table: MTable = fillBottomUp(sentence, sentenceTags, n, brackets, invalidSpans)

    if (table(0)(n).get.filterKeys(rootSet).values.exists(_.nonEmpty)) { // there's at least one parse
      val clean = makeCleanChart(table, n, brackets, invalidSpans)
      val result = toCfgGuideChart(sentence, clean, unaryRules, rootSet)
      if (verbose) result.draw()

      // DEBUG START
      //      //result.draw()
      //      for {
      //        (i, j, cell) <- result.bottomUpNodes
      //        (ij, entries) <- cell
      //        entry <- entries
      //      } entry match {
      //        case BinaryGuideChartEntry(k, BinaryProd(ik, kj)) =>
      //          assert(result(i)(k).contains(ik), f"($i,$j)($ij): $k[$ik,$kj];  result($i,$k) doesn't contain $ik;  result($i,$k)=${result(i)(k).map(x => "  " + x.toString).mkString("\n", "\n", "")}")
      //          assert(result(k)(j).contains(kj), f"($i,$j)($ij): $k[$ik,$kj];  result($k,$j) doesn't contain $kj;  result($k,$j)=${result(k)(j).map(x => "  " + x.toString).mkString("\n", "\n", "")}")
      //          assert(rules.collect { case r: BinaryCcgRule => r(ik, kj) }.flatten.contains(ij), f"invalid rule!  ($i,$j)($ij -> $k[$ik,$kj]")
      //        case UnaryGuideChartEntry(UnaryProd(sub)) =>
      //          assert(result(i)(j).contains(sub), f"($i,$j)($ij): [$sub];  result($i,$j) doesn't contain $sub;  result($i,$j)=${result(i)(j).map(x => "  " + x.toString).mkString("\n", "\n", "")}")
      //          assert(rules.collect { case r: UnaryCcgRule => r(sub) }.flatten.contains(ij), f"invalid rule!  ($i,$j)($ij -> [$sub]")
      //        case TermGuideChartEntry(TermProd(word)) =>
      //
      //      }
      // DEBUG END

      //result.draw()
      if (!result(0, n).values.exists(_.nonEmpty)) {
        toCfgGuideChart(sentence, table, unaryRules, rootSet).draw()
        result.draw()
        sys.error(f"table had parse, but result doesn't ...")
      }
      Some(result)
    }
    else None
  }

  def makeBracketList(sentence: Vector[Word], fudgAnnotation: Option[FudgSentence]) = {
    try {
      //fudgAnnotation.foreach(a => println(a.fudgTree))
      fudgAnnotation.map(_.brackets.toVector).getOrElse(Vector.empty)
    }
    catch {
      case e: Exception =>
        println(sentence.mkString(" "))
        println(fudgAnnotation)
        println(fudgAnnotation.get)
        println(sentence.mkString(" "))
        println(fudgAnnotation.get.tokens.map(_.token).mkString(" "))

        Vector.empty
    }
  }

  def fillBottomUp(sentence: Vector[Word], sentenceTags: Vector[Set[Cat]], n: Int, brackets: Vector[(Int, Int)], invalidSpans: Set[(Int, Int)]): MTable = {
    val table: MTable = newTable(n, brackets, invalidSpans)
    //new CfgGuideChart(table.toVector.map { row => row.toVector.map { cell => if (cell.isDefined) Map(cat"X" -> Set.empty[GuideChartEntry]) else Map.empty[Cat, Set[GuideChartEntry]] } }).draw()

    if (verbose) println(s"${sentence zipSafe sentenceTags}")
    fillTerminalLevel(table, sentence, sentenceTags)
    if (verbose) toCfgGuideChart(sentence, table, unaryRules, rootSet).draw()

    fillHigherLevels(table, n)
    if (verbose) toCfgGuideChart(sentence, table, unaryRules, rootSet).draw()

    table
  }

  /**
   * Add supertags to bottom level of the chart
   */
  private[this] def fillTerminalLevel(table: MTable, sentence: Vector[Word], sentenceTags: Vector[Set[Cat]]): Unit = {
    for (((word, tags), i) <- (sentence zipSafe sentenceTags).zipWithIndex) {
      val cell = table(i)(i + 1).get // lexical cell must exist
      val entries = repeatedlyApplyUnaryRules(tags.mapToVal(TermGuideChartEntry(TermProd(word))))
      for ((ij, entry) <- entries) {
        cell.getOrElseUpdate(ij, MSet.empty) += entry
      }
    }
  }

  /**
   * Apply rules to add higher-level chart entries
   */
  private[this] def fillHigherLevels(table: MTable, n: Int): Unit = {
    for {
      j <- 2 to n //             end of span
      i <- j - 2 downto 0 //     start of span
      cellij <- table(i)(j) //   search stops here if this span is unavailable (due to known bracketing)
    } {
      val entries = higherLevelEntries(i, j, table)
      for ((ij, entry) <- repeatedlyApplyUnaryRules(entries.toSet)) {
        if (verbose) println(f"adding entry $entry")
        cellij.getOrElseUpdate(ij, MSet.empty) += entry
      }

      if (allowTerminalDeletion && j == i + 2) { // if children are terminal cells
        val k = i + 1

        // Unary rule version
        val cellik = table(i)(k).get
        val cellkj = table(k)(j).get
        val ikNewEntries =
          for {
            (ik, ikEntries) <- cellik if !Set[Cat](DeleteFromLeftCat, DeleteFromRightCat)(ik) //      for every left-side category...
            if ikEntries.exists { case TermGuideChartEntry(ikTermProd) => true; case _ => false } //    ...that is a supertag
          } yield {
            cellij.getOrElseUpdate(ik, MSet.empty) += BinaryGuideChartEntry(k, BinaryProd(ik, DeleteFromLeftCat)) // add a `ik -> ik DFL` production
            ik
          }
        for (ik <- ikNewEntries)
          cellik.getOrElseUpdate(DeleteFromRightCat, MSet.empty) += UnaryGuideChartEntry(UnaryProd(ik)) //         add a `DFR -> ik` unary rule
        val kjNewEntries =
          for {
            (kj, kjEntries) <- cellkj if !Set[Cat](DeleteFromLeftCat, DeleteFromRightCat)(kj) //      for every right-side category...
            if kjEntries.exists { case TermGuideChartEntry(kjTermProd) => true; case _ => false } //    ...that is a supertag
          } yield {
            cellij.getOrElseUpdate(kj, MSet.empty) += BinaryGuideChartEntry(k, BinaryProd(DeleteFromRightCat, kj)) // add `kj -> DFRC kj` production
            kj
          }
        for (kj <- kjNewEntries)
          cellkj.getOrElseUpdate(DeleteFromLeftCat, MSet.empty) += UnaryGuideChartEntry(UnaryProd(kj)) //         add a `DFL -> kj` unary rule

        //        // Terminal rule version
        //        for {
        //          cellkj <- table(k)(j);
        //          kjTermProd = cellkj.flatMap(_._2).collect { case e: TermGuideChartEntry => e }.toSet.only // get the right-side terminal
        //          cellik <- table(i)(k)
        //          (ik, ikEntries) <- cellik if !Set[Cat](DeleteFromLeftCat, DeleteFromRightCat)(ik) //      for every left-side category...
        //          if ikEntries.exists { case TermGuideChartEntry(ikTermProd) => true; case _ => false } //    ...that is a supertag
        //        } {
        //          cellij.getOrElseUpdate(ik, MSet.empty) += BinaryGuideChartEntry(k, BinaryProd(ik, DeleteFromLeftCat)) // add `ik -> ik DFLC` production
        //          cellkj(DeleteFromLeftCat) = MSet(kjTermProd) // this will be done multiple times, but whatever
        //        }
        //        for {
        //          cellik <- table(i)(k);
        //          ikTermProd = cellik.flatMap(_._2).collect { case e: TermGuideChartEntry => e }.toSet.only // get the left-side terminal
        //          cellkj <- table(k)(j)
        //          (kj, kjEntries) <- cellkj if !Set[Cat](DeleteFromLeftCat, DeleteFromRightCat)(kj) //      for every right-side category...
        //          if kjEntries.exists { case TermGuideChartEntry(kjTermProd) => true; case _ => false } //    ...that is a supertag
        //        } {
        //          cellij.getOrElseUpdate(kj, MSet.empty) += BinaryGuideChartEntry(k, BinaryProd(DeleteFromRightCat, kj)) // add `kj -> DFRC kj` production
        //          cellik(DeleteFromRightCat) = MSet(ikTermProd) // this will be done multiple times, but whatever
        //        }
      }
    }
  }

  private[this] def higherLevelEntries(i: Int, j: Int, table: MTable): Vector[(Cat, BinaryGuideChartEntry)] = {
    val b = new VectorBuilder[(Cat, BinaryGuideChartEntry)]()
    for {
      k <- i + 1 until j //      split of span
      cellik <- table(i)(k) //   search stops here if this span is unavailable (due to known bracketing)
      cellkj <- table(k)(j) //   search stops here if this span is unavailable (due to known bracketing)
      ik <- cellik.keys
      kj <- cellkj.keys
      rule <- binaryRules
      ij <- rule(ik, kj)
    } {
      b += (ij -> BinaryGuideChartEntry(k, BinaryProd(ik, kj)))
    }
    b.result
  }

  private[this] def repeatedlyApplyUnaryRules(prods: Set[(Cat, GuideChartEntry)]): Set[(Cat, GuideChartEntry)] = {
    @tailrec def inner(prods: Set[(Cat, GuideChartEntry)], accum: Set[(Cat, GuideChartEntry)]): Set[(Cat, GuideChartEntry)] = {
      if (verbose) println(f"repeatedlyApplyUnaryRules: prods=$prods, accum=$accum")
      val newProds = for {
        (cat, prod) <- prods
        r <- unaryRules
        newCat <- r(cat)
        newProd = newCat -> (UnaryGuideChartEntry(UnaryProd(cat)): GuideChartEntry)
        if !prods.contains(newProd)
      } yield newProd
      if (newProds.isEmpty) prods | accum
      else inner(newProds, prods | accum)
    }
    inner(prods, Set.empty)
  }

  /**
   * Make a clean chart so we can put only useful entries (those that
   * can actually be used in a parse).  Go top-down following paths that
   * start with a root node.
   */
  private[this] def makeCleanChart(table: MTable, n: Int, brackets: Vector[(Int, Int)], invalidSpans: Set[(Int, Int)]) = {
    //toCfgGuideChart(table, unaryRules).draw()

    val cleanCfgTable: MTable = newTable(n, brackets, invalidSpans)

    for {
      (ij, ijSet) <- table(0)(n).get
      if rootSet.contains(ij) // Filter root cell to only contain categories in rootSet
    } {
      cleanCfgTable(0)(n).get(ij) = ijSet
    }

    for {
      spanSize <- n downto 1
      i <- 0 to (n - spanSize)
      j = i + spanSize
    } {
      followUnaryRules(i, j, cleanCfgTable, table)

      for {
        cleanCellij <- cleanCfgTable(i)(j)
        (ij, ijSet) <- cleanCellij
        BinaryGuideChartEntry(k, BinaryProd(ik, kj)) <- ijSet
      } {
        cleanCfgTable(i)(k).get(ik) = table(i)(k).get(ik)
        cleanCfgTable(k)(j).get(kj) = table(k)(j).get(kj)
      }
    }

    cleanCfgTable
  }

  /**
   * Copy useful entries into clean chart.
   * This can potentially re-copy the same chart entries multiple times, but that's ok i guess
   */
  @tailrec private[this] def followUnaryRules(i: Int, j: Int, cleanCfgTable: MTable, table: MTable): Unit = {
    val newEntries =
      for {
        cleanCellij <- cleanCfgTable(i)(j).toSet
        (ij, ijSet) <- cleanCellij
        UnaryGuideChartEntry(UnaryProd(sub)) <- ijSet // find unary rules
        if !cleanCellij.contains(sub) //                 that add additional categories
        //if table(i)(j).get.contains(sub)
      } yield sub

    /*
     * If there are new entries to be handled
     */
    if (newEntries.nonEmpty) {
      for (sub <- newEntries) {
        cleanCfgTable(i)(j).get(sub) = table(i)(j).get(sub)
      }
      followUnaryRules(i, j, cleanCfgTable, table)
    }
  }

  override def toString = f"SimpleCfgGuideChartBuilder(${rules.mkString("[", ",", "]")}, ${additionalSupertagAdder})))"
}

class PunctBracketingCfgGuideChartBuilder(
  punct: Set[String],
  delegate: CfgGuideChartBuilder)
    extends CfgGuideChartBuilder {
  def buildFromSupertagSetSentence(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[CfgGuideChart] = {
    val indexedWords = supertagSetSentence.map(_._1).zipWithIndex
    val splits = indexedWords.splitWhere { case (w, i) => punct(w) }
    val additionalBrackets = splits.collect { case xs if xs.nonEmpty => (xs.head._2, xs.last._2 + 1) }
    ??? //delegate.buildFromSupertagSetSentence(supertagSetSentence, brackets ++ additionalBrackets, tagdict)
  }
  def buildUnpruned(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence] = None, tagdict: TagDictionary[Cat] = DummyCatTagDictionary): CfgGuideChart = ???
}

object CfgGuideChartBuilderUtil {
  type MTable = Array[Array[Option[MMap[Cat, MSet[GuideChartEntry]]]]]

  def newTable[A, B](n: Int, brackets: Vector[(Int, Int)], invalidSpans: Set[(Int, Int)]): Array[Array[Option[MMap[A, B]]]] = {
    val table: Array[Array[Option[MMap[A, B]]]] =
      Array.tabulate(n) { i =>
        Array.tabulate(n + 1) { j =>
          val itoj = (i + 1 to j)
          if (i >= j || invalidSpans(i -> j) ||
            brackets.exists { // if there exists a cross-bracketing
              case (a, b) =>
                assert(a < b, f"improper bracket: ($a,$b)")
                (a < i && i < b && b < j) ||
                  (i < a && a < j && j < b)
            }) None
          else Some(MMap.empty)
        }
      }
    table
  }

  def toCfgGuideChart(words: Vector[String], table: MTable, unaryRules: Vector[UnaryCcgRule], rootSet: Set[Cat]): CfgGuideChart = {
    val allCats = table.flatten.flatten.flatten.map(_._1).toSet
    val catOrdering = makeCatOrdering(unaryRules)
    val ord = catOrdering.on[(Cat, Set[GuideChartEntry])](_._1)
    new CfgGuideChart(
      words,
      table.toVector.zipWithIndex.map {
        case (row, i) =>
          row.toVector.zipWithIndex.map {
            case (cell, j) =>
              cell.fold(Map.empty[Cat, Set[GuideChartEntry]]) { contents =>
                val sorted = contents.mapVals(_.to[Set]).toVector.sorted(ord)
                //if(contents.values.flatten.exists{ case TermGuideChartEntry(TermProd("Arizona")) => true; case _ => false }) println(sorted)
                ListMap.empty ++ sorted
              }
          }
      }, rootSet)
  }

  /**
   * Order the categories for iterating such that for any unary rule where
   * A can be a child of B, A is visited before B.
   */
  private[this] def makeCatOrdering(unaryRules: Vector[UnaryCcgRule]) = {
    val allLinks = unaryRules.map(rule => rule.child -> rule.parent).toSet
    val catOrdering = toDagOrder(allLinks) ++ Vector(DeleteFromLeftCat, DeleteFromRightCat)
    val orderMap = catOrdering.zipWithIndex.toMap.withDefaultValue(-1) // categories uninvolved in the ordering are handled first, as a block
    scala.math.Ordering.by[Cat, Int](orderMap)
  }

}

//

object CfgGuideChart {
  //  def read(path: String): CfgGuideChart = {
  //    val file = File(path)
  //    assert(file.exists, "Serialized CfgGuideChart file " + path + " not found")
  //    using(new ObjectInputStream(new GZIPInputStream(new FileInputStream(file)))) { in =>
  //      in.readObject().asInstanceOf[CfgGuideChart]
  //    }
  //  }

  def readVector(path: String): Vector[(Option[CfgGuideChart], Option[CcgTree])] = {
    val file = File(path)
    assert(file.exists, "Serialized CfgGuideChart file " + path + " not found")
    using(new ObjectInputStream(new GZIPInputStream(new FileInputStream(file)))) { in =>
      in.readObject().asInstanceOf[Vector[(Option[CfgGuideChart], Option[CcgTree])]]
    }
  }

  //  def write(gc: CfgGuideChart, path: String): Unit = {
  //    using(new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(path)))) { out =>
  //      out.writeObject(gc)
  //    }
  //  }

  def writeVector(gcs: Vector[(Option[CfgGuideChart], Option[CcgTree])], path: String): Unit = {
    using(new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(path)))) { out =>
      out.writeObject(gcs)
    }
  }
}

class CfgGuideChartSerDeser() {
  private[this] val Cat: CatInterner = NonRemovingCcgBankCatInterner // only need one format, as long as its consistent

  val HeaderRe = """length=(\d+)""".r
  val MapEntryRe = """(\S+) -> (.+)""".r
  val BinaryEntryRe = """(\d+):\[([^,\s]+),([^,\s]+)\]""".r
  val BinaryEntryRe1 = """(\d+):\[(,),([^\s]+)\]""".r
  val BinaryEntryRe2 = """(\d+):\[([^\s]+),(,)\]""".r
  val BinaryEntryRe3 = """(\d+):\[([^\s]+),(\(,\\,\))\]""".r
  val BinaryEntryRe4 = """(\d+):\[(\(,\\,\)),([^\s]+)\]""".r
  val BinaryEntryRe5 = """(\d+):\[([^\s]+),(\(,/,\))\]""".r
  val BinaryEntryRe6 = """(\d+):\[(\(,/,\)),([^\s]+)\]""".r
  val BinaryEntryRe7 = """(\d+):\[([^\s]+),(RRB)\]""".r
  val BinaryEntryRe8 = """(\d+):\[(LRB),([^\s]+)\]""".r
  val BinaryEntryRe9 = """(\d+):\[([^\s]+),(:)\]""".r
  val BinaryEntryRe10 = """(\d+):\[(:),([^\s]+)\]""".r
  val BinaryEntryRe11 = """(\d+):\[([^\s]+),(\.)\]""".r
  val BinaryEntryRe12 = """(\d+):\[(\.),([^\s]+)\]""".r
  val UnaryEntryRe = """\[(.+)\]""".r
  val TermEntryRe = """(.+)""".r

  def ser(gc: CfgGuideChart): Vector[String] = {
    val matrix: Vector[Vector[Map[Cat, Set[GuideChartEntry]]]] = gc.matrix
    val b = new VectorBuilder[String]
    b += f"length=${gc.length}"
    for ((row, i) <- matrix.zipWithIndex) {
      for ((m, j) <- row.zipWithIndex.drop(i + 1)) {
        b += f"($i,$j) ;; " + m.mapt { (cat, entries) =>
          f"$cat -> ${
            entries.map {
              case BinaryGuideChartEntry(k, BinaryProd(a, b)) => f"$k:[${a.toString.replace(",", "<COMMA>")},${b.toString.replace(",", "<COMMA>")}]"
              case UnaryGuideChartEntry(UnaryProd(c)) => f"[$c]"
              case TermGuideChartEntry(TermProd(w)) => w
            }.mkString(" ")
          }"
        }.mkString(" ;; ")
      }
    }
    b.result
  }

  def deser(s: Vector[String]): CfgGuideChart = {
    val HeaderRe(UInt(length)) +: tail = s
    val ti = tail.iterator
    new CfgGuideChart(???, Vector.tabulate(length) { i =>
      val front = Vector.fill(i + 1)(Map.empty[Cat, Set[GuideChartEntry]])
      val back: Vector[Map[Cat, Set[GuideChartEntry]]] = Vector.fill(length - i) {
        ListMap() ++ ti.next.lsplit(" ;; ").drop(1).map {
          case MapEntryRe(Cat(cat), entriesString) => cat -> entriesString.splitWhitespace.map {
            case BinaryEntryRe(UInt(k), a, b) => BinaryGuideChartEntry(k, BinaryProd(Cat(a.replace("<COMMA>", ",")), Cat(b.replace("<COMMA>", ","))))
            case BinaryEntryRe1(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
            case BinaryEntryRe2(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
            case BinaryEntryRe3(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
            case BinaryEntryRe4(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
            case BinaryEntryRe5(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
            case BinaryEntryRe6(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
            case BinaryEntryRe7(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
            case BinaryEntryRe8(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
            case BinaryEntryRe9(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
            case BinaryEntryRe10(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
            case BinaryEntryRe11(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
            case BinaryEntryRe12(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
            case UnaryEntryRe(Cat(c)) => UnaryGuideChartEntry(UnaryProd(c))
            case TermEntryRe(w) => TermGuideChartEntry(TermProd(w))
          }.toSet[GuideChartEntry]
        }
      }
      front ++ back
    }, ???)
  }
}
