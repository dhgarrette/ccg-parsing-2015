package dhg.ccg.parse.pcfg

import java.io.FileInputStream
import java.io.ObjectInputStream
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.util.zip._
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable.ListMap
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import scala.collection.breakOut
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.util.DrawMatrix
import dhg.util.GraphUtil._
import dhg.gfl.FudgSentence
import dhg.util._
import dhg.util.viz._
import scalaz._
import scalaz.Scalaz._
import scala.util.Try
import dhg.ccg.util._
import scala.collection.immutable.BitSet
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer

trait GuideChartEntryI extends Serializable
case class BinaryGuideChartEntryI(k: Int, ik: Int, kj: Int) extends GuideChartEntryI
case class UnaryGuideChartEntryI(sub: Int) extends GuideChartEntryI
case class TermGuideChartEntryI(w: Int) extends GuideChartEntryI
object GuideChartEntryI {
  type Word = String
  def to(e: GuideChartEntry, catIndexer: Indexer[Cat], wordIndexer: Indexer[Word]): GuideChartEntryI = e match {
    case BinaryGuideChartEntry(k, BinaryProd(ik, kj)) => BinaryGuideChartEntryI(k, catIndexer(ik), catIndexer(kj))
    case UnaryGuideChartEntry(UnaryProd(sub)) => UnaryGuideChartEntryI(catIndexer(sub))
    case TermGuideChartEntry(TermProd(w)) => TermGuideChartEntryI(wordIndexer(w))
  }
  def from(e: GuideChartEntryI, catIndexer: Indexer[Cat], wordIndexer: Indexer[Word]): GuideChartEntry = e match {
    case BinaryGuideChartEntryI(k, ik, kj) => BinaryGuideChartEntry(k, BinaryProd(catIndexer.obj(ik), catIndexer.obj(kj)))
    case UnaryGuideChartEntryI(sub) => UnaryGuideChartEntry(UnaryProd(catIndexer.obj(sub)))
    case TermGuideChartEntryI(w) => TermGuideChartEntry(TermProd(wordIndexer.obj(w)))
  }
}

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
case class CfgGuideChartI(
  words: Array[Int],
  matrix: Chart[Array[(Int, Array[GuideChartEntryI])]], //  chart[t -> gc_entries]
  rootSet: Set[Int] = UniversalSet())
    extends Serializable {
  def apply(i: Int, j: Int) = matrix(i, j)

  def length = matrix.length

  def root = this(0, length)
  def terminals: Array[Array[(Int, Array[GuideChartEntryI])]] = (0 until length).map(i => matrix(i, i + 1))(breakOut)
  def supertagSets: Array[Array[Int]] = wordSupertagsets.map(_._2)
  def wordSupertagsets: Array[(Int, Array[Int])] = terminals.map { cell =>
    val (words, cats) = cell.flatMap { case (t, entries) => entries.collect { case TermGuideChartEntryI(word) => word -> t } }.unzip
    (words.toSet.only, cats.sorted)
  }

  /**
   * Iterate over all cells
   */
  def bottomUpNodes = {
    for {
      span <- 1 to length //        span size
      i <- 0 to (length - span) //  start of span
      j = i + span //               end of span
    } yield (i, j, this(i, j))
  }

  /**
   * Iterate over all cells
   */
  def topDownNodes = {
    for {
      span <- length downto 1 //    span size
      i <- 0 to (length - span) //  start of span
      j = i + span //               end of span
    } yield (i, j, this(i, j))
  }

  //  lazy val numPossibleParses = {
  //    val table: Vector[Vector[IntMap[MutableNumber[BigInt]]]] =
  //      matrix.map(_.map(_.mapVals(_ => new MutableNumber[BigInt]())))
  //
  //    for {
  //      (i, j, cell) <- bottomUpNodes if cell.nonEmpty // visit all relevant cells
  //      (ij, entries) <- cell // cell is sorted according to unary rule dependency relationships
  //      entry <- entries
  //    } entry match {
  //      case BinaryGuideChartEntryI(k, BinaryProdI(ik, kj)) =>
  //        val ikCount = table(i)(k)(ik).get
  //        val kjCount = table(k)(j)(kj).get
  //        //println(f"table($i)($j)($ij)=${table(i)(j)(ij).get}  +=  table($i)($k)($ik)=${ikCount.get}  *  table($k)($j)($kj)=${kjCount.get}")
  //        table(i)(j)(ij) += (ikCount * kjCount)
  //      case UnaryGuideChartEntryI(UnaryProdI(sub)) =>
  //        val subCount = table(i)(j)(sub).get
  //        //println(f"table($i)($j)($ij)=${table(i)(j)(ij).get}  +=  table($i)($j)($sub)=${subCount.get}")
  //        table(i)(j)(ij) += subCount
  //      case TermGuideChartEntryI(TermProdI(word)) =>
  //        //println(f"table($i)($j)($ij)=${table(i)(j)(ij).get}  +=  1")
  //        table(i)(j)(ij) += 1
  //    }
  //
  //    table(0)(length).values.sumBy(_.get)
  //  }

  //  def allParses: Set[CcgTree] = {
  //    val table: Vector[Vector[Map[Cat, MSet[CcgTree]]]] =
  //      matrix.map(_.map(_.mapVals(_ => MSet.empty[CcgTree])))
  //
  //    for {
  //      (i, j, cell) <- bottomUpNodes if cell.nonEmpty // visit all relevant cells
  //      (ij, entries) <- cell // cell is sorted according to unary rule dependency relationships
  //      entry <- entries
  //    } entry match {
  //      case BinaryGuideChartEntry(k, BinaryProd(ik, kj)) =>
  //        for {
  //          ikTree <- table(i)(k)(ik)
  //          kjTree <- table(k)(j)(kj)
  //        } {
  //          //println(f"table($i)($j)($ij)=${table(i)(j)(ij).get}  +=  table($i)($k)($ik)=${ikCount.get}  *  table($k)($j)($kj)=${kjCount.get}")
  //          table(i)(j)(ij) += CcgBinode(ij, ikTree, kjTree)
  //        }
  //      case UnaryGuideChartEntry(UnaryProd(sub)) =>
  //        for { subTree <- table(i)(j)(sub) } {
  //          //println(f"table($i)($j)($ij)=${table(i)(j)(ij).get}  +=  table($i)($j)($sub)=${subCount.get}")
  //          table(i)(j)(ij) += CcgUnode(ij, subTree)
  //        }
  //      case TermGuideChartEntry(TermProd(word)) =>
  //        //println(f"table($i)($j)($ij)=${table(i)(j)(ij).get}  +=  1")
  //        table(i)(j)(ij) += CcgLeaf(ij, word)
  //    }
  //
  //    //DrawMatrix.drawMatrix(table.map(_.tail)) { _.map { case (c, i) => f"$c -> ${i.get}" }.mkString("\n") }
  //
  //    table(0)(length).values.flatten.toSet
  //  }

  //  def draw(): Unit = {
  //    DrawMatrix.drawMatrix(matrix.map(_.tail))(_.map {
  //      case (cat, set) =>
  //        val left = f"$cat -> "
  //        f"$left${
  //          set.map {
  //            case BinaryGuideChartEntry(k, BinaryProd(l, r)) => f"$k:[$l $r]"
  //            case UnaryGuideChartEntry(UnaryProd(sub)) => f"$sub"
  //            case TermGuideChartEntry(TermProd(word)) => f""""$word""""
  //          }.mkString("\n" + " " * left.length)
  //        }"
  //    }.mkString("\n"))
  //  }

  //  def repr: String = {
  //    "new CfgGuideChart(Vector[Vector[Map[Cat, Set[GuideChartEntry]]]](\n" + matrix.map(row => s"  ${
  //      row.map(m => "ListMap(" + m.map {
  //        case (cat, set) => f"$cat -> ${
  //          set.map {
  //            case BinaryGuideChartEntry(k, BinaryProd(l, r)) => f"BinaryGuideChartEntry($k, BinaryProd($l, $r))"
  //            case UnaryGuideChartEntry(UnaryProd(sub)) => f"UnaryGuideChartEntry(UnaryProd($sub))"
  //            case TermGuideChartEntry(TermProd(w)) => s"""TermGuideChartEntry(TermProd("$w"))"""
  //          }
  //        }"
  //      }.mkString(", ") + ")")
  //    } ").mkString(",\n") + "))"
  //  }

  override def toString = f"CfgGuideChartI($matrix)"
}

//

trait CfgGuideChartBuilderI {
  type Word = Int
  type Cat = Int
  type CatSet = BitSet

  final def build(sentence: Vector[Word], fudgAnnotation: Option[FudgSentence], tagdict: IndirectSparseVec[CatSet], allCats: CatSet): Option[CfgGuideChartI] = buildFromSupertagSetSentence(sentence.mapToVal(BitSet.empty), fudgAnnotation, tagdict, allCats)
  //  final def buildFromSupertagged(supertaggedSentence: Vector[(Word, Cat)], fudgAnnotation: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[CfgGuideChart] = buildFromSupertagSetSentence(supertaggedSentence.mapVals(Set(_)), fudgAnnotation, tagdict)
  def buildFromSupertagSetSentence(supertagSetSentence: Vector[(Word, CatSet)], fudgAnnotation: Option[FudgSentence], tagdict: IndirectSparseVec[CatSet], allCats: CatSet): Option[CfgGuideChartI]
}

class SimpleCfgGuideChartBuilderI(
  binaryRules: Array[IndirectSparseVec[Int]], // Map[LeftCat, Map[RightCat, ParentCat]]
  unaryRules: IndirectSparseVec[BitSet], // Map[ChildCat, Set[ParentCat]]
  //additionalSupertagAdder: AdditionalTagAdder[Cat] = new StandardTagDictAdditionalTagAdder[Cat](),
  rootSet: BitSet,
  allowTerminalDeletion: Boolean)
    extends CfgGuideChartBuilderI {

  require(allowTerminalDeletion == false)

  import CfgGuideChartBuilderUtilI._

  def buildFromSupertagSetSentence(supertagSetSentence: Vector[(Word, CatSet)], fudgAnnotation: Option[FudgSentence], tagdict: IndirectSparseVec[CatSet], allCats: CatSet): Option[CfgGuideChartI] = {
    val (sentence, sentenceExistingTags) = supertagSetSentence.unzip
    println("TODO: USE additionalSupertagAdder")
    val sentenceTags = supertagSetSentence.map { case (w, cs) => tagdict.getOrElse(w, allCats) } // additionalSupertagAdder(sentence, sentenceExistingTags, tagdict) // get supertags set for each word
    //println(f"      SimpleCfgGuideChartBuilder: brackets=${fudgAnnotation.map(_.brackets)}")
    doBuild(sentence, sentenceTags, sentence.length, fudgAnnotation)
  }

  /**
   * Go bottom-up with CKY to fill the chart with potential node entries
   *
   * Brackets are given as start/end index pairs, with indexing as:
   *         the   old   man   the   sea    .
   *       0     1     2     3     4     5     6
   * so that bracket (1,3) would cover "old man".
   */
  private[this] def doBuild(sentence: Vector[Word], sentenceTags: Vector[CatSet], n: Int, fudgAnnotation: Option[FudgSentence]): Option[CfgGuideChartI] = {
    val brackets: Vector[(Int, Int)] = fudgAnnotation.map(_.brackets.toVector).getOrElse(Vector.empty)
    val invalidSpans: Set[(Int, Int)] = Set.empty
    val table: Chart[Option[Map[Cat, Set[GuideChartEntryI]]]] = newTable(n, brackets, invalidSpans, Map.empty)
    //new CfgGuideChart(table.toVector.map { row => row.toVector.map { cell => if (cell.isDefined) Map(cat"X" -> Set.empty[GuideChartEntry]) else Map.empty[Cat, Set[GuideChartEntry]] } }).draw()

    fillTerminalLevel(table, sentence, sentenceTags)
    //toCfgGuideChart(table).draw()

    fillHigherLevels(table, n)
    //toCfgGuideChart(table).draw()

    if (table(0, n).get.filterKeys(rootSet).values.exists(_.nonEmpty)) { // there's at least one parse
      val clean = makeCleanChart(table, n, brackets, invalidSpans)
      val result = toCfgGuideChart(sentence.toArray, clean, unaryRules, rootSet)

      //result.draw()
      Some(result)
    }
    else None
  }

  /**
   * Add supertags to bottom level of the chart
   */
  private[this] def fillTerminalLevel(table: Chart[Option[Map[Cat, Set[GuideChartEntryI]]]], sentence: Vector[Word], sentenceTags: Vector[CatSet]): Unit = {
    for (((word, tags), i) <- (sentence.iterator zipSafe sentenceTags).zipWithIndex) {
      //val cellij = table(i, i + 1).get // terminal level must always be present
      val entries: Set[(Cat, GuideChartEntryI)] = tags.mapToVal(TermGuideChartEntryI(word))(breakOut)
      val newEntries = repeatedlyApplyUnaryRules(entries)
      table(i, i + 1) = Some(newEntries.groupByKey)
    }
  }

  /**
   * Apply rules to add higher-level chart entries
   */
  private[this] def fillHigherLevels(table: Chart[Option[Map[Cat, Set[GuideChartEntryI]]]], n: Int): Unit = {
    for {
      j <- 2 to n //             end of span
      i <- j - 2 downto 0 //     start of span
      cellij <- table(i, j) //   search stops here if this span is unavailable (due to known bracketing)
    } {

      val entries: Set[(Cat, GuideChartEntryI)] =
        (for {
          k <- i + 1 until j //               split of span
          cellik <- table(i, k).iterator //   search stops here if this span is unavailable (due to known bracketing)
          cellkj <- table(k, j).iterator //   search stops here if this span is unavailable (due to known bracketing)
          ik <- cellik.keys
          kj <- cellkj.keys
          ij <- binaryRules(ik).get(kj)
        } yield {
          (ij -> BinaryGuideChartEntryI(k, ik, kj))
        })(breakOut)

      val newEntries = repeatedlyApplyUnaryRules(entries)
      table(i, j) = Some(newEntries.groupByKey)
    }
  }

  private[this] def repeatedlyApplyUnaryRules(prods: Set[(Cat, GuideChartEntryI)]): Set[(Cat, GuideChartEntryI)] = {
    @tailrec def inner(recent: Set[(Cat, GuideChartEntryI)], accum: Set[(Cat, GuideChartEntryI)]): Set[(Cat, GuideChartEntryI)] = {
      val newProds = for {
        (cat, prod) <- recent
        newCats <- unaryRules.get(cat).iterator
        newCat <- newCats
        newProd = newCat -> (UnaryGuideChartEntryI(cat): GuideChartEntryI)
        if !accum.contains(newProd)
      } yield newProd
      if (newProds.isEmpty) accum | recent
      else inner(newProds, accum | recent)
    }
    inner(prods, Set.empty)
  }

  /**
   * Make a clean chart so we can put only useful entries (those that
   * can actually be used in a parse).  Go top-down following paths that
   * start with a root node.
   */
  private[this] def makeCleanChart(table: Chart[Option[Map[Cat, Set[GuideChartEntryI]]]], n: Int, brackets: Vector[(Int, Int)], invalidSpans: Set[(Int, Int)]) = {
    val cleanCfgTable: Chart[Option[MMap[Cat, Set[GuideChartEntryI]]]] = newTable(n, brackets, invalidSpans, MMap.empty)
    cleanCfgTable(0, n).get ++= table(0, n).get
    for {
      spanSize <- n downto 1
      i <- 0 to (n - spanSize)
      j = i + spanSize
    } {
      makeCleanCellRecursively(i, j, cleanCfgTable, table)
    }
    cleanCfgTable
  }

  /**
   * Copy useful entries into clean chart.
   * This can potentially re-copy the same chart entries multiple times, but that's ok i guess
   */
  @tailrec private[this] def makeCleanCellRecursively(i: Int, j: Int, cleanCfgTable: Chart[Option[MMap[Cat, Set[GuideChartEntryI]]]], table: Chart[Option[Map[Cat, Set[GuideChartEntryI]]]]): Unit = {
    for {
      cleanCellij <- cleanCfgTable(i, j)
      (ij, ijSet) <- cleanCellij
      BinaryGuideChartEntryI(k, ik, kj) <- ijSet
    } {
      cleanCfgTable(i, k).get(ik) = table(i, k).get(ik)
      cleanCfgTable(k, j).get(kj) = table(k, j).get(kj)
    }

    val newEntries =
      for {
        cleanCellij <- cleanCfgTable(i, j).toSet
        (ij, ijSet) <- cleanCellij
        UnaryGuideChartEntryI(sub) <- ijSet // find unary rules
        if !cleanCellij.contains(sub) //         that add additional categories
      } yield sub

    /*
     * If there are new entries to be handled
     */
    if (newEntries.nonEmpty) {
      for (sub <- newEntries) {
        cleanCfgTable(i, j).get(sub) = table(i, j).get(sub)
      }
      makeCleanCellRecursively(i, j, cleanCfgTable, table)
    }
  }

  override def toString = f"SimpleCfgGuideChartBuilderI()" //${rules.mkString("[", ",", "]")}, ${additionalSupertagAdder})))"
}

//class PunctBracketingCfgGuideChartBuilder(
//  punct: Set[String],
//  delegate: CfgGuideChartBuilder)
//    extends CfgGuideChartBuilder {
//  def buildFromSupertagSetSentence(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[CfgGuideChart] = {
//    val indexedWords = supertagSetSentence.map(_._1).zipWithIndex
//    val splits = indexedWords.splitWhere { case (w, i) => punct(w) }
//    val additionalBrackets = splits.collect { case xs if xs.nonEmpty => (xs.head._2, xs.last._2 + 1) }
//    ??? //delegate.buildFromSupertagSetSentence(supertagSetSentence, brackets ++ additionalBrackets, tagdict)
//  }
//}

object CfgGuideChartBuilderUtilI {
  type Cat = Int
  type CatSet = BitSet
  //type MTable = Array[Array[Option[Map[Cat, Set[GuideChartEntryI]]]]]

  def newTable[A](n: Int, brackets: Vector[(Int, Int)], invalidSpans: Set[(Int, Int)], newthing: => A): Chart[Option[A]] = {
    val table: Chart[Option[A]] = Chart.fill(n)(None)
    for (i <- 0 until n) {
      for (j <- (i + 1) to n) {
        val itoj = (i + 1 to j)
        if (i >= j || invalidSpans(i -> j) ||
          brackets.exists { // if there exists a cross-bracketing
            case (a, b) =>
              assert(a < b, f"improper bracket: ($a,$b)")
              /*  */ (a < i && i < b && b < j) ||
                /**/ (i < a && a < j && j < b)
          }) {}
        else {
          table(i, j) = Some(newthing)
        }
      }
    }
    table
  }

  def toCfgGuideChart(words: Array[Int], table: Chart[Option[MMap[Cat, Set[GuideChartEntryI]]]], unaryRules: Vec[CatSet], rootSet: CatSet): CfgGuideChartI = {
    val n = table.length
    val chart: Chart[Array[(Cat, Array[GuideChartEntryI])]] = Chart.empty(n)
    val catOrdering = CfgGuideChartI.makeCatOrdering(unaryRules)
    val ord = catOrdering.on[(Cat, Array[GuideChartEntryI])](_._1)

    for {
      i <- 0 until n
      j <- i + 1 to n
    } {
      chart(i, j) =
        table(i, j).fold(Array.empty[(Cat, Array[GuideChartEntryI])]) { contents =>
          contents.mapVals(_.toArray).toArray.sorted(ord)
        }
    }

    CfgGuideChartI(words, chart, rootSet)
  }
}

//class CascadingAttemptsCfgGuideChartBuilder(orderedDelegates: Vector[CfgGuideChartBuilder]) extends CfgGuideChartBuilder {
//  def buildFromSupertagSetSentence(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[CfgGuideChart] = {
//    orderedDelegates.iterator.map { gcb =>
//      val gc = gcb.buildFromSupertagSetSentence(supertagSetSentence, fudgAnnotation, tagdict)
//      //if (gc.isEmpty) println(f"    failed guide chart building: ${gcb}")
//      gc
//    }.collectFirst { case Some(gc) => gc }
//  }
//}
//
//class DecisionCfgGuideChartBuilder(val a: CfgGuideChartBuilder, val b: CfgGuideChartBuilder, useA: ((Vector[(String, Set[Cat])], Option[FudgSentence]) => Boolean)) extends CfgGuideChartBuilder {
//  def buildFromSupertagSetSentence(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[CfgGuideChart] = {
//    val builder = if (useA(supertagSetSentence, fudgAnnotation)) a else b
//    //println(f"      decision: $builder")
//    builder.buildFromSupertagSetSentence(supertagSetSentence, fudgAnnotation, tagdict)
//  }
//
//  override def toString() = f"DecisionCfgGuideChartBuilder(a=$a, b=$b)"
//}
//
///**
// * Ignore any annotation except for brackets
// */
//class BracketOnlyCfgGuideChartBuilder(delegate: CfgGuideChartBuilder) extends CfgGuideChartBuilder {
//  def buildFromSupertagSetSentence(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[CfgGuideChart] = {
//    delegate.buildFromSupertagSetSentence(supertagSetSentence, fudgAnnotation.map(_.bracketsOnly()), tagdict)
//  }
//
//  override def toString() = f"BracketOnlyCfgGuideChartBuilder($delegate)"
//}

//

object CfgGuideChartI {

  type Word = String
  type CatI = Int
  type CatSet = BitSet
  type CatMap[A] = Vec[A]

  /**
   * Make a CfgGuideChartI by extracting the word vector out of the given matrix.
   * Will not work if there are any empty terminal cells.
   */
  @deprecated("Use the version that has `words` explicitly given", "")
  def make(
    matrix: Chart[Array[(Int, Array[GuideChartEntryI])]], //  chart[t -> gc_entries]
    rootSet: Set[Int] = UniversalSet()): CfgGuideChartI = {
    val words = CfgGuideChartI(Array.empty, matrix, rootSet).terminals.map { cell =>
      val (words, cats) = cell.flatMap { case (t, entries) => entries.collect { case TermGuideChartEntryI(word) => word -> t } }.unzip
      words.toSet.only
    }
    CfgGuideChartI(words, matrix, rootSet)
  }

  /**
   * Order the categories for iterating such that for any unary rule that
   * rewrites A as B, A is visited before B.
   */
  def makeCatOrdering(unaryRules: CatMap[CatSet]) = {
    val allLinks: Set[(CatI, CatI)] = (for { (c, ps) <- unaryRules.activePairs; p <- ps } yield (c -> p))(breakOut)
    val catOrdering = new IndirectSparseVec(toDagOrder(allLinks).toArray, (0 until allLinks.size).toArray, allLinks.size, Int.MaxValue)
    sys.error("Vector(DeleteFromLeftCat, DeleteFromRightCat)")
    val orderMap = catOrdering.withDefaultValue(-1) // categories uninvolved in the ordering are handled first, as a block
    scala.math.Ordering.by[CatI, Int](orderMap)
  }

  def to(gc: CfgGuideChart, catIndexer: Indexer[Cat], wordIndexer: Indexer[Word]) = {
    CfgGuideChartI(
      gc.words.map(wordIndexer.index).toArray,
      Chart.tabulate(gc.length) { (i, j) =>
        gc(i, j).toArray.map {
          case (t, entries) =>
            val gces: Array[GuideChartEntryI] = entries.map(e => GuideChartEntryI.to(e, catIndexer, wordIndexer)).toArray
            catIndexer(t) -> gces
        }
      }, gc.rootSet.map(catIndexer).toBitSet)
  }
  def from(gc: CfgGuideChartI, catIndexer: Indexer[Cat], wordIndexer: Indexer[Word]) = {
    // Vector[Vector[Map[Cat, Set[GuideChartEntry]]]]
    val n = gc.length
    CfgGuideChart(
      gc.words.map(wordIndexer.obj).toVector,
      Vector.tabulate(n) { i =>
        Vector.tabulate(n + 1) { j =>
          if (i < j)
            gc(i, j).map {
              case (t, entries) =>
                catIndexer.obj(t) -> entries.map(e => GuideChartEntryI.from(e, catIndexer, wordIndexer)).toSet
            }.toMap
          else Map.empty
        }
      }, gc.rootSet.map(catIndexer.obj))
  }

  //
  //
  //

  def read(path: String): CfgGuideChartI = {
    val file = File(path)
    assert(file.exists, "Serialized CfgGuideChartI file " + path + " not found")
    using(new ObjectInputStream(new GZIPInputStream(new FileInputStream(file)))) { in =>
      in.readObject().asInstanceOf[CfgGuideChartI]
    }
  }

  def readVector(path: String): Vector[(Option[CfgGuideChartI], Option[CcgTreeI])] = {
    val file = File(path)
    assert(file.exists, "Serialized CfgGuideChartI vector file " + path + " not found")
    using(new ObjectInputStream(new GZIPInputStream(new FileInputStream(file)))) { in =>
      in.readObject().asInstanceOf[Vector[(Option[CfgGuideChartI], Option[CcgTreeI])]]
    }
  }

  def write(gc: CfgGuideChartI, path: String): Unit = {
    using(new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(path)))) { out =>
      out.writeObject(gc)
    }
  }

  def writeVector(gcs: Vector[(Option[CfgGuideChartI], Option[CcgTreeI])], path: String): Unit = {
    using(new ObjectOutputStream(new GZIPOutputStream(new FileOutputStream(path)))) { out =>
      out.writeObject(gcs)
    }
  }
}

//class CfgGuideChartSerDeser() {
//  private[this] val Cat: CatInterner = ??? // new StandardCatInterner(CatInterner.CcgBankPuncRe, CatInterner.CcgBankAtomRe, removeFeatures = false) // only need one format, as long as its consistent
//
//  val HeaderRe = """length=(\d+)""".r
//  val MapEntryRe = """(\S+) -> (.+)""".r
//  val BinaryEntryRe = """(\d+):\[([^,\s]+),([^,\s]+)\]""".r
//  val BinaryEntryRe1 = """(\d+):\[(,),([^\s]+)\]""".r
//  val BinaryEntryRe2 = """(\d+):\[([^\s]+),(,)\]""".r
//  val BinaryEntryRe3 = """(\d+):\[([^\s]+),(\(,\\,\))\]""".r
//  val BinaryEntryRe4 = """(\d+):\[(\(,\\,\)),([^\s]+)\]""".r
//  val BinaryEntryRe5 = """(\d+):\[([^\s]+),(\(,/,\))\]""".r
//  val BinaryEntryRe6 = """(\d+):\[(\(,/,\)),([^\s]+)\]""".r
//  val BinaryEntryRe7 = """(\d+):\[([^\s]+),(RRB)\]""".r
//  val BinaryEntryRe8 = """(\d+):\[(LRB),([^\s]+)\]""".r
//  val BinaryEntryRe9 = """(\d+):\[([^\s]+),(:)\]""".r
//  val BinaryEntryRe10 = """(\d+):\[(:),([^\s]+)\]""".r
//  val BinaryEntryRe11 = """(\d+):\[([^\s]+),(\.)\]""".r
//  val BinaryEntryRe12 = """(\d+):\[(\.),([^\s]+)\]""".r
//  val UnaryEntryRe = """\[(.+)\]""".r
//  val TermEntryRe = """(.+)""".r
//
//  def ser(gc: CfgGuideChart): Vector[String] = {
//    val matrix: Vector[Vector[Map[Cat, Set[GuideChartEntry]]]] = gc.matrix
//    val b = new VectorBuilder[String]
//    b += f"length=${gc.length}"
//    for ((row, i) <- matrix.zipWithIndex) {
//      for ((m, j) <- row.zipWithIndex.drop(i + 1)) {
//        b += f"($i,$j) ;; " + m.mapt { (cat, entries) =>
//          f"$cat -> ${
//            entries.map {
//              case BinaryGuideChartEntry(k, BinaryProd(a, b)) => f"$k:[${a.toString.replace(",", "<COMMA>")},${b.toString.replace(",", "<COMMA>")}]"
//              case UnaryGuideChartEntry(UnaryProd(c)) => f"[$c]"
//              case TermGuideChartEntry(TermProd(w)) => w
//            }.mkString(" ")
//          }"
//        }.mkString(" ;; ")
//      }
//    }
//    b.result
//  }
//
//  def deser(s: Vector[String]): CfgGuideChart = {
//    val HeaderRe(UInt(length)) +: tail = s
//    val ti = tail.iterator
//    new CfgGuideChart(Vector.tabulate(length) { i =>
//      val front = Vector.fill(i + 1)(Map.empty[Cat, Set[GuideChartEntry]])
//      val back: Vector[Map[Cat, Set[GuideChartEntry]]] = Vector.fill(length - i) {
//        ListMap() ++ ti.next.lsplit(" ;; ").drop(1).map {
//          case MapEntryRe(Cat(cat), entriesString) => cat -> entriesString.splitWhitespace.map {
//            case BinaryEntryRe(UInt(k), a, b) => BinaryGuideChartEntry(k, BinaryProd(Cat(a.replace("<COMMA>", ",")), Cat(b.replace("<COMMA>", ","))))
//            case BinaryEntryRe1(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
//            case BinaryEntryRe2(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
//            case BinaryEntryRe3(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
//            case BinaryEntryRe4(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
//            case BinaryEntryRe5(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
//            case BinaryEntryRe6(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
//            case BinaryEntryRe7(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
//            case BinaryEntryRe8(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
//            case BinaryEntryRe9(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
//            case BinaryEntryRe10(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
//            case BinaryEntryRe11(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
//            case BinaryEntryRe12(UInt(k), Cat(a), Cat(b)) => BinaryGuideChartEntry(k, BinaryProd(a, b))
//            case UnaryEntryRe(Cat(c)) => UnaryGuideChartEntry(UnaryProd(c))
//            case TermEntryRe(w) => TermGuideChartEntry(TermProd(w))
//          }.toSet[GuideChartEntry]
//        }
//      }
//      front ++ back
//    })
//  }
//}
