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
import dhg.gfl.Sentence
import dhg.util._
import dhg.util.viz.TreeViz
import scalaz._
import scalaz.Scalaz._
import scala.util.Try
import dhg.ccg.parse.dep.DepTree
import dhg.util.viz.VizTree
import dhg.gfl.Edge
import dhg.gfl.WordNode
import dhg.gfl.Token
import dhg.gfl.Fudg

class DependencyTrackingCfgGuideChartBuilder(
  val rules: Vector[CcgRule],
  additionalSupertagAdder: AdditionalTagAdder[Cat] = new StandardTagDictAdditionalTagAdder[Cat](),
  rootSet: Set[Cat] = UniversalSet(),
  allowTerminalDeletion: Boolean = false)
    extends CfgGuideChartBuilder {

  private[this] val unaryRules = rules.collect { case r: UnaryCcgRule => r }
  private[this] val binaryRules = rules.collect { case r: BinaryCcgRule => r }

  import DependencyTrackingCfgGuideChartBuilderUtil._

  def buildFromSupertagSetSentence(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence] = None, tagdict: TagDictionary[Cat] = DummyCatTagDictionary): Option[CfgGuideChart] = {
    val (sentence, sentenceExistingTags) = supertagSetSentence.unzip
    val sentenceTags = additionalSupertagAdder(sentence, sentenceExistingTags, tagdict) // get supertags set for each word
    //println(f"      SimpleCfgGuideChartBuilder: brackets=${fudgAnnotation.map(_.brackets)}")
    doBuild(sentence, sentenceTags, sentence.length, fudgAnnotation)
  }

  def buildUnpruned(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence] = None, tagdict: TagDictionary[Cat] = DummyCatTagDictionary): CfgGuideChart = {
    val (sentence, sentenceExistingTags) = supertagSetSentence.unzip
    val sentenceTags = additionalSupertagAdder(sentence, sentenceExistingTags, tagdict) // get supertags set for each word
    val knownDependencies: Set[(Int, Int)] = fudgAnnotation.fold(Set.empty[(Int, Int)])(fudgAnnotationToDependencyIndexSet)

    val brackets: Vector[(Int, Int)] = makeBracketList(sentence, fudgAnnotation)
    val invalidSpans: Set[(Int, Int)] = Set.empty

    val table: MTable = fillBottomUp(sentence, sentenceTags, supertagSetSentence.length, brackets, invalidSpans, knownDependencies)
    toCfgGuideChart(supertagSetSentence.map(_._1), table, unaryRules, rootSet)
  }

  def fudgAnnotationToDependencyIndexSet(fudgAnnotation: FudgSentence): Set[(Int, Int)] = {
    fudgAnnotation.edges.map {
      case Edge(
        WordNode(_, Token(_, parentIndex)),
        WordNode(_, Token(_, childIndex)),
        label) => parentIndex -> childIndex
    }.toSet
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
    val brackets: Vector[(Int, Int)] = makeBracketList(sentence, fudgAnnotation)
    val invalidSpans: Set[(Int, Int)] = Set.empty
    val knownDependencies: Set[(Int, Int)] = fudgAnnotation.fold(Set.empty[(Int, Int)])(fudgAnnotationToDependencyIndexSet)

    val table: MTable = fillBottomUp(sentence, sentenceTags, n, brackets, invalidSpans, knownDependencies)
    //toCfgGuideChart(sentence, table, unaryRules, rootSet).draw() // TODO: REMOVE

    if (table(0)(n).get.filterKeys(rootSet).values.exists(_.nonEmpty)) { // there's at least one parse
      val clean = makeCleanChart(table, n, brackets, invalidSpans, knownDependencies)
      //toCfgGuideChart(sentence, clean, unaryRules, rootSet).draw() // TODO: REMOVE

      val result = toCfgGuideChart(sentence, clean, unaryRules, rootSet)

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
      fudgAnnotation.flatMap { a =>
        if (Fudg.isSemanticallyValid(a.edges)) {
          Some(a.brackets.toVector)
        }
        else {
          Console.err.println("Non-semantically-valid annotation found. Will not attempt to extract bracketing.\n  In the future, we should explore the possiblity of extracting a partial bracketing")
          None
        }
      }.getOrElse(Vector.empty)
    }
    catch {
      case e: Exception =>
        Console.err.println(s"Failure in DependencyTrackingCfgGuideChartBuilder.makeBracketList")
        Console.err.println(e.getMessage)
        e.printStackTrace()
        Console.err.println(sentence.mkString(" "))
        Console.err.println(fudgAnnotation)
        Console.err.println(fudgAnnotation.get)
        Console.err.println(sentence.mkString(" "))
        Console.err.println(fudgAnnotation.get.tokens.map(_.token).mkString(" "))

        Vector.empty
    }
  }

  def fillBottomUp(sentence: Vector[Word], sentenceTags: Vector[Set[Cat]], n: Int, brackets: Vector[(Int, Int)], invalidSpans: Set[(Int, Int)], knownDependencies: Set[(Int, Int)]): MTable = {
    val table: MTable = newTable(n, brackets, invalidSpans)
    //new CfgGuideChart(table.toVector.map { row => row.toVector.map { cell => if (cell.isDefined) Map(cat"X" -> Set.empty[GuideChartEntry]) else Map.empty[Cat, Set[GuideChartEntry]] } }).draw()

    fillTerminalLevel(table, sentence, sentenceTags)
    //toCfgGuideChart(sentence, table, unaryRules).draw()

    fillHigherLevels(table, n, knownDependencies)
    //toCfgGuideChart(sentence, table, unaryRules).draw()

    table
  }

  /**
   * Add supertags to bottom level of the chart
   */
  private[this] def fillTerminalLevel(table: MTable, sentence: Vector[Word], sentenceTags: Vector[Set[Cat]]): Unit = {
    for (((word, tags), i) <- (sentence zipSafe sentenceTags).zipWithIndex) {
      val cellij = table(i)(i + 1).get // lexical cell must exist
      val termEntry: GuideChartEntryDT = TermGuideChartEntryDT(TermProdDT(word))
      val termDT = DTree(word, i)
      val entries = tags.map(c => (c, termDT, termEntry))
      addEntriesToTable(table, cellij, entries)
    }
  }

  def addEntriesToTable(table: MTable, cellij: MMap[Cat, MMap[DTree, MSet[GuideChartEntryDT]]], entries: Set[(Cat, DTree, GuideChartEntryDT)]) = {
    for ((ij, ijDT, entry) <- repeatedlyApplyUnaryRules(entries.toSet)) {
      val deptreeToEntriesMap = cellij.getOrElseUpdate(ij, MMap.empty[DTree, MSet[GuideChartEntryDT]])
      val entrySet = deptreeToEntriesMap.getOrElseUpdate(ijDT, MSet.empty[GuideChartEntryDT])
      entrySet += entry
    }
  }

  /**
   * Apply rules to add higher-level chart entries
   */
  private[this] def fillHigherLevels(table: MTable, n: Int, knownDependencies: Set[(Int, Int)]): Unit = {
    for {
      j <- 2 to n //             end of span
      i <- j - 2 downto 0 //     start of span
      cellij <- table(i)(j) //   search stops here if this span is unavailable (due to known bracketing)
    } {
      val entries = higherLevelEntries(i, j, table, knownDependencies).toSet
      addEntriesToTable(table, cellij, entries)

      if (allowTerminalDeletion && j == i + 2) { // if children are terminal cells
        val k = i + 1

        // Unary rule version
        val cellik = table(i)(k).get
        val cellkj = table(k)(j).get
        val ikNewEntries =
          for {
            (ik, ikTreesToEntries) <- cellik if !Set[Cat](DeleteFromLeftCat, DeleteFromRightCat)(ik) //   for every left-side category...
            (ikDT, ikEntries) <- ikTreesToEntries
            if ikEntries.exists { case TermGuideChartEntryDT(ikTermProd) => true; case _ => false } //    ...that is a supertag
          } yield {
            val deptreeToEntriesMap = cellij.getOrElseUpdate(ik, MMap.empty[DTree, MSet[GuideChartEntryDT]])
            val entrySet = deptreeToEntriesMap.getOrElseUpdate(???, MSet.empty[GuideChartEntryDT])
            entrySet += BinaryGuideChartEntryDT(k, BinaryProdDT(ik, ???, DeleteFromLeftCat, ???)) // add a `ik -> ik DFL` production
            ik
          }
        for (ik <- ikNewEntries) {
          val deptreeToEntriesMap = cellik.getOrElseUpdate(DeleteFromRightCat, MMap.empty[DTree, MSet[GuideChartEntryDT]])
          val entrySet = deptreeToEntriesMap.getOrElseUpdate(???, MSet.empty[GuideChartEntryDT])
          entrySet += UnaryGuideChartEntryDT(UnaryProdDT(ik, ???)) //         add a `DFR -> ik` unary rule
        }
        val kjNewEntries =
          for {
            (kj, kjTreesToEntries) <- cellkj if !Set[Cat](DeleteFromLeftCat, DeleteFromRightCat)(kj) //   for every right-side category...
            (kjDT, kjEntries) <- kjTreesToEntries
            if kjEntries.exists { case TermGuideChartEntryDT(kjTermProd) => true; case _ => false } //    ...that is a supertag
          } yield {
            val deptreeToEntriesMap = cellij.getOrElseUpdate(kj, MMap.empty[DTree, MSet[GuideChartEntryDT]])
            val entrySet = deptreeToEntriesMap.getOrElseUpdate(???, MSet.empty[GuideChartEntryDT])
            entrySet += BinaryGuideChartEntryDT(k, BinaryProdDT(DeleteFromRightCat, ???, kj, ???)) // add `kj -> DFRC kj` production
            kj
          }
        for (kj <- kjNewEntries) {
          val deptreeToEntriesMap = cellik.getOrElseUpdate(DeleteFromLeftCat, MMap.empty[DTree, MSet[GuideChartEntryDT]])
          val entrySet = deptreeToEntriesMap.getOrElseUpdate(???, MSet.empty[GuideChartEntryDT])
          entrySet += UnaryGuideChartEntryDT(UnaryProdDT(kj, ???)) //         add a `DFL -> kj` unary rule
        }

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

  private[this] def higherLevelEntries(i: Int, j: Int, table: MTable, knownDependencies: Set[(Int, Int)]): Vector[(Cat, DTree, GuideChartEntryDT)] = {
    val b = new VectorBuilder[(Cat, DTree, GuideChartEntryDT)]()
    for {
      k <- i + 1 until j //      split of span
      cellik <- table(i)(k) //   search stops here if this span is unavailable (due to known bracketing)
      cellkj <- table(k)(j) //   search stops here if this span is unavailable (due to known bracketing)
      (ik, ikDTs) <- cellik
      (kj, kjDTs) <- cellkj
      //ij <- binaryRules.iterator.flatMap(rule => rule(ik, kj)).headOption
      rule <- binaryRules
      ij <- rule(ik, kj)
      ikDT <- ikDTs.keySet
      kjDT <- kjDTs.keySet
    } {

      val (ijDT, newEdge) =
        if (DepTree.leftIsHead(ij, ik, kj))
          (ikDT.addRightChild(kjDT), ikDT.index -> kjDT.index)
        else
          (kjDT.addLeftChild(ikDT), kjDT.index -> ikDT.index)

      if (!knownDependencies.contains(newEdge.swap)) {
        b += ((ij, ijDT, BinaryGuideChartEntryDT(k, BinaryProdDT(ik, ikDT, kj, kjDT))))
      }
      else {
        //println(f"Rejected combination: ($i,$k,$j)  $ik $kj -> $ij  would have produced dependency  ${newEdge._1} -> ${newEdge._2}")
      }
    }
    b.result
  }

  private[this] def repeatedlyApplyUnaryRules(prods: Set[(Cat, DTree, GuideChartEntryDT)]): Set[(Cat, DTree, GuideChartEntryDT)] = {
    @tailrec def inner(prods: Set[(Cat, DTree, GuideChartEntryDT)], accum: Set[(Cat, DTree, GuideChartEntryDT)]): Set[(Cat, DTree, GuideChartEntryDT)] = {
      val newProds = for {
        (cat, dt, prod) <- prods
        rule <- unaryRules
        newCat <- rule(cat)
        newProd = (newCat, dt, (UnaryGuideChartEntryDT(UnaryProdDT(cat, dt)): GuideChartEntryDT))
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
  private[this] def makeCleanChart(table: MTable, n: Int, brackets: Vector[(Int, Int)], invalidSpans: Set[(Int, Int)], knownDependencies: Set[(Int, Int)]) = {
    //toCfgGuideChart(table, unaryRules).draw()

    val cleanCfgTable: MTable = newTable(n, brackets, invalidSpans)

    for {
      (ij, ijSet) <- table(0)(n).get
      if rootSet.contains(ij) // Filter root cell to only contain categories in rootSet
    } {
      cleanCfgTable(0)(n).get(ij) = ijSet
    }
    //toCfgGuideChart(Vector.fill(table.length)(""), cleanCfgTable, unaryRules, rootSet).draw() // TODO: REMOVE

    for {
      spanSize <- n downto 1
      i <- 0 to (n - spanSize)
      j = i + spanSize
    } {
      followUnaryRules(i, j, cleanCfgTable, table)
      //println(s"($i,$j)") // TODO: REMOVE
      //toCfgGuideChart(Vector.fill(table.length)(""), cleanCfgTable, unaryRules, rootSet).draw() // TODO: REMOVE

      for {
        cleanCellij <- cleanCfgTable(i)(j)
        (ij, ijTreesToEntries) <- cleanCellij
        (ijDT, ijEntries) <- ijTreesToEntries
        BinaryGuideChartEntryDT(k, BinaryProdDT(ik, ikDT, kj, kjDT)) <- ijEntries
      } {
        cleanCfgTable(i)(k).get.getOrElseUpdate(ik, MMap.empty[DTree, MSet[GuideChartEntryDT]])(ikDT) = table(i)(k).get(ik)(ikDT)
        cleanCfgTable(k)(j).get.getOrElseUpdate(kj, MMap.empty[DTree, MSet[GuideChartEntryDT]])(kjDT) = table(k)(j).get(kj)(kjDT)
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
        (ij, ijTreesToEntries) <- cleanCellij
        (ijDT, ijEntries) <- ijTreesToEntries
        UnaryGuideChartEntryDT(UnaryProdDT(sub, subDT)) <- ijEntries // find unary rules
        if !cleanCellij.contains(sub) //                                that add additional categories
        //_ = { println(s"($i, $j): ij=$ij  ->  sub=$sub") } // TODO: REMOVE
      } yield sub

    /*
     * If there are new entries to be handled
     */
    if (newEntries.nonEmpty) {
      for {
        sub <- newEntries
        //_ = { try { cleanCfgTable(i)(j).get(sub) } catch { case e: NoSuchElementException => toCfgGuideChart(Vector.fill(cleanCfgTable.length)(""), cleanCfgTable, unaryRules, rootSet).draw() } } // TODO: REMOVE
        (subDT, subEntries) <- table(i)(j).get(sub)
      } {
        cleanCfgTable(i)(j).get.getOrElseUpdate(sub, MMap.empty[DTree, MSet[GuideChartEntryDT]])(subDT) = subEntries
      }
      followUnaryRules(i, j, cleanCfgTable, table)
    }
  }

  override def toString = f"DependencyTrackingCfgGuideChartBuilder(${rules.mkString("[", ",", "]")}, ${additionalSupertagAdder})))"
}

object DependencyTrackingCfgGuideChartBuilderUtil {
  //  case class DTree(word: String, index: Int, children: Vector[DTree] = Vector.empty) extends VizTree {
  //    def label = s"${word}_${index}"
  //    def addRightChild(kjDT: DTree) = DTree(this.word, this.index, this.children :+ kjDT)
  //    def addLeftChild(ikDT: DTree) = DTree(this.word, this.index, ikDT +: this.children)
  //  }
  case class DTree(index: Int) {
    def addRightChild(kjDT: DTree) = this
    def addLeftChild(ikDT: DTree) = this
  }
  object DTree {
    def apply(word: String, index: Int, children: Vector[DTree] = Vector.empty): DTree = DTree(index)
  }

  type MTable = Array[Array[Option[MMap[Cat, MMap[DTree, MSet[GuideChartEntryDT]]]]]]

  trait GuideChartEntryDT extends Serializable { def prod: ProdDT }
  case class BinaryGuideChartEntryDT(k: Int, prod: BinaryProdDT) extends GuideChartEntryDT
  case class UnaryGuideChartEntryDT(prod: UnaryProdDT) extends GuideChartEntryDT
  case class TermGuideChartEntryDT(prod: TermProdDT) extends GuideChartEntryDT

  sealed trait ProdDT extends Serializable {
    def isNt: Boolean
    def isTerm: Boolean
  }

  sealed trait NontProdDT extends ProdDT {
    final def isNt = true
    final def isTerm = false
  }

  case class BinaryProdDT(left: Cat, leftDT: DTree, right: Cat, rightDT: DTree) extends NontProdDT {
    override def toString: String = f"[$left $right]"
  }

  case class UnaryProdDT(sub: Cat, subDT: DTree) extends NontProdDT {
    override def toString: String = f"[$sub]"
  }

  case class TermProdDT(word: String) extends ProdDT {
    override def isNt = false
    override def isTerm = true
    override def toString: String = word
  }

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
                val treelessContents = contents.mapVals(_.flatMap {
                  case (_, entries) => entries.map {
                    case BinaryGuideChartEntryDT(k, BinaryProdDT(ik, _, kj, _)) => BinaryGuideChartEntry(k, BinaryProd(ik, kj)): GuideChartEntry
                    case UnaryGuideChartEntryDT(UnaryProdDT(sub, _)) => UnaryGuideChartEntry(UnaryProd(sub)): GuideChartEntry
                    case TermGuideChartEntryDT(TermProdDT(word)) => TermGuideChartEntry(TermProd(word)): GuideChartEntry
                  }
                }.to[Set])
                val sorted = treelessContents.toVector.sorted(ord)
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

object DependencyTrackingCfgGuideChartBuilder {
  def main(args: Array[String]): Unit = {
    val gcb = new DependencyTrackingCfgGuideChartBuilder(Vector(FA, BA), new NoOpAdditionalTagAdder)
    val sentence = Vector(
      ("John" -> Set(cat"""NP""")),
      ("walks" -> Set(cat"""((S\NP)/NP)""", cat"""((S\NP)/N)""")),
      ("a" -> Set(cat"""(NP/(N\N))""",cat"""(N/N)""")),
      ("dog" -> Set(cat"""N""", cat"""(N\N)""")))

    println("gc1:")
    val gc1 = gcb.buildFromSupertagSetSentence(sentence)
    gc1.map(_.draw())
    println("END gc1")
    println("gc2:")
    val gc2 = gcb.buildFromSupertagSetSentence(sentence, Some(Sentence.fromDepIndices(sentence.map(_._1), Vector(2 -> 3, 3->1))))
    gc2.map(_.draw())
    println("END gc2")
    println("gc3:")
    val gc3 = gcb.buildFromSupertagSetSentence(sentence, Some(Sentence.fromDepIndices(sentence.map(_._1), Vector(3 -> 2, 3->1))))
    gc3.map(_.draw())
    println("END gc3");
  }
}

