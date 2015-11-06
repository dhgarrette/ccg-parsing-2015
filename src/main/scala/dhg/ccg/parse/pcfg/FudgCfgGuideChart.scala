package dhg.ccg.parse.pcfg

import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import dhg.util._
import scalaz._
import scalaz.Scalaz._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.parse._
import dhg.ccg.tagdict.TagDictionary
import dhg.util.GraphUtil._
import dhg.ccg.util.DrawMatrix
import annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.immutable.ListMap
import scala.collection.immutable.VectorBuilder
import dhg.ccg.data._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.tagdict.SimpleStartEndTags
import dhg.ccg.tagdict.SimpleTagDictionaryFactory
import dhg.gfl.FudgSentence
import dhg.gfl._
import dhg.ccg.parse.dep.DepTree
import scala.collection.mutable.SetBuilder
import dhg.ccg.tagdict.DummyCatTagDictionary

class SimpleFudgCfgGuideChartBuilder(
  val rules: Vector[CcgRule],
  additionalSupertagAdder: AdditionalTagAdder[Cat] = new StandardTagDictAdditionalTagAdder[Cat](),
  rootSet: Set[Cat] = UniversalSet(),
  allowTerminalDeletion: Boolean)
    extends CfgGuideChartBuilder {

  require(allowTerminalDeletion == false)

  private[this] val unaryRules = rules.collect { case r: UnaryCcgRule => r }
  private[this] val binaryRules = rules.collect { case r: BinaryCcgRule => r }

  private[this]type MTable = Array[Array[Option[MMap[Cat, (MSet[GuideChartEntry], MSet[DepTree])]]]]

  def buildFromSupertagSetSentence(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[CfgGuideChart] = {
    buildWithDepTreesFromSupertagSetSentence(supertagSetSentence, fudgAnnotation, tagdict).map(_._1)
  }

  def buildWithDepTreesFromSupertagSetSentence(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence], tagdict: TagDictionary[Cat]): Option[(CfgGuideChart, Set[DepTree])] = {
    val (sentence, sentenceExistingTags) = supertagSetSentence.unzip
    val sentenceTags = additionalSupertagAdder(sentence, sentenceExistingTags, tagdict) // get supertags set for each word 
    doBuild(sentence, sentenceTags, sentence.length, fudgAnnotation)
  }

  def buildUnpruned(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence] = None, tagdict: TagDictionary[Cat] = DummyCatTagDictionary): CfgGuideChart = ???

  /**
   * Go bottom-up with CKY to fill the chart with potential node entries
   *
   * Brackets are given as start/end index pairs, with indexing as:
   *         the   old   man   the   sea    .
   *       0     1     2     3     4     5     6
   * so that bracket (1,3) would cover "old man".
   */
  private[this] def doBuild(sentence: Vector[Word], sentenceTags: Vector[Set[Cat]], n: Int, fudgAnnotation: Option[FudgSentence]): Option[(CfgGuideChart, Set[DepTree])] = {
    val brackets: Vector[(Int, Int)] = fudgAnnotation.map(_.brackets.toVector).getOrElse(Vector.empty)
    val invalidSpans: Set[(Int, Int)] = Set.empty
    val table: MTable = newTable(n, brackets, invalidSpans)
    //new CfgGuideChart(table.toVector.map { row => row.toVector.map { cell => if (cell.isDefined) Map(cat"X" -> Set.empty[GuideChartEntry]) else Map.empty[Cat, Set[GuideChartEntry]] } }).draw()

    fillTerminalLevel(table, sentence, sentenceTags)
    //toCfgGuideChart(sentence, table).draw()

    fillHigherLevels(table, n, fudgAnnotation)
    //toCfgGuideChart(table).draw()

    if (table(0)(n).get.filterKeys(rootSet).values.exists(_._1.nonEmpty)) { // there's at least one parse
      val clean = makeCleanChart(table, n, brackets, invalidSpans)
      val result = toCfgGuideChart(sentence, clean, rootSet)

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

      val allDepTrees = table(0)(n).get.filterKeys(rootSet).values.map(_._2).flatten.toSet

      //result.draw()
      Some(result, allDepTrees)
    }
    else None
  }

  /**
   * Add supertags to bottom level of the chart
   */
  private[this] def fillTerminalLevel(table: MTable, sentence: Vector[Word], sentenceTags: Vector[Set[Cat]]): Unit = {
    for (((word, tags), i) <- (sentence zipSafe sentenceTags).zipWithIndex) {
      val cellij = table(i)(i + 1).get // lexical cell must exist
      val (entries, depTreeMap) = repeatedlyApplyUnaryRules(tags.mapToVal(TermGuideChartEntry(TermProd(word))), tags.mapTo(t => Set(DepTree(word, i, t, Vector.empty))).toMap)
      for ((ij, entry) <- entries; depTrees <- depTreeMap.get(ij) if depTrees.nonEmpty) {
        // cell : Map[Cat, (Set[GuideChartEntry], Set[DepTree])]
        val (ijEntries, ijDeptrees) = cellij.getOrElseUpdate(ij, (MSet.empty, MSet.empty))
        ijEntries += entry
        ijDeptrees ++= depTrees
      }
    }
  }

  /**
   * Apply rules to add higher-level chart entries
   */
  private[this] def fillHigherLevels(table: MTable, n: Int, fudgAnnotation: Option[FudgSentence]): Unit = {
    for {
      j <- 2 to n //             end of span
      i <- j - 2 downto 0 //     start of span
      cellij <- table(i)(j) //   search stops here if this span is unavailable (due to known bracketing)
    } {
      val (entriesWithoutUnary, depTreeMapWithoutUnary) = higherLevelEntries(i, j, table, fudgAnnotation)
      val (entries, depTreeMap) = repeatedlyApplyUnaryRules(entriesWithoutUnary.toSet, depTreeMapWithoutUnary)
      for ((ij, entry) <- entries; depTrees <- depTreeMap.get(ij) if depTrees.nonEmpty) {
        // cellij : Map[Cat, (Set[GuideChartEntry], Set[DepTree])]
        val (ijEntries, ijDeptrees) = cellij.getOrElseUpdate(ij, (MSet.empty, MSet.empty))
        ijEntries += entry
        ijDeptrees ++= depTrees
      }
    }
  }

  //private[this] def higherLevelEntries(i: Int, j: Int, table: MTable, fudgAnnotation: Option[FudgSentence]): Vector[(Cat, (BinaryGuideChartEntry, Set[DepTree]))] = {
  private[this] def higherLevelEntries(i: Int, j: Int, table: MTable, fudgAnnotation: Option[FudgSentence]): (Vector[(Cat, BinaryGuideChartEntry)], Map[Cat, Set[DepTree]]) = {
    val entriesBuilder = new VectorBuilder[(Cat, BinaryGuideChartEntry)]()
    val deptreesBuilder = new VectorBuilder[(Cat, DepTree)]()
    for {
      k <- i + 1 until j //      split of span
      cellik <- table(i)(k) //   search stops here if this span is unavailable (due to known bracketing)
      cellkj <- table(k)(j) //   search stops here if this span is unavailable (due to known bracketing)
      (ik, (_, ikDepTrees)) <- cellik
      (kj, (_, kjDepTrees)) <- cellkj
      rule <- binaryRules
      ij <- rule(ik, kj)
    } {
      val depTrees =
        for {
          ikt <- ikDepTrees
          kjt <- kjDepTrees
          t = DepTree.joinToBinary(ij, ik, kj, ikt, kjt)
          if fudgAnnotation.forall(consistent(t, _))
        } yield t
      if (depTrees.nonEmpty) {
        entriesBuilder += (ij -> BinaryGuideChartEntry(k, BinaryProd(ik, kj)))
        deptreesBuilder ++= depTrees.map(ij -> _)
      }
    }
    (entriesBuilder.result, deptreesBuilder.result.groupByKey.mapVals(_.toSet))
  }

  private[this] def consistent(depTree: DepTree, gfl: FudgSentence): Boolean = {
    // DepTree(word,index,children)
    // Edge(parent: Node, child: Node, label: Option[String])
    def getEdges(t: DepTree): Vector[Edge] = {
      val node = gfl.node(t.index)
      t.children.flatMap(c => Edge(node, gfl.node(c.index), None) +: getEdges(c))
    }
    Fudg.isSemanticallyValid(gfl.edges ++ getEdges(depTree))
  }

  type ProdSet = Set[(Cat, GuideChartEntry)]
  type DepTreeMap = Map[Cat, Set[DepTree]]
  private[this] def repeatedlyApplyUnaryRules(prods: ProdSet, depTreeMap: DepTreeMap): (ProdSet, DepTreeMap) = {
    @tailrec def inner(prods: ProdSet, depTreeMap: DepTreeMap, accum: ProdSet): (ProdSet, DepTreeMap) = {
      val newProdAndTrees = for {
        (cat, prod) <- prods
        depTrees <- depTreeMap.get(cat).toSeq if depTrees.nonEmpty
        r <- unaryRules
        newCat <- r(cat)
        newProd = newCat -> (UnaryGuideChartEntry(UnaryProd(cat)): GuideChartEntry)
        if !prods.contains(newProd)
      } yield (newProd, newCat -> depTrees)
      if (newProdAndTrees.isEmpty) (prods | accum, depTreeMap)
      else {
        val (newProds, newTrees) = newProdAndTrees.unzip
        inner(newProds, depTreeMap |+| newTrees.groupByKey.mapValues(_.flatten), prods | accum)
      }
    }
    inner(prods, depTreeMap, Set.empty)
  }

  /**
   * Make a clean chart so we can put only useful entries (those that
   * can actually be used in a parse).  Go top-down following paths that
   * start with a root node.
   */
  private[this] def makeCleanChart(table: MTable, n: Int, brackets: Vector[(Int, Int)], invalidSpans: Set[(Int, Int)]) = {
    val cleanCfgTable: MTable = newTable(n, brackets, invalidSpans)
    for { (ij, ijSet) <- table(0)(n).get if rootSet(ij) } {
      cleanCfgTable(0)(n).get(ij) = ijSet
    }
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
  @tailrec private[this] def makeCleanCellRecursively(i: Int, j: Int, cleanCfgTable: MTable, table: MTable): Unit = {
    for {
      cleanCellij <- cleanCfgTable(i)(j)
      (ij, ijSet) <- cleanCellij
    } {
      for (BinaryGuideChartEntry(k, BinaryProd(ik, kj)) <- ijSet._1) {
        cleanCfgTable(i)(k).get(ik) = table(i)(k).get(ik)
        cleanCfgTable(k)(j).get(kj) = table(k)(j).get(kj)
      }
      //  No longer do this because we dont want to change the list as we are iterating over it
      //      for (UnaryGuideChartEntry(UnaryProd(sub)) <- ijSet) {
      //        cleanCfgTable(i)(j).get(sub) = table(i)(j).get(sub)
      //      }
    }

    val newEntries =
      for {
        cleanCellij <- cleanCfgTable(i)(j).toSet
        (ij, ijSet) <- cleanCellij
        UnaryGuideChartEntry(UnaryProd(sub)) <- ijSet._1 // find unary rules
        if !cleanCellij.contains(sub) //                    that add additional categories
      } yield sub

    /*
     * If there are new entries to be handled
     */
    if (newEntries.nonEmpty) {
      for (sub <- newEntries) {
        cleanCfgTable(i)(j).get(sub) = table(i)(j).get(sub)
      }
      makeCleanCellRecursively(i, j, cleanCfgTable, table)
    }
  }

  private[this] def toCfgGuideChart(words: Vector[Word], table: MTable, rootSet: Set[Cat]): CfgGuideChart = {
    val allCats = table.flatten.flatten.flatten.map(_._1).toSet
    val catOrdering = makeCatOrdering(allCats)
    val ord = catOrdering.on[(Cat, Set[GuideChartEntry])](_._1)
    new CfgGuideChart(
      words,
      table.toVector.map { row =>
        row.toVector.map { cell =>
          cell.fold(Map.empty[Cat, Set[GuideChartEntry]]) { contents =>
            ListMap.empty ++ contents.mapValues(_._1.to[Set]).toVector.sorted(ord)
          }
        }
      }, rootSet)
  }

  /**
   * Order the categories for iterating such that for any unary rule that
   * rewrites A as B, A is visited before B.
   */
  private[this] def makeCatOrdering(allCats: Set[Cat]) = {
    val allLinks = allCats.flatMap(c => unaryRules.flatMap(r => r(c).map(c -> _)))
    val catOrdering = toDagOrder(allLinks)
    val orderMap = catOrdering.zipWithIndex.toMap.withDefaultValue(-1) // categories uninvolved in the ordering are handled first, as a block
    scala.math.Ordering.by[Cat, Int](orderMap)
  }

  private[this] def newTable[A, B](n: Int, brackets: Vector[(Int, Int)], invalidSpans: Set[(Int, Int)]): Array[Array[Option[MMap[A, B]]]] = {
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

  override def toString = f"SimpleFudgCfgGuideChartBuilder(${rules.mkString("[", ",", "]").replace("N2NP,TR1,TR2,TR3,RRC1,RRC2,RRC3,RRC4,RRC5,RRC6,VPSM1,VPSM2,VPSM3", "UNARY")}, ${additionalSupertagAdder})))"
}

object FudgDecisionCfgGuideChartBuilder {
  type Word = String
  def apply(
    rules: Vector[CcgRule],
    additionalSupertagAdder: AdditionalTagAdder[Cat] = new StandardTagDictAdditionalTagAdder[Cat](),
    allowTerminalDeletion: Boolean): CfgGuideChartBuilder = {
    new DecisionCfgGuideChartBuilder(
      new SimpleFudgCfgGuideChartBuilder(rules, additionalSupertagAdder, allowTerminalDeletion = allowTerminalDeletion),
      new SimpleCfgGuideChartBuilder(rules, additionalSupertagAdder, allowTerminalDeletion = allowTerminalDeletion),
      useA _)
  }
  private[this] def useA(supertagSetSentence: Vector[(Word, Set[Cat])], fudgAnnotation: Option[FudgSentence]): Boolean = {
    fudgAnnotation.exists(f => f.edges.exists { case Edge(parent @ FeNode(_), child, label) => false; case _ => true })
  }
}
