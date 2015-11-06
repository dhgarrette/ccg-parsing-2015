//package dhg.ccg.parse.inf
//
//import scala.collection.immutable.BitSet
//import scala.collection.mutable.ArrayBuffer
//import scala.collection.mutable.{ BitSet => MBitSet }
//
//import org.apache.commons.math3.random.RandomGenerator
//
//import dhg.ccg.math._
//import dhg.ccg.parse._
//import dhg.ccg.parse.pcfg._
//import dhg.ccg.util._
//import dhg.util._
//import dhg.util.FastMathUtil._
//import scalaz.Scalaz._
//
///**
// */
//class SlicingInfTreeResamplerI(
//    //canCombine: Array[BitSet], //         Map[LeftCat, Set[RightCat]]
//    //binaryRules: Array[Vec[Int]], //      Map[LeftCat, Map[RightCat, ParentCat]]
//    unaryRules: Vec[BitSet], //            Map[ChildCat, Set[ParentCat]]
//    inferRight: Array[Array[BitSet]], //  Map[ParentCat, Map[LeftCat, Set[RightCat]]]
//    catPrior: Array[LogDouble], // InfCatPrior,  
//    rootSet: BitSet, // Set[Cat],
//    dirSampler: DirichletSampler,
//    qBetaA: LogDouble, qBetaB: LogDouble,
//    rand: RandomGenerator) //
//    //    extends InfTreeResamplerI 
//    {
//
//  type Word = Int
//  type Cat = Int
//  type CatSet = BitSet
//  type MCatSet = MBitSet
//  type CatMap[A] = Vec[A]
//  type OrderedCatMap[A] = OrderedIndirectSparseVec[A]
//
//  private[this] val catsDESCENDINGByPrior: Array[Int] = catPrior.zipWithIndex.sortBy(_._1).map(_._2)
//
//  private[this] val catOrdering = CfgGuideChartI.makeCatOrdering(unaryRules).on[(Cat, ArrayBuffer[(GuideChartEntryI, LogDouble)])](_._1)
//
//  //  def resampleTrees(
//  //    currentTrees: Vector[CcgTreeI], // ParVector[CcgTree],    
//  //    knownBinyProdProbs: CatMap[CatMap[CatMap[LogDouble]]], // Map[Cat, Map[BinaryLeft, Map[BinaryRight, LogDouble]]], // Map[Cat, Map[BinaryProd, LogDouble]],
//  //    binyScaleFactors: CatMap[LogDouble], // \theta^{obsv}_t(z) / sum_{u' \in U_t}(\theta^0_t(u'))  ;   should be present for exactly the known cats (those in `knownBinyProdProbs.keys`) 
//  //    unaryProbs: CatMap[CatMap[LogDouble]], // Map[Cat, Map[SubCat, LogDouble]] // has prior means as defaults
//  //    termProbs: CatMap[Array[LogDouble]], // Map[Cat, Map[Word, LogDouble]] // has prior means as defaults
//  //    prodMixes: CatMap[(LogDouble, LogDouble, LogDouble)], // has prior means as defaults
//  //    rootDist: CatMap[LogDouble], // LogProbabilityDistribution[Cat],
//  //    sentences: Vector[Array[Word]],
//  //    annotations: Vector[Option[FudgSentence]]): Vector[CcgTreeI] = {
//  //
//  //    zipSafe(sentences, currentTrees, annotations).mapt { (sentence, currentTree, fudgAnnotation) =>
//  //      val brackets = fudgAnnotation.map(_.brackets.toVector).getOrElse(Vector.empty)
//  //      val invalidSpans: Set[(Int, Int)] = Set.empty
//  //      resampleTree(sentence, currentTree, brackets, invalidSpans, knownBinyProdProbs, binyScaleFactors, unaryProbs, termProbs, prodMixes, rootDist)
//  //    }
//  //  }
//
//  def resampleTree(
//    sentence: Array[Word],
//    currentTree: CcgTreeI,
//    brackets: Vector[(Int, Int)], invalidSpans: Set[(Int, Int)], // fudgAnnotation: Option[FudgSentence],
//    knownBinyProdProbs: CatMap[CatMap[CatMap[LogDouble]]], // Map[Cat, Map[BinaryLeft, Map[BinaryRight, LogDouble]]], // Map[Cat, Map[BinaryProd, LogDouble]],
//    binyScaleFactors: CatMap[LogDouble], // \theta^{obsv}_t(z) / sum_{u' \in U_t}(\theta^0_t(u'))  ;   should be present for exactly the known cats (those in `knownBinyProdProbs.keys`) 
//    unaryProbs: CatMap[CatMap[LogDouble]], // Map[Cat, Map[SubCat, LogDouble]] // has prior means as defaults
//    termProbs: CatMap[Array[LogDouble]], // Map[Cat, Map[Word, LogDouble]] // has prior means as defaults
//    prodMixes: CatMap[(LogDouble, LogDouble, LogDouble)], // has prior means as defaults
//    rootDist: CatMap[LogDouble]) = {
//
//    val Some(guideChart) = constructChart(sentence, currentTree, brackets, invalidSpans, knownBinyProdProbs, binyScaleFactors, unaryProbs, termProbs, prodMixes)
//    val insideChart = buildInsideChart(guideChart)
//    sample(guideChart, insideChart, rootDist)
//  }
//
//  def constructChart(
//    sentence: Array[Word],
//    currentTree: CcgTreeI,
//    brackets: Vector[(Int, Int)], invalidSpans: Set[(Int, Int)], // fudgAnnotation: Option[FudgSentence],
//    knownBinyProdProbs: CatMap[CatMap[CatMap[LogDouble]]], // Map[Cat, Map[BinaryLeft, Map[BinaryRight, LogDouble]]], // Map[Cat, Map[BinaryProd, LogDouble]],
//    binyScaleFactors: CatMap[LogDouble], // \theta^{obsv}_t(z) / sum_{u' \in U_t}(\theta^0_t(u'))  ;   should be present for exactly the known cats (those in `knownBinyProdProbs.keys`) 
//    //    knownBinyProdProbs: CatMap[( //
//    //        CatMap[CatMap[LogDouble]], // Map[Cat, Map[BinaryLeft, Map[BinaryRight, LogDouble]]],
//    //        LogDouble)], // scale factor:  
//    unaryProbs: CatMap[CatMap[LogDouble]], // Map[Cat, Map[SubCat, LogDouble]] // has prior means as defaults
//    termProbs: CatMap[Array[LogDouble]], // Map[Cat, Map[Word, LogDouble]] // has prior means as defaults
//    prodMixes: CatMap[(LogDouble, LogDouble, LogDouble)] // has prior means as defaults
//    ) = {
//    val n = currentTree.length
//    val T: Chart[MCatSet] = Chart.fill(n)(MBitSet.empty)
//
//    val R: Array[Array[Option[ArrayBuffer[(Cat, ArrayBuffer[(GuideChartEntryI, LogDouble)])]]]] = newTable(n, brackets, invalidSpans)
//
//    //val ySpans = CcgTreeUtil.getSpans(currentTree)
//    val (y, yks) = treeBinaryCatTable(currentTree)
//
//    T(0, n) ++= rootSet
//
//    for {
//      span <- n downto 1
//      i <- 0 to n - span
//      j = i + span
//      Rij <- R(i)(j)
//    } {
//      val Tij: MCatSet = T(i, j)
//      val Tvisited: MCatSet = MBitSet.empty
//      while ((Tij -- Tvisited).nonEmpty) {
//        val t = (Tij -- Tvisited).head
//        Tvisited += t
//        val (binyProdMix, unryProdMix, termProdMix) = prodMixes(t)
//
//        // Build a set of entries (with probs) that `t` can produce in this cell
//        val entries = ArrayBuffer.empty[(GuideChartEntryI, LogDouble)]
//
//        // follow any relevant unary rules
//        for (us <- unaryProbs.get(t); (u, up) <- us.activePairs) {
//          val p = up * unryProdMix
//          entries += (UnaryGuideChartEntryI(u) -> p)
//          Tij += u
//        }
//
//        if (span > 1) { // non-terminal cell (can binary-branch)
//
//          // sample a threshold q for binary productions
//          val q: LogDouble =
//            if (y(i, j).isDefined) {
//              val k = yks(i, j).get // split point from (i,j) in the current tree
//              val yij = y(i, j).get
//              val yik = y(i, k).get
//              val ykj = y(k, j).get
//              val p = knownBinyProdProbs(yij)(yik)(ykj) * prodMixes(yij)._1
//              LogDouble(rand.nextDouble()) * p
//            }
//            else {
//              dirSampler.logBeta(qBetaA, qBetaB)
//            }
//
//          // Observed categories (directly sampled probabilities)
//          for {
//            us <- knownBinyProdProbs.get(t)
//            (u, vs) <- us.activePairs
//            (v, uvp) <- vs.activePairs
//            p = uvp * binyProdMix
//            if p > q
//            k <- i + 1 to j - 1
//          } {
//            entries += (BinaryGuideChartEntryI(k, u, v) -> p)
//            T(i, k) += u
//            T(k, j) += v
//          }
//          //println(f"        observed categories: $knownProdsToUse")
//
//          // Unobserved categories
//          val binaryScaleFactor = binyScaleFactors.getOrElse(t, LogDouble.one) * binyProdMix
//          var ui = 0
//          val uLen = catsDESCENDINGByPrior.length
//          var uDone = false
//          while (!uDone && ui < uLen) {
//            val u = catsDESCENDINGByPrior(ui)
//            val uProdPrior = catPrior(u)
//            val up = uProdPrior * binaryScaleFactor
//            if (up > q) {
//              val vs = inferRight(t)(u)
//              val knownVs = for { us <- knownBinyProdProbs.get(t).iterator; vs <- us.get(u).iterator; v <- vs.activeKeys } yield v
//              val unknownVs = (vs -- knownVs).iterator
//              while (unknownVs.hasNext) {
//                val v = unknownVs.next
//                val vProdPrior = catPrior(v)
//                val p = up * vProdPrior
//                if (p > q) {
//                  var k = i + 1
//                  while (k < j - 1) {
//                    entries += (BinaryGuideChartEntryI(k, u, v) -> p)
//                    T(i, k) += u
//                    T(k, j) += v
//                    k += 1
//                  }
//                }
//              }
//            }
//            else { uDone = true }
//            ui += 1
//          }
//        }
//        else {
//          // Add entries for terminal productions
//          val w = sentence(i)
//          val wp = termProbs(t)(w)
//          val p = wp * termProdMix
//          entries += (TermGuideChartEntryI(w) -> p)
//        }
//
//        if (entries.nonEmpty)
//          Rij += (t -> entries)
//      }
//    }
//
//    if (R(0)(n).get.nonEmpty) { // there's at least one parse
//        rootSet?
//      val result = toCfgGuideChart(R)
//
//      //      // DEBUG START
//      //      //result.draw()
//      //      for {
//      //        (i, j, cell) <- result.bottomUpNodes
//      //        (ij, entries) <- cell
//      //        entry <- entries
//      //      } entry match {
//      //        case BinaryGuideChartEntryI(k, BinaryProdI(ik, kj)) =>
//      //          assert(result(i)(k).contains(ik), f"($i,$j)($ij): $k[$ik,$kj];  result($i,$k) doesn't contain $ik;  result($i,$k)=${result(i)(k).map(x => "  " + x.toString).mkString("\n", "\n", "")}")
//      //          assert(result(k)(j).contains(kj), f"($i,$j)($ij): $k[$ik,$kj];  result($k,$j) doesn't contain $kj;  result($k,$j)=${result(k)(j).map(x => "  " + x.toString).mkString("\n", "\n", "")}")
//      //          assert(rules.collect { case r: BinaryCcgRule => r(ik, kj) }.flatten.contains(ij), f"invalid rule!  ($i,$j)($ij -> $k[$ik,$kj]")
//      //        case UnaryGuideChartEntryI(UnaryProdI(sub)) =>
//      //          assert(resulT(i, j).contains(sub), f"($i,$j)($ij): [$sub];  result($i,$j) doesn't contain $sub;  result($i,$j)=${resulT(i, j).map(x => "  " + x.toString).mkString("\n", "\n", "")}")
//      //          assert(rules.collect { case r: UnaryCcgRule => r(sub) }.flatten.contains(ij), f"invalid rule!  ($i,$j)($ij -> [$sub]")
//      //        case TermGuideChartEntryI(TermProdI(word)) =>
//      //
//      //      }
//      //      // DEBUG END
//
//      //result.draw()
//      Some(result)
//    }
//    else None
//  }
//
//  def toCfgGuideChart(table: Array[Array[Option[ArrayBuffer[(Cat, ArrayBuffer[(GuideChartEntryI, LogDouble)])]]]]) = {
//    table.map { row =>
//      row.map { cell =>
//        cell.fold(OrderedIndirectSparseVec.empty[ArrayBuffer[(GuideChartEntryI, LogDouble)]]) { contents =>
//          val (keys, values) = contents.toArray.sorted(catOrdering).unzip
//          OrderedIndirectSparseVec(keys, values)
//        }
//      }
//    }
//  }
//
//  /**
//   * @param gc  For each cell, a traversal-ordered (by unary constraints) list
//   *            of categories, along with productions from them, and probabilities
//   *            of those productions.
//   */
//  def buildInsideChart(
//    gc: Array[Array[OrderedCatMap[ArrayBuffer[(GuideChartEntryI, LogDouble)]]]]) = {
//
//    val n = gc.length
//    val inside: Chart[CatMap[LogDouble]] = Chart.fillVal(n)(IndirectSparseVec.empty[LogDouble])
//
//    for {
//      span <- 1 to n //        span size
//      i <- 0 to (n - span) //  start of span
//    } {
//      val j = i + span //      end of span
//      val cell = gc(i)(j).activePairs
//      val cellLen = cell.length
//      val newEntries = new Array[LogDouble](cellLen)
//      var celli = 0
//      while (celli < cellLen) {
//        val (ij, entries) = cell(celli)
//        val entriesLen = entries.length
//        val pValues = new Array[Double](entriesLen)
//        var entriesi = 0
//        while (entriesi < entriesLen) {
//          entries(entriesi) match {
//            case (BinaryGuideChartEntryI(k, ik, kj), prodP) =>
//              val ikP = inside(i, k)(ik)
//              val kjP = inside(k, j)(kj)
//              val p = prodP * ikP * kjP
//              pValues(entriesi) = p.logValue
//
//            case (UnaryGuideChartEntryI(sub), prodP) =>
//              val subP = inside(i, j)(sub)
//              val p = prodP * subP
//              pValues(entriesi) = p.logValue
//
//            case (TermGuideChartEntryI(word), prodP) =>
//              val p = prodP
//              pValues(entriesi) = p.logValue
//          }
//          entriesi += 1
//        }
//        val pSum = new LogDouble(logSum(pValues, entriesi))
//        newEntries(celli) = pSum
//        celli += 1
//      }
//      val keys = cell.map(_._1)
//      inside(i, j) = new IndirectSparseVec(keys, newEntries, n)
//    }
//
//    inside
//  }
//
//  def sample(
//    guideChart: Array[Array[OrderedCatMap[ArrayBuffer[(GuideChartEntryI, LogDouble)]]]],
//    insideChart: Chart[CatMap[LogDouble]],
//    rootDist: CatMap[LogDouble]): CcgTreeI = {
//
//    val n = guideChart.length
//
//    val cellij = insideChart(0, n)
//    val rootCats = cellij.activeKeys
//    val logWeightedRootProds = cellij.activePairs.map { case (ij, ijP) => (rootDist(ij) * ijP).logValue }
//    val ij = rootCats(logChoose(logWeightedRootProds, rootCats.length, rand))
//
//    sampleRecursivelyWithoutTable(ij, 0, n, guideChart, insideChart)
//  }
//
//  private[this] def sampleRecursivelyWithoutTable(ij: Cat, i: Int, j: Int,
//    guideChart: Array[Array[OrderedCatMap[ArrayBuffer[(GuideChartEntryI, LogDouble)]]]],
//    insideChart: Chart[CatMap[LogDouble]]): CcgTreeI = {
//
//    val gce = sampleGce(i, j, ij, guideChart(i)(j)(ij), insideChart)
//    gce match {
//      case BinaryGuideChartEntryI(k, ik, kj) =>
//        CcgBinodeI(ij,
//          sampleRecursivelyWithoutTable(ik, i, k, guideChart, insideChart),
//          sampleRecursivelyWithoutTable(kj, k, j, guideChart, insideChart))
//      case UnaryGuideChartEntryI(subCat) =>
//        CcgUnodeI(ij, sampleRecursivelyWithoutTable(subCat, i, j, guideChart, insideChart))
//      case TermGuideChartEntryI(word) =>
//        CcgLeafI(ij, word)
//    }
//  }
//
//  private[this] def sampleGce(
//    i: Int, j: Int, ij: Cat,
//    entries: ArrayBuffer[(GuideChartEntryI, LogDouble)],
//    insideChart: Chart[CatMap[LogDouble]]): GuideChartEntryI = {
//
//    val entriesLen = entries.length
//    //val entriesCats = new Array[Cat](entriesLen)
//    val logProdProbs = new Array[Double](entriesLen)
//    var entriesi = 0
//    while (entriesi < entriesLen) {
//      entries(entriesi) match {
//        case (BinaryGuideChartEntryI(k, ik, kj), prodP) =>
//          val ikP = insideChart(i, k)(ik)
//          val kjP = insideChart(k, j)(kj)
//          logProdProbs(entriesi) = (prodP * ikP * kjP).logValue
//
//        case (UnaryGuideChartEntryI(subCat), prodP) =>
//          val subP = insideChart(i, j)(subCat)
//          logProdProbs(entriesi) = (prodP * subP).logValue
//
//        case (_, prodP) => // TerminalGuideChartEntryI(word)
//          logProdProbs(entriesi) = prodP.logValue
//      }
//      entriesi += 1
//    }
//
//    val gce = entries(logChoose(logProdProbs, entriesLen, rand))._1
//    gce
//  }
//
//  def newTable[A](n: Int, brackets: Vector[(Int, Int)], invalidSpans: Set[(Int, Int)]): Array[Array[Option[ArrayBuffer[A]]]] = {
//    val table: Array[Array[Option[ArrayBuffer[A]]]] =
//      Array.tabulate(n) { i =>
//        Array.tabulate(n + 1) { j =>
//          val itoj = (i + 1 to j)
//          if (i >= j || invalidSpans(i -> j) ||
//            brackets.exists { // if there exists a cross-bracketing
//              case (a, b) =>
//                assert(a < b, f"improper bracket: ($a,$b)")
//                (a < i && i < b && b < j) ||
//                  (i < a && a < j && j < b)
//            }) None
//          else Some(ArrayBuffer.empty[A])
//        }
//      }
//    table
//  }
//
//  /**
//   * Make a table of the categories in the tree that are part of
//   * *binary* productions (either at the top or bottom).
//   */
//  def treeBinaryCatTable(tree: CcgTreeI) = {
//    val n = tree.length
//    val ytable = Chart.fillVal(n)(none[Cat])
//    val ktable = Chart.fillVal(n)(none[Int])
//    def traverse(t: CcgTreeI, i: Int, j: Int): Unit = t match {
//      case CcgLeafI(cat, word) =>
//        assert(i + 1 == j)
//
//      case CcgUnodeI(cat, s) =>
//        assert(i + 1 < j)
//        traverse(s, i, j)
//
//      case CcgBinodeI(cat, l, r) =>
//        assert(i + 1 < j)
//        val k = i + l.length
//        ktable(i, j) = Some(k)
//
//        if (ytable(i, j).isDefined)
//          assert(ytable(i, j).get == cat)
//        else
//          ytable(i, j) = Some(cat)
//        ytable(i, k) = Some(l.cat)
//        ytable(k, j) = Some(r.cat)
//
//        traverse(l, i, k)
//        traverse(r, k, j)
//    }
//    traverse(tree, 0, n)
//    (ytable, ktable)
//  }
//
//}
