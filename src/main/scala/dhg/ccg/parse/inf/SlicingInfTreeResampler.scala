package dhg.ccg.parse.inf

import scala.annotation.tailrec
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import scala.collection.breakOut
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.prob._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.tagdict.TagDictionary
import dhg.util._
import scalaz._
import Scalaz._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import org.apache.commons.math3.random.RandomGenerator
import dhg.ccg.math._
import dhg.ccg.math.Util._
import dhg.ccg.tagdict.StartEndTags
import dhg.ccg.tagdict.SimpleStartEndTags
import scala.collection.parallel.immutable.ParVector
import dhg.ccg.math.DirichletSampler
import dhg.ccg.parse.dep.ParserEvaluator
import dhg.gfl.FudgSentence
import dhg.ccg.math.SimpleDirichletSampler
import breeze.stats.distributions.Rand
import scala.util.control.Breaks._
import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.random.SynchronizedRandomGenerator
import dhg.util.viz._
import scala.collection.mutable.ArrayBuffer
import dhg.util.GraphUtil
import dhg.ccg.util.DrawMatrix

/**
 */
class SlicingInfTreeResampler(
    binaryRules: Set[BinaryCcgRule],
    unaryRules: Set[UnaryCcgRule],
    catPrior: InfCatPrior,
    rootSet: Set[Cat],
    dirSampler: DirichletSampler,
    qBetaA: LogDouble, qBetaB: LogDouble,
    rand: RandomGenerator,
    fullAllowedCatSet: Set[Cat] = UniversalSet()) //extends InfTreeResampler 
    {
  type Word = String

  private[this] val unaryProductions: Map[Cat, Set[Cat]] = unaryRules.map(r => r.parent -> r.child).groupByKey.withDefaultValue(Set.empty)
  private[this] val allUnaryChildren: Set[Cat] = unaryProductions.flatMap(_._2).toSet

  private[this] val catOrdering = makeCatOrdering(unaryProductions).on[(Cat, ArrayBuffer[(GuideChartEntry, LogDouble)])](_._1)

  def resampleTree(
    sentence: Vector[Word], currentTree: CcgTree,
    brackets: Vector[(Int, Int)], invalidSpans: Set[(Int, Int)], // fudgAnnotation: Option[FudgSentence],
    knownBinyProds: Map[Cat, (Map[BinaryProd, LogDouble], LogDouble)],
    unaryDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd],
    termDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
    prodMixes: Map[Cat, (LogDouble, LogDouble, LogDouble)],
    infCatPriorsDesc: Vector[(Cat, LogDouble)],
    rootDist: LogProbabilityDistribution[Cat]) = {

    val Some(guideChart) =
      //      time("construct inf guide chart", 
      constructChart(sentence, currentTree, brackets, invalidSpans, knownBinyProds, unaryDist, termDist, prodMixes, infCatPriorsDesc)
    //          )
    //DrawMatrix.drawMatrix(guideChart) { x => x.map(_._1).mkString(", ").wrap(30) }
    //println("Guide Chart:::")
    //DrawMatrix.drawMatrix(guideChart) { x => x.getOrElse(ArrayBuffer.empty).flatMap { case (c, ps) => ps.map(c -> _) }.map { case (c, (prod, p)) => f"$c -> [$prod]  (${p.logValue}%.2f)" }.mkString("\n") }
    val (insideChart, cleanGuideChart) =
      //      time("compute inf inside chart", 
      buildInsideChart(guideChart)
    //          )
    //println("Clean Chart:::")
    //DrawMatrix.drawMatrix(cleanGuideChart) { x => x.getOrElse(ArrayBuffer.empty).flatMap { case (c, ps) => ps.map(c -> _) }.map { case (c, (prod, p)) => f"$c -> [$prod]  (${p.logValue}%.2f)" }.mkString("\n") }
    //println("Inside Chart:::")
    //DrawMatrix.drawMatrix(insideChart) { x => x.map { case (c, p) => f"$c -> ${p.logValue}%.2f" }.mkString("\n") }
    val newTree =
      //    time("sample tree from inf chart", 
      sample(cleanGuideChart, insideChart, rootDist)
    //        )
    newTree
  }

  def constructChart(
    sentence: Vector[Word], currentTree: CcgTree,
    brackets: Vector[(Int, Int)], invalidSpans: Set[(Int, Int)], // fudgAnnotation: Option[FudgSentence],
    knownBinyProds: Map[Cat, (Map[BinaryProd, LogDouble], LogDouble)],
    unaryDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd],
    termDist: ConditionalLogProbabilityDistribution[Cat, TermProd],
    prodMixes: Map[Cat, (LogDouble, LogDouble, LogDouble)],
    infCatPriorsDesc: Vector[(Cat, LogDouble)]) = {
    val n = currentTree.length
    val T: Chart[MSet[Cat]] = Chart.fill(n)(MSet.empty)

    val R: Chart[Option[ArrayBuffer[(Cat, ArrayBuffer[(GuideChartEntry, LogDouble)])]]] = newTable(n, brackets, invalidSpans)

    //val ySpans = CcgTreeUtil.getSpans(currentTree)
    val yBinyProds = getBinaryProds(currentTree, 0, n)

    T(0, n) ++= rootSet

    for ((i, j, Some(rij)) <- R.topDownNodes) {
      //println(f"($i,$j, $rij)")
      val Tij = T(i, j)
      //println(f"  Tij.size = ${Tij.size}")

      @tailrec def repeatedlyAddUnary(cats: Set[Cat], recent: collection.Set[Cat]): Set[Cat] = {
        if (recent.nonEmpty) repeatedlyAddUnary(cats | recent, (for { t <- recent.iterator; us <- unaryProductions.get(t).iterator; u <- us if !cats.contains(u) } yield u).toSet)
        else (cats | recent)
      }

      for (t <- repeatedlyAddUnary(Set.empty, Tij)) {
        //println(f"  t=$t")
        val (binyProdMix, unryProdMix, termProdMix) = prodMixes(t)

        // Build a set of entries (with probs) that `t` can produce in this cell
        val entries = ArrayBuffer.empty[(GuideChartEntry, LogDouble)]

        // follow any relevant unary rules
        for (u <- unaryProductions(t)) {
          val prod = UnaryProd(u)
          val p = unaryDist(prod, t) * unryProdMix
          entries += (UnaryGuideChartEntry(prod) -> p)
        }

        if ((j - i) > 1) { // non-terminal cell (can binary-branch)

          // sample a threshold q for binary productions
          val q =
            yBinyProds.get(i, j).map {
              case (yij, k, yik, ykj) =>
                val yijBinyProdMix = prodMixes(yij)._1
                val yijKnownBinyProd = knownBinyProds(yij)._1
                val p = yijKnownBinyProd(BinaryProd(yik, ykj)) * yijBinyProdMix
                LogDouble(rand.nextDouble()) * p
            }.getOrElse {
              dirSampler.logBeta(qBetaA, qBetaB)
            }

          // Observed categories (directly sampled probabilities)
          for {
            probs <- knownBinyProds.get(t).map(_._1)
            (uvProd @ BinaryProd(u, v), uvp) <- probs
            p = uvp * binyProdMix
            if p > q
            k <- i + 1 to j - 1
          } {
            entries += (BinaryGuideChartEntry(k, uvProd) -> p)
            T(i, k) += u
            T(k, j) += v
          }
          //println(f"        observed categories: $knownProdsToUse")

          // Unobserved categories
          val binaryScaleFactor = knownBinyProds.get(t).fold(LogDouble.one)(_._2) * binyProdMix
          val uItr = infCatPriorsDesc.iterator
          breakable {
            while (uItr.hasNext) {
              val (u, uProdPrior) = uItr.next()
              val up = uProdPrior * binaryScaleFactor
              if (up > q) {
                for (v <- binaryRules.flatMap(_.inferRight(t, u)) if fullAllowedCatSet(v); vProdPrior = catPrior(v)) {
                  val uvProd = BinaryProd(u, v)
                  if (knownBinyProds.get(t).forall(!_._1.contains(uvProd))) {
                    val p = up * vProdPrior
                    if (p > q) {
                      if (!uItr.hasNext) println(f"OK THIS IS ACTUALLY BAD...  infCatPriorsDesc ran out!!  t=$t, infCatPriorsDesc.size=${infCatPriorsDesc.size}; last=${infCatPriorsDesc.last}, q=$q, p=$p")
                      for (k <- i + 1 to j - 1) {
                        entries += (BinaryGuideChartEntry(k, uvProd) -> p)
                        T(i, k) += u
                        T(k, j) += v
                      }
                    }
                  }
                }
              }
              else break
            }
          }
          //if (!uItr.hasNext) println(f"YIKES!  infCatPriorsDesc ran out!!  t=$t, infCatPriorsDesc.size=${infCatPriorsDesc.size}; last=${infCatPriorsDesc.last}, q=$q")
        }
        else {
          // Add entries for terminal productions
          val w = sentence(i)
          val prod = TermProd(w)
          val wp = termDist(prod, t)
          val p = wp * termProdMix
          if (p.isZero) println(f"UH... termDist([$w] | $t)=${wp.logValue}%.2f  *  termProdMix=$termProdMix")
          entries += (TermGuideChartEntry(prod) -> p)
        }

        if (entries.nonEmpty)
          rij += (t -> entries)
      }
    }

    if (R(0, n).get.filter { case (ij, entries) => rootSet(ij) && entries.nonEmpty }.nonEmpty) { // there's at least one parse
      val result = sortGuideChartEntries(R)

      //      // TODO: DEBUG START
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
      //      // DEBUG END

      //result.draw()
      Some(result)
    }
    else None
  }

  /**
   * @param gc  For each cell, a traversal-ordered (by unary constraints) list
   *            of categories, along with productions from them, and probabilities
   *            of those productions.
   */
  def buildInsideChart(
    gc: Chart[Option[ArrayBuffer[(Cat, ArrayBuffer[(GuideChartEntry, LogDouble)])]]]) = {

    val n = gc.length
    val inside: Chart[Map[Cat, LogDouble]] = Chart.fillVal(n)(Map.empty)
    val cleanGC: Chart[Option[ArrayBuffer[(Cat, ArrayBuffer[(GuideChartEntry, LogDouble)])]]] = Chart.fillVal(n)(None) // remove entries that can't be combined into a full tree

    for ((i, j, Some(cell)) <- gc.bottomUpNodes) {
      val newInsideEntries = ArrayBuffer.empty[(Cat, LogDouble)]
      val ijUnaryInsides = MMap.empty[Cat, LogDouble]
      val newGcEntries = ArrayBuffer.empty[(Cat, ArrayBuffer[(GuideChartEntry, LogDouble)])]

      for ((ij, entries) <- cell) {
        val pValues = ArrayBuffer.empty[LogDouble]
        val usedGcEntries = ArrayBuffer.empty[(GuideChartEntry, LogDouble)]
        for (gcEntry <- entries) {
          val (gce, prodP) = gcEntry
          gce match {
            case BinaryGuideChartEntry(k, prod @ BinaryProd(ik, kj)) =>
              for {
                ikP <- inside(i, k).get(ik) // skip things that were pruned from the chart
                kjP <- inside(k, j).get(kj)
              } {
                val p = prodP * ikP * kjP
                //println(f"v$i$j($ij) += p($ij => $prod) * v$i$k($ik) * v$k$j($kj) = ${prodP.toDouble} * ${ikP.toDouble} ${kjP.toDouble} = ${p.toDouble}")
                pValues += p
                usedGcEntries += gcEntry
                if (allUnaryChildren(ij)) ijUnaryInsides.updateOrElseWith(ij, LogDouble.zero) { _ + p }
              }

            case UnaryGuideChartEntry(prod @ UnaryProd(sub)) =>
              for {
                subP <- ijUnaryInsides.get(sub) // newInsideEntries.find(_._1 == sub).map(_._2) // inside(i, j).get(sub)
              } {
                val p = prodP * subP
                //println(f"v$i$j($ij) += p($ij => $sub) * v$i$j($sub) = ${prodP.toDouble} * ${subP.toDouble} = ${p.toDouble}")
                pValues += p
                usedGcEntries += gcEntry
                if (allUnaryChildren(ij)) ijUnaryInsides.updateOrElseWith(ij, LogDouble.zero) { _ + p }
              }

            case TermGuideChartEntry(prod @ TermProd(word)) =>
              val p = prodP
              //println(f"v$i$j($ij) += p($word|$ij) = ${p.toDouble}")
              pValues += p
              usedGcEntries += gcEntry
              if (allUnaryChildren(ij)) ijUnaryInsides.updateOrElseWith(ij, LogDouble.zero) { _ + p }
          }
        }

        if (pValues.nonEmpty) {
          newInsideEntries += (ij -> LogDouble.sum(pValues))
          newGcEntries += (ij -> usedGcEntries)
        }
      }

      inside(i, j) = newInsideEntries.toMap
      cleanGC(i, j) = Some(newGcEntries)
    }

    (inside, cleanGC)
  }

  def sample(
    cleanGuideChart: Chart[Option[ArrayBuffer[(Cat, ArrayBuffer[(GuideChartEntry, LogDouble)])]]],
    insideChart: Chart[Map[Cat, LogDouble]],
    rootDist: LogProbabilityDistribution[Cat]): CcgTree = {

    val n = cleanGuideChart.length

    val weightedRootProds = insideChart(0, n).mapt((ij, ijP) => ij -> (rootDist(ij) * ijP))
    val ij = new SimpleLogProbabilityDistribution(weightedRootProds).sample()
    sampleRecursivelyWithoutTable(ij, 0, n, cleanGuideChart, insideChart)
  }

  private[this] def sampleRecursivelyWithoutTable(ij: Cat, i: Int, j: Int,
    cleanGuideChart: Chart[Option[ArrayBuffer[(Cat, ArrayBuffer[(GuideChartEntry, LogDouble)])]]],
    insideChart: Chart[Map[Cat, LogDouble]]): CcgTree = {

    val entries = cleanGuideChart(i, j).get.toMap.apply(ij)
    val gce = makePD(i, j, ij, entries, insideChart).sample()
    gce match {
      case BinaryGuideChartEntry(k, BinaryProd(ik, kj)) =>
        CcgBinode(ij,
          sampleRecursivelyWithoutTable(ik, i, k, cleanGuideChart, insideChart),
          sampleRecursivelyWithoutTable(kj, k, j, cleanGuideChart, insideChart))
      case UnaryGuideChartEntry(UnaryProd(subCat)) =>
        CcgUnode(ij, sampleRecursivelyWithoutTable(subCat, i, j, cleanGuideChart, insideChart))
      case TermGuideChartEntry(TermProd(word)) =>
        CcgLeaf(ij, word, "FAKEPOS")
    }
  }

  private[this] def makePD(
    i: Int, j: Int, ij: Cat,
    entries: ArrayBuffer[(GuideChartEntry, LogDouble)],
    insideChart: Chart[Map[Cat, LogDouble]]) = {

    val weightedProds: Map[GuideChartEntry, LogDouble] = entries.map {
      case (gce @ BinaryGuideChartEntry(k, BinaryProd(ik, kj)), prodP) =>
        val ikP = insideChart(i, k).toMap.apply(ik)
        val kjP = insideChart(k, j).toMap.apply(kj)
        gce -> (prodP * ikP * kjP)

      case (gce @ UnaryGuideChartEntry(UnaryProd(sub)), prodP) =>
        val subP = insideChart(i, j).toMap.apply(sub)
        gce -> (prodP * subP)

      case (gce @ TermGuideChartEntry(TermProd(word)), prodP) =>
        gce -> prodP
    }(breakOut)
    new SimpleLogProbabilityDistribution(weightedProds)
  }

  /**
   * Order the categories for iterating such that for any unary rule where
   * A can be a child of B, A is visited before B.
   */
  def makeCatOrdering(unaryProductions: Map[Cat, Set[Cat]]) = {
    val allLinks: Set[(Cat, Cat)] = (for { (p, cs) <- unaryProductions; c <- cs } yield (c -> p))(breakOut)
    val catOrdering = GraphUtil.toDagOrder(allLinks).zipWithIndex.toMap
    val orderMap = catOrdering.withDefaultValue(-1) // categories uninvolved in the ordering are handled first, as a block
    scala.math.Ordering.by[Cat, Int](orderMap)
  }

  def sortGuideChartEntries(table: Chart[Option[ArrayBuffer[(Cat, ArrayBuffer[(GuideChartEntry, LogDouble)])]]]) = {
    for ((i, j, Some(cell)) <- table.topDownNodes) {
      table(i, j) = Some(cell.sorted(catOrdering))
    }
    table
  }

  def newTable[A, B](n: Int, brackets: Vector[(Int, Int)], invalidSpans: Set[(Int, Int)]): Chart[Option[ArrayBuffer[A]]] = {
    val table: Chart[Option[ArrayBuffer[A]]] = Chart.fill(n)(None)
    for (i <- 0 until n) {
      for (j <- (i + 1) to n) {
        val itoj = (i + 1 to j)
        if (i >= j || invalidSpans(i -> j) ||
          brackets.exists { // if there exists a cross-bracketing
            case (a, b) =>
              assert(a < b, f"improper bracket: ($a,$b)")
              (a < i && i < b && b < j) ||
                (i < a && a < j && j < b)
          }) {}
        else {
          table(i, j) = Some(ArrayBuffer.empty[A])
        }
      }
    }
    table
  }

  //  /**
  //   * Make a table of the categories in the tree that are part of
  //   * *binary* productions (either at the top or bottom).
  //   */
  //  def treeBinaryCatTable(tree: CcgTree) = {
  //    val n = tree.length
  //    val ytable = Chart.fillVal(n)(none[Cat])
  //    val ktable = Chart.fillVal(n)(none[Int])
  //    def traverse(t: CcgTree, i: Int, j: Int): Unit = {
  //      t match {
  //        case CcgLeaf(cat, word, _) =>
  //          assert(i + 1 == j)
  //
  //        case CcgUnode(cat, s) =>
  //          traverse(s, i, j)
  //
  //        case CcgBinode(cat, l, r) =>
  //          assert(i + 1 < j)
  //          val k = i + l.length
  //          ktable(i, j) = Some(k)
  //
  //          if (ytable(i, j).isDefined) {
  //            /*
  //             * Just skip over all unary rules.  They will be automatically 
  //             * incorporated into the inf chart since they can be finitely 
  //             * enumerated.  We only need to sample for the binary productions
  //             * from each cell.
  //             */
  //
  //            // removing this since it doesn't properly consider unary rules
  //            // assert(ytable(i, j).get == cat, {
  //            //   TreeViz.drawTree(tree)
  //            //   f"treeBinaryCatTable found conflicting cats: ($i,$j)  ${ytable(i, j).get} vs ${cat}"
  //            // })
  //          }
  //          else
  //            ytable(i, j) = Some(cat)
  //          ytable(i, k) = Some(l.cat)
  //          ytable(k, j) = Some(r.cat)
  //
  //          traverse(l, i, k)
  //          traverse(r, k, j)
  //      }
  //    }
  //    traverse(tree, 0, n)
  //    (ytable, ktable)
  //  }

  /**
   * Make a table of the categories in the tree that are part of
   * *binary* productions (either at the top or bottom).
   *
   * Just skip over all unary rules.  They will be automatically
   * incorporated into the inf chart since they can be finitely
   * enumerated.  We only need to sample for the binary productions
   * from each cell.
   */
  def getBinaryProds(tree: CcgTree, i: Int, j: Int): Map[(Int, Int), (Cat, Int, Cat, Cat)] = tree match {
    case CcgLeaf(cat, word, _) =>
      assert(i + 1 == j)
      Map.empty

    case CcgUnode(cat, s) =>
      getBinaryProds(s, i, j)

    case CcgBinode(cat, l, r) =>
      assert(i + 1 < j)
      val k = i + l.length
      Map((i, j) -> (cat, k, l.cat, r.cat)) ++ getBinaryProds(l, i, k) ++ getBinaryProds(r, k, j)
  }

}

