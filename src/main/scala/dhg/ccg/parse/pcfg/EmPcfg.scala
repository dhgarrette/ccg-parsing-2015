package dhg.ccg.parse.pcfg

import scala.annotation.tailrec
import scala.collection.mutable.{ Map => MMap }
import dhg.ccg.cat._
import dhg.ccg.prob._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.tagdict.TagDictionary
import dhg.util._
import scalaz._
import Scalaz._

class EmPcfg(
  maxIterations: Int,
  alphaPriorMaker: PcfgAlphaPriorMaker,
  alphaRoot: Double, alphaBiny: Double, alphaUnry: Double, alphaTerm: Double,
  priorBinyProdMix: Double, priorUnryProdMix: Double, priorTermProdMix: Double,
  alphaProd: Double,
  pcfgInsideChartBuilder: PcfgInsideChartBuilder,
  pcfgParserInstantiater: PcfgParserInstantiater,
  itermediateEvaluatorAndEvalIters: Option[(GuideChartParser => Unit, Set[Int])] = None,
  verbose: Boolean = false) // extends SemisupervisedPcfgParserTrainer(guideChartBuilder, alphaRoots, alphaProds) 
  {

  def trainGuideChartsWithSomeGold(
    guideCharts: Vector[CfgGuideChart], goldTrees: Vector[CcgTree],
    priorRootDist: LogProbabilityDistribution[Cat],
    priorBinyDist: ConditionalLogProbabilityDistribution[Cat, BinaryProd],
    priorUnryDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd],
    priorTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd]) = {

    for ((evaluator, evalIters) <- itermediateEvaluatorAndEvalIters if evalIters.contains(-1)) evaluator(makeResultParser(Map.empty, Map.empty, priorRootDist, priorBinyDist, priorUnryDist, priorTermDist))

    if (verbose) { println(f"McmcPcfg.trainGuideChartsWithSomeGold"); guideCharts.foreach(_.draw()) }

    val (alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts) =
      alphaPriorMaker.makeAll(guideCharts, goldTrees,
        priorRootDist, priorBinyDist, priorUnryDist, priorTermDist,
        alphaRoot, alphaBiny, alphaUnry, alphaTerm)

    val (estRootCounts, estProdCounts) =
      doTrain(guideCharts,
        alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts)

    makeResultParser(estRootCounts, estProdCounts, priorRootDist, priorBinyDist, priorUnryDist, priorTermDist)
  }

  private[this] def makeResultParser(
    estRootCounts: Map[Cat, LogDouble],
    estProdCounts: Map[Cat, Map[Prod, LogDouble]],
    priorRootDist: LogProbabilityDistribution[Cat],
    priorBinyDist: ConditionalLogProbabilityDistribution[Cat, BinaryProd],
    priorUnryDist: ConditionalLogProbabilityDistribution[Cat, UnaryProd],
    priorTermDist: ConditionalLogProbabilityDistribution[Cat, TermProd]) = {

    val priorProdDist = new ICPDU(priorBinyDist, priorUnryDist, priorTermDist,
      Map().withDefaultValue((LogDouble(priorBinyProdMix), LogDouble(priorUnryProdMix), LogDouble(priorTermProdMix))))

    val allRootSet = estRootCounts.keySet
    val allProdSet = estProdCounts.mapValues(_.keySet).values.flatten.toSet
    val rootDist = if (alphaRoot.isInfinite) priorRootDist else new AlphaBetaLogProbabilityDistribution[Cat /*                 */ ](estRootCounts, LogDouble(alphaRoot), priorRootDist, Some(allRootSet))
    val prodDist = if (alphaProd.isInfinite) priorProdDist else new AlphaBetaConditionalLogProbabilityDistribution[Cat, Prod](estProdCounts.mapt { (a, bCounts) => a -> new AlphaBetaLogProbabilityDistribution[Prod](bCounts, LogDouble(alphaProd), new ConditionalWrappingLogProbabilityDistribution(a, priorProdDist)) }, LogDouble(alphaProd), priorProdDist, Some(allProdSet))
    pcfgParserInstantiater(rootDist, prodDist)
  }

  private[this] final def doTrain(
    guideCharts: Vector[CfgGuideChart],
    alphaPriorRootCounts: Map[Cat, LogDouble],
    alphaPriorBinyCounts: Map[Cat, Map[BinaryProd, LogDouble]],
    alphaPriorUnryCounts: Map[Cat, Map[UnaryProd, LogDouble]],
    alphaPriorTermCounts: Map[Cat, Map[TermProd, LogDouble]]) = {
    val allProdHeadCats = alphaPriorBinyCounts.keySet | alphaPriorUnryCounts.keySet | alphaPriorTermCounts.keySet

    val initRootDist = new SimpleLogProbabilityDistribution(alphaPriorRootCounts)
    val initProdDist = new SimpleConditionalLogProbabilityDistribution(allProdHeadCats.mapTo { cat =>
      val binyDist = new SimpleLogProbabilityDistribution(alphaPriorBinyCounts.getOrElse(cat, Map.empty))
      val unryDist = new SimpleLogProbabilityDistribution(alphaPriorUnryCounts.getOrElse(cat, Map.empty))
      val termDist = new SimpleLogProbabilityDistribution(alphaPriorTermCounts.getOrElse(cat, Map.empty))
      val prodDist = new IPDU(binyDist, unryDist, termDist, LogDouble(priorBinyProdMix), LogDouble(priorUnryProdMix), LogDouble(priorTermProdMix))
      prodDist
    }.toMap)

    //    val initPcfgParser = pcfgParserInstantiater(initRootDist, initProdDist)
    //    val initialTrees = guideCharts.map { gc =>
    //      val (t, p) = initPcfgParser.parseAndProbKBestFromGuideChart(gc, k = 1).only
    //      assert(p.nonZero, "Sentence parsed with zero probability")
    //      t
    //    }
    //    for ((evaluator, evalIters) <- itermediateEvaluatorAndEvalIters if evalIters.contains(0)) evaluator(supPcfgTrainer.train(initialTrees))

    val (estRootCounts, estProdCounts) = iterate(guideCharts, allProdHeadCats,
      initRootDist, initProdDist,
      alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts,
      1, LogDouble.zero,
      lastKnownGoodResult = (Map.empty, Map.empty))
    (estRootCounts, estProdCounts)
  }

  @tailrec private[this] def iterate(
    guideCharts: Vector[CfgGuideChart], allProdHeadCats: Set[Cat],
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    alphaPriorRootCounts: Map[Cat, LogDouble],
    alphaPriorBinyCounts: Map[Cat, Map[BinaryProd, LogDouble]],
    alphaPriorUnryCounts: Map[Cat, Map[UnaryProd, LogDouble]],
    alphaPriorTermCounts: Map[Cat, Map[TermProd, LogDouble]],
    iteration: Int, prevAvgProb: LogDouble,
    lastKnownGoodResult: (Map[Cat, LogDouble], Map[Cat, Map[Prod, LogDouble]])): // 
    (Map[Cat, LogDouble], Map[Cat, Map[Prod, LogDouble]]) = {

    val startTime = System.currentTimeMillis()
    val (estRootCounts, estProdCounts, avgProb) = reestimate(guideCharts, rootDist, prodDist)
    println(f"iteration ${(iteration + ":").padRight(4)} ${(System.currentTimeMillis() - startTime) / 1000.0}%.3f sec   avgLogProb=${(avgProb.logValue + ",").padRight(22)} avgProb=${avgProb.toDouble}")

    if (iteration >= maxIterations) {
      println(f"MAX ITERATIONS REACHED")
      (estRootCounts, estProdCounts)
    }
    else if (avgProb < prevAvgProb) {
      println(f"DIVERGENCE!")
      //(estRootCounts, estProdCounts)
      lastKnownGoodResult
    }
    //    else if (avgProb.logValue - prevAvgProb.logValue < convergence) {
    //      println(f"CONVERGENCE (${avgProb.logValue - prevAvgProb.logValue} < $convergence)")
    //      (estRootCounts, estProdCounts)
    //    }
    else {
      //      for ((evaluator, evalIters) <- itermediateEvaluatorAndEvalIters if evalIters.contains(iteration + 1)) evaluator(makeResultParser(estRootCounts, estProdCounts, priorRootDist, priorBinyDist, priorUnryDist, priorTermDist))

      val estBinyCounts: Map[Cat, Map[BinaryProd, LogDouble]] = estProdCounts.mapVals(_.collect { case (a: BinaryProd, b: LogDouble) => (a, b) }).filter(_._2.nonEmpty)
      val estUnryCounts: Map[Cat, Map[UnaryProd, LogDouble]] = estProdCounts.mapVals(_.collect { case (a: UnaryProd, b: LogDouble) => (a, b) }).filter(_._2.nonEmpty)
      val estTermCounts: Map[Cat, Map[TermProd, LogDouble]] = estProdCounts.mapVals(_.collect { case (a: TermProd, b: LogDouble) => (a, b) }).filter(_._2.nonEmpty)

      iterate(guideCharts, allProdHeadCats,
        new SimpleLogProbabilityDistribution(estRootCounts.mapVals(LogDouble(_)) |+| alphaPriorRootCounts),
        new SimpleConditionalLogProbabilityDistribution(allProdHeadCats.mapTo { cat =>
          val bc = estBinyCounts.getOrElse(cat, Map.empty).mapVals(LogDouble(_))
          val uc = estUnryCounts.getOrElse(cat, Map.empty).mapVals(LogDouble(_))
          val tc = estTermCounts.getOrElse(cat, Map.empty).mapVals(LogDouble(_))
          val bcCountsWithPrior = bc |+| alphaPriorBinyCounts.getOrElse(cat, Map.empty) // these could be empty, since `cat` might not produce binary, unary, AND term productions for the training data
          val ucCountsWithPrior = uc |+| alphaPriorUnryCounts.getOrElse(cat, Map.empty)
          val tcCountsWithPrior = tc |+| alphaPriorTermCounts.getOrElse(cat, Map.empty)
          val binyDist = new SimpleLogProbabilityDistribution(bcCountsWithPrior)
          val unryDist = new SimpleLogProbabilityDistribution(ucCountsWithPrior)
          val termDist = new SimpleLogProbabilityDistribution(tcCountsWithPrior)
          val Vector(priorBinyProdMix, priorUnryProdMix, priorTermProdMix) = Vector(bc.values.sum + LogDouble(1.0), uc.values.sum + LogDouble(1.0), tc.values.sum + LogDouble(1.0))
          val prodDist = new IPDU(binyDist, unryDist, termDist, priorBinyProdMix, priorUnryProdMix, priorTermProdMix)
          prodDist
        }.toMap),
        alphaPriorRootCounts, alphaPriorBinyCounts, alphaPriorUnryCounts, alphaPriorTermCounts,
        iteration + 1, avgProb,
        lastKnownGoodResult = (estRootCounts, estProdCounts))
    }
  }

  private[this] final def reestimate(
    guideCharts: Vector[CfgGuideChart],
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]) = {
    val ((estRootCounts, estProdCounts), totalProb) =
      guideCharts.par.zipWithIndex.flatMap {
        case (guideChart, i) => expectations(guideChart, rootDist, prodDist)
      }.reduce(_ |+| _)

    //    if (verbose) {
    //      for ((cat, count) <- estRootCounts.desc) {
    //        println(f"$cat%-20s -> ${count.toDouble}%.10f")
    //      }; println
    //      for { (cat, prods) <- estProdCounts; (NontermProd(b, c), count) <- prods.desc } {
    //        println(f"$cat%-20s -> $b%-20s$c%-20s  ${count.toDouble}%.10f")
    //      }; println
    //      for { (cat, prods) <- estProdCounts; (prod, count) <- prods.desc } {
    //        println(f"$cat%-20s -> $prod%-40s  ${count.toDouble}%.10f")
    //      }
    //    }

    val avgProb = totalProb / LogDouble(guideCharts.size)
    (estRootCounts, estProdCounts, avgProb)
  }

  //  private[this] def expectations(
  //    guideChart: CfgGuideChart,
  //    rootDist: LogProbabilityDistribution[Cat],
  //    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): Option[((Map[Cat, LogDouble], Map[Cat, Map[Prod, LogDouble]]), LogDouble)]
  //
  //  final def drawPcfgInsideChart(table: Table, tokens: Vector[Word]): Unit = {
  //    val n = table.length
  //    val maxCatLen = table.flatten.flatten.flatten.map(_._1.toString.size).max
  //    val maxKLen = table.flatten.flatten.flatten.flatMap(_._2.keys.map(_._1.toString.size)).max
  //    val maxProdLen = table.flatten.flatten.flatten.flatMap(_._2.keys.map(_._2.toString.size)).max
  //    val width = maxCatLen + 2 + maxKLen + 1 + maxProdLen + 2 + 7
  //    val emptyCol = "-+" +: Vector.fill(table.flatten.flatten.map(_.values.flatten.size).max)(" |")
  //    for ((row, i) <- (table ++ (Array(Array.fill(n + 1)(Some(MMap.empty))): Table)).zipWithIndex) {
  //      println(sideBySide(0, (Vector(f"$i ") +: row.zipWithIndex.map {
  //        case (m, j) =>
  //          (("-" * width)) +: (m match {
  //            case _ if (m.isEmpty || m.get.isEmpty) && i == (j + 1) => Vector(tokens(i - 1).toString.padLeft(width))
  //            case _ if (m.isEmpty || m.get.isEmpty) && i == j => Vector(i.toString.padLeft(width))
  //            case Some(mx) =>
  //              mx.toVector
  //                .sortBy { case (_, prods) => prods.values.sum }.reverse
  //                .flatMap { case (cat, prods) => sideBySide(0, Vector(f" $cat  ".padRight(maxCatLen + 3)), prods.toVector.sortBy(_._2).reverse.map { case ((k, prod), p) => f"${f"${if (k >= 0) k.toString else ""}".padLeft(maxKLen)}/${f"$prod".padRight(maxProdLen)}  ${f"${p.logValue}%.2f".padLeft(6)}" }) }
  //            case None => Vector("")
  //          })
  //      }.toVector.flatMap(v => Vector(v, emptyCol))): _*).mkString("\n"))
  //    }
  //    println; println
  //  }
  //
  //  final def drawOutsideTable(table: Table, tokens: Vector[Word]): Unit = {
  //    drawPcfgInsideChart(table.map(_.map(_.map(_.mapVals(p => MMap((-1, TermProd(""): Prod) -> p))))), tokens)
  //  }
  //
  //}
  //
  //class SoftEmPcfg(
  //  maxIterations: Int,
  //  distributioner: SupervisedPcfgParserTrainer,
  //  guideChartBuilder: OldToRemoveCfgGuideChartBuilder,
  //  alphaRoots: Double = 0.0, alphaProds: Double = 0.0,
  //  convergence: Double = 1e-20,
  //  verbose: Boolean = false)
  //  extends EmPcfg(maxIterations, distributioner, guideChartBuilder, alphaRoots, alphaProds, convergence, verbose) {
  //
  //  import OldToRemoveCfgGuideChartBuilder.GuideTable

  private[this] final def expectations(
    guideChart: CfgGuideChart,
    rootDist: LogProbabilityDistribution[Cat], prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): Option[((Map[Cat, LogDouble], Map[Cat, Map[Prod, LogDouble]]), LogDouble)] = {
    val n = guideChart.length

    // Compute Inside Probabilities
    val insideTable = pcfgInsideChartBuilder.buildInsideChart(guideChart, prodDist)
    // drawTable(inside, tokens)

    // Compute normalizing value
    sentenceProb(insideTable, rootDist, prodDist).map { z =>

      if (verbose) println(f"z = ${z.toDouble}%.10f")

      // Compute Outside Probabilities
      val outsideTable = outside(guideChart, insideTable, rootDist, prodDist)
      outsideTable.draw()

      // Get estimated rule counts
      (estimateCounts(n, guideChart, insideTable, outsideTable, z, rootDist, prodDist), z)
    }
  }

  /** Compute normalizing value */
  def sentenceProb(insideTable: PcfgInsideChart, rootDist: LogProbabilityDistribution[Cat], prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]) = {
    val rootCell = insideTable.root
    if (rootCell.isEmpty) None
    else
      Some(rootCell.sumBy {
        case (rootCat, insideP) =>
          val rootP = rootDist(rootCat)
          val p = rootP * insideP
          if (verbose) println(f"rootCat=$rootCat : ${p.toDouble}%.10f")
          p
      })
  }

  private[this] final def outside(
    guideChart: CfgGuideChart,
    insideTable: PcfgInsideChart,
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]) = {
    val n = guideChart.length

    val outsideTable: Vector[Vector[MMap[Cat, LogDouble]]] =
      Vector.tabulate(n, n + 1) { case (i, j) => MMap.empty[Cat, LogDouble] }

    for ((rootCat, rootProds) <- insideTable.root if guideChart.rootSet(rootCat)) {
      outsideTable(0)(n)(rootCat) = rootDist(rootCat)
      println(f"REMOVE ME : rootDist(rootCat) = rootDist($rootCat) = ${rootDist(rootCat)}")
    }

    for {
      (i, j, cell) <- guideChart.topDownNodes
      (ij, entries) <- cell
      ijOutsideP <- outsideTable(i)(j).get(ij)
    } {
      for (entry <- entries) {
        entry match {
          case BinaryGuideChartEntry(k, prod @ BinaryProd(ik, kj)) =>
            val prodP = prodDist(prod, ij)
            println(f"REMOVE ME : prodDist(prod, ij) = prodDist($prod, $ij) = ${prodP}")
            for {
              ikInsideP <- insideTable(i)(k).get(ik) // skip entries that weren't added to the chart
              kjInsideP <- insideTable(k)(j).get(kj)
            } {
              val ikPaddition =
                prodP * //       prob of this production: B -> A C
                  kjInsideP * // prob of possible subtree rooted by C
                  ijOutsideP
              if (shouldAdd(ikPaddition)) { outsideTable(i)(k).updateOrElseWith(ik, LogDouble.zero)(_ + ikPaddition) }
              if (verbose) println(f"o($i,$k,$ik) += p($ij -> $prod) * i($k,$j,$kj) * o($i,$j,$ij) = ${prodP.toDouble} * ${kjInsideP.toDouble} * ${ijOutsideP.toDouble} = ${ikPaddition.toDouble}%.10f   ${ikPaddition.logValue}")

              val kjPaddition =
                prodP * //       prob of this production: B -> C A
                  ikInsideP * // prob of possible subtree rooted by C
                  ijOutsideP
              if (shouldAdd(ikPaddition)) { outsideTable(k)(j).updateOrElseWith(kj, LogDouble.zero)(_ + kjPaddition) }
              if (verbose) println(f"o($k,$j,$kj) += p($ij -> $prod) * i($i,$k,$ik) * o($i,$j,$ij) = ${prodP.toDouble} * ${ikInsideP.toDouble} * ${ijOutsideP.toDouble} = ${kjPaddition.toDouble}%.10f   ${kjPaddition.logValue}")
            }

          case UnaryGuideChartEntry(prod @ UnaryProd(sub)) =>
            val prodP = prodDist(prod, ij)
            for (subInsideP <- insideTable(i)(j).get(sub)) { // skip entries that weren't added to the chart
              val subPaddition =
                prodP * //        prob of this production: B -> A
                  ijOutsideP
              if (shouldAdd(subPaddition)) { outsideTable(i)(j).updateOrElseWith(sub, LogDouble.zero)(_ + subPaddition) }
              if (verbose) println(f"o($i,$j,$sub) += p($ij -> $prod) * o($i,$j,$ij) = ${prodP.toDouble} * ${ijOutsideP.toDouble} = ${subPaddition.toDouble}%.10f   ${subPaddition.logValue}")
            }

          case TermGuideChartEntry(prod @ TermProd(word)) => // Nothing to do
        }
      }
    }

    new PcfgInsideChart(outsideTable.map(_.map(_.toMap)))
  }

  private[this] def shouldAdd(p: LogDouble) = {
    assert(!p.isNaN)
    //assert(p.nonZero)
    //true
    p.nonZero
  }

  private[this] final def estimateCounts(n: Int,
    guideChart: CfgGuideChart, insideTable: PcfgInsideChart, outsideTable: PcfgInsideChart, z: LogDouble,
    rootDist: LogProbabilityDistribution[Cat], prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]): (Map[Cat, LogDouble], Map[Cat, Map[Prod, LogDouble]]) = {

    val estimatedRootCounts = MMap.empty[Cat, LogDouble]
    val estimatedProdCounts = MMap.empty[Cat, MMap[Prod, LogDouble]]

    for {
      i <- 0 until n
      j <- (i + 1) to n
      (ij, entries) <- guideChart(i)(j) //   search stops here if this span is unavailable (due to known bracketing)
      ijOutside = outsideTable(i)(j)
      ijOutsideP <- ijOutside.get(ij)
      BinaryGuideChartEntry(k, prod @ BinaryProd(ik, kj)) <- entries
      prodP = prodDist(prod, ij)
      ikInsideP <- insideTable(i)(k).get(ik)
      kjInsideP <- insideTable(k)(j).get(kj)
    } {
      val estCount = ijOutsideP * prodP * ikInsideP * kjInsideP / z
      estimatedProdCounts
        .getOrElseUpdate(ij, MMap.empty)
        .updateOrElseWith(prod, LogDouble.zero)(_ + estCount)
      if (verbose) println(f"c($i,$j,$ij) = o($i,$j,$ij) * p($ij -> $prod) * i($i,$k,$ik) * $k,$j,$kj) / z = ${ijOutsideP.toDouble} * ${prodP.toDouble} * ${ikInsideP.toDouble} * ${kjInsideP.toDouble}  ${estCount.toDouble}%.10f")

      if (i == 0 && j == n) {
        estimatedRootCounts.updateOrElseWith(ij, LogDouble.zero)(_ + estCount)
      }
    }

    for {
      i <- 0 until n
      j = i + 1
      ijInside = insideTable(i)(j)
      ijOutside = outsideTable(i)(j)
      (ij, ijProds) <- guideChart(i)(j) //   search stops here if this span is unavailable (due to known bracketing)
      insideP <- ijInside.get(ij)
      outsideP <- ijOutside.get(ij)
      TermGuideChartEntry(prod @ TermProd(word)) <- ijProds
    } {
      val estCount = insideP * outsideP / z
      if (verbose) println(f"c($i,$j,$ij) = i($i,$j,$ij) * o($i,$j,$ij) / $z = ${insideP.toDouble} * ${outsideP.toDouble} / ${z.toDouble} = ${estCount.toDouble}%.10f")
      estimatedProdCounts
        .getOrElseUpdate(ij, MMap.empty)
        .updateOrElseWith(prod, LogDouble.zero)(_ + estCount)
    }

    (estimatedRootCounts.toMap, estimatedProdCounts.toMap.mapVals(_.toMap))
  }
}

//class HardEmPcfg(
//  maxIterations: Int,
//  distributioner: SupervisedPcfgParserTrainer,
//  guideChartBuilder: OldToRemoveCfgGuideChartBuilder,
//  alphaRoots: Double = 0.0, alphaProds: Double = 0.0,
//  convergence: Double = 1e-20,
//  verbose: Boolean = false)
//  extends EmPcfg(maxIterations, distributioner, guideChartBuilder, alphaRoots, alphaProds, convergence, verbose) {
//
//  import OldToRemoveCfgGuideChartBuilder.GuideTable
//
//  private[this] final def expectations(
//    guideChart: CfgGuideChart,
//    rootDist: LogProbabilityDistribution[Cat], prodDist: ConditionalLogProbabilityDistribution[Cat, Prod]) = {
//    val n = guideChart.size
//
//    val viterbiTable: Array[Array[Option[MMap[Cat, LogDouble]]]] = Array.fill(n, n + 1) { None }
//    val backpointers: Array[Array[Option[MMap[Cat, (Int, Prod)]]]] = Array.fill(n, n + 1) { None }
//
//    for {
//      i <- 0 until n
//      viterbiCell = MMap.empty[Cat, LogDouble]
//      backpointerCell = MMap.empty[Cat, (Int, Prod)]
//      (supertag, prods) <- guideChart(i)(i + 1).get
//    } {
//      val Coll((`i`, termProd: TermProd)) = prods
//      val p = prodDist(termProd, supertag)
//      if (p.nonZero) {
//        viterbiCell(supertag) = p
//        backpointerCell(supertag) = (i, termProd)
//      }
//      viterbiTable(i)(i + 1) = Some(viterbiCell)
//      backpointers(i)(i + 1) = Some(backpointerCell)
//    }
//
//    for {
//      j <- 2 to n //  end of span
//      i <- (j - 2) downto 0 //   start of span
//      guideij <- guideChart(i)(j) //   search stops here if this span is unavailable (due to known bracketing)
//    } {
//      val cellij = MMap.empty[Cat, LogDouble]
//      val bpij = MMap.empty[Cat, (Int, Prod)]
//      for {
//        (ij, ijProds) <- guideij
//        prodKey @ (k, prod @ NontermProd(ik, kj)) <- ijProds
//        ikP <- viterbiTable(i)(k).get.get(ik)
//        kjP <- viterbiTable(k)(j).get.get(kj)
//      } {
//        val prodP = prodDist(prod, ij)
//        val p = prodP * ikP * kjP
//        //println(f"$ij -> $prod   ${prodP}%.3f")
//        //println(f"prodP * ikP * kjP = ${prodP} * ${ikP.toDouble} * ${kjP.toDouble} = ${prodP * ikP * kjP}")
//        if (verbose) println(f"($i,$j) : $k, $ij, ${p.toDouble}%.10f, ${p.logValue}")
//        if (p > LogDouble.zero && cellij.get(ij).forall(existingP => p > existingP)) {
//          cellij(ij) = p
//          bpij(ij) = prodKey
//        }
//      }
//      viterbiTable(i)(j) = Some(cellij)
//      backpointers(i)(j) = Some(bpij)
//    }
//
//    var bestRoot: Option[Cat] = None
//    var bestTreeP: LogDouble = LogDouble.zero
//    for {
//      roots <- viterbiTable.root
//      (rootCat, rootP) <- roots
//    } {
//      val prodP = rootDist(rootCat)
//      val p = prodP * rootP
//      if (verbose) println(f"root : $rootCat, ${p.toDouble}%.10f, ${p.logValue}")
//      if (p > bestTreeP) {
//        bestRoot = Some(rootCat)
//        bestTreeP = p
//      }
//      else
//        None
//    }
//
//    bestRoot.map(root => (estimateCounts(n, root, backpointers), bestTreeP))
//  }
//
//  private[this] final def estimateCounts(n: Int, root: Cat, backpointers: Array[Array[Option[MMap[Cat, (Int, Prod)]]]]) = {
//
//    val estimatedRootCounts: MMap[Cat, Int] = MMap.empty
//    val estimatedProdCounts: MMap[Cat, MMap[Prod, Int]] = MMap.empty
//
//    def recurse(cat: Cat, i: Int, j: Int): Unit = {
//      backpointers(i)(j).get(cat) match {
//        case (k, prod @ NontermProd(left, right)) =>
//          estimatedProdCounts
//            .getOrElseUpdate(cat, MMap.empty)
//            .updateOrElseWith(prod, 0)(_ + 1)
//          recurse(left, i, k)
//          recurse(right, k, j)
//        case (k, prod @ TermProd(word)) =>
//          assert(j - i == 1)
//          assert(i == k)
//          estimatedProdCounts
//            .getOrElseUpdate(cat, MMap.empty)
//            .updateOrElseWith(prod, 0)(_ + 1)
//      }
//    }
//
//    estimatedRootCounts.updateOrElseWith(root, 0)(_ + 1)
//    recurse(root, 0, n)
//
//    if (verbose) {
//      for ((cat, count) <- estimatedRootCounts) {
//        println(f"$cat%-20s -> ${count.toDouble}%.2f")
//      }; println
//      for { (cat, prods) <- estimatedProdCounts; (NontermProd(b, c), count) <- prods } {
//        println(f"$cat%-20s -> $b%-20s$c%-20s  ${count.toDouble}%.2f")
//      }; println
//      for { (cat, prods) <- estimatedProdCounts; (TermProd(word), count) <- prods } {
//        println(f"$cat%-20s -> $word%-40s  ${count.toDouble}%.2f")
//      }
//    }
//
//    (estimatedRootCounts.toMap.mapVals(LogDouble(_)), estimatedProdCounts.toMap.mapVals(_.toMap.mapVals(LogDouble(_))))
//  }
//}
