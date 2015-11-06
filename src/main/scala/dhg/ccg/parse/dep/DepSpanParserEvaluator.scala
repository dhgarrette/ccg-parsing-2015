package dhg.ccg.parse.dep

import dhg.ccg.cat.Cat
import dhg.util._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.cat._
import dhg.util.viz._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg.CfgGuideChart

trait ParserEvaluator {
  def evaluate(model: GuideChartParser, testData: TraversableOnce[(Option[CfgGuideChart], CcgTree)], tagdict: TagDictionary[Cat],
    printFailedParses: Boolean = false,
    verbose: Boolean = false): Double
}

class DepSpanParserEvaluator(outputFile: Option[String]) extends ParserEvaluator {

  def evaluate(model: GuideChartParser, testData: TraversableOnce[(Option[CfgGuideChart], CcgTree)], tagdict: TagDictionary[Cat],
    printFailedParses: Boolean = false,
    verbose: Boolean = false): Double = {

    var modelConstInGold = 0
    var modelConstituents = 0
    var goldConstituents = 0
    //    var constituentParsedTotal = 0

    var parseFound = 0
    var fullParseCorrect = 0
    var totalSentences = 0

    val errors = collection.mutable.Map.empty[(Cat, Cat), Int] // (gold, model) -> count
    val failures = collection.mutable.Set.empty[(CcgTree, CcgTree)] // (gold, model)

    var totalTime = 0L
    writeUsing(File(outputFile.getOrElse("/dev/null"))) { f =>
      time("testing", {
        for (((guideChart, goldTree), i) <- testData.toIterator.zipWithIndex) {
          if (verbose) print(f"${i + 1}%5s, length=${goldTree.length}%3d,  avg-ambig=${goldTree.words.map(tagdict(_).size).avg}%5.2f,  ") // num-parses=${parseCounter.countParses(goldTree.words, Vector.empty, tagdict)}%18s,  ")
          val startTime = System.currentTimeMillis()
          val modelParsed = guideChart.flatMap(model.parseAndProbFromGuideChart).map(_._1)
          val parseTime = System.currentTimeMillis() - startTime
          totalTime += parseTime
          if (verbose) println(f"time=${parseTime / 1000.0}%.3f sec,  avg time: ${totalTime / 1000.0 / (i + 1)}%.3f sec")
          if ((i + 1) % 100 == 0) println(f"${i + 1}%5s, total time: ${totalTime / 1000.0}%.3f sec, avg time: ${totalTime / 1000.0 / (i + 1)}%.4f sec")
          val n = goldTree.length

          val goldDepTree = DepTree.fromCcgTree(goldTree)
          val goldConstMap = constituentSet(goldDepTree).toMap

          modelParsed match {
            case Some(modelParse) =>
              assert(modelParse.length == n, f"\n${goldTree.words.mkString(" ")}\n${modelParse.words.mkString(" ")}")
              f.wl(goldTree.words.mkString(" "))
              f.wl(modelParse.toString)
              f.wl(goldTree.toString)
              f.wl

              val modelDepTree = DepTree.fromCcgTree(modelParse)
              val modelConstMap = constituentSet(modelDepTree).toMap

              if (goldDepTree == modelDepTree) { fullParseCorrect += 1 }
              else { failures.add(goldTree, modelParse) }
              //TODO: constituentParsedTotal += (goldConstMap.size + 1)
              parseFound += 1

              for (((gw, gt), (mw, mt)) <- goldTree.tagged zipSafe modelParse.tagged) {
                if (gt != mt) {
                  errors.updateOrElseWith((gt, mt), 0)(_ + 1)
                }
              }

              // TreeViz.drawTree(goldDepTree)
              // TreeViz.drawTree(modelDepTree)
              for ((midx, mspan) <- modelConstMap) {
                if (goldConstMap.get(midx).contains(mspan)) {
                  modelConstInGold += 1
                }
              }
              modelConstituents += modelConstMap.size

            case None =>
              if (printFailedParses) {
                println(f"\nFailed parse:")
                for (w <- goldTree.words) {
                  println(f"  $w : ${tagdict(w).mkString(", ")}")
                }
                println(goldTree.pretty)
                println
              }
          }

          goldConstituents += goldConstMap.size
          totalSentences += 1
        }
      })
    }
    println(f"Parse Found: ${parseFound * 100.0 / totalSentences}%.2f  ($parseFound/$totalSentences)")
    //println(f"Parse Found Constituent Accuracy: ${constituentCorrect * 100.0 / constituentParsedTotal}%.2f  ($constituentCorrect/$constituentParsedTotal)  - includes terminal constituents")
    //println(f"Full Parse Accuracy: ${fullParseCorrect * 100.0 / totalSentences}%.2f  (${fullParseCorrect}/${totalSentences})")
    val pr = modelConstInGold * 100.0 / modelConstituents
    val re = modelConstInGold * 100.0 / goldConstituents
    val f1 = 2 * (pr * re) / (pr + re)
    println(f"Constituent precision: ${pr}%.2f  ($modelConstInGold/$modelConstituents)")
    println(f"Constituent recall:    ${re}%.2f  ($modelConstInGold/$goldConstituents)")
    println(f"Constituent f1:        ${f1}%.2f")
    //val errorsToShow = errors.desc.take(10).map { case (((gtc, gtp), (mtc, mtp)), count) => (count.toString, f"$gtc -> $gtp", f"$mtc -> $mtp") }
    val errorsToShow = errors.desc.take(10).map { case ((g, m), count) => (count.toString, f"$g", f"$m") }
    val (maxCountWidth, maxTagWidth) =
      if (errorsToShow.isEmpty) (0, 0)
      else {
        val a = errorsToShow.map(_._1.length).max max 5
        val b = errorsToShow.map { case (_, gt, mt) => gt.size }.max
        (a, b)
      }
    println(f" ${"count".padLeft(maxCountWidth)}  ${"gold".padRight(maxTagWidth)}  ${"model"}")
    for ((count, gt, mt) <- errorsToShow) {
      println(f" ${count.padLeft(maxCountWidth)}  ${gt.padRight(maxTagWidth)}  ${mt}")
    }
    println(f"avg tagging: ${totalTime / 1000.0 / testData.size}%.4f sec")

    //    failures.toVector.sortBy(_._1.length).foreach {
    //      case (g, m) =>
    //        TreeViz.drawTree(g)
    //        TreeViz.drawTree(DepTree.fromCcgTree(g))
    //        TreeViz.drawTree(m)
    //        TreeViz.drawTree(DepTree.fromCcgTree(m))
    //    }

    f1
  }

  private[this] def constituentSet(deptree: DepTree): Vector[(Int, (Int, Int))] = {
    def inner(t: DepTree): (Set[Int], Vector[(Int, (Int, Int))]) = t match {
      case DepTree(word, index, _, children) =>
        if (children.nonEmpty) {
          val (descIndices, descEntries) = children.map(inner).unzip
          val decendentIndices = descIndices.flatten :+ index
          (Set(index) ++ decendentIndices) -> (((index, (decendentIndices.min, decendentIndices.max))) +: descEntries.flatten)
        }
        else {
          Set(index) -> Vector.empty
        }
    }
    inner(deptree)._2
  }

  override def toString = f"DepSpanParserEvaluator()" // f"ParserEvaluator(${guideChartBuilder}, goldSupertags=$goldSupertags)"

}
