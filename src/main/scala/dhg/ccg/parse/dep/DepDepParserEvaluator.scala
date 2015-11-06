package dhg.ccg.parse.dep

import dhg.ccg.cat.Cat
import dhg.util._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.cat._
import dhg.util.viz._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg.CfgGuideChart

class DepDepParserEvaluator(outputFile: Option[String], printEverything: Boolean = false) {
  type Word = String

  def evaluate(model: GuideChartParser, testData: TraversableOnce[(Option[CfgGuideChart], Vector[String], Int, DepTree)], tagdict: TagDictionary[Cat],
    printFailedParses: Boolean = false,
    verbose: Boolean = false): Double = {

    var dependencyCorrect = 0
    var dependencyTotal = 0
    var dependencyParsedTotal = 0

    var parseFound = 0
    var fullDepParseCorrect = 0
    var totalSentences = 0

    var totalTime = 0L

    def printAll() = {
      println(f"Parse Found: ${parseFound * 100.0 / totalSentences}%.2f  ($parseFound/$totalSentences)")
      println(f"Full Parse Correct: ${fullDepParseCorrect * 100.0 / totalSentences}%.2f  (${fullDepParseCorrect}/${totalSentences})")
      println(f"Parse Found Dependency Accuracy: ${dependencyCorrect * 100.0 / dependencyParsedTotal}%.2f  ($dependencyCorrect/$dependencyParsedTotal)")
      val dependencyAccuracy = dependencyCorrect * 100.0 / dependencyTotal
      println(f"Dependency Accuracy: ${dependencyAccuracy}%.2f  ($dependencyCorrect/$dependencyTotal)")
      //val errorsToShow = errors.desc.take(10).map { case (((gtc, gtp), (mtc, mtp)), count) => (count.toString, f"$gtc -> $gtp", f"$mtc -> $mtp") }
      println(f"avg parse time: ${totalTime / 1000.0 / testData.size}%.4f sec")
      dependencyAccuracy
    }

    writeUsing(File(outputFile.getOrElse("/dev/null"))) { output =>
      time("testing", {
        for (((guideChart, goldTreeWords, n, goldDepTree), i) <- testData.toIterator.zipWithIndex) {
          if (verbose) print(f"${i + 1}%5s, length=${n}%3d,  avg-ambig=${goldTreeWords.map(tagdict(_).size).avg}%5.2f,  ") // num-parses=${parseCounter.countParses(goldTree.words, Vector.empty, tagdict)}%18s,  ")
          val startTime = System.currentTimeMillis()
          val modelParsed = guideChart.flatMap(model.parseAndProbFromGuideChart).map(_._1)
          val parseTime = System.currentTimeMillis() - startTime
          totalTime += parseTime
          if (verbose) println(f"time=${parseTime / 1000.0}%.3f sec,  avg time: ${totalTime / 1000.0 / (i + 1)}%.3f sec")
          if ((i + 1) % 100 == 0) println(f"${i + 1}%5s, total time: ${totalTime / 1000.0}%.3f sec, avg time: ${totalTime / 1000.0 / (i + 1)}%.4f sec")

          output.wl(i)
          output.wl(goldTreeWords.mkString(" "))
          output.wl(f"model: $modelParsed")
          output.wl(f"gold: $goldDepTree")
          output.wl(f"${parseTime / 1000.0}%.3f sec")
          output.wl

          val goldDepMap = depSet(goldDepTree).toMap

          modelParsed match {
            case Some(modelParse) =>
              assert(modelParse.length == n, f"\n${goldTreeWords.mkString(" ")}\n${modelParse.words.mkString(" ")}")
              val modelDepTree = DepTree.fromCcgTree(modelParse)
              val modelDepMap = depSet(modelDepTree).toMap

              //              val fullTreeCorrect = traverse(modelParse, 0, n)
              //              if (fullTreeCorrect) { fullConstituentParseCorrect += 1 }

              if (goldDepTree == modelDepTree) { fullDepParseCorrect += 1 }
              else { /*failures.add(goldTree, modelParse)*/ }
              dependencyParsedTotal += (goldDepMap.size + 1)
              parseFound += 1

              if ((modelDepTree.word, modelDepTree.index) == (goldDepTree.word, goldDepTree.index)) { // check root
                dependencyCorrect += 1
              }
              for ((mc, mp) <- modelDepMap) {
                if (goldDepMap.get(mc).exists(_ == mp)) {
                  dependencyCorrect += 1
                }
              }

            case None =>
              if (printFailedParses) {
                println(f"\nFailed parse:")
                for (w <- goldTreeWords) {
                  println(f"  $w : ${tagdict(w).mkString(", ")}")
                }
                //println(goldTree.pretty)
                println
              }

          }

          dependencyTotal += (goldDepMap.size + 1)
          totalSentences += 1
          
          if(printEverything) printAll()
        }
      })
    }

    val dependencyAccuracy = printAll()

    //    failures.toVector.sortBy(_._1.length).foreach {
    //      case (g, m) =>
    //        TreeViz.drawTree(g)
    //        TreeViz.drawTree(DepTree.fromCcgTree(g))
    //        TreeViz.drawTree(m)
    //        TreeViz.drawTree(DepTree.fromCcgTree(m))
    //    }

    dependencyAccuracy
  }

  private[this] def depSet(deptree: DepTree): Vector[((Word, Int), (Word, Int))] = {
    deptree match {
      case DepTree(word, index, _, children) =>
        children.map { c =>
          (c.word, c.index) -> (word, index)
        } ++ children.flatMap(depSet)
    }
  }

  override def toString = f"DepParserEvaluator(outputFile=$outputFile)"

}
