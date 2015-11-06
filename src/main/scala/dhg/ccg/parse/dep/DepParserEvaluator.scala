package dhg.ccg.parse.dep

import dhg.ccg.cat.Cat
import dhg.util._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.cat._
import dhg.util.viz._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg.CfgGuideChart

class DepParserEvaluator(outputFile: Option[String], printEverything: Boolean = false) extends ParserEvaluator {
  type Word = String

  def evaluate(model: GuideChartParser, testData: TraversableOnce[(Option[CfgGuideChart], CcgTree)], tagdict: TagDictionary[Cat],
    printFailedParses: Boolean = false,
    verbose: Boolean = false): Double = {

    var dependencyCorrect = 0
    var dependencyTotal = 0
    var dependencyParsedTotal = 0

    var constituentCorrect = 0
    var constituentTotal = 0
    var constituentParsedTotal = 0
    var fullConstituentParseCorrect = 0

    var supertagCorrect = 0
    var supertagTotal = 0
    var supertagParsedTotal = 0

    var parseFound = 0
    var fullDepParseCorrect = 0
    var totalSentences = 0

    val errors = collection.mutable.Map.empty[(Cat, Cat), Int] // (gold, model) -> count
    val failures = collection.mutable.Set.empty[(CcgTree, CcgTree)] // (gold, model)

    var totalTime = 0L

    def printAll() = {
      println(f"Parse Found: ${parseFound * 100.0 / totalSentences}%.2f  ($parseFound/$totalSentences)")
      println(f"Full Parse Correct: ${fullDepParseCorrect * 100.0 / totalSentences}%.2f  (${fullDepParseCorrect}/${totalSentences})")
      println(f"Parse Found Supertag Accuracy: ${supertagCorrect * 100.0 / supertagParsedTotal}%.2f  ($supertagCorrect/$supertagParsedTotal)")
      println(f"Parse Found Dependency Accuracy: ${dependencyCorrect * 100.0 / dependencyParsedTotal}%.2f  ($dependencyCorrect/$dependencyParsedTotal)")
      val dependencyAccuracy = dependencyCorrect * 100.0 / dependencyTotal
      println(f"Supertag Accuracy: ${supertagCorrect * 100.0 / supertagTotal}%.2f  ($supertagCorrect/$supertagTotal)")
      println(f"Dependency Accuracy: ${dependencyAccuracy}%.2f  ($dependencyCorrect/$dependencyTotal)")
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
      dependencyAccuracy
    }

    writeUsing(File(outputFile.getOrElse("/dev/null"))) { output =>
      time("testing", {
        for (((guideChart, goldTree), i) <- testData.toIterator.zipWithIndex) {
          if (verbose) print(f"${i + 1}%5s, length=${goldTree.length}%3d,  avg-ambig=${goldTree.words.map(tagdict(_).size).avg}%5.2f,  ") // num-parses=${parseCounter.countParses(goldTree.words, Vector.empty, tagdict)}%18s,  ")
          val startTime = System.currentTimeMillis()
          val modelParsed = guideChart.flatMap(model.parseAndProbFromGuideChart).map(_._1)
          val parseTime = System.currentTimeMillis() - startTime
          totalTime += parseTime
          if (verbose) println(f"time=${parseTime / 1000.0}%.3f sec,  avg time: ${totalTime / 1000.0 / (i + 1)}%.3f sec")
          if ((i + 1) % 100 == 0) println(f"${i + 1}%5s, total time: ${totalTime / 1000.0}%.3f sec, avg time: ${totalTime / 1000.0 / (i + 1)}%.4f sec")

          output.wl(i)
          output.wl(goldTree.words.mkString(" "))
          output.wl(f"model: $modelParsed")
          output.wl(f"gold: $goldTree")
          output.wl(f"${parseTime / 1000.0}%.3f sec")
          output.wl

          val n = goldTree.length

          val goldDepTree = DepTree.fromCcgTree(goldTree)
          val goldDepMap = depSet(goldDepTree).toMap

          //          val goldNtTable = CcgTreeUtil.toNtTable(goldTree)
          //
          //          def traverse(t: CcgTree, i: Int, j: Int): Boolean = {
          //            val gl = goldNtTable(i)(j)
          //            val ml = Some(t.cat)
          //            val correct = gl == ml
          //            if (correct) { constituentCorrect += 1 }
          //
          //            constituentTotal += 1
          //            constituentParsedTotal += 1
          //
          //            t match {
          //              case CcgLeaf(cat, word, _) =>
          //                correct
          //
          //              case CcgBinode(cat, l, r) =>
          //                val li = i
          //                val lj = i + l.length
          //                val ri = i + l.length
          //                val rj = j
          //                val lmatch = traverse(l, li, lj)
          //                val rmatch = traverse(r, ri, rj)
          //                correct && lmatch && rmatch
          //
          //              case CcgUnode(cat, s) =>
          //                correct && traverse(s, i, j)
          //            }
          //          }

          modelParsed match {
            case Some(modelParse) =>
              assert(modelParse.length == n, f"\n${goldTree.words.mkString(" ")}\n${modelParse.words.mkString(" ")}")
              val modelDepTree = DepTree.fromCcgTree(modelParse)
              val modelDepMap = depSet(modelDepTree).toMap

              //              val fullTreeCorrect = traverse(modelParse, 0, n)
              //              if (fullTreeCorrect) { fullConstituentParseCorrect += 1 }

              if (goldDepTree == modelDepTree) { fullDepParseCorrect += 1 }
              else { failures.add(goldTree, modelParse) }
              dependencyParsedTotal += (goldDepMap.size + 1)
              supertagParsedTotal += goldTree.length
              parseFound += 1

              for (((gw, gt), (mw, mt)) <- goldTree.tagged zipSafe modelParse.tagged) {
                if (gt != mt) {
                  errors.updateOrElseWith((gt, mt), 0)(_ + 1)
                }
                else {
                  supertagCorrect += 1
                }
              }

              // TreeViz.drawTree(goldDepTree)
              // TreeViz.drawTree(modelDepTree)
              if ((modelDepTree.word, modelDepTree.index) == (goldDepTree.word, goldDepTree.index)) { // check root
                dependencyCorrect += 1
              }
              else {
                //              errors.updateOrElseWith((
                //                (goldDepTree.word.toString, goldDepMap.get((modelDepTree.word, modelDepTree.index)).fold("<ROOT>")(_._1.toString)),
                //                (modelDepTree.word.toString, "<ROOT>") //
                //                ), 0)(_ + 1)
              }
              for ((mc, mp) <- modelDepMap) {
                if (goldDepMap.get(mc).exists(_ == mp)) {
                  dependencyCorrect += 1
                }
                else {
                  //                errors.updateOrElseWith((
                  //                  (mc._1.toString, goldDepMap.get(mc).fold("<ROOT>")(_._1.toString)),
                  //                  (mc._1.toString, mp._1.toString) //
                  //                  ), 0)(_ + 1)
                }
              }

            case None =>
              if (printFailedParses) {
                println(f"\nFailed parse:")
                for (w <- goldTree.words) {
                  println(f"  $w : ${tagdict(w).mkString(", ")}")
                }
                println(goldTree.pretty)
                println
              }

              def traverseTotal(t: CcgTree): Unit = {
                constituentTotal += 1
                t match {
                  case CcgBinode(_, l, r) =>
                    traverseTotal(l); traverseTotal(r)
                  case CcgUnode(_, s) =>
                    traverseTotal(s)
                  case CcgLeaf(_, _, _) =>
                    supertagTotal += 1
                }
              }
              traverseTotal(goldTree)
          }

          dependencyTotal += (goldDepMap.size + 1)
          supertagTotal += goldTree.length
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
