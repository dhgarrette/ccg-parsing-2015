//package dhg.ccg.parse
//
//import dhg.ccg.cat.Cat
//import dhg.util._
//import dhg.ccg.tagdict.TagDictionary
//import dhg.ccg.cat._
//
//class ParserEvaluator(guideChartBuilder: OldToRemoveCfgGuideChartBuilder, goldSupertags: Boolean = false) {
//
//  def evaluate(model: Parser, testData: TraversableOnce[CcgTree], tagdict: TagDictionary[Cat],
//    printFailedParses: Boolean = false,
//    verbose: Boolean = false) = {
//    val parseCounter = new OldToRemoveCfgParseCounter(guideChartBuilder)
//    
//    var constituentCorrect = 0
//    var constituentTotal = 0
//    var constituentParsedTotal = 0
//
//    var supertagCorrect = 0
//    var supertagTotal = 0
//    var supertagParsedTotal = 0
//
//    var parseFound = 0
//    var fullParseCorrect = 0
//    var totalSentences = 0
//
//    val errors = collection.mutable.Map.empty[(Option[Cat], Option[Cat]), Int] // (gold, model) -> count
//    val failures = collection.mutable.Set.empty[(CcgTree, CcgTree)] // (gold, model)
//
//    var totalTime = 0L
//    time("testing", {
//      for ((goldTree, i) <- testData.toIterator.zipWithIndex) {
//        if (verbose) print(f"${i + 1}%5s, length=${goldTree.length}%3d,  avg-ambig=${goldTree.words.map(tagdict(_).size).avg}%5.2f,  num-parses=${parseCounter.countParses(goldTree.words, Vector.empty, tagdict)}%18s,  ")
//        val startTime = System.currentTimeMillis()
//        val modelParsed =
//          if (goldSupertags)
//            model.parseFromSupertags(goldTree.tagged)
//          else
//            model.parse(goldTree.words)
//        val parseTime = System.currentTimeMillis() - startTime
//        totalTime += parseTime
//        if (verbose) println(f"time=${parseTime / 1000.0}%.3f sec,  avg time: ${totalTime / 1000.0 / (i + 1)}%.3f sec")
//        if ((i + 1) % 100 == 0) println(f"${i + 1}%5s, total time: ${totalTime / 1000.0}%.3f sec, avg time: ${totalTime / 1000.0 / (i + 1)}%.4f sec")
//        val n = goldTree.length
//
//        val goldNtTable = CcgTree.toNtTable(goldTree)
//
//        def traverse(t: CcgTree, i: Int, j: Int): Boolean = {
//          val gl = goldNtTable(i)(j)
//          val ml = Some(t.cat)
//          val correct = gl == ml
//          if (correct) { constituentCorrect += 1 }
//
//          constituentTotal += 1
//          constituentParsedTotal += 1
//
//          t match {
//            case CcgLeaf(cat, word, _) =>
//              assert(i + 1 == j)
//              if (correct) { supertagCorrect += 1 }
//              else { errors((gl, ml)) = errors.getOrElse((gl, ml), 0) + 1 }
//              supertagTotal += 1
//              supertagParsedTotal += 1
//              correct
//
//            case CcgNode(cat, l, r) =>
//              assert(i + 1 < j)
//
//              val li = i
//              val lj = i + l.length
//              val ri = i + l.length
//              val rj = j
//
//              assert(li + l.length == lj, f"i=$i, j=$j: li=$li, lj=$lj, l.length=${l.length}")
//              assert(ri + r.length == rj, f"i=$i, j=$j: ri=$ri, rj=$rj, r.length=${r.length}")
//
//              assert(rj == i + t.length, f"i=$i, j=$j: rj=$rj, t.length=${t.length}")
//              assert(ri == rj - r.length, f"i=$i, j=$j: t.length=${t.length}, r.length=${r.length}")
//
//              val lmatch = traverse(l, li, lj)
//              val rmatch = traverse(r, ri, rj)
//              correct && lmatch && rmatch
//          }
//        }
//
//        modelParsed match {
//          case Some(modelParse) =>
//            assert(modelParse.length == n, f"\n${goldTree.words.mkString(" ")}\n${modelParse.words.mkString(" ")}")
//            val fullTreeCorrect = traverse(modelParse, 0, n)
//            if (fullTreeCorrect) { fullParseCorrect += 1 }
//            else { failures.add(goldTree, modelParse) }
//            parseFound += 1
//          case None =>
//            def traverseTotal(t: CcgTree): Unit = {
//              constituentTotal += 1
//              t match {
//                case CcgNode(_, l, r) =>
//                  traverseTotal(l); traverseTotal(r)
//                case CcgLeaf(_, _, _) =>
//                  supertagTotal += 1
//              }
//            }
//            traverseTotal(goldTree)
//            if(printFailedParses) {
//              println(f"\nFailed parse:")
//              for(w <- goldTree.words){
//                println(f"  $w : ${tagdict(w).mkString(", ")}")
//              }
//              println(goldTree.pretty)
//              println
//            }
//        }
//
//        totalSentences += 1
//      }
//    })
//    println(f"Parse Found: ${parseFound * 100.0 / totalSentences}%.2f  ($parseFound/$totalSentences)")
//    println(f"Parse Found Supertag Accuracy: ${supertagCorrect * 100.0 / supertagParsedTotal}%.2f  ($supertagCorrect/$supertagParsedTotal)")
//    println(f"Parse Found Constituent Accuracy: ${constituentCorrect * 100.0 / constituentParsedTotal}%.2f  ($constituentCorrect/$constituentParsedTotal)  - includes terminal constituents")
//    println(f"Full Parse Accuracy: ${fullParseCorrect * 100.0 / totalSentences}%.2f  (${fullParseCorrect}/${totalSentences})")
//    println(f"Supertag Accuracy: ${supertagCorrect * 100.0 / supertagTotal}%.2f  ($supertagCorrect/$supertagTotal)")
//    val accuracy = constituentCorrect * 100.0 / constituentTotal
//    println(f"Constituent Accuracy: ${accuracy}%.2f  ($constituentCorrect/$constituentTotal)  - includes terminal constituents")
//    val errorsToShow = errors.desc.take(10).map { case ((gt, mt), count) => (count.toString, gt.fold("None")(_.toString), mt.fold("None")(_.toString)) }
//    val (maxCountWidth, maxTagWidth) =
//      if (errorsToShow.isEmpty) (0, 0)
//      else {
//        val a = errorsToShow.map(_._1.length).max max 5
//        val b = errorsToShow.map { case (_, gt, mt) => gt.size }.max
//        (a, b)
//      }
//    println(f" ${"count".padLeft(maxCountWidth)}  ${"gold".padRight(maxTagWidth)}  ${"model"}")
//    for ((count, gt, mt) <- errorsToShow) {
//      println(f" ${count.padLeft(maxCountWidth)}  ${gt.padRight(maxTagWidth)}  ${mt}")
//    }
//    println(f"avg tagging: ${totalTime / 1000.0 / testData.size}%.4f sec")
//
//    //    failures.toVector.sortBy(_._1.length).foreach {
//    //      case (g, m) =>
//    //        TreeViz.drawTree(g)
//    //        TreeViz.drawTree(m)
//    //    }
//
//    accuracy
//  }
//
//  override def toString = f"ParserEvaluator(${guideChartBuilder}, goldSupertags=$goldSupertags)"
//
//}
