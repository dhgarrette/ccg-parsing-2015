//package dhg.ccg.dd
//
//import dhg.ccg.cat._
//import dhg.ccg.parse.pcfg._
//import dhg.ccg.prob._
//import dhg.ccg.tagdict.SimpleTagDictionary
//import dhg.ccg.tagdict.TagDictionary
//import dhg.util._
//import scala.math.{ log, exp, pow }
//import scalaz._
//import Scalaz._
//import dhg.ccg.tag._
//import dhg.ccg.parse._
//import dhg.ccg.parse.pcfg.OldToRemoveCfgGuideChartBuilder.GuideTable
//import java.util.concurrent.atomic.AtomicInteger
//
//class DualParser(
//  val parser: WeightedParser,
//  val tagger: WeightedTagger[Cat],
//  tagdict: TagDictionary[Cat],
//  guideChartBuilder: OldToRemoveCfgGuideChartBuilder,
//  maxIterations: Int = 10,
//  deltaConst: Double = 1.0,
//  verbose: Boolean = false)
//  extends Parser {
//
//  private[this] val convergences = new AtomicInteger(0)
//  private[this] val totaldecodes = new AtomicInteger(0)
//
//  override def parseAndProb(sentence: Vector[Word], brackets: Vector[(Int, Int)]): Option[(CcgTree, LogDouble)] = {
//    guideChartBuilder.build(sentence, brackets, tagdict).flatMap(parseAndProbFromGuideChart)
//  }
//
//  override def parseAndProbFromGuideChart(guideChart: GuideTable): Option[(CcgTree, LogDouble)] = {
//    val n = guideChart.length
//    def iterate(k: Int, us: Vector[Map[Cat, LogDouble]], prevL: LogDouble, lagrangianIncreases: Int): Option[(CcgTree, LogDouble)] = {
//      val startTime = System.currentTimeMillis()
//      val umat = Vector.tabulate(n, n + 1)((i, j) => if (j == i + 1) us(i) else Map.empty[Cat, LogDouble])
//      parser.parseAndProbWithWeightsFromGuideChart(guideChart, umat).flatMap {
//        case (tree, pTree) =>
//          assert(pTree.nonZero, f"pTree is zero!  ${tree.supertags.mkString(" ")}")
//          val y = tree.supertags
//          val sentenceWithTags = (0 until n).map { i =>
//            val cell = guideChart(i)(i + 1).get
//            val word = cell.values.head.head._2 match { case TermProd(word) => word }
//            val supertags = cell.keySet.toSet
//            (word, supertags)
//          }.toVector
//          val (z, pTags) = tagger.tagAndProbWithWeightsFromTagSet(sentenceWithTags, us)
//          assert(pTags.nonZero, f"pTags is zero!  ${z.mkString(" ")}")
//          val delta = new LogDouble(deltaConst / lagrangianIncreases) // NOTE: Based on Rush&Collins advice 
//          val (newUs, newMatches) =
//            ((y zipSafe z) zipSafe us).foldLeft((Vector.empty[Map[Cat, LogDouble]], true)) {
//              case ((rus, matches), ((ty, tz), u)) =>
//                val (newU, newMatches) =
//                  if (ty == tz) (u, matches)
//                  else {
//                    val newU = u
//                      .updated(ty, u.getOrElse(ty, LogDouble.one) / delta)
//                      .updated(tz, u.getOrElse(tz, LogDouble.one) * delta)
//                    (newU, false)
//                  }
//                (rus :+ newU, newMatches)
//            }
//          //          println(f"    k=${(k + ":").padRight(4)} ${(System.currentTimeMillis() - startTime) / 1000.0}%.3f sec")
//          //          println(f"        d = ${delta.logValue}%.4f")
//          //          println(f"        u = ${us.map(_.mapVals(u => f"${u.logValue}%.4f")).mkString(",  ")}")
//          //          println(f"        y = ${pTree.logValue}%.2f : ${y.mkString(" ")}")
//          //          println(f"        z = ${pTags.logValue}%.2f : ${z.mkString(" ")}")
//          if (newMatches || k >= maxIterations) {
//            if (newMatches) convergences.incrementAndGet
//            Some((tree, pTree)) // matches. stop.
//          }
//          else {
//            val l = pTree * pTags // Lagrangian object L(u)
//            iterate(k + 1, newUs, l, lagrangianIncreases + (if (l > prevL) 1 else 0)) // next iteration
//          }
//      }
//    }
//    //println(f"    begin dual decomp iterations")
//    val us = Vector.fill(n)(Map.empty[Cat, LogDouble])
//    val tree = iterate(1, us, LogDouble.one, 1)
//    //    tree.foreach { case (t,p) => println(t.tagged) }
//    totaldecodes.incrementAndGet
//    if (verbose && totaldecodes.get % 100 == 0) println(f"DualParser: ${convergences.get}/${totaldecodes.get} = ${convergences.get * 100.0 / totaldecodes.get}")
//    tree
//  }
//}
