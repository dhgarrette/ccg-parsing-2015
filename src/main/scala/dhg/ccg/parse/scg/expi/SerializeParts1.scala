package dhg.ccg.parse.scg.expi

import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.dep._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.parse.pcfg.typesup._
import dhg.ccg.parse.scg._
import dhg.ccg.parse.scg.mcmc._
import dhg.ccg.data._
import dhg.util._
import dhg.util.viz._
import dhg.ccg.parse._
import scalaz._
import scalaz.Scalaz._
import dhg.ccg.tagdict.SimpleTagDictionaryFactory
import dhg.ccg.rule._
import dhg.ccg.util.SimpleIndexer
import dhg.ccg.util._
import scala.collection.parallel.immutable.ParVector
import scala.collection.GenTraversableOnce
import dhg.ccg.parse.scg.expi.Serialize._

/**
 * target/start dhg.ccg.parse.scg.expi.SerializeParts1 0301 --lang la
 */
object SerializeParts1 {
  type Word = String

  val takeMax = Int.MaxValue
  val maxCatSize = Int.MaxValue // 16

  val groupSize = 100

  def main(args: Array[String]): Unit = {
    val argString = args.mkString(" ")
    println(argString)

    //    val (arguments, options_) = parseArgs(args)
    //    val options = CommandLineOptions(options_)
    val arguments = args.toVector

    val Vector(name,
      "--lang", lang,
      "--rawTok", rawTokStr,
      "--testSent", testSentStr,
      "--tdTok", tdTokStr,
      "--maxSentLen", maxSentLenStr,
      "--part", part,
      "--partNum", UInt(partNum)) = arguments

    val argFilenameString = serializationArgFilenameString(lang, rawTokStr, testSentStr, tdTokStr, maxSentLenStr)
    val path = getPath(name, argFilenameString)

    val reader = getReader(lang, maxSentLenStr)

    //

    part match {
      case "raw" =>
        val sentences = reader.raw

        val numTokens = rawTokStr match { case UInt(n) => n * 1000; case "inf" | "max" => Int.MaxValue }
        val sents: Vector[(Vector[Word], Int)] =
          sentences.take(takeMax).zipWithIndex
            .groupedAsVector(groupSize)
            .drop(partNum).next

        if (sents.nonEmpty) {
          val (trainingGcBuilder, parsingGcBuilder, tagdict) = getGcBuilders(reader, tdTokStr)
          val gcGroup: Vector[(Option[CfgGuideChart], Option[CcgTree])] =
            sents.par.flatMap { case (s, i) => time1(f"    raw $i", trainingGcBuilder.build(s, None, tagdict)) }.map(gc => (some(gc), none[CcgTree])).seq

          if (gcGroup.nonEmpty) {
            val catIndexer = new SimpleIndexer[Cat](Vector(StartCat, EndCat, DeleteFromLeftCat, DeleteFromRightCat)).append(gcGroup.flatMap(_._1).flatMap(_.allCats).toSet)
            val wrdIndexer = new SimpleIndexer[Word](Vector("<S>", "<E>")).append(gcGroup.flatMap(_._1).flatMap(_.words).toSet)

            val fn = f"$path-raw-part$partNum%05d"
            time(f"serializing raw gcv to $fn.gcv.gz", CfgGuideChartI.writeVector(gcGroup.mapt { (gc, t) => (gc.map(CfgGuideChartI.to(_, catIndexer, wrdIndexer)), t.map(CcgTreeI.to(_, catIndexer, wrdIndexer))) }, f"$fn.gcv.gz"))

            println(f"raw gc:         ${gcGroup.flatMap(_._1).sumBy(_.length)} tokens, ${gcGroup.size} sentences, max sent len = ${max(gcGroup.flatMap(_._1).map(_.length))}")

            time(f"serializing cat indexer to $fn-cats.txt", writeUsing(File(f"$fn-cats.txt")) { f => catIndexer.objects.foreach(f.wl) })
            time(f"serializing wrd indexer to $fn-wrds.txt", writeUsing(File(f"$fn-wrds.txt")) { f => wrdIndexer.objects.foreach(f.wl) })
          }
          else println(f"part=$part, partNum=$partNum  has no viable guide charts.  skipping.")
        }
        else println(f"part=$part, partNum=$partNum  has no sentences.  skipping.")

      case "tst" =>
        val sentences = reader.testData // .devData

        val sents: Vector[(CcgTree, Int)] =
          sentences.take(takeMax).zipWithIndex
            .take(testSentStr match { case UInt(n) => n; case "inf" | "max" => Int.MaxValue })
            .groupedAsVector(groupSize)
            .drop(partNum).next

        if (sents.nonEmpty) {
          val (trainingGcBuilder, parsingGcBuilder, tagdict) = getGcBuilders(reader, tdTokStr)
          val gcGroup: Vector[(Option[CfgGuideChart], Option[CcgTree])] =
            sents.par.map { case (t, i) => time1(f"    tst $i", (parsingGcBuilder.build(t.words, None, tagdict), some(t))) }.seq

          if (gcGroup.nonEmpty) {
            val catIndexer = new SimpleIndexer[Cat](Vector(StartCat, EndCat, DeleteFromLeftCat, DeleteFromRightCat)).append(gcGroup.flatMap(_._2.get.allCats).toSet ++ gcGroup.flatMap(_._1).flatMap(_.allCats))
            val wrdIndexer = new SimpleIndexer[Word](Vector("<S>", "<E>")).append(gcGroup.flatMap(_._2.get.words).toSet)

            val fn = f"$path-tst-part$partNum%05d"
            time(f"serializing tst gcv to $fn.gcv.gz", CfgGuideChartI.writeVector(gcGroup.mapt { (gc, t) => (gc.map(CfgGuideChartI.to(_, catIndexer, wrdIndexer)), t.map(CcgTreeI.to(_, catIndexer, wrdIndexer))) }, f"$fn.gcv.gz"))

            println(f"test gc:        ${gcGroup.sumBy(_._2.get.length)} tokens, ${gcGroup.size} sentences, max sent len = ${max(gcGroup.map(_._2.get.length))}")
            println(f"usable test gc: ${gcGroup.flatMap(_._1).sumBy(_.length)} tokens, ${gcGroup.count(_._1.isDefined)} sentences, max sent len = ${max(gcGroup.flatMap(_._1).map(_.length))}")

            time(f"serializing cat indexer to $fn-cats.txt", writeUsing(File(f"$fn-cats.txt")) { f => catIndexer.objects.foreach(f.wl) })
            time(f"serializing wrd indexer to $fn-wrds.txt", writeUsing(File(f"$fn-wrds.txt")) { f => wrdIndexer.objects.foreach(f.wl) })
          }
          else println(f"part=$part, partNum=$partNum  has no viable guide charts.  skipping.")
        }
        else println(f"part=$part, partNum=$partNum  has no sentences.  skipping.")

    }

  }

  def max(xs: GenTraversableOnce[Int]) = if (xs.nonEmpty) xs.max else -1

}

