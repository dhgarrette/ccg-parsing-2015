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

/**
 * target/start dhg.ccg.parse.scg.expi.SerializeParts 0301 --lang la
 */
object SerializeParts3 {
  type Word = String

  val takeMax = Int.MaxValue
  val maxCatSize = Int.MaxValue // 16

  def serializationArgFilenameString(lang: String, rawTokStr: String, testSentStr: String, tdTokStr: String, maxSentLenStr: String) = {
    f"${lang}_rawtok${rawTokStr}_testsent${testSentStr}_tdtok${tdTokStr}_maxSentLen$maxSentLenStr"
  }

  def getPath(name: String, argFilenameString: String) = {
    val dir = f"data/gcser/$name"
    File(dir).mkdirs()
    f"$dir/$argFilenameString"
  }

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
    val fn = f"$path-$part-part$partNum%05d"
    if (File(f"$fn-cats.txt").exists) {
      val newCatIndexer = time(f"deserializing new cat indexer from $path-cats.txt", new SimpleIndexer(File(f"$path-cats.txt").readLines.map(NonRemovingCcgBankCatInterner.apply).toVector))
      val newWrdIndexer = time(f"deserializing new wrd indexer from $path-wrds.txt", new SimpleIndexer(File(f"$path-wrds.txt").readLines.toVector))

      val oldCatIndexer = time(f"deserializing old cat indexer from $fn-cats.txt", new SimpleIndexer(File(f"$fn-cats.txt").readLines.map(NonRemovingCcgBankCatInterner.apply).toVector))
      val oldWrdIndexer = time(f"deserializing old wrd indexer from $fn-wrds.txt", new SimpleIndexer(File(f"$fn-wrds.txt").readLines.toVector))

      val gcGroup: Vector[(Option[CfgGuideChartI], Option[CcgTreeI])] =
        time(f"deserializing gcv from $fn.gcv.gz", CfgGuideChartI.readVector(f"$fn.gcv.gz"))

      if (gcGroup.nonEmpty) {
        val words: Array[Int] = ???
        val gciGroup: Vector[(Option[CfgGuideChartI], Option[CcgTreeI])] =
          time(f"translating gcv to gciv", translateIndexed(words, gcGroup, oldCatIndexer, oldWrdIndexer, newCatIndexer, newWrdIndexer))

        time(f"serializing gciv to $fn.gciv.gz", CfgGuideChartI.writeVector(gciGroup, f"$fn.gciv.gz"))
      }
      else println(f"$fn.gcv.gz was empty.  skipping.")
    }
    else println(f"part=$part, partNum=$partNum  does not exist.  skipping.")

  }

  def translateIndexed(words: Array[Int], gcv: Vector[(Option[CfgGuideChartI], Option[CcgTreeI])],
    oldCatIndexer: Indexer[Cat], oldWrdIndexer: Indexer[Word],
    newCatIndexer: Indexer[Cat], newWrdIndexer: Indexer[Word]) = {
    gcv.par.mapt { (gc, t) =>
      (gc.map(translateIndexedGC(words, _, oldCatIndexer, oldWrdIndexer, newCatIndexer, newWrdIndexer)),
        t.map(translateIndexedTree(_, oldCatIndexer, oldWrdIndexer, newCatIndexer, newWrdIndexer)))
    }.seq
  }

  def translateIndexedGC(words: Array[Int], gci: CfgGuideChartI,
    oldCatIndexer: Indexer[Cat], oldWrdIndexer: Indexer[Word],
    newCatIndexer: Indexer[Cat], newWrdIndexer: Indexer[Word]) = {
    CfgGuideChartI(
      words, 
      Chart.tabulate(gci.length) { (i, j) =>
      gci(i, j).toArray.map { case (t, entries) => newCatIndexer(oldCatIndexer.obj(t)) -> entries.map(e => GuideChartEntryI.to(GuideChartEntryI.from(e, oldCatIndexer, oldWrdIndexer), newCatIndexer, newWrdIndexer)).toArray }
    }, gci.rootSet.map(t => newCatIndexer(oldCatIndexer.obj(t))))
  }

  def translateIndexedTree(ti: CcgTreeI,
    oldCatIndexer: Indexer[Cat], oldWrdIndexer: Indexer[Word],
    newCatIndexer: Indexer[Cat], newWrdIndexer: Indexer[Word]) = {
    CcgTreeI.to(CcgTreeI.from(ti, oldCatIndexer, oldWrdIndexer), newCatIndexer, newWrdIndexer)
  }

}

