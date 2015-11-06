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
 * target/start dhg.ccg.parse.scg.expi.SerializeParts2 0301 --lang la
 */
object SerializeParts2 {
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
      "--maxSentLen", maxSentLenStr) = arguments

    val argFilenameString = serializationArgFilenameString(lang, rawTokStr, testSentStr, tdTokStr, maxSentLenStr)
    val path = getPath(name, argFilenameString)
    val dir = f"data/gcser/$name"
    val allCats = File(dir).ls(f"$argFilenameString-(raw|tst)-part\\d+-cats.txt".r).flatMap { f =>
      println(f"  reading cats from ${f.path}")
      f.readLines
    }.toSet.map(NonRemovingCcgBankCatInterner.fromString)
    val allWrds = File(dir).ls(f"$argFilenameString-(raw|tst)-part\\d+-wrds.txt".r).flatMap { f =>
      println(f"  reading words from ${f.path}")
      f.readLines
    }.toSet
    assert(allCats.nonEmpty)
    assert(allWrds.nonEmpty)
    val baseCatIndexer = new SimpleIndexer[Cat](Vector(StartCat, EndCat, DeleteFromLeftCat, DeleteFromRightCat))
    val baseWordIndexer = new SimpleIndexer[Word](Vector("<S>", "<E>"))
    val newCatIndexer = baseCatIndexer.append(allCats)
    val newWordIndexer = baseWordIndexer.append(allWrds)

    time(f"serializing full cat indexer to $path-cats.txt", writeUsing(File(f"$path-cats.txt")) { f => newCatIndexer.objects.foreach(f.wl) })
    time(f"serializing full wrd indexer to $path-wrds.txt", writeUsing(File(f"$path-wrds.txt")) { f => newWordIndexer.objects.foreach(f.wl) })

  }
}

