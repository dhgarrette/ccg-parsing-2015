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
import dhg.ccg.parse.inf.SimpleInfCatPrior
import org.apache.commons.math3.random.MersenneTwister
import dhg.ccg.util.IndirectSparseVec
import org.apache.commons.math3.random.SynchronizedRandomGenerator
import dhg.ccg.tag.learn._
import dhg.condor.Condor

/**
 * target/start dhg.ccg.parse.scg.expi.SerializeCondor 0308
 * target/start dhg.ccg.parse.scg.expi.RunCondor 0308 8
 * target/start dhg.ccg.parse.scg.exp.ScgExp results 0308
 */
object RunCondor {
  type Word = String

  def main(args: Array[String]): Unit = {
    val argString = args.mkString(" ")
    println(argString)

    //    val (arguments, options_) = parseArgs(args)
    //    val options = CommandLineOptions(options_)
    val arguments = args.toVector

    //val Vector(name, model, lang, UInt(rawTok), UInt(tdTok), ctxPrior, UDouble(combinableMultiplier), UInt(alphaCtx)) = arguments
    val Vector(name, "--lang", lang, "--mem", mem) = arguments.toVector

    new Condor(pathjoin("condorfiles", name), Some(mem.toInt * 1000)).makeWithNames {
      argLists(lang).map { args =>
        ("dhg.ccg.parse.scg.expi.Run", (Vector(name) ++ args.dropRight(2)).mkString(" "), args.grouped(2).map(_.last).mkString("_"))
      }
    }
  }

  /*
   * English: tdData: 275084 tokens, 14630 sentences, max size 32
   * Chinese: tdData: 110403 tokens,  6923 sentences, max size 64
   * Italian: tdData:  26593 tokens,  1137 sentences, max size 32
   */
  def argLists(lang: String): Vector[Vector[String]] = {
    for {
      number <- Vector("a")//, "b", "c", "d", "e")
      catPriorOpt <- Vector("cat", "uni")
      ctxPrior <- Vector("com", "uni", "cac")
      maxSentLenStr <- Vector("50") // lang match { case "ch" => Vector("50"); case _ => Vector("inf") }
      rawTok <- Vector("100") //, "200")
      testSent <- Vector("inf") // 1000) // , 2000, 3000, "inf")
      samplingIterations <- Vector(50).map(_.toString)
      burninIterations <- Vector(50, 0).map(_.toString)
      tdTok <- Vector(250, 200, 150, 100, 50, 25).map(n => f"$n%03d") // :+ "inf"
      model <- Vector("scg", "pcfg")
      combinableMultiplier <- Vector("10k", "1k", "100k", "-1")
      alphaCtx <- Vector("1k", "100", "10k", "-1")
      treesPerIteration <- Vector(-1, 1).map(_.toString) //, 10)

      if !(catPriorOpt == "uni") || (model == "pcfg")
      if !(model == "pcfg") || (ctxPrior == "uni")
      if (model == "pcfg") == (combinableMultiplier == "-1")
      if (model == "pcfg") == (alphaCtx == "-1")
      if (model == "pcfg") == (treesPerIteration == "-1")

      if !(lang == "en") || (tdTok.toInt < 276)
      if !(lang == "ch") || (tdTok.toInt < 111)
      if !(lang == "it") || (tdTok.toInt < 27)

      // TEMP:
      //if !(model == "scg") || (ctxPrior != "uni")

    } yield {
      Vector(
        "--lang", lang, //                                            serialized
        "--rawTok", rawTok, //                                        serialized
        "--testSent", testSent, //                                    serialized
        "--tdTok", tdTok, //                                          serialized
        "--maxSentLen", maxSentLenStr, //                             serialized
        "--model", model,
        "--samplingIter", samplingIterations,
        "--burninIter", burninIterations,
        "--treesPerIteration", treesPerIteration,
        "--catPrior", catPriorOpt,
        "--ctxPrior", ctxPrior,
        "--combinableMultiplier", combinableMultiplier,
        "--alphaCtx", alphaCtx,
        "--num", number)
    }
  }
}

