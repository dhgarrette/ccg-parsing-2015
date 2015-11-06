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
 * target/start dhg.ccg.parse.scg.expi.SerializeParts1Condor 0301 --lang la
 * target/start dhg.ccg.parse.scg.expi.SerializeParts2Condor 0301 --lang la
 * target/start dhg.ccg.parse.scg.expi.SerializeParts3Condor 0301 --lang la
 */
object SerializeParts1Condor {
  type Word = String

  def main(args: Array[String]): Unit = {
    val argString = args.mkString(" ")
    println(argString)

    //    val (arguments, options_) = parseArgs(args)
    //    val options = CommandLineOptions(options_)
    val arguments = args.toVector

    //val Vector(name, model, lang, UInt(rawTok), UInt(tdTok), ctxPrior, UDouble(combinableMultiplier), UInt(alphaCtx)) = arguments
    val Vector(name, "--lang", lang) = arguments

    new Condor(pathjoin("condorfiles", name), Some(24000)).makeWithNames {
      RunCondor.argLists(lang).flatMap { runArgs =>
        val baseArgs = runArgs.take(10)
        for {
          part <- Vector("raw", "tst")
          partNum <- (0 until (100000 / 5 / SerializeParts1.groupSize))
        } yield {
          val addlArgs = Vector("--part", part, "--partNum", f"$partNum%05d")

          val args = baseArgs ++ addlArgs
          ("dhg.ccg.parse.scg.expi.SerializeParts1", (Vector(name) ++ args).mkString(" "), "sp1_" + args.grouped(2).map(_.last).mkString("_"))
        }
      }.distinct
    }
  }
}

