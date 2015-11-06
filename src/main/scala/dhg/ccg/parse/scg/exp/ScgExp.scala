package dhg.ccg.parse.scg.exp

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
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import scalaz._
import Scalaz._
import scala.collection.breakOut
import scala.annotation.tailrec
import java.io.FileOutputStream
import java.io.FileInputStream
import dhg.condor.Condor
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tagdict.SimpleTagDictionaryFactory
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.tag.HmmTagger
import dhg.ccg.tag.WeightedTagger
import dhg.ccg.tag.SemisupervisedTaggerTrainer
import dhg.ccg.tag.AddLambdaSmoothedHmmTaggerTrainer
import dhg.ccg.tag.TaggerEvaluator
import dhg.ccg.tag.learn.AddLambdaTransitionDistributioner
import dhg.ccg.tag.learn.AddLambdaEmissionDistributioner
import dhg.ccg.tag.learn.NormalizingCatgramCatPriorInitializer
import dhg.ccg.tag.learn.CcgCombinabilityTrInitializer
import dhg.ccg.tag.learn.CheatingTagPriorInitializer
import dhg.ccg.tag.learn.EmissionInitializer
import dhg.ccg.tag.learn.EmTagDictionaryEstimate
import dhg.ccg.tag.learn.EmUniform
import dhg.ccg.tag.learn.FfbsHmmTaggerTrainer
import dhg.ccg.tag.learn.TagdictInformedAtomCatDistInitializer
import dhg.ccg.tag.learn.TrTagDictEntriesPossibilities
import dhg.ccg.tag.learn.TrUniform
import dhg.ccg.tag.learn.UniformAtomCatDistInitializer
import dhg.ccg.tag.learn.UniformTagPriorInitializer
import dhg.ccg.tagdict.SimpleStartEndTags
import dhg.ccg.tagdict.TagDictionaryFactory
import scala.collection.immutable.ListMap

/**
 * target/start dhg.ccg.parse.scg.exp.ScgExp condor scg serialize
 * target/start dhg.ccg.parse.scg.exp.ScgExp condor scg 0206
 * target/start dhg.ccg.parse.scg.exp.ScgExp results 0206
 *
 * //
 *
 * target/start dhg.ccg.parse.scg.exp.ScgExp serialize
 *
 * target/start dhg.ccg.parse.scg.exp.ScgExp condor run1 standard
 * target/start dhg.ccg.parse.scg.exp.ScgExp results run1
 *
 * target/start dhg.ccg.parse.scg.exp.ScgExp --model scg --learning mcmc --cat-prior unicatgram --root-init catprior --nt-prod-init catprior --term-prod-init uniform --tr-init catprior-combine --num 24 --add-st-train no --add-st-parse no,fbmod --sampling-iterations 20 --burnin-iterations 10 --takemax 100
 */
object ScgExp {
  import ScgExp.BooleanWithImplication

  val DependencyAccuracyLine = "^Dependency Accuracy: ([0-9.]+)".r.unanchored
  val ConstituentAccuracyLine = "^Constituent Accuracy: ([0-9.]+)".r.unanchored
  val DepSpanAccuracyLine = "^Constituent f1:\\s*([0-9.]+)".r.unanchored

  def main(args: Array[String]): Unit = {
    val argString = args.mkString(" ")
    println(argString)

    val (arguments, options) = parseArgs(args)

    //val np2n = options.b("np2n", false); assert(!np2n)

    //

    /**
     * --model: pcfg, scg, dd
     * --learning: sup, mcmc
     * --cat-prior: uniform, unicatgram, corpcatgram
     * --prod-dist: uniform, interp
     * --term-prod-init: uniform, tdentry
     * --combine: false, true, true-noprior, false-noprior
     * --tr-init: none, uniform, tdentry    (none=catprior-only)
     */
    arguments match {
      // case Seq("serialize") =>
      // println("serializing only")

      // val cliPrefix = "--model pcfg --learning mcmc  --cat-prior uniform      --root-init uniform   --nt-prod-init uniform   --term-prod-init uniform  --tr-init x  --additional-rules x  --just-serialize true"
      // val cliStrings =
      //   (for {
      //     b <- (0 to 100 by 25)
      //     d <- (0 to 0 by 25)
      //     bp = b / 100.0
      //     dp = d / 100.0
      //     bs = if (b == 100) "99" else f"$b"
      //     ds = if (d == 100) "99" else f"$d"
      //     s <- Vector(
      //       cliPrefix ++ f"--train-bracket-prop 0.00                         --train-dep-prop 0.0 --use-serialized-gc false --train-serialized-gc train-nv-b00-d00-gc    --test-serialized-gc test-nv-fb-b00-d00-gc     --sampling-iterations 0 --burnin-iterations 0  --num b00-d00-serialize",
      //       cliPrefix ++ f"--train-bracket-prop 0.00 --train-bracket-cats NP --train-dep-prop 0.0 --use-serialized-gc false --train-serialized-gc train-nv-b00-np-d00-gc --test-serialized-gc test-nv-fb-b00-np-d00-gc  --sampling-iterations 0 --burnin-iterations 0  --num b00-np-d00-serialize")
      //   } yield {
      //     s
      //   }).toVector

      // //        val cliStrings = Vector(
      // //          f"--model pcfg --learning mcmc  --cat-prior uniform      --root-init uniform   --nt-prod-init uniform   --term-prod-init uniform  --tr-init x  --additional-rules x  --train-bracket-prop 0.00                         --train-dep-prop 0.0 --use-serialized-gc false --train-serialized-gc train-nv-b00-d00-gc    --test-serialized-gc test-nv-fb-b00-d00-gc     --sampling-iterations 0 --burnin-iterations 0  --num b00-d00-serialize",
      // //          f"--model pcfg --learning mcmc  --cat-prior uniform      --root-init uniform   --nt-prod-init uniform   --term-prod-init uniform  --tr-init x  --additional-rules x  --train-bracket-prop 0.25                         --train-dep-prop 0.0 --use-serialized-gc false --train-serialized-gc train-nv-b25-d00-gc    --test-serialized-gc test-nv-fb-b25-d00-gc     --sampling-iterations 0 --burnin-iterations 0  --num b25-d00-serialize",
      // //          f"--model pcfg --learning mcmc  --cat-prior uniform      --root-init uniform   --nt-prod-init uniform   --term-prod-init uniform  --tr-init x  --additional-rules x  --train-bracket-prop 0.50                         --train-dep-prop 0.0 --use-serialized-gc false --train-serialized-gc train-nv-b50-d00-gc    --test-serialized-gc test-nv-fb-b50-d00-gc     --sampling-iterations 0 --burnin-iterations 0  --num b50-d00-serialize",
      // //          f"--model pcfg --learning mcmc  --cat-prior uniform      --root-init uniform   --nt-prod-init uniform   --term-prod-init uniform  --tr-init x  --additional-rules x  --train-bracket-prop 0.75                         --train-dep-prop 0.0 --use-serialized-gc false --train-serialized-gc train-nv-b75-d00-gc    --test-serialized-gc test-nv-fb-b75-d00-gc     --sampling-iterations 0 --burnin-iterations 0  --num b75-d00-serialize",
      // //          f"--model pcfg --learning mcmc  --cat-prior uniform      --root-init uniform   --nt-prod-init uniform   --term-prod-init uniform  --tr-init x  --additional-rules x  --train-bracket-prop 1.00                         --train-dep-prop 0.0 --use-serialized-gc false --train-serialized-gc train-nv-b99-d00-gc    --test-serialized-gc test-nv-fb-b99-d00-gc     --sampling-iterations 0 --burnin-iterations 0  --num b99-d00-serialize",
      // //
      // //          f"--model pcfg --learning mcmc  --cat-prior uniform      --root-init uniform   --nt-prod-init uniform   --term-prod-init uniform  --tr-init x  --additional-rules x  --train-bracket-prop 0.00 --train-bracket-cats NP --train-dep-prop 0.0 --use-serialized-gc false --train-serialized-gc train-nv-b00-np-d00-gc --test-serialized-gc test-nv-fb-b00-np-d00-gc  --sampling-iterations 0 --burnin-iterations 0  --num b00-np-d00-serialize",
      // //          f"--model pcfg --learning mcmc  --cat-prior uniform      --root-init uniform   --nt-prod-init uniform   --term-prod-init uniform  --tr-init x  --additional-rules x  --train-bracket-prop 0.25 --train-bracket-cats NP --train-dep-prop 0.0 --use-serialized-gc false --train-serialized-gc train-nv-b25-np-d00-gc --test-serialized-gc test-nv-fb-b25-np-d00-gc  --sampling-iterations 0 --burnin-iterations 0  --num b25-np-d00-serialize",
      // //          f"--model pcfg --learning mcmc  --cat-prior uniform      --root-init uniform   --nt-prod-init uniform   --term-prod-init uniform  --tr-init x  --additional-rules x  --train-bracket-prop 0.50 --train-bracket-cats NP --train-dep-prop 0.0 --use-serialized-gc false --train-serialized-gc train-nv-b50-np-d00-gc --test-serialized-gc test-nv-fb-b50-np-d00-gc  --sampling-iterations 0 --burnin-iterations 0  --num b50-np-d00-serialize",
      // //          f"--model pcfg --learning mcmc  --cat-prior uniform      --root-init uniform   --nt-prod-init uniform   --term-prod-init uniform  --tr-init x  --additional-rules x  --train-bracket-prop 0.75 --train-bracket-cats NP --train-dep-prop 0.0 --use-serialized-gc false --train-serialized-gc train-nv-b75-np-d00-gc --test-serialized-gc test-nv-fb-b75-np-d00-gc  --sampling-iterations 0 --burnin-iterations 0  --num b75-np-d00-serialize",
      // //          f"--model pcfg --learning mcmc  --cat-prior uniform      --root-init uniform   --nt-prod-init uniform   --term-prod-init uniform  --tr-init x  --additional-rules x  --train-bracket-prop 1.00 --train-bracket-cats NP --train-dep-prop 0.0 --use-serialized-gc false --train-serialized-gc train-nv-b99-np-d00-gc --test-serialized-gc test-nv-fb-b99-np-d00-gc  --sampling-iterations 0 --burnin-iterations 0  --num b99-np-d00-serialize")
      // handleCliStrings("serialize", cliStrings)

      case Seq("condor", "scg", name) =>
        val serialize = (name == "serialize")
        val optsList =
          for {
            numTestSentences <- Vector("1000") //, "ALL") //                                        // gc

            language <- Vector("en") //, "ch", "it") //                                             // gc
            learning <- Vector("mcmc") //, "learning")
            additionalRules <- Vector("x") //, "BX")                                                // gc
            //np2n <- Vector(false) //, true)                                                       // gc
            //punctSplit <- Vector(false) //, true)                                                 // gc
            alphaRoot <- Vector(10)
            alphaBiny <- Vector(100)
            alphaUnry <- Vector(10000)
            alphaTerm <- Vector(10000)
            alphaCntx <- Vector(1, 10, 100, 1000, 10000, 100000)
            alphaProd <- Vector(100)
            samplingIterations <- Vector(500)
            burninIterations <- Vector(0)
            tdTokToUse <- Vector("050k", "100k", "150k", "200k", "250k", "300k", "999k") // total: 323,190  // gc
            tdCutoff <- Vector("0.0") //, "0.001", "0.01", "0.1")                                   // gc

            numTrainSentences <- Vector("1000") //                                                  // gc

            ((model, prodInit, terminit, trinit), i) <- Vector(

              //(("pcfg", "uniform    ", "uniform", "x              "), "01"),
              //(("pcfg", "catprior   ", "uniform", "x              "), "02"),
              (("pcfg", "tdecatprior", "tdentry", "x              "), "03"),

              (("scg ", "uniform    ", "uniform", "uniform        "), "11"),
              //              (("scg ", "catprior   ", "uniform", "uniform        "), "12"),
              //              (("scg ", "uniform    ", "uniform", "combine        "), "21"),
              //              (("scg ", "catprior   ", "uniform", "combine        "), "22"),
              (("scg ", "tdecatprior", "tdentry", "combine-tdentry"), "23"))

            if model.trim == "scg" || alphaCntx == 1
            if serialize || tdTokToUse != "999k"

          } yield {

            val trainGcfile = Vector(
              f"$language",
              "x",
              f"${additionalRules}",
              //if (np2n) "n_" else "",
              //if (punctSplit) "ps" else "",
              f"tdtok$tdTokToUse",
              f"tdc$tdCutoff",
              f"n$numTrainSentences",
              f"gc").mkString("-")

            val testGcfile = Vector(
              f"$language",
              "fb",
              f"${additionalRules}",
              //if (np2n) "n_" else "",
              //if (punctSplit) "ps" else "",
              f"tdtok$tdTokToUse",
              f"tdc$tdCutoff",
              f"n$numTestSentences",
              f"gc").mkString("-")

            Vector(
              f"--lang $language",
              f"--model $model",
              f"--learning $learning",
              f"--additional-rules ${additionalRules}%-12s",

              //f"--np2n $np2n%-5s",
              //f"--punct-split $punctSplit%-5s",

              f"--root-init $prodInit%-8s",
              f"--nt-prod-init $prodInit%-8s",
              f"--term-prod-init $terminit%-8s",
              f"--tr-init $trinit%-15s",

              f"--use-serialized-train-gc true",
              f"--train-serialized-gc train-$trainGcfile%-60s",
              f"--train-gcb x",

              f"--use-serialized-test-gc true",
              f"--test-serialized-gc test-$testGcfile%-60s",
              f"--train-gcb fb",

              f"--alpha-root $alphaRoot",
              f"--alpha-biny $alphaBiny",
              f"--alpha-unry $alphaUnry",
              f"--alpha-term $alphaTerm",
              f"--alpha-cntx $alphaCntx",
              f"--alpha-prod $alphaProd",
              f"--sampling-iterations $samplingIterations",
              f"--burnin-iterations $burninIterations",

              f"--td-tok $tdTokToUse%-5s",
              f"--td-cutoff $tdCutoff%-5s",

              f"--train-sent $numTrainSentences%-6s",

              f"--test-sent $numTestSentences%-6s",

              f"--num ",
              Vector(
                f"$language",
                f"x", f"fb",
                f"${model.trim}",
                f"$learning",
                f"${additionalRules}",
                //if (np2n) "n_" else "",
                //if (punctSplit) "ps" else "",
                f"r$alphaRoot",
                f"b$alphaBiny",
                f"u$alphaUnry",
                f"t$alphaTerm",
                f"c$alphaCntx",
                f"p$alphaProd",
                f"it$samplingIterations",
                f"bu$burninIterations",
                f"tdtok$tdTokToUse",
                f"tdc$tdCutoff",

                f"n$numTrainSentences",
                f"n$numTestSentences",

                f"${i}").mkString("-"))
              .mkString(" ")
          }

        if (serialize) {
          val TrainSerName = "--train-serialized-gc (train-\\S+) ".r.unanchored
          val TestSerName = "--test-serialized-gc (test-\\S+) ".r.unanchored
          var trainGCs = ListMap.empty[String, String]
          var testGCs = ListMap.empty[String, String]
          for (x @ TrainSerName(sername) <- optsList if !trainGCs.contains(sername)) { trainGCs = trainGCs.updated(sername, x) }
          for (x @ TestSerName(sername) <- optsList if !testGCs.contains(sername)) { testGCs = testGCs.updated(sername, x) }
          //trainGCs.values.foreach(println)
          val allCliStrings = Vector.empty ++
            testGCs.mapt((testGc, opts) => opts.replaceAll("--num\\s+(\\S+)", f"--num $testGc").replaceAll("--use-serialized-test-gc true", "--use-serialized-test-gc false").replaceAll("--train-sent (\\d+)", "") + " --train-sent 0  --just-serialize true ") ++
            trainGCs.mapt((trainGc, opts) => opts.replaceAll("--num\\s+(\\S+)", f"--num $trainGc").replaceAll("--use-serialized-train-gc true", "--use-serialized-train-gc false").replaceAll("--test-sent (\\d+)", "") + " --test-sent 0  --just-serialize true ")
          handleCliStrings(name, allCliStrings)
        }
        else handleCliStrings(name, optsList)

      case Seq("condor", name) =>
        SimpleScgExpRunner(options, argString).makeInits(SimpleTagDictionary(Map(), "<S>", cat"<S>", "<E>", cat"<E>"), Vector.empty) // check that the options are valid
        new Condor(pathjoin("condorfiles", name), Some(24000)).makeNumbered(Vector(("dhg.ccg.parse.scg.exp.ScgExp", options.toVector.map { case (o, a) => f"--$o $a" }.mkString(" "))))

      case Seq() =>
        val runner = SimpleScgExpRunner(options, argString)
        runner.run()

      case Seq("results", name) =>
        File("condorfiles", name).ls(".*\\.out".r).foreach { f =>
          val fn = f.name
          val lines = f.readLines
          if (lines.hasNext) {
            val head = lines.next
            val accs = lines.flatMap {
              case DependencyAccuracyLine(acc) => Some(acc)
              case ConstituentAccuracyLine(acc) => Some(acc)
              case DepSpanAccuracyLine(acc) => Some(acc)
              case _ => None
            }
            println((Vector(fn) ++ head.splitWhitespace ++ accs).mkString("\t"))
          }
        }

    }

    def handleCliStrings(name: String, cliStrings: Vector[String]): Unit = {
      val split =
        cliStrings.toVector.map { optString =>
          println(optString)
          val optStringOpts = optString.splitWhitespace.grouped(2).toVector.map(_.toTuple2).map { case (o, a) => (o.drop(2), a) }
          assert(optStringOpts.toMap.contains("num"))
          assert(!options.contains("num"))
          optStringOpts
        }
      val numCounts = split.map(_.toMap.apply("num")).counts.filter(_._2 > 1); assert(numCounts.isEmpty, f"duplicated nums: [${numCounts.map { case (k, v) => f"$k: $v" }.mkString(", ")}]")
      new Condor(pathjoin("condorfiles", name), Some(24000)).makeWithNames(split.map { optStringOpts =>
        val opts = (optStringOpts ++ Vector(
          "mcmc-output-count-file" -> pathjoin("condorfiles", name, optStringOpts.toMap.apply("num") + "-mcmc-out.txt"),
          "output-file" -> pathjoin("condorfiles", name, optStringOpts.toMap.apply("num") + "-test-out.txt"))) ++ options.toVector
        SimpleScgExpRunner(CommandLineOptions(ListMap() ++ opts), argString).makeInits(SimpleTagDictionary(Map(), "<S>", cat"<S>", "<E>", cat"<E>"), Vector.empty) // check that the options are valid
        ("dhg.ccg.parse.scg.exp.ScgExp", opts.map { case (o, a) => f"--$o $a" }.mkString(" "), opts.toMap.apply("num"))
      })
    }
  }

  implicit class BooleanWithImplication(val b1: Boolean) extends AnyVal {
    def -->(b2: Boolean): Boolean = !b1 || b2
  }
}
