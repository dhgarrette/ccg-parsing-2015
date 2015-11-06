package dhg.ccg.run

import dhg.util._
import math.{ log, exp, abs }
import com.typesafe.scalalogging.slf4j.{ StrictLogging => Logging }
import scala.collection.immutable.BitSet
import scala.collection.breakOut
import scala.util.Random
import annotation.tailrec
import scala.util.Try
import scalaz._
import Scalaz._
import dhg.ccg.tag.learn._
import dhg.ccg.data._
import dhg.ccg.prob._
import dhg.ccg.tag.learn._
import dhg.ccg.tag._
import dhg.ccg.tag.learn._
import dhg.ccg.cat._
import dhg.ccg.tagdict.SimpleTagDictionaryFactory
import dhg.ccg.gen._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.condor.Condor

class Naacl2013Trainer[Tag](
  labelPropIterations: Int = 200,
  emIterations: Int = 50,
  memmIterations: Int = 100,
  //
  tagToString: (Tag => String),
  tagFromString: (String => Tag),
  //
  useLP: Boolean = true,
  useMM: Boolean = true,
  //  
  intermediateEvaluator: Option[Tagger[Tag] => Unit] = None)
    //
    extends Logging {
  type Word = String

  /**
   * LP -> ModelMin -> EM -> MEMM
   */
  def train(
    rawSentences: Vector[Vector[Word]],
    labeledSentences: Vector[Vector[(Word, Tag)]],
    initialTagdict: TagDictionary[Tag]): Tagger[Tag] = {
    val startTime = System.currentTimeMillis()

    val annotationTagdict = SimpleTagDictionary(
      initialTagdict.entries |+| labeledSentences.flatten.groupByKey.mapVals(_.toSet),
      initialTagdict.startWord, initialTagdict.startTag, initialTagdict.endWord, initialTagdict.endTag,
      initialTagdict.allWords ++ rawSentences.flatten ++ labeledSentences.flatten.map(_._1), initialTagdict.allTags ++ labeledSentences.flatten.map(_._2).toSet,
      initialTagdict.excludedTags)

    val tsmooth = new AddLambdaTransitionDistributioner[Tag](0.1)
    val esmooth = new AddLambdaEmissionDistributioner[Tag](0.1)

    val emTrainer = new SoftEmHmmTaggerTrainer[Tag](emIterations, new UnsmoothedTransitionDistributioner, new UnsmoothedEmissionDistributioner, alphaT = 0.0, alphaE = 0.0, 1e-10)
    val supervisedMemmTrainer = new MemmTaggerTrainer(memmIterations, cutoff = 100, tdRestricted = true, tagToString, tagFromString)

    println(f"Raw tokens: ${rawSentences.flatten.size}  (${rawSentences.size} sentences)")
    println(f"Token-supervision tokens: ${labeledSentences.flatten.size}  (${labeledSentences.size} sentences)")
    println(f"Type-supervision TD-entries: ${annotationTagdict.entries.flatMap(_._2).size}  (${annotationTagdict.entries.size} word types)")
    println(f"tsmooth: ${tsmooth}")
    println(f"esmooth: ${esmooth}")
    println(f"emTrainer: ${emTrainer}")
    println(f"supervisedMemmTrainer: ${supervisedMemmTrainer}")

    val (modelminTaggedRawSentences, generalizedTagdict) =
      if (useLP || useMM) {
        val lpSoftTagger: SoftTagger[Tag] =
          if (useLP) {
            val lpTaggingGraphBuilder =
              new JuntoAdaptingLpTaggingGraphBuilder[Tag](
                new SimpleLpTaggingGraphBuilder[Tag](
                  new Type2TokenLpEdgeExtractor(),
                  Vector(
                    TokenPrevLpEdgeExtractor(annotationTagdict.startWord),
                    TokenNextLpEdgeExtractor(annotationTagdict.endWord),
                    WordPrefixLpEdgeExtractor(5),
                    WordSuffixLpEdgeExtractor(5))))
            new JuntoTagger[Tag](lpTaggingGraphBuilder, labelPropIterations, threshold = 0.1, tagToString, tagFromString)
          }
          else new UniformSoftTagger[Tag]()
        println(f"lpSoftTagger = $lpSoftTagger")

        val modelMinTagger: SoftToHardTagger[Tag] =
          if (useMM) new ModelMinSoftToHardTagger[Tag]()
          else new HighProbSoftToHardTagger[Tag]()
        println(f"modelMinTagger = $modelMinTagger")

        println("Induce a soft tagging of the raw data")
        val softTaggedRawSentences = time(f"LP = $useLP", lpSoftTagger.tagFromAnnotations(rawSentences, labeledSentences, annotationTagdict))
        println("Extract a generalized tag dictionary")
        val generalizedTagdict = SimpleTagDictionary(annotationTagdict.entries |+| softTaggedRawSentences.flatten.mapVals(_.keySet).toMap, annotationTagdict.startWord, annotationTagdict.startTag, annotationTagdict.endWord, annotationTagdict.endTag, annotationTagdict.allWords, annotationTagdict.allTags, annotationTagdict.excludedTags)
        println("Induce a hard tagging via model minimization on the soft LP output")
        val modelminTaggedRawSentences = time(f"MM = $useMM", modelMinTagger.tagFromSoftTaggedSentences(softTaggedRawSentences, labeledSentences, generalizedTagdict))
        (modelminTaggedRawSentences, generalizedTagdict)
      }
      else {
        println("Baseline: no LP or MM")
        (labeledSentences, annotationTagdict)
      }

    println("learn a smoothed HMM from EM")
    val transitions = tsmooth(modelminTaggedRawSentences, generalizedTagdict)
    val emissions = esmooth(modelminTaggedRawSentences, generalizedTagdict)

    //      else { // Skip the LP/ModelMin stuff
    //        val trInitializer =
    //          if (baseline.contains("tr")) new TrTagDictEntriesPossibilities[Tag](new AddLambdaTransitionDistributioner(0.1))
    //          else new TrUniform[Tag]()
    //        val emInitializer =
    //          if (baseline.contains("em")) new EmTagDictionaryEstimate[Tag](new TagDictionaryEstimateTagPriorInitializer(0.1), 0.1, 0.1)
    //          else new EmUniform[Tag]()
    //        println(f"trInitializer: ${trInitializer}")
    //        println(f"emInitializer: ${emInitializer}")
    //        val generalizedTagdict = annotationTagdict
    //        println("Initialize transitions")
    //        val transitions = trInitializer.fromRaw(rawSentences, generalizedTagdict)
    //        println("Initialize emissions")
    //        val emissions = emInitializer.fromRaw(rawSentences, generalizedTagdict)
    //        (transitions, emissions, generalizedTagdict)
    //      }

    val preEmHmm = new HmmTagger(transitions, emissions, generalizedTagdict)
    val preEmMemm = time("Pre-EM MEMM", supervisedMemmTrainer.train(rawSentences.map(s => s zipSafe preEmHmm.tag(s)) ++ labeledSentences, generalizedTagdict))

    println("learn an HMM initialized with the estimated transition and emission distributions")
    val emHmm = time("EM", emTrainer.trainWithSomeGold(rawSentences, labeledSentences, generalizedTagdict, transitions, emissions))
    println("learn an MEMM from auto-tagged data produced by the smoothed HMM")
    val memm = time("MEMM", supervisedMemmTrainer.train(rawSentences.map(s => s zipSafe emHmm.tag(s)) ++ labeledSentences, generalizedTagdict))

    println(f"Total NAACL Trainer training time: ${(System.currentTimeMillis() - startTime) / 1000} seconds\n\n")

    intermediateEvaluator.foreach { e => println("EVALUATING... Pre-EM HMM:"); e(preEmHmm) }
    intermediateEvaluator.foreach { e => println("EVALUATING... Pre-EM HMM + MEMM:"); e(preEmMemm) }
    intermediateEvaluator.foreach { e => println("EVALUATING... EM-HMM:"); e(emHmm) }
    intermediateEvaluator.foreach { e => println("EVALUATING... EM-HMM + MEMM:"); e(memm) }

    memm
  }

}

//
//
//

/**
 * sbt start-script
 * target/start dhg.ccg.run.Naacl2013Run --toksup none --typesup data/prepared/eng-exp/train/eng-tagdict-exp-4hr-lc8.txt --raw data/prepared/eng-exp/raw.txt --eval data/prepared/eng-exp/dev.txt --rawtok 100k
 * target/start dhg.ccg.run.Naacl2013Run condor naacl2013 --rawtok 100k eng-exp dev --baseline false
 */
object Naacl2013Run {

  def main(args: Array[String]): Unit = {
    val (arguments, options) = parseArgs(args)

    arguments match {

      //      case Seq("condor", name, commandFile) =>
      //        new Condor(pathjoin("condorfiles", name), Some(16000)).makeWithNames(
      //          File(commandFile).readLines.map { line =>
      //            val Seq(c, as @ _*) = line.splitWhitespace
      //            val fn = "dhg.ccg.run.Naacl2013Run_" + as.map { s =>
      //              s.drop(s.lastIndexOf(java.io.File.separator) + 1)
      //            }.mkString("_")
      //            (c, as.mkString(" "), fn)
      //          }.toVector)

      case Seq("condor", name, eval) =>
        val ToksupFileRe = f""".*-sentences-(.*-)?4hr-lc(\\d+)\\.txt""".r
        val TypesupFileRe = f""".*-tagdict-(.*-)?4hr-lc(\\d+)\\.txt""".r

        val lines =
          for {
            lang <- Vector("eng-exp", "eng-nov", "mlg", "kin")
            dir = File("data/prepared", lang)
            rawFile = File(dir, "raw.txt")
            evalFile = File(dir, f"$eval.txt")
            (toksupN, toksupFile) <- File(dir, "train").ls(ToksupFileRe).map(f => f.name match { case ToksupFileRe(_, UInt(n)) => n -> Some(f) }).toMap + (0 -> None)
            (typesupN, typesupFile) <- File(dir, "train").ls(TypesupFileRe).map(f => f.name match { case TypesupFileRe(_, UInt(n)) => n -> Some(f) }).toMap + (0 -> None)
            useLP <- Vector(true, false)
            useMM <- Vector(true, false)
            if (
              (toksupN + typesupN == 8) ||
              (toksupN > 0 && typesupN == 0) ||
              (toksupN == 0 && typesupN > 0))
            if useLP || ((toksupN == 4 && typesupN == 0) || (toksupN == 8 && typesupN == 0) || (toksupN == 0 && typesupN == 4) || (toksupN == 0 && typesupN == 8))
            if useMM || ((toksupN == 4 && typesupN == 0) || (toksupN == 8 && typesupN == 0) || (toksupN == 0 && typesupN == 4) || (toksupN == 0 && typesupN == 8))
          } yield {
            Vector(
              "--toksup", toksupFile.fold("none")(_.getAbsolutePath),
              "--typesup", typesupFile.fold("none")(_.getAbsolutePath),
              "--useLP", useLP, "--useMM", useMM,
              "--raw", rawFile.getAbsolutePath,
              "--eval", evalFile.getAbsolutePath).map(_.toString) ++
              options.options.flatMap { case (o, a) => Vector("--" + o, a) }
          }
        new Condor(pathjoin("condorfiles", name), Some(16000)).makeWithNames(
          lines.toVector.map { args =>
            val fn = "dhg.ccg.run.Naacl2013Run_" + args.map { s =>
              s.drop(s.lastIndexOf(java.io.File.separator) + 1)
            }.mkString("_")
            ("dhg.ccg.run.Naacl2013Run", args.mkString(" "), fn)
          })

      case Seq("results", name) =>
        val FnRe = """(.*)\.out""".r
        val AccuracyLineRe = """Accuracy = (.*)""".r
        val (experiments, accuracies) =
          File("condorfiles", name).ls(FnRe).flatMap { f =>
            (f.name, f.readLines.toVector.last) match {
              case (FnRe(fn), AccuracyLineRe(UDouble(acc))) =>
                // dhg.ccg.run.Naacl2013Run_--toksup_eng-sentences-nov-4hr-lc6.txt_--typesup_eng-tagdict-nov-4hr-lc2.txt_--raw_raw.txt_--eval_dev.txt_--rawtok_100k_--baseline_trem.out
                val parts = fn.lsplit("_").filterNot(_.startsWith("--"))
                Some((parts, f"$acc%.2f"))
              case _ => None
            }
          }.unzip
        val maxFields = experiments.map(_.size).max
        val lines = experiments.map(v => v ++ Vector.fill(maxFields - v.size)(""))
        for ((line, acc) <- lines zipSafe accuracies) {
          println((line :+ acc).mkString("\t"))
        }

      case Seq() =>
        println(args.mkString(" "))
        
        val labelPropIterations = options.get("lpi").fold(100)(_.toInt)
        val emIterations = options.get("emi").fold(50)(_.toInt)
        val memmIterations = options.get("memmi").fold(100)(_.toInt)

        val tokenSupervisionFile: Option[String] = options.get("toksup").filter(_ != "none")
        val typeSupervisionFile: Option[String] = options.get("typesup").filter(_ != "none")
        val rawDataFile: Option[String] = options.get("raw")
        val rawtok: Int = options.get("rawtok").map { case s if s.endsWith("k") => s.dropRight(1) + "000"; case s => s }.fold(100000)(_.toInt)
        val evalDataFile: Option[String] = options.get("eval").filter(_ != "none")
        val applyDataFile: Option[String] = options.get("apply").filter(_ != "none")

        val useLP: Boolean = options.get("useLP").map(_.toBoolean).getOrElse(true)
        val useMM: Boolean = options.get("useMM").map(_.toBoolean).getOrElse(true)

        assert(tokenSupervisionFile.isDefined || typeSupervisionFile.isDefined, "must specify at least one of --toksup or --typesup")
        assert(rawDataFile.isDefined, "must specify --raw")
        assert(evalDataFile.isDefined || applyDataFile.isDefined, "must specify at least one of --eval or --apply")

        println("Get all the data")
        def parsePipeFile(fn: String) = File(fn).readLines.map(_.splitWhitespace.map(_.rsplit("\\|", 2)).zipWithIndex.mapt { case (Seq(w, t), i) => (w, t); case (v,i) => sys.error(f"bad token: [${v.map(x => f""""$x"""").mkString(", ")}]") }).toVector
        val labeledSentences = tokenSupervisionFile.map(parsePipeFile).getOrElse(Vector.empty)
        val tdData = typeSupervisionFile.map(parsePipeFile).getOrElse(Vector.empty).flatten.groupByKey.mapVals(_.toSet)
        val tagdict = SimpleTagDictionary(tdData |+| labeledSentences.flatten.groupByKey.mapVals(_.toSet), "<S>", "<S>", "<E>", "<E>", excludedTags = Set("X"))
        val rawSentences = File(rawDataFile.get).readLines.map(_.splitWhitespace).toVector.takeSub(rawtok)

        val evaluator: Option[Tagger[String] => Unit] = evalDataFile.map { eval =>
          val evalData = parsePipeFile(eval)
          (tagger: Tagger[String]) => {
            val accuracy = TaggerEvaluator(tagger, evalData, tagdict.allWords)
            println(f"Accuracy = $accuracy")
          }
        }

        println("Train the model")
        val trainer = new Naacl2013Trainer[String](labelPropIterations, emIterations, memmIterations, identity, identity, useLP, useMM, evaluator)
        val tagger = trainer.train(rawSentences, labeledSentences, tagdict)

        println("Run the evaluation")
        evaluator.foreach { e => e(tagger) }

        for (apply <- applyDataFile) {
          println("Run the application file")
          for (s <- File(apply).readLines.map(_.splitWhitespace).toVector) {
            println((s zipSafe tagger.tag(s)).map { case (w, t) => f"$w|$t" }.mkString(" "))
          }
        }

    }
  }

}


/*
 * target/start dhg.ccg.run.Naacl2013Run --typesup data/prepared/test/train/test-tagdict-exp-4hr-lc8.txt --raw data/prepared/test/raw.txt --eval data/prepared/test/dev.txt --useLP true  --useMM true 
 * target/start dhg.ccg.run.Naacl2013Run --typesup data/prepared/test/train/test-tagdict-exp-4hr-lc8.txt --raw data/prepared/test/raw.txt --eval data/prepared/test/dev.txt --useLP true  --useMM false
 * target/start dhg.ccg.run.Naacl2013Run --typesup data/prepared/test/train/test-tagdict-exp-4hr-lc8.txt --raw data/prepared/test/raw.txt --eval data/prepared/test/dev.txt --useLP false --useMM true 
 * target/start dhg.ccg.run.Naacl2013Run --typesup data/prepared/test/train/test-tagdict-exp-4hr-lc8.txt --raw data/prepared/test/raw.txt --eval data/prepared/test/dev.txt --useLP false --useMM false
 * 
 * 
 * target/start dhg.ccg.run.Naacl2013Run --typesup data/prepared/eng-exp/train/eng-tagdict-exp-4hr-lc8.txt --raw data/prepared/eng-exp/raw.txt --eval data/prepared/eng-exp/dev.txt --lpi 100
 * Accuracy: 87.82  [took <1hr: 30min for LP, 15 min for model-min]
 * target/start dhg.ccg.run.Naacl2013Run --typesup data/prepared/eng-exp/train/eng-tagdict-exp-4hr-lc8.txt --raw data/prepared/eng-exp/raw.txt --eval data/prepared/eng-exp/dev.txt --lpi 100 --baseline true
 * Accuracy: 87.42  [30min for trInit]
 * 
 * target/start dhg.ccg.run.Naacl2013Run --typesup data/prepared/eng-exp/train/eng-tagdict-exp-4hr-lc2.txt --raw data/prepared/eng-exp/raw.txt --eval data/prepared/eng-exp/dev.txt --lpi 100
 * Accuracy: 
 * target/start dhg.ccg.run.Naacl2013Run --typesup data/prepared/eng-exp/train/eng-tagdict-exp-4hr-lc2.txt --raw data/prepared/eng-exp/raw.txt --eval data/prepared/eng-exp/dev.txt --lpi 100 --baseline true
 * Accuracy: 
 */
