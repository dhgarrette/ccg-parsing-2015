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
import dhg.ccg.cat._
import dhg.ccg.data._
import dhg.ccg.rule._
import dhg.ccg.prob._
import dhg.ccg.tag.learn._
import dhg.ccg.tag._
import dhg.ccg.tag.learn._
import org.apache.commons.math3.random.{ MersenneTwister, RandomGenerator }
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.tagdict.SimpleTagDictionaryFactory
import dhg.condor.Condor

class Conll2014Trainer(
  catFeatures: Boolean = true,
  combinabilityCcgRules: Vector[CcgRule] = Vector(FA, FAn2np, BA, BAn2np, FC, FC2, BC, /*FX,*/ BX, BX2),

  pTerm: Double = 0.6,
  pMod: Double = 0.8,
  pFwd: Double = 0.5,
  atomLambda: Double = 1000.0,

  alphaT: Double = 3000, alphaE: Double = 7000,
  samplingIterations: Int = 200, burninIterations: Int = 100,
  maxMemmIterations: Int = 50,

  catpriorMass: Double = 0.5,
  combinableTransitionMass: Double = 0.95,

  startTag: Cat, endTag: Cat,

  rand: RandomGenerator = new MersenneTwister)
  extends TypeSupervisedTaggerTrainer[Cat] {

  val canCombine = new SimpleCatCanCombine(combinabilityCcgRules, startTag, endTag)
  val catpriorInit = new NormalizingCatgramCatPriorInitializer(new TagdictInformedAtomCatDistInitializer(atomLambda), pTerm, pMod, pFwd)
  val catPriorTrInit = new TagPriorTrInitializer(catpriorInit)
  val baseTrInit = new CcgCombinabilityTrInitializer(new TrTagDictEntriesPossibilities(new AddLambdaTransitionDistributioner(0.1)), canCombine, combinableTransitionMass, totalSmoothing = LogDouble.zero)
  val trInit = new InterpolatingTransitionInitializer[Cat](Vector(catPriorTrInit -> catpriorMass, baseTrInit -> (1 - catpriorMass)))
  val emInit = new EmTagDictionaryEstimate[Cat](catpriorInit, 0.1, 0.1)
  //  val trSmooth = new AddLambdaTransitionDistributioner[Cat](0.1)
  //  val emSmooth = new AddLambdaEmissionDistributioner[Cat](0.1)

  val emTrainer =
    new SimpleTypeSupervisedTaggerTrainer(
      new FfbsHmmTaggerTrainer[Cat](
        samplingIterations, burninIterations,
        alphaT, alphaE,
        new AddLambdaTransitionDistributioner(0.1), new AddLambdaEmissionDistributioner(0.1),
        rand),
      trInit, emInit)

  val catmap: CatInterner = ??? // new StandardCatInterner(CatInterner.CcgBankPuncRe, CatInterner.CcgBankAtomRe, removeFeatures = !catFeatures)
  val meTrainer = new MemmTaggerTrainer[Cat](maxMemmIterations, cutoff = 100, tdRestricted = true, _.toString, catmap.fromString _)

  def typesupTrain(rawSentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Cat]) = {

    println(f"emTrainer = $emTrainer")
    println(f"meTrainer = $meTrainer")

    val tagdict = initialTagdict.withWords(rawSentences.flatten.toSet)

    /* learn a smoothed HMM from EM */
    val emHmm = emTrainer.typesupTrain(rawSentences, tagdict)
    /* learn an MEMM from auto-tagged data produced by the smoothed HMM */
    val memm = meTrainer.train(rawSentences.map(s => s zipSafe emHmm.tag(s)), tagdict)

    memm
  }
}

class Conll2014Runner[Tag](
  bankRunner: Conll2014BankRunner[Tag]) {
  type Word = String

  val bankReader = bankRunner.bankReader

  def run(algorithm: String, cmd: String, arguments: Vector[String], options: Map[String, String]): Unit = {
    val algo = algorithm.toUpperCase

    println(cmd); Console.err.println(cmd)

    //val lambda = options.get("lambda").fold(1.0)(_.toDouble)
    val tsmooth: TransitionDistributioner[Tag] =
      options.getOrElse("tsmooth", "no") match {
        case x if x.startsWith("un") | x.startsWith("no") => new UnsmoothedTransitionDistributioner()
        case x if x.startsWith("ad") => new AddLambdaTransitionDistributioner(0.1) // 0.2
        case x if x.startsWith("on") => new OneCountTransitionDistributioner(???, ???)
      }
    val esmooth: EmissionDistributioner[Tag] =
      options.getOrElse("esmooth", "no") match {
        case x if x.startsWith("un") | x.startsWith("no") => new UnsmoothedEmissionDistributioner()
        case x if x.startsWith("ad") => new AddLambdaEmissionDistributioner(0.1)
        case x if x.startsWith("on") => new OneCountEmissionDistributioner(???, ???)
      }
    val trInitializer = bankRunner.getTransitionInitializer(options.getOrElse("trinit", "uniform") match {
      case x if x.startsWith("un") => new TrUniform[Tag]()
      case x if x.startsWith("tde") => new TrTagDictEntriesPossibilities[Tag](new AddLambdaTransitionDistributioner(0.1)) // 0.05
      case x if x.startsWith("ch") => new TrCheat[Tag](bankReader.rawCHEATING, new AddLambdaTransitionDistributioner(0.1)) // 0.2
    }, arguments, options)
    val emInitializer = bankRunner.getEmissionInitializer(options.getOrElse("eminit", "uniform") match {
      case x if x.startsWith("un") => new EmUniform[Tag]()
      case x if x.startsWith("cr") => new EmTagDictionaryEstimate[Tag](new TagDictionaryEstimateTagPriorInitializer(0.1), 0.1, 0.1) // 0.05, 0.25 
      case x if x.startsWith("ch") => new EmCheat[Tag](bankReader.rawCHEATING, new AddLambdaEmissionDistributioner(0.1))
    }, arguments, options)

    val maxIterations = options.get("it").fold(50)(_.toInt)
    val samplingIterations = options.get("samples").fold(100)(_.toInt)
    val burninIterations = options.get("burnin").fold(100)(_.toInt)
    val trAlpha = options.get("tralpha").fold(3000)(_.toInt)
    val emAlpha = options.get("emalpha").fold(7000)(_.toInt)
    val memmCutoff = options.get("memmcut").fold(100)(_.toInt)
    val tdcut = options.get("tdcut").fold(0.1)(_.toDouble)
    val tdCutoff = Some(tdcut)
    val tdFactory = new SimpleTagDictionaryFactory[Tag](tdCutoff)
    val td = tdFactory(bankReader.tdData, bankReader.startWord, bankReader.startTag, bankReader.endWord, bankReader.endTag)

    val emTrainer = algo match {
      case "EM" => new SimpleTypeSupervisedTaggerTrainer(new SoftEmHmmTaggerTrainer(maxIterations, tsmooth, esmooth, alphaT = trAlpha, alphaE = emAlpha, 1e-10), trInitializer, emInitializer)
      case "FFBS" => new SimpleTypeSupervisedTaggerTrainer(new FfbsHmmTaggerTrainer(samplingIterations, burninIterations, trAlpha, emAlpha, tsmooth, esmooth, new MersenneTwister), trInitializer, emInitializer)
    }
    val meTrainer = new MemmTaggerTrainer(maxIterations, memmCutoff, tdRestricted = true, bankReader.tagToString, bankReader.tagFromString)

    def printInfo() {
      println(algo)
      println(f"trainer: $emTrainer")
      println
      println(f"tagset = ${bankReader.getClass.getName}")
      println
      println(f"td cutoff = $tdcut")
      println(f"td tokens  = ${bankReader.tdData.flatten.size}%6s  (${bankReader.tdData.size} sentences)")
      println(f"td words   = ${td.entries.keySet.size}%6s")
      println(f"td entries = ${td.entries.sumBy(_._2.size)}%6s")
      println
      println(f"raw tokens =  ${bankReader.raw.flatten.size}%6s  (${bankReader.raw.size} sentences)")
      println(f"dev tokens =  ${bankReader.devData.flatten.size}%6s  (${bankReader.devData.size} sentences)")
      println(f"test tokens = ${bankReader.testData.flatten.size}%6s  (${bankReader.testData.size} sentences)")
      println(f"raw data ambiguity (based on td), type =  ${bankReader.raw.flatten.map(w => td(w).size).avg}%.2f")
      println(f"raw data ambiguity (based on td), token = ${bankReader.raw.flatten.distinct.map(w => td(w).size).avg}%.2f")
      println
      println(f"tr initializer = $trInitializer")
      println(f"em initializer = $emInitializer")
      println(f"tr distributioner = $tsmooth")
      println(f"em distributioner = $esmooth")
      println(f"memm cutoff = $memmCutoff")
      println
      algo match {
        case "EM" =>
          println(f"max iterations = $maxIterations")
        case "FFBS" =>
          println(f"sampling iterations = $samplingIterations")
          println(f"burn-in iterations = $burninIterations")
          println(f"tr alpha = $trAlpha")
          println(f"em alpha = $emAlpha")
      }
    }
    printInfo()

    val raw = bankReader.raw
    /* train an HMM directly on initialization distributions (without EM) */
    val hmm = new HmmTagger(trInitializer.fromRaw(raw, td), emInitializer.fromRaw(raw, td), td)
    /* learn a smoothed HMM from EM */
    val emHmm = emTrainer.typesupTrain(raw, td)
    /* learn an MEMM from auto-tagged data produced by the smoothed HMM */
    val memm = meTrainer.train(raw.map(s => s zipSafe emHmm.tag(s)), td)

    printInfo()

    val testData = bankReader.devData
    println(f"\nNo $algo")
    val a = TaggerEvaluator(hmm, testData)
    println(f"\nSmoothed HMM from $algo")
    val b = TaggerEvaluator(emHmm, testData)
    println(f"\nMEMM trained from auto-tagged data produced by the smoothed HMM")
    val c = TaggerEvaluator(memm, testData)
    println(cmd)
    println(cmd.replaceAll("\\s+|\\.", "_"))
    println(f"${cmd.replaceAll("\\s+", "\t").replaceAll("\\t--", "\t'--")}\t$a\t$b\t$c")
  }

}

trait Conll2014BankRunner[Tag] {
  def bankReader: TagBankReader[Tag]
  def getTransitionInitializer(trInitializer: TransitionInitializer[Tag], arguments: Vector[String], options: Map[String, String]): TransitionInitializer[Tag] = trInitializer
  def getEmissionInitializer(emInitializer: EmissionInitializer[Tag], arguments: Vector[String], options: Map[String, String]): EmissionInitializer[Tag] = emInitializer
}

class Conll2014SimpleCcgBankRunner(val bankReader: TagBankReader[String]) extends Conll2014BankRunner[String] {
}

class Conll2014CatCcgBankRunner(val bankReader: TagBankReader[Cat]) extends Conll2014BankRunner[Cat] {
  val WRe = "w(.+)".r
  val TRe = "t(.+)".r
  val FRe = "f(.+)".r
  val MRe = "m(.+)".r
  val LRe = "l(.+)".r

  private[this] def getCatPriorInit(options: Map[String, String]) = {
    options.get("ccgtrinit-catprior").fold(none[(TagPriorInitializer[Cat], Double)]) { v =>
      v.lsplit(",") match {
        case Seq("cplx", WRe(UDouble(catpriorMass))) =>
          Some((new CatComplexityInitializer, catpriorMass))
        case Seq("pcfg", TRe(UDouble(pTerm)), FRe(UDouble(pFwd)), MRe(UDouble(pMod)), WRe(UDouble(catpriorMass))) =>
          Some((new NormalizingCatgramCatPriorInitializer(new UniformAtomCatDistInitializer(), pTerm, pMod, pFwd), catpriorMass))
        case Seq("tdipcfg", TRe(UDouble(pTerm)), FRe(UDouble(pFwd)), MRe(UDouble(pMod)), WRe(UDouble(catpriorMass)), rest @ _*) =>
          val atomLambda = rest match { case Seq(LRe(UDouble(atomLambda))) => atomLambda; case Seq() => 1.0 }
          Some((new NormalizingCatgramCatPriorInitializer(new TagdictInformedAtomCatDistInitializer(atomLambda), pTerm, pMod, pFwd), catpriorMass))
        case Seq("pcfg-cheat", WRe(UDouble(catpriorMass))) =>
          val pAtom = new LogProbabilityDistribution[AtomCat] {
            def apply(b: AtomCat) = LogDouble(Map(
              "NP" /*  */ -> 0.39328,
              "N" /*   */ -> 0.30571,
              "S" /*   */ -> 0.23530,
              "<E>" /* */ -> 0.01818,
              "<S>" /* */ -> 0.01818,
              "PP" /*  */ -> 0.01810,
              "conj" /**/ -> 0.00956,
              ":" /*   */ -> 0.00086,
              "RRB" /* */ -> 0.00063,
              "LRB" /* */ -> 0.00022)(b.atom))
            def sample(): AtomCat = ???
            def defaultProb: LogDouble = ???
          }
          val pTerm = 0.6511
          val pFwd = 0.4949
          val pMod = 0.4292 //  (67 mod types, 402 non-mod types)
          Some((new NormalizingCatgramCatPriorInitializer(new KnownAtomCatDistInitializer(pAtom), pTerm, pMod, pFwd), catpriorMass))
        case Seq("false") => None
      }
    }
  }

  override def getTransitionInitializer(trInitializer: TransitionInitializer[Cat], arguments: Vector[String], options: Map[String, String]) = {
    val a =
      options.get("ccgtrinit-comb").fold(none[Double])(v => Try(v.toDouble).toOption).map { combMass =>
        val canCombine = new SimpleCatCanCombine(Vector(FA, FAn2np, BA, BAn2np, FC, FC2, BC, /*FX,*/ BX, BX2), bankReader.startTag, bankReader.endTag)
        new CcgCombinabilityTrInitializer(trInitializer, canCombine, combMass, totalSmoothing = LogDouble.zero)
      }.getOrElse(trInitializer)
    val b = options.get("ccgtrinit-catprior").fold(none[TransitionInitializer[Cat]]) { v =>
      getCatPriorInit(options).map {
        case (catpriorInit, catpriorMass) =>
          val catPriorTrInit = new TagPriorTrInitializer(catpriorInit)
          new InterpolatingTransitionInitializer[Cat](Vector(a -> (1 - catpriorMass), catPriorTrInit -> catpriorMass))
      }
    }.getOrElse(a)
    b
  }

  override def getEmissionInitializer(emInitializer: EmissionInitializer[Cat], arguments: Vector[String], options: Map[String, String]): EmissionInitializer[Cat] = {
    getCatPriorInit(options).fold(emInitializer) {
      case (catpriorInit, catpriorMass) =>
        options.getOrElse("eminit", "uniform") match {
          case x if x.startsWith("cr") => new EmTagDictionaryEstimate[Cat](catpriorInit, 0.1, 0.1) // 0.05, 0.25 
          case _ => emInitializer
        }
    }
  }

}

//
//
//

/*
 * target/start dhg.ccg.run.Conll2014Run condor NAME (EM|FBSS) TAGSET 
 * target/start dhg.ccg.run.Conll2014Run (EM|FBSS) TAGSET [OPTIONS] 
 * target/start dhg.ccg.run.Conll2014Run experiments NAME TAGSET 
 * target/start dhg.ccg.run.Conll2014Run results NAME 
 */
object Conll2014Run {
  val NumRuns = 1

  def main(args: Array[String]): Unit = {
    val (arguments, options) = parseArgs(args)

    arguments match {
      case Seq("experiments", name, tagset) =>
        val lines =
          Vector("0.1", "0.01", "0.001", "0.0").flatMap(tdcut =>
            //            Vector(
            //              /* Uniform                        */ f"em   $tagset run1    --samples 200 --burnin 100 --tdcut $tdcut --trinit un  --eminit un    --ccgtrinit-comb false --ccgtrinit-catprior false", //
            //              /* B08                            */ f"em   $tagset run1    --samples 200 --burnin 100 --tdcut $tdcut --trinit un  --eminit un    --ccgtrinit-comb 0.95  --ccgtrinit-catprior cplx,w0.5" //
            //              ) ++
            (1 to NumRuns).flatMap(run =>
              Vector(
                // /* FFBS B08                       */ f"ffbs $tagset run$run --samples 200 --burnin 100 --tdcut $tdcut --trinit un  --eminit un    --ccgtrinit-comb 0.95  --ccgtrinit-catprior cplx,w0.5", //
                /* FFBS B08 + pcfg                */ f"ffbs $tagset run$run --samples 200 --burnin 100 --tdcut $tdcut --trinit un  --eminit un    --ccgtrinit-comb 0.95  --ccgtrinit-catprior tdipcfg,t0.6,f0.5,m0.8,w0.5,l1000", //
                // /* FFBS B08 + pcfg + eminit       */ f"ffbs $tagset run$run --samples 200 --burnin 100 --tdcut $tdcut --trinit un  --eminit tde --ccgtrinit-comb 0.95  --ccgtrinit-catprior tdipcfg,t0.6,f0.5,m0.8,w0.5,l1000", //
                /* FFBS B08 + pcfg + tde          */ f"ffbs $tagset run$run --samples 200 --burnin 100 --tdcut $tdcut --trinit tde --eminit un    --ccgtrinit-comb 0.95  --ccgtrinit-catprior tdipcfg,t0.6,f0.5,m0.8,w0.5,l1000", //
                /* FFBS B08 + pcfg + tde + eminit */ f"ffbs $tagset run$run --samples 200 --burnin 100 --tdcut $tdcut --trinit tde --eminit tde --ccgtrinit-comb 0.95  --ccgtrinit-catprior tdipcfg,t0.6,f0.5,m0.8,w0.5,l1000", //
                /* FFBS Skyline                   */ f"ffbs $tagset run$run --samples 200 --burnin 100 --tdcut $tdcut --trinit ch  --eminit ch    --ccgtrinit-comb 0.95  --ccgtrinit-catprior pcfg-cheat,w0.5" //
                )))
        new Condor(pathjoin("condorfiles", name), Some(16000)).makeNamed(lines.map(("dhg.ccg.run.Conll2014Run", _)))

      case Seq("condor", name, algorithm, tagset, tdcut) =>

        val ffbs = algorithm.toUpperCase match { case "EM" => false; case "FFBS" => true }

        val lines =
          for {
            memmcut <- Seq("100")
            it <- if (ffbs) Seq("") else Seq("50")
            samples <- if (!ffbs) Seq("") else Seq("200")
            burnin <- if (!ffbs) Seq("") else Seq("100")
            tralpha <- if (!ffbs) Seq("") else Seq("3000")
            emalpha <- if (!ffbs) Seq("") else Seq("7000")
            tsmooth <- Seq("ad")
            esmooth <- Seq("ad")
            ccgtrinitComb <- Seq("0.95", "false") //, "0.8")
            trinit <- Seq("un", "tde", "ch")
            eminit <- Seq("un", "tde", "ch")
            if !(trinit == "ch" || eminit == "ch") || trinit == eminit // only run "cheat" experiments where both are cheating
            ccgtrinitCatprior <- Seq("pcfg,t0.6,f0.5,m0.4,w0.5", "false", "pcfg-cheat,w0.5", "cplx,w0.5", "pcfg,t0.6,f0.5,m0.8,w0.5")
          } yield {
            ("dhg.ccg.run.Conll2014Run", (Vector(
              algorithm, tagset) ++
              //              (if (it.isEmpty) Vector() else Vector("--it", it)) ++
              (if (samples.isEmpty) Vector() else Vector("--samples", samples)) ++
              (if (burnin.isEmpty) Vector() else Vector("--burnin", burnin)) ++
              //              (if (tralpha.isEmpty) Vector() else Vector("--tralpha", tralpha)) ++
              //              (if (emalpha.isEmpty) Vector() else Vector("--emalpha", emalpha)) ++
              //              Vector("--memmcut", memmcut) ++
              Vector("--tdcut", tdcut) ++
              //              Vector("--tsmooth", tsmooth) ++
              //              Vector("--esmooth", esmooth) ++
              Vector("--trinit", trinit) ++
              Vector("--eminit", eminit) ++
              Vector("--ccgtrinit-comb", ccgtrinitComb) ++
              Vector("--ccgtrinit-catprior", ccgtrinitCatprior))
              .mkString(" "))
          }
        new Condor(pathjoin("condorfiles", name), Some(16000)).makeNamed(lines.toVector)

      case Seq("results", name) =>
        writeUsing(File("results.txt")) { w =>
          val things =
            File(pathjoin("condorfiles", name)).ls(".*\\.out".r).toVector
              .map(f => f.readLines.toVector)
              .filter(_.nonEmpty)
              .map(_.last.split("\\s+").toVector)

          val x =
            things.flatMap { l =>
              l.reverse match {
                case Vector(UDouble(e), UDouble(d), UDouble(c), UDouble(b), UDouble(a), _*) =>
                  Some((l.take(2) ++ l.slyce(3 until -5)) -> Vector(a, b, c, d, e))
                case _ => None
              }
            }.groupByKey.mapVals(nums => (nums.transpose.map(_.avg), nums.size))

          x.foreach {
            case (l, (xs, n)) =>
              w.wl((l ++ xs :+ n).mkString("\t").replaceAll("\\t--", "\t'--"))
          }
        }

      case Seq(algorithm, tagset, runNum) =>
        val runner = tagset match {
          case "ccgpos" =>
            new Conll2014Runner(new Conll2014SimpleCcgBankRunner(PosEnglishCcgTagBankReader))
          case "ccg" =>
            new Conll2014Runner(new Conll2014CatCcgBankRunner(FeaturelessEnglishCcgTagBankReader))
          case "ccgfeat" =>
            new Conll2014Runner(new Conll2014CatCcgBankRunner(FeaturedEnglishCcgTagBankReader))
          case "tut" =>
            new Conll2014Runner(new Conll2014CatCcgBankRunner(FeaturelessTutCcgTagBankReader))
          case "tutfeat" =>
            new Conll2014Runner(new Conll2014CatCcgBankRunner(FeaturedTutCcgTagBankReader))
          case "ctb" =>
            new Conll2014Runner(new Conll2014CatCcgBankRunner(FeaturelessChineseCcgTagBankReader))
          case "ctbfeat" =>
            new Conll2014Runner(new Conll2014CatCcgBankRunner(FeaturedChineseCcgTagBankReader))
        }
        runner.run(algorithm, args.mkString(" "), arguments, options.options)

      case Seq(algorithm, tagset) => main(Array(algorithm, tagset, "run1") ++ args.drop(2))

    }
  }

}
