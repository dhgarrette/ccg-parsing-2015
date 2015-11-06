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
import dhg.ccg.util._
import org.apache.commons.math3.random.SynchronizedRandomGenerator
import dhg.ccg.tag.learn._

/**
 * target/start dhg.ccg.parse.scg.expi.Run       0227a
 */
object Run {
  type Word = String

  def getData(name: String, lang: String, rawTokStr: String, testSentStr: String, tdTokStr: String, maxSentLenStr: String) = {
    val serializationArgPathString = Serialize.serializationArgFilenameString(lang, rawTokStr, testSentStr, tdTokStr, maxSentLenStr)
    val fn = f"data/gcser/$name/$serializationArgPathString"
    val rawGCsIWithTrees = {
      val filepaths = File(f"data/gcser/$name").ls(f"$serializationArgPathString-raw-part\\d+.gciv.gz".r).map(_.getPath)
      val rawGCsIWithTreesItr = {
        if (filepaths.nonEmpty) {
          filepaths.sorted.iterator.flatMap { path =>
            time(f"deserializing raw gciv from $path", CfgGuideChartI.readVector(path))
          }
        }
        else {
          time(f"deserializing raw gciv from $fn-raw.gciv.gz", CfgGuideChartI.readVector(f"$fn-raw.gciv.gz")).iterator
        }
      }.filter(_._1.isDefined)

      val rawTok = rawTokStr match { case UInt(n) => n * 1000; case "inf" | "max" => Int.MaxValue }
      val rawGCsTokLimitingItr: Iterator[(Option[CfgGuideChartI], Option[CcgTreeI])] = new Iterator[(Option[CfgGuideChartI], Option[CcgTreeI])] {
        var totalSize = 0
        def next(): (Option[CfgGuideChartI], Option[CcgTreeI]) = {
          val x = rawGCsIWithTreesItr.next
          totalSize += x._1.get.length
          //println(f"   -->  ${x.words.mkString(" ")}  ->  ${x.length}  ->  ${totalSize}")
          x
        }
        def hasNext(): Boolean = rawGCsIWithTreesItr.hasNext && totalSize <= rawTok
      }

      rawGCsTokLimitingItr.toVector
    }
    val tstGCsIWithTrees = {
      val filepaths = File(f"data/gcser/$name").ls(f"$serializationArgPathString-tst-part\\d+.gciv.gz".r).map(_.getPath)
      if (filepaths.nonEmpty) {
        filepaths.sorted.flatMap { path =>
          time(f"deserializing tst gciv from $path", CfgGuideChartI.readVector(path))
        }
      }
      else {
        time(f"deserializing tst gciv from $fn-tst.gciv.gz", CfgGuideChartI.readVector(f"$fn-tst.gciv.gz"))
      }
    }

    println(f"raw gc: ${rawGCsIWithTrees.size} total sentences, ${rawGCsIWithTrees.count(_._1.isDefined)} usable sentences")
    println(f"tst gc: ${tstGCsIWithTrees.size} total sentences, ${tstGCsIWithTrees.count(_._1.isDefined)} usable sentences")

    println(f"raw gc:         ${rawGCsIWithTrees.sumBy(_._1.get.length)} tokens, ${rawGCsIWithTrees.size} sentences, max sent len = ${rawGCsIWithTrees.map(_._1.get.length).max}")
    println(f"test gc:        ${tstGCsIWithTrees.sumBy(_._2.get.length)} tokens, ${tstGCsIWithTrees.size} sentences, max sent len = ${tstGCsIWithTrees.map(_._2.get.length).max}")
    println(f"usable test gc: ${tstGCsIWithTrees.flatMap(_._1).sumBy(_.length)} tokens, ${tstGCsIWithTrees.count(_._1.isDefined)} sentences, max sent len = ${tstGCsIWithTrees.flatMap(_._1).map(_.length).max}")

    val catIndexer = time("deserializing cat indexer", new SimpleIndexer(File(f"$fn-cats.txt").readLines.map(NonRemovingCcgBankCatInterner(_)).toVector))
    val wordIndexer = time("deserializing wrd indexer", new SimpleIndexer(File(f"$fn-wrds.txt").readLines.toVector))

    (catIndexer, wordIndexer, rawGCsIWithTrees, tstGCsIWithTrees)
  }

  def main(args: Array[String]): Unit = {
    val argString = args.mkString(" ")
    println(argString)

    //    val (arguments, options_) = parseArgs(args)
    //    val options = CommandLineOptions(options_)
    val arguments = args.toVector

    val Vector(
      ("--name", name),
      ("--lang", lang), //                                            serialized
      ("--rawTok", rawTokStr), //                                     serialized
      ("--testSent", testSentStr), //                                 serialized
      ("--tdTok", tdTokStr), //                                       serialized
      ("--maxSentLen", maxSentLenStr), //                             serialized
      ("--model", model),
      ("--samplingIter", UInt(samplingIterations)),
      ("--burninIter", UInt(burninIterations)),
      ("--treesPerIteration", UInt(treesPerIteration)),
      ("--catPrior", catPriorOpt),
      ("--ctxPrior", ctxPrior),
      ("--combinableMultiplier", combinableMultiplierStr),
      ("--alphaCtx", alphaCtxStr)) = ("--name" +: arguments).grouped(2).map(_.toTuple2).toVector

    val combinableMultiplier = combinableMultiplierStr.replaceAll("k$", "000").toDouble
    val alphaCtx = alphaCtxStr.replaceAll("k$", "000").toDouble

    val serializationArgPathString = Serialize.serializationArgFilenameString(lang, rawTokStr, testSentStr, tdTokStr, maxSentLenStr)

    val (catIndexer, wordIndexer, rawGCsIWithTrees, tstGCsIWithTrees) = getData(name, lang, rawTokStr, testSentStr, tdTokStr, maxSentLenStr)
    //val (catIndexer, wordIndexer, rawGCsIWithTrees, tstGCsIWithTrees) = Serialize.getData(lang, tdTok)

    val rawGCsI = rawGCsIWithTrees.flatMap(_._1)
    val tstGCsI = tstGCsIWithTrees.flatMap(_._1)

    val numCats = catIndexer.size
    val numWords = wordIndexer.size
    println(f"num words = $numWords, num cats = $numCats")

    //

    val rand = new SynchronizedRandomGenerator(new MersenneTwister())

    val pcfgProductionCounter = new SimplePcfgProductionCounterI(catIndexer, wordIndexer) //                                         TESTED
    val scgProductionCounter = new SimpleScgProductionCounterI(catIndexer, wordIndexer) //                                           TESTED
    val scgGuideChartProdFinder = new SimpleScgGuideChartProdFinderI(catIndexer, wordIndexer) //                                     meh
    val pcfgAlphaPriorMaker = new TrainDataPcfgAlphaPriorMakerI(catIndexer, wordIndexer) //                                          TESTED
    val scgAlphaPriorMaker = new TrainDataScgAlphaPriorMakerI(catIndexer, wordIndexer) //                                            TESTED
    val pcfgInsideChartBuilder = new SimplePcfgInsideChartBuilderI(catIndexer, wordIndexer) //                                       TESTED
    val pcfgTreeSampler = new SimplePcfgTreeSamplerI(pcfgInsideChartBuilder, rand)(catIndexer, wordIndexer) //                       TESTED
    val scgTreeSampler = new MetHastScgTreeSamplerI(pcfgTreeSampler, treesPerIteration, rand)(catIndexer, wordIndexer) //            TESTED
    val pcfgParserInstantiater = new ExactPcfgParserInstantiaterI()

    new TagdictInformedAtomCatDistInitializer(atomLambda = 1000.0)

    val catPriorI: Array[LogDouble] = time("catPriorI", {
      val estCatCountsI = time("estCatCountsI", {
        val catCounts = Array.fill(numCats)(1.0)
        for {
          gc <- rawGCsI
          //(i, j, cell) <- gc.bottomUpNodes
          span <- 1 to gc.length //        span size
          i <- 0 to (gc.length - span) //  start of span
          j = i + span //                  end of span
          cell = gc(i, j)
          partialCount = (1.0 / cell.size)
          (t, _) <- cell
        } {
          catCounts(t) += partialCount
        }
        catCounts
      })

      val estCatCounts = time("estCatCounts", estCatCountsI.zipWithIndex.map { case (count, cat) => catIndexer.obj(cat) -> count }.toMap)
      def getAtomCounts(c: Cat): Map[AtomCat, Int] = c match { case a: AtomCat => Map(a -> 1); case l || r => getAtomCounts(l) |+| getAtomCounts(r); case _ => Map.empty }
      val estAtomCounts = time("estAtomCounts", estCatCounts.map { case (cat, count) => getAtomCounts(cat).mapVals(_ * count) }.reduce(_ |+| _))
      val estPuncCounts = time("estPuncCounts", estCatCounts.collect { case (cat: PuncCat, count) => cat -> count })
      val startEndProb = rawGCsI.size / (2.0 * rawGCsI.size + rawGCsI.sumBy(_.length * 2 - 1))
      val catPrior = new SimpleInfCatPrior(
        allPunc = estPuncCounts.keySet, puncDist = new SimpleLogProbabilityDistribution[PuncCat](estPuncCounts.mapVals(LogDouble(_)).normalizeValues.withDefault { c => sys.error("unsupported punc cat $c") }),
        pPunc = 0.05,
        allAtoms = estAtomCounts.keySet, pAtom = new SimpleLogProbabilityDistribution[AtomCat](estAtomCounts.mapVals(LogDouble(_)).normalizeValues.withDefault { c => sys.error("unsupported punc cat $c") }),
        pTerm = 0.7, pMod = 0.25, pFwd = 0.5,
        deletionProb = 1e-100, startEndProb = startEndProb)
      catIndexer.objects.map { cat => val p = catPrior(cat); assert(p.nonZero, f"catPrior($cat) = ${catPrior(cat).toDouble}"); p }.normalize.toArray
    })

    //

    val (knownRoots, knownBinys, knownUnrys, knownTerms, knownLctxs, knownRctxs) = time("find all productions", scgGuideChartProdFinder.all((rawGCsI ++ tstGCsI).toArray, numCats, numWords, startCat = 0, endCat = 1))

    val priorRootDist: IndirectSparseVec[LogDouble] = time("make priorRootDist", catPriorOpt match { //                         t -> p
      case "uni" =>
        val p = LogDouble(1.0 / numCats); IndirectSparseVec(knownRoots.map(t => t -> p), numCats)
      case "cat" => IndirectSparseVec(knownRoots.map(t => t -> catPriorI(t)), numCats)
    })
    val priorBinyDist: Array[IndirectSparseVec[IndirectSparseVec[LogDouble]]] = time("make priorBinyDist", { //       t -> u -> v -> p
      val uniformP = LogDouble(1.0 / (numCats * numCats))
      val uvPrior = IndirectSparseVec(
        knownBinys.flatMap(Option(_))
          .flatMap { uvs =>
            uvs.activePairs.map {
              case (u, vs) => u ->
                vs.map { v =>
                  v -> (catPriorOpt match {
                    case "uni" => uniformP
                    case "cat" => catPriorI(u) * catPriorI(v)
                  })
                }.toMap
            }
          }.toVector.groupByKey.mapValues(vs => IndirectSparseVec(vs.flatten.toMap, numCats)), numCats)
      knownBinys.map { tKnown =>
        if (tKnown != null) uvPrior
        else null
      }
    })
    val priorUnryDist: IndirectSparseVec[IndirectSparseVec[LogDouble]] = time("make priorUnryDist", { //              t -> u -> p
      val uniformP = LogDouble(1.0 / numCats)
      val uPrior = IndirectSparseVec(
        knownUnrys.activeValues.flatMap { us =>
          us.map { u =>
            u -> (catPriorOpt match {
              case "uni" => uniformP
              case "cat" => catPriorI(u)
            })
          }
        }.toMap, numCats)

      // TODO: Could be WithDefaultValue
      IndirectSparseVec(knownUnrys.activeKeys.map { t =>
        t -> uPrior
      }, numCats)
    })
    val priorTermDist: Array[Vec[LogDouble]] = time("make priorTermDist", { //                  t -> w -> p
      new TagdictSupervisedTermDistInitializerI(catPriorI).apply(
        rawGCsI, knownTerms, numWords)
    })
    val priorPmixDist: Array[Array[LogDouble]] = time("make priorPmixDist", { //                      t -> p
      val probs = Array(LogDouble(1 / 3.0), LogDouble(1 / 3.0), LogDouble(1 / 3.0))
      Array.fill(numCats)(probs)
    })
    val priorLctxDist: Array[IndirectSparseVec[LogDouble]] = time("make priorLctxDist", ctxPrior match { //                  t -> l -> p
      case "com" =>
        new UniformCombLctxDistInitializerI(catIndexer, wordIndexer).apply(rawGCsI, knownLctxs, combinableMultiplier, numCats)
      case "cac" =>
        new CatpriorCombLctxDistInitializerI(catIndexer, wordIndexer, catPriorI).apply(rawGCsI, knownLctxs, combinableMultiplier, numCats)
      case "uni" =>
        knownLctxs.zipWithIndex.map {
          case (knownLs, t) =>
            if (knownLs != null)
              IndirectSparseVec(knownLs.map { _ -> LogDouble(1.0 / numCats) }, numCats)
            else null
        }
    })
    val priorRctxDist: Array[IndirectSparseVec[LogDouble]] = time("make priorRctxDist", ctxPrior match { //                  t -> r -> p
      case "com" =>
        new UniformCombRctxDistInitializerI(catIndexer, wordIndexer).apply(rawGCsI, knownRctxs, combinableMultiplier, numCats)
      case "cac" =>
        new CatpriorCombRctxDistInitializerI(catIndexer, wordIndexer, catPriorI).apply(rawGCsI, knownRctxs, combinableMultiplier, numCats)
      case "uni" =>
        knownRctxs.zipWithIndex.map {
          case (knownRs, t) =>
            if (knownRs != null)
              new IndirectSparseVec(knownRs, knownRs.map { r => LogDouble(1.0 / numCats) }, knownRs.length, numCats)
            else null
        }
    })

    val alphaRoot = 1.0
    val alphaBiny = 100.0
    val alphaUnry = 100.0
    val alphaTerm = 10000.0
    val alphaPmix = 3.0
    val alphaLctx = alphaCtx
    val alphaRctx = alphaCtx

    val supPcfgTrainer = new AlphaBetaSupPcfgTrainerI(
      priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, priorPmixDist,
      alphaRoot = LogDouble(alphaRoot), alphaBiny = LogDouble(alphaBiny), alphaUnry = LogDouble(alphaUnry), alphaTerm = LogDouble(alphaTerm), alphaPmix = LogDouble(alphaPmix),
      pcfgProductionCounter,
      pcfgParserInstantiater,
      knownRoots, knownBinys, knownUnrys, knownTerms,
      numCats, numWords)( //
      catIndexer, wordIndexer)

    val evaluator = new DepParserEvaluatorI(false)(catIndexer, wordIndexer)
    val evalData = tstGCsIWithTrees.mapVals(_.get)
    val itermediateEvaluatorAndEvalIters: Option[(PcfgParserI => Unit, Set[Int])] = Some(((parser: PcfgParserI) => {
      println("Full PCFG Parser on test"); println(argString); evaluator.evaluate(parser, evalData, None, verbose = false)
    }, Set(-1, 0, 5, 10, 20, 50, 100, 200, 300, 400) ++ (500 until 1000000 by 500)))

    val mcmcSampler: McmcSamplerI = {
      val mcmcPcfgI = new McmcPcfgI(
        samplingIterations,
        burninIterations,
        pcfgProductionCounter,
        pcfgAlphaPriorMaker,
        initialParserInstantiater = pcfgParserInstantiater,
        pcfgTreeSampler,
        priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, priorPmixDist,
        alphaRoot, alphaBiny, alphaUnry, alphaTerm, alphaPmix,
        knownRoots, knownBinys, knownUnrys, knownTerms,
        supPcfgTrainer,
        rand,
        itermediateEvaluatorAndEvalIters,
        numCats, numWords,
        accumulate = false,
        verbose = false)( //
        catIndexer, wordIndexer)
      val mcmcScgI = new McmcScgI(
        samplingIterations,
        burninIterations,
        scgProductionCounter,
        scgAlphaPriorMaker,
        initialParserInstantiater = pcfgParserInstantiater,
        scgTreeSampler,
        priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, priorPmixDist, priorLctxDist, priorRctxDist,
        alphaRoot, alphaBiny, alphaUnry, alphaTerm, alphaPmix, alphaLctx, alphaRctx,
        knownRoots, knownBinys, knownUnrys, knownTerms, knownLctxs, knownRctxs,
        supPcfgTrainer,
        rand,
        itermediateEvaluatorAndEvalIters,
        numCats, numWords,
        startCat = 0, endCat = 1,
        accumulate = false,
        verbose = false)( //
        catIndexer, wordIndexer)

      model match {
        case "pcfg" => mcmcPcfgI
        case "scg" => mcmcScgI
      }
    }

    val sampledTrees: Array[CcgTreeI] = mcmcSampler.trainGuideChartsWithSomeGold(rawGCsI.toArray, Array.empty)
    val parser: PcfgParserI = supPcfgTrainer.train(sampledTrees)
    //TreeViz.drawTree(CcgTreeI.from(parser_mcmcPCfgI.parseAndLogProbFromGuideChart(tstGCsI.head)._1, catIndexer, wordIndexer))

    evaluator.evaluate(parser, evalData, Some(f"condorfiles/$name/$serializationArgPathString.parse"), false, false)

  }
}
