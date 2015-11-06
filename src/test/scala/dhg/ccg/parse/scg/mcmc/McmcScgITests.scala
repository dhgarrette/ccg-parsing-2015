package dhg.ccg.parse.scg.mcmc

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.util._
import dhg.ccg.tagdict._
import dhg.ccg.test.TestUtil._
import dhg.ccg.parse.pcfg._
import dhg.util._
import org.apache.commons.math3.random.MersenneTwister
import dhg.ccg.parse.scg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.tagdict.SimpleStartEndTags
import org.apache.commons.math3.random.RandomGenerator
import dhg.ccg.tagdict.StartEndTags
import scala.collection.parallel.immutable.ParVector
import dhg.util._
import dhg.ccg.math._
import dhg.ccg.parse.scg._
import scalaz._
import Scalaz._
import dhg.ccg.util.SimpleIndexer
import scala.collection.immutable.BitSet
import org.apache.commons.math3.random.SynchronizedRandomGenerator
import dhg.ccg.parse.pcfg.typesup._
import dhg.ccg.parse.inf._
import dhg.ccg.parse.dep.DepParserEvaluatorI
import dhg.util.viz.TreeViz

class McmcScgITests {

  val A: AtomCat = cat"A".asInstanceOf[AtomCat]
  val B: AtomCat = cat"B".asInstanceOf[AtomCat]
  val C: AtomCat = cat"C".asInstanceOf[AtomCat]
  val D: AtomCat = cat"D".asInstanceOf[AtomCat]
  val E: AtomCat = cat"E".asInstanceOf[AtomCat]
  val F: AtomCat = cat"F".asInstanceOf[AtomCat]
  val X: AtomCat = cat"X".asInstanceOf[AtomCat]

  val S: AtomCat = cat"S".asInstanceOf[AtomCat]
  val NP: AtomCat = cat"NP".asInstanceOf[AtomCat]
  val N: AtomCat = cat"N".asInstanceOf[AtomCat]
  val PP: AtomCat = cat"PP".asInstanceOf[AtomCat]
  val STA = cat"<S>"
  val END = cat"<E>"
  val SE = new SimpleStartEndTags(STA: Cat, END: Cat)

  val s: AtomCat = cat"S".asInstanceOf[AtomCat]
  val np: AtomCat = cat"NP".asInstanceOf[AtomCat]
  val n: AtomCat = cat"N".asInstanceOf[AtomCat]
  val pp: AtomCat = cat"PP".asInstanceOf[AtomCat]

  val startWord = "<S>"
  val startTag = STA
  val endWord = "<E>"
  val endTag = END

  @Test
  def integration_againstEM {
    type Word = String
    type Tag = Cat

    val (rawGCsI, tstGCsI, catIndexer, wordIndexer) = {
      val Det: Set[Cat] = Set(
        np / n)
      val Adj: Set[Cat] = Set(
        n / n)
      val IV: Set[Cat] = Set(
        s \ np,
        (s \ np) / pp)
      val TV: Set[Cat] = Set(
        (s \ np) / np,
        ((s \ np) / pp) / np,
        (((s \ np) / pp) / pp) / np)
      val N: Set[Cat] = Set(
        n)
      val NNP: Set[Cat] = Set(
        np,
        np / pp,
        (np / pp) / pp)
      val Prep: Set[Cat] = Set(
        pp / np)

      val tagdict = SimpleTagDictionary.apply(
        Map[Word, Set[Cat]](
          "the" -> Det,
          "a" -> Det,
          "big" -> Adj,
          "man" -> N,
          "dog" -> N,
          "dogs" -> N,
          "cat" -> N,
          "cats" -> N,
          "telescope" -> N,
          "saw" -> (IV | TV),
          "walked" -> (IV | TV),
          "chase" -> TV,
          "run" -> IV,
          "ran" -> IV,
          "John" -> NNP,
          "Mary" -> NNP,
          "with" -> Prep,
          "nnp" -> Set(n, np)),
        "<S>", StartCat, "<E>", EndCat)

      val binaryRules = Set[CcgRule](FA, BA)
      val unaryRules = Set[CcgRule](N2NP)
      val rules: Set[CcgRule] = binaryRules ++ unaryRules
      val rootSet: Set[Cat] = Set(s, np, n)
      val stGcBuilder = new SimpleCfgGuideChartBuilder(rules.toVector, additionalSupertagAdder = new StandardTagDictAdditionalTagAdder, rootSet, allowTerminalDeletion = false)
      val fbGcBuilder = new SimpleCfgGuideChartBuilder(rules.toVector, additionalSupertagAdder = new StandardTagDictAndFwdBkdAdditionalTagAdder, rootSet, allowTerminalDeletion = false)

      //      val sentences = (0 to 100).flatMap(_ => Vector(
      //        "the dogs walked",
      //        "the man walked the dog",
      //        "dogs chase cats",
      //        "the man ran with a dog",
      //        "big dogs run",
      //        "the big dogs run",
      //        "John saw Mary with the dog",
      //        "John saw a cat with the dog",
      //        "John saw Mary with the telescope",
      //        "John saw Mary with the dog with the telescope")).toVector
      //      val rawGCs = sentences.flatMap(s => stGcBuilder.build(s.splitWhitespace, None, tagdict))
      //      val tstGCs = sentences.flatMap(s => fbGcBuilder.build(s.splitWhitespace, None, tagdict))

      //    val B1 = (B / C) \ A
      //    val B2 = (B \ A) / C
      //    val X = C / D
      //    val XS: Vector[(Word, Set[Cat])] = Vector(("s", Set(S / E)), ("x", Set(B / C)), ("y", Set(X)), ("z", Set((E \ (B / C)) \ X)))
      //    val sentences: Vector[Vector[(Word, Set[Cat])]] = (1 to 100).toVector.flatMap(_ => Vector[Vector[(Word, Set[Cat])]](
      //      XS, XS, XS, XS, XS, XS, XS,
      //      Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B1)), ("c", Set(C / D)), ("d", Set(D))),
      //      Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B2)), ("c", Set(C / D)), ("d", Set(D))),
      //      Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B1, B2)), ("c", Set(C / D)), ("d", Set(D))),
      //      Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B1)), ("c", Set(C / D)), ("d", Set(D)))))

      val B1 = (B / C) \ A
      val B2 = (B \ A) / C
      val X = F //C / D
      val XS: Vector[(Word, Set[Cat])] = Vector(("s", Set(S / E)), ("x", Set(B / C)), ("y", Set(X)), ("z", Set((E \ (B / C)) \ X)))
      val sentences: Vector[Vector[(Word, Set[Cat])]] = (1 to 100).toVector.flatMap(_ => Vector[Vector[(Word, Set[Cat])]](
        Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B1)), ("c", Set(C / D)), ("d", Set(D))),
        Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B2)), ("c", Set(C / D)), ("d", Set(D))),
        Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B1, B2)), ("c", Set(C / D)), ("d", Set(D))),
        Vector(("s", Set(S / B)), ("a", Set(A)), ("b", Set(B1)), ("c", Set(C / D)), ("d", Set(D))),
        XS, XS, XS, XS, XS, XS, XS))

      val rawGCs = sentences.flatMap(s => stGcBuilder.buildFromSupertagSetSentence(s, None, tagdict))
      val tstGCs = sentences.flatMap(s => fbGcBuilder.buildFromSupertagSetSentence(s, None, tagdict))

      val wordIndexer = new SimpleIndexer[Word](Vector(startWord, endWord) ++ ((rawGCs ++ tstGCs).flatMap(_.words).toSet -- Set(startWord, endWord)))
      val catIndexer = new SimpleIndexer[Cat](Vector(startTag, endTag) ++ ((rawGCs ++ tstGCs).flatMap(_.allCats).toSet -- Set(startTag, endTag)))
      val rawGCsI = rawGCs.map(gc => CfgGuideChartI.to(gc, catIndexer, wordIndexer))
      val tstGCsI = tstGCs.map(gc => CfgGuideChartI.to(gc, catIndexer, wordIndexer))

      (rawGCsI, tstGCsI, catIndexer, wordIndexer)
    }
    val numCats = catIndexer.size
    val numWords = wordIndexer.size

    //

    //val sentencesI = sentences.map(_.map { case (w, ts) => wordIndexer(w) -> ts.map(catIndexer).toBitSet })
    //val (binaryRulesI, unaryRulesI) = CcgRuleI.makeIndexedRules(rules, catIndexer)
    //val allSupertagsI = tagdict.allTags.map(catIndexer).toBitSet
    //val tagdictI = TagDictionaryI.to(tagdict, catIndexer, wordIndexer)
    //val gcBuilderI = new SimpleCfgGuideChartBuilderI(binaryRulesI, unaryRulesI, rootSetI)
    //val rawDataGCI = sentencesI.flatMap(s => gcBuilderI.buildFromSupertagSetSentence(s, None, tagdictI, allSupertagsI))

    //

    val rand = new SynchronizedRandomGenerator(new MersenneTwister())

    val pcfgProductionCounter = new SimplePcfgProductionCounterI(catIndexer, wordIndexer) //                                         TESTED
    val scgProductionCounter = new SimpleScgProductionCounterI(catIndexer, wordIndexer) //                                           TESTED
    val scgGuideChartProdFinder = new SimpleScgGuideChartProdFinderI(catIndexer, wordIndexer) //                                     meh
    val scgAlphaPriorMaker = new TrainDataScgAlphaPriorMakerI(catIndexer, wordIndexer) //                                            TESTED
    val pcfgInsideChartBuilder = new SimplePcfgInsideChartBuilderI(catIndexer, wordIndexer) //                                       TESTED
    val pcfgTreeSampler = new SimplePcfgTreeSamplerI(pcfgInsideChartBuilder, rand)(catIndexer, wordIndexer) //                       TESTED
    val scgTreeSampler = new MetHastScgTreeSamplerI(pcfgTreeSampler, treesPerIteration = 1, rand)(catIndexer, wordIndexer) //        TESTED
    val pcfgParserInstantiater = new ExactPcfgParserInstantiaterI()

    val catPrior = new SimpleInfCatPrior(
      allPunc = Set(cat".", cat",", cat";", cat":", cat"LRB", cat"RRB").map(_.asInstanceOf[PuncCat]), puncDist = new SimpleLogProbabilityDistribution(Map(
        cat"," -> 0.50,
        cat"." -> 0.45,
        cat":" -> 0.02,
        cat";" -> 0.01,
        cat"LRB" -> 0.01,
        cat"RRB" -> 0.01).mapt((c, p) => c.asInstanceOf[PuncCat] -> LogDouble(p)).normalizeValues),
      pPunc = 0.10,
      allAtoms = catIndexer.objects.collect { case a: AtomCat => a }.toSet, pAtom = new LaplaceLogProbabilityDistribution[AtomCat](Map(
        s -> 0.5,
        np -> 0.2,
        n -> 0.3,
        A -> 0.3,
        B -> 0.2,
        C -> 0.1)
        .mapVals(LogDouble(_)), None, None, lambda = LogDouble(0.1)),
      pTerm = 0.8, pMod = 0.2, pFwd = 0.5,
      Set(startTag, endTag))

    val catPriorI: Array[LogDouble] = catIndexer.objects.map(catPrior).normalize.toArray

    //

    val (knownRoots, knownBinys, knownUnrys, knownTerms, knownLctxs, knownRctxs) = scgGuideChartProdFinder.all((rawGCsI ++ tstGCsI).toArray, numCats, numWords, startCat = 0, endCat = 1)

    val priorRootDist: IndirectSparseVec[LogDouble] = { //                         t -> p
      IndirectSparseVec(knownRoots.map(t => t -> catPriorI(t)), numCats)
    }
    val priorBinyDist: Array[IndirectSparseVec[IndirectSparseVec[LogDouble]]] = { //       t -> u -> v -> p
      val uvPrior = IndirectSparseVec(
        knownBinys.flatMap(Option(_))
          .flatMap { uvs =>
            uvs.activePairs.map {
              case (u, vs) => u ->
                vs.map { v =>
                  //println(f"calculate priorBinyDist::    ${catIndexer.obj(0)}%-20s -> Binary(${catIndexer.obj(u)}%-20s, ${catIndexer.obj(v)}%-20s)    = ${catPriorI(u)} * ${catPriorI(v)}     = ${catPriorI(u) * catPriorI(v)}")
                  v -> (catPriorI(u) * catPriorI(v))
                }.toMap
            }
          }.toVector.groupByKey.mapValues(vs => IndirectSparseVec(vs.flatten.toMap, numCats)), numCats)
      knownBinys.map { tKnown =>
        if (tKnown != null) uvPrior
        else null
      }
    }
    val priorUnryDist: IndirectSparseVec[IndirectSparseVec[LogDouble]] = { //              t -> u -> p
      val uPrior = IndirectSparseVec(
        knownUnrys.activeValues.flatMap { us =>
          us.map { u =>
            u -> catPriorI(u)
          }
        }.toMap, numCats)

      // TODO: Could be WithDefaultValue
      IndirectSparseVec(knownUnrys.activeKeys.map { t =>
        t -> uPrior
      }, numCats)
    }
    val priorTermDist: Array[Vec[LogDouble]] = { //                  t -> w -> p
      new TagdictSupervisedTermDistInitializerI(catPriorI).apply(
        rawGCsI, knownTerms, numWords: Int)
    }
    val priorPmixDist: Array[Array[LogDouble]] = { //                      t -> p
      val probs = Array(LogDouble(1 / 3.0), LogDouble(1 / 3.0), LogDouble(1 / 3.0))
      Array.fill(numCats)(probs)
    }
    val priorLctxDist: Array[IndirectSparseVec[LogDouble]] = { //                  t -> l -> p
      knownLctxs.zipWithIndex.map {
        case (knownLs, t) =>
          if (knownLs != null)
            IndirectSparseVec(knownLs.map { l => l -> LogDouble(1.0 / numCats) }, numCats)
          else null
      }
    }
    val priorRctxDist: Array[IndirectSparseVec[LogDouble]] = { //                  t -> r -> p
      knownRctxs.zipWithIndex.map {
        case (knownRs, t) =>
          if (knownRs != null)
            IndirectSparseVec(knownRs.map { l => l -> LogDouble(1.0 / numCats) }, numCats)
          else null
      }
    }

    val alphaRoot = 1.0
    val alphaBiny = 1.0
    val alphaUnry = 1.0
    val alphaTerm = 1.0
    val alphaPmix = 3.0
    val alphaLctx = 1.0
    val alphaRctx = 1.0

    val supPcfgTrainer = new AlphaBetaSupPcfgTrainerI(
      priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, priorPmixDist,
      alphaRoot = LogDouble(alphaRoot), alphaBiny = LogDouble(alphaBiny), alphaUnry = LogDouble(alphaUnry), alphaTerm = LogDouble(alphaTerm), alphaPmix = LogDouble(alphaPmix),
      pcfgProductionCounter,
      pcfgParserInstantiater,
      knownRoots, knownBinys, knownUnrys, knownTerms,
      numCats, numWords)( //
      catIndexer, wordIndexer)

    val mcmcPcfgI = new McmcScgI(
      samplingIterations = 1000,
      burninIterations = 100,
      scgProductionCounter,
      scgAlphaPriorMaker,
      initialParserInstantiater = pcfgParserInstantiater,
      scgTreeSampler,
      priorRootDist, priorBinyDist, priorUnryDist, priorTermDist, priorPmixDist, priorLctxDist, priorRctxDist,
      alphaRoot, alphaBiny, alphaUnry, alphaTerm, alphaPmix, alphaLctx, alphaRctx,
      knownRoots, knownBinys, knownUnrys, knownTerms, knownLctxs, knownRctxs,
      supPcfgTrainer,
      rand,
      itermediateEvaluatorAndEvalIters = None,
      numCats, numWords,
      startCat = 0, endCat = 1,
      accumulate = false,
      verbose = false)( //
      catIndexer, wordIndexer)

    val sampledTrees: Array[CcgTreeI] = mcmcPcfgI.trainGuideChartsWithSomeGold(rawGCsI.toArray, Array.empty)
    val parser_mcmcPCfgI: PcfgParserI = supPcfgTrainer.train(sampledTrees)
    tstGCsI.foreach(parser_mcmcPCfgI.parseAndLogProbFromGuideChart)
    //TreeViz.drawTree(CcgTreeI.from(parser_mcmcPCfgI.parseAndLogProbFromGuideChart(tstGCsI.head)._1, catIndexer, wordIndexer))

    //    new DepParserEvaluatorI(None, false)(catIndexer, wordIndexer)
    //      .evaluate(parser_mcmcPCfgI, tstGCsI, false, false)

    val (resultRootCounts, resultBinyCounts, resultUnryCounts, resultTermCounts, resultPmixCounts, resultLctxCounts, resultRctxCounts) =
      scgProductionCounter.counts(sampledTrees.toArray, sampledTrees.size, knownRoots, knownBinys, knownUnrys, knownTerms, knownLctxs, knownRctxs, numCats, numWords, startCat = 0, endCat = 1)

    val mcmcPcfgI_sc = resultRootCounts.activePairs.map { case (t, c) => catIndexer.obj(t) -> c }.toMap
    val mcmcPcfgI_pc = {
      val mcmcPcfgI_bc: Map[Cat, Map[Prod, Double]] =
        resultBinyCounts.zipWithIndex.flatMap {
          case (tCounts, t) if tCounts != null => Some(catIndexer.obj(t) ->
            (for {
              (u, uCounts) <- tCounts.activePairs
              (v, c) <- uCounts.activePairs
            } yield (BinaryProd(catIndexer.obj(u), catIndexer.obj(v)): Prod) -> c).toMap)
          case _ => None
        }.toMap
      val mcmcPcfgI_uc: Map[Cat, Map[Prod, Double]] =
        resultUnryCounts.activePairs.map {
          case (t, tCounts) => catIndexer.obj(t) ->
            tCounts.activePairs.map {
              case (u, c) => (UnaryProd(catIndexer.obj(u)): Prod) -> c
            }.toMap
        }.toMap
      val mcmcPcfgI_tc: Map[Cat, Map[Prod, Double]] =
        resultTermCounts.zipWithIndex.flatMap {
          case (tCounts, t) if tCounts != null => Some(catIndexer.obj(t) ->
            tCounts.activePairs.map {
              case (w, c) => (TermProd(wordIndexer.obj(w)): Prod) -> c
            }.toMap)
          case _ => None
        }.toMap
      mcmcPcfgI_bc |+| mcmcPcfgI_uc |+| mcmcPcfgI_tc
    }
    val mcmcPcfgI_lc = resultLctxCounts.zipWithIndex.flatMap {
      case (tCounts, t) if tCounts != null => Some(catIndexer.obj(t) ->
        tCounts.activePairs.map {
          case (l, c) => catIndexer.obj(l) -> c
        }.toMap)
      case _ => None
    }.toMap
    val mcmcPcfgI_rc = resultRctxCounts.zipWithIndex.flatMap {
      case (tCounts, t) if tCounts != null => Some(catIndexer.obj(t) ->
        tCounts.activePairs.map {
          case (r, c) => catIndexer.obj(r) -> c
        }.toMap)
      case _ => None
    }.toMap

    //    def parser_mcmcPCfgI_rootDist(t: Cat) = parser_mcmcPCfgI.logRootDist(catIndexer(t))
    //    def parser_mcmcPCfgI_prodDist(t: Cat, p: Prod) = p match {
    //      case BinaryProd(u, v) =>
    //        parser_mcmcPCfgI.logBinyDist(catIndexer(t))(catIndexer(u))(catIndexer(v)) +
    //          parser_mcmcPCfgI.logPmixDist(catIndexer(t))(0)
    //      case UnaryProd(u) =>
    //        parser_mcmcPCfgI.logUnryDist(catIndexer(t))(catIndexer(u)) +
    //          parser_mcmcPCfgI.logPmixDist(catIndexer(t))(1)
    //      case TermProd(w) =>
    //        parser_mcmcPCfgI.logTermDist(catIndexer(t))(wordIndexer(w)) +
    //          parser_mcmcPCfgI.logPmixDist(catIndexer(t))(2)
    //    }

    //

    val em = new DumbScgEm(100)
    val (dumbScgEm_sc, dumbScgEm_pc, dumbScgEm_lc, dumbScgEm_rc) = em.train(rawGCsI.map(CfgGuideChartI.from(_, catIndexer, wordIndexer)))(CatStartEndTags)

    println("ROOTS:")
    dumbScgEm_sc.normalizeValues.foreach {
      case (t, p) =>
        println(f"  $t%-20s ->  MCMC=${mcmcPcfgI_sc.normalizeValues.apply(t)}%-20s  DUMB=${p.toDouble}%-20s  ")
    }
    println("PRODS:")
    dumbScgEm_pc.foreach {
      case (t, prods) =>
        prods.normalizeValues.foreach {
          case (prod, p) => println(f"  $t%-20s -> $prod%-20s  ->  MCMC=${mcmcPcfgI_pc(t).normalizeValues.apply(prod)}%-20s  DUMB=${p.toDouble}%-20s  ")
        }
    }
    println("L-CTX:")
    dumbScgEm_lc.foreach {
      case (t, ls) =>
        ls.normalizeValues.foreach {
          case (l, p) => println(f"  $t%-20s -> $l%-20s  ->  MCMC=${mcmcPcfgI_lc(t).normalizeValues.apply(l)}%-20s  DUMB=${p.toDouble}%-20s  ")
        }
    }
    println("R-CTX:")
    dumbScgEm_rc.foreach {
      case (t, rs) =>
        rs.normalizeValues.foreach {
          case (r, p) => println(f"  $t%-20s -> $r%-20s  ->  MCMC=${mcmcPcfgI_rc(t).normalizeValues.apply(r)}%-20s  DUMB=${p.toDouble}%-20s  ")
        }
    }

  }

  def assertEqualsLog(a: LogDouble, b: LogDouble, t: Double) {
    assertEquals(a.toDouble, b.toDouble, t)
  }
}
