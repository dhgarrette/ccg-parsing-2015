//package dhg.ccg.dd
//
//import dhg.ccg.cat._
//import dhg.ccg.parse.pcfg._
//import dhg.ccg.prob._
//import dhg.ccg.tagdict.TagDictionary
//import dhg.ccg.tagdict.SimpleTagDictionary
//import dhg.ccg.tagdict.SimpleTagDictionaryFactory
//import dhg.util._
//import scalaz._
//import Scalaz._
//import dhg.ccg.prob._
//import dhg.ccg.cat._
//import dhg.ccg.data._
//import dhg.util.viz._
//import dhg.ccg.parse._
//import dhg.ccg.tag._
//import scala.collection.mutable.{ Map => MMap }
//import scala.collection.mutable.{ Set => MSet }
//import scalaz._
//import Scalaz._
//import scala.collection.breakOut
//import scala.util.Try
//import dhg.ccg.tag.learn._
//import dhg.ccg.parse._
//import dhg.ccg.parse.pcfg._
//import dhg.ccg.parse.scg._
//
//object DualExp {
//
//  def main(args: Array[String]): Unit = {
//    val (arguments, options) = parseArgs(args)
//
//    val tdCutoff = options.get("tdc").fold(0.0)(_.toDouble)
//    val tdIncludesTest = options.get("tdit").fold(false)(_.toBoolean)
//
//    type Word = String
//    val rules = Vector(FA, BA)
//    val reader = EnglishCcgTreeBankReader(rules, Vector(FAb, BAb), RebankRules.standard)
//    val parseTrainData = reader.rawCHEATING
//    val supertaggedTrainData = parseTrainData.map(_.tagged)
//    val rawData = supertaggedTrainData.map(_.map(_._1))
//    val testParseData = reader.devData
//    val testSupertagData = testParseData.map(_.tagged)
//    val tdData = reader.tdData.map(_.tagged)
//    val tdTagdict = new SimpleTagDictionaryFactory(Some(tdCutoff))(tdData, "<S>", cat"<S>", "<E>", cat"<E>").withWords(rawData.flatten.toSet)
//    val tagdict: TagDictionary[Cat] =
//      if (!tdIncludesTest) tdTagdict
//      else SimpleTagDictionary(
//        (tdTagdict.entries.ungroup ++ (supertaggedTrainData ++ testSupertagData).flatten).groupByKey.mapVals(_.toSet),
//        tdTagdict.startWord, tdTagdict.startTag, tdTagdict.endWord, tdTagdict.endTag,
//        tdTagdict.allWords, tdTagdict.allTags,
//        tdTagdict.excludedTags)
//
//    val guideChartBuilder = new OldToRemoveCfgGuideChartBuilder(rules) // TODO: ?
//
//    val taggerEvaluator = TaggerEvaluator
//    val parserEvaluator = new ParserEvaluator(guideChartBuilder, goldSupertags = options.get("gs").fold(false)(_.toBoolean))
//
//    println(f"tdCutoff = ${tdCutoff}")
//    println(f"tdIncludesTest = ${tdIncludesTest}")
//    println(f"taggerEvaluator = ${taggerEvaluator}")
//    println(f"parserEvaluator = ${parserEvaluator}")
//
//    val maxIterations = 50
//    val combinabilityCcgRules = Vector(FA, FAn2np, BA, BAn2np, FC, FC2, BC, /*FX,*/ BX, BX2)
//    val pTerm = 0.6
//    val pMod = 0.8
//    val pFwd = 0.5
//    val atomLambda = 1000.0
//    val catpriorMass = 0.5
//    val canCombine = new SimpleCatCanCombine(combinabilityCcgRules, tagdict.startTag, tagdict.endTag)
//    val baseTrInit = new CcgCombinabilityTrInitializer(new TrTagDictEntriesPossibilities(new AddLambdaTransitionDistributioner(0.1)), canCombine, 0.95)
//    val catPriorTrInit = new TagPriorTrInitializer(new NormalizingCatgramCatPriorInitializer(new TagdictInformedAtomCatDistInitializer(atomLambda), pTerm, pMod, pFwd))
//    val trInit = new InterpolatingTransitionInitializer[Cat](Vector(baseTrInit -> (1 - catpriorMass), catPriorTrInit -> catpriorMass))
//    val emInit = new EmTagDictionaryEstimate[Cat](new TagDictionaryEstimateTagPriorInitializer(0.1), 0.1, 0.1)
//    val trSmooth = new AddLambdaTransitionDistributioner[Cat](0.1)
//    val emSmooth = new AddLambdaEmissionDistributioner[Cat](0.1)
//    val hmmDistributioner = new SmoothedHmmTaggerTrainer(trSmooth, emSmooth)
//    val pcfgDistributioner = new AddLambdaSmoothedPcfgParserTrainer(0.1, guideChartBuilder)
//    val alphaT = 500.0
//    val alphaE = 500.0
//    val alphaRoot = 0.0
//    val alphaProd = 0.0
//    val CHEATING_SUPERVISED_DATA = reader.readTrees(0 to 24).toVector
//
//    //    val (rootDistInit, prodDistInit, _, _, _, _) = ScgExp.makeInits(options, rules, tagdict, CHEATING_SUPERVISED_DATA)
//    //
//    //    val supTagger = hmmDistributioner.train(supertaggedTrainData, tagdict)
//    //    println("\nSupervised HMM"); taggerEvaluator(supTagger, testSupertagData)
//    //    val taggerNoEm = new HmmTagger(trInit(rawData, tagdict), emInit(rawData, tagdict), tagdict)
//    //    println("\nHMM NoEm"); taggerEvaluator(taggerNoEm, testSupertagData)
//    //    val softTagger = new SimpleTypeSupervisedTaggerTrainer(new SoftEmHmmTaggerTrainer(maxIterations, trSmooth, emSmooth, alphaT, alphaE, 1e-20), trInit, emInit).typesupTrain(rawData, tagdict)
//    //    println("\nSoft EM HMM"); taggerEvaluator(softTagger, testSupertagData)
//    //    val hardTagger = new SimpleTypeSupervisedTaggerTrainer(new HardEmHmmTaggerTrainer(maxIterations, trSmooth, emSmooth, alphaT, alphaE, 1e-20), trInit, emInit).typesupTrain(rawData, tagdict)
//    //    println("\nHard EM HMM"); taggerEvaluator(hardTagger, testSupertagData)
//    //
//    //    val supParser = pcfgDistributioner.train(parseTrainData, tagdict)
//    //    println("\nSupervised PCFG"); parserEvaluator.evaluate(supParser, testParseData, tagdict)
//    //    val parserNoEm = new OldToRemovePcfgParser(rootDistInit(rawData, tagdict), nontermDistInit(rawData, tagdict), termDistInit(rawData, tagdict), tagdict, rules)
//    //    println("\nPCFG NoEm"); parserEvaluator.evaluate(parserNoEm, testParseData, tagdict)
//    //    val softParser = new SimpleTypeSupervisedParserTrainer(new SoftEmPcfg(maxIterations, pcfgDistributioner, rules, alphaRoot, alphaNonterm, alphaTerm, 1e-20), rootDistInit, nontermDistInit, termDistInit).typesupTrain(rawData, tagdict)
//    //    println("\nSoft EM PCFG"); parserEvaluator.evaluate(softParser, testParseData, tagdict)
//    //    val hardParser = new SimpleTypeSupervisedParserTrainer(new HardEmPcfg(maxIterations, pcfgDistributioner, rules, alphaRoot, alphaProd, 1e-20), rootDistInit, prodDistInit).typesupTrain(rawData, tagdict)
//    //    println("\nHard EM PCFG"); parserEvaluator.evaluate(hardParser, testParseData, tagdict)
//    //
//    //    val separateSupDualParser = new DualParser(PcfgParser(supParser.asInstanceOf[PcfgParser]), HmmTagger(supTagger.asInstanceOf[HmmTagger[Cat]]), maxIterations)
//    //    println("\nSeparately supervised Dual HMM/PCFG"); parserEvaluator.evaluate(separateSupDualParser, testParseData, tagdict)
//    //    val separateSoftDualParser = new DualParser(PcfgParser(softParser.asInstanceOf[PcfgParser]), HmmTagger(softTagger.asInstanceOf[HmmTagger[Cat]]), maxIterations)
//    //    println("\nSeparately soft EM trained Dual HMM/PCFG"); parserEvaluator.evaluate(separateSoftDualParser, testParseData, tagdict)
//    //    val separateHardDualParser = new DualParser(PcfgParser(hardParser.asInstanceOf[PcfgParser]), HmmTagger(hardTagger.asInstanceOf[HmmTagger[Cat]]), maxIterations)
//    //    println("\nSeparately hard EM trained Dual HMM/PCFG"); parserEvaluator.evaluate(separateHardDualParser, testParseData, tagdict)
//    //    val jointDualParser: Parser = new DualParserTrainer(trInit, emInit, pcfgDistributioner, hmmDistributioner, rules, maxEmIterations = 10, maxDecompIterations = 20, alphaT, alphaE, alpha1 = 1.0, alpha2 = 1.0, alpha3 = 1.0).train(rawData, tagdict)
//    //    println("\nJoint hard EM trained Dual HMM/PCFG"); parserEvaluator.evaluate(jointDualParser, testParseData, tagdict)
//
//  }
//
//}
