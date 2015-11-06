package dhg.ccg.parse.pcfg

import dhg.util._
import scalaz._
import scalaz.Scalaz._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tag.learn.EmissionInitializer // interface
import dhg.ccg.tag.learn.TagPriorInitializer // interface


//class ICPDUInitializer(
//  binaryProdDistInitializer: BinaryProdDistInitializer,
//  unaryProdDistInitializer: UnaryProdDistInitializer,
//  termProdDistInitializer: TermProdDistInitializer,
//  priorBinyProdMix: LogDouble,
//  priorUnryProdMix: LogDouble,
//  priorTermProdMix: LogDouble)
//  extends ProdDistInitializer {
//
//  private[this] val parseCounter = new OldToRemoveCfgParseCounter(guideChartBuilder)
//
//  override def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, Prod] = {
//    val tagdict = initialTagdict.withWords(bracketedSentences.flatMap(_._1).toSet)
//    val partialProductionCounts = bracketedSentences.mapt { (sentence, brackets) =>
//      val numParses = parseCounter.countParses(sentence, brackets, tagdict)
//      val productionCounts = parseCounter.countProductions(sentence, brackets, tagdict)
//      productionCounts.mapVals(_.mapVals(BigDecimal(_) / BigDecimal(numParses)))
//    }.reduce(_ |+| _)
//
//    val overallNontermCount = partialProductionCounts.values.flatten.count(_._1.isNt)
//    val overallTermCount = partialProductionCounts.values.flatten.count(_._1.isTerm)
//    val overallNontermLambda = overallNontermCount / (overallNontermCount + overallTermCount).toDouble
//
//    val nontermProdDist = nontermProdDistInitializer.fromBracketed(bracketedSentences, initialTagdict)
//    val termProdDist = termProdDistInitializer.fromBracketed(bracketedSentences, initialTagdict)
//
//    val nontermLambdas = partialProductionCounts.mapVals { prodCounts =>
//      val nontermCount = prodCounts.count(_._1.isNt)
//      val termCount = prodCounts.count(_._1.isTerm)
//      val ntl = nontermCount / (nontermCount + termCount).toDouble
//      (ntl * (1 - priorInterp)) + (overallNontermLambda * priorInterp)
//    }
//
//    new ICPDU(
//      nontermProdDist,
//      termProdDist,
//      nontermLambdas,
//      overallNontermLambda)
//  }
//
//  override def toString = f"ICPDUInitializer(nt=$nontermProdDistInitializer, term=$termProdDistInitializer, ${guideChartBuilder}, priorInterp=$priorInterp)"
//}
//
//class CheatingICPDUInitializer(
//  nontermProdDistInitializer: NontermProdDistInitializer,
//  termProdDistInitializer: TermProdDistInitializer,
//  supervisedCorpus: Vector[CcgTree])
//  extends ProdDistInitializer {
//
//  override def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, Prod] = {
//    val tagdict = initialTagdict.withWords(bracketedSentences.flatMap(_._1).toSet)
//
//    val partialProductionCounts = supervisedCorpus.map(SupervisedPcfgParserTrainer.getProdCounts).reduce(_ |+| _)
//
//    val overallNontermCount = partialProductionCounts.values.flatten.count(_._1.isNt)
//    val overallTermCount = partialProductionCounts.values.flatten.count(_._1.isTerm)
//    val overallNontermLambda = overallNontermCount / (overallNontermCount + overallTermCount).toDouble
//
//    val nontermProdDist = nontermProdDistInitializer.fromBracketed(bracketedSentences, initialTagdict)
//    val termProdDist = termProdDistInitializer.fromBracketed(bracketedSentences, initialTagdict)
//
//    val nontermLambdas = partialProductionCounts.mapVals { prodCounts =>
//      val nontermCount = prodCounts.count(_._1.isNt) + 0.1
//      val termCount = prodCounts.count(_._1.isTerm) + 0.1
//      nontermCount / (nontermCount + termCount).toDouble
//    }
//
//    new ICPDU(
//      nontermProdDist,
//      termProdDist,
//      nontermLambdas,
//      overallNontermLambda)
//  }
//
//  override def toString = f"CheatingICPDUInitializer(nt=$nontermProdDistInitializer, term=$termProdDistInitializer)"
//}
//
//class CheatingProdDistInitializer(supervisedCorpus: Vector[CcgTree], lambda: Double = 0.1)
//  extends ProdDistInitializer {
//
//  override def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, Prod] = {
//    val prodCounts = supervisedCorpus.map(SupervisedPcfgParserTrainer.getProdCounts).reduce(_ |+| _).mapVals(_.mapVals(LogDouble(_)))
//
//    val allKnownSupertags = initialTagdict.allTags | prodCounts.ungroup.collect { case (supertag, (TermProd(_), _)) => supertag }.toSet
//    val allKnownCats = prodCounts.ungroup.collect { case (a, (NontermProd(b, c), _)) => Vector(a, b, c) }.flatten.toSet | allKnownSupertags
//    val tagdict = initialTagdict
//      .withWords(prodCounts.values.flatten.collect { case (TermProd(word), _) => word }.toSet)
//      //.withTags(allKnownCats)
//      .withTags(allKnownSupertags)
//
//    val allKnownProds = Some(tagdict.allWords.map(TermProd(_)) ++ prodCounts.values.flatten.map(_._1).toSet)
//    new SimpleConditionalLogProbabilityDistribution(
//      prodCounts.mapt((c, cCounts) => c -> new LaplaceLogProbabilityDistribution(cCounts, allKnownProds, /* Some[Set[Prod]]((tagdict.entries.keySet -- tagdict.knownWordsForTag.getOrElse(c, Set.empty)).map(TermProd(_)))*/ None, LogDouble(lambda))),
//      new LaplaceLogProbabilityDistribution(Map(), allKnownProds, /*TODO: Some[Set[Prod]](tagdict.entries.keySet.map(TermProd(_)))*/ None, LogDouble(lambda)))
//  }
//
//  override def toString = f"CheatingProdDistInitializer(lambda=$lambda)"
//}
//
//
//
//class CatpriorNontermProdDistInitializer(
//  tagPriorInitializer: TagPriorInitializer[Cat],
//  rules: Vector[CcgRule],
//  lambda: Double)
//  extends NontermProdDistInitializer {
//  def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, NontermProd] = {
//    val tagdict = initialTagdict.withWords(bracketedSentences.flatMap(_._1).toSet)
//    val tagprior = tagPriorInitializer(bracketedSentences.map(_._1), tagdict)
//    //val allProds = for { a <- tagdict.allTags; b <- tagdict.allTags if rules.exists(r => r(a, b).isDefined) } yield NontermProd(a, b)
//    new ConditionalLogProbabilityDistribution[Cat, NontermProd] {
//      def apply(x: NontermProd, given: Cat): LogDouble = x match { case NontermProd(a, b) => tagprior(a) * tagprior(b) }
//      def sample(given: Cat): NontermProd = ???
//    }
//  }
//  override def toString = f"CatpriorNontermProdDistInitializer($tagPriorInitializer, ${rules.mkString("[", ",", "]")}, lambda=$lambda)"
//}
//
//class CheatingNontermProdDistInitializer(supervisedCorpus: Vector[CcgTree], lambda: Double = 0.1)
//  extends NontermProdDistInitializer {
//  def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, NontermProd] = {
//    val prodCounts = supervisedCorpus.map(SupervisedPcfgParserTrainer.getProductions).flatten.collect { case (a, p: NontermProd) => (a, p) }.groupByKey.mapVals(_.counts.mapVals(LogDouble(_)))
//    new SimpleConditionalLogProbabilityDistribution(
//      prodCounts.mapt((c, cCounts) => c -> new LaplaceLogProbabilityDistribution(cCounts, None, None, LogDouble(lambda))),
//      new LaplaceLogProbabilityDistribution[NontermProd](Map(), None, /*TODO: Some[Set[Prod]](tagdict.entries.keySet.map(TermProd(_)))*/ None, LogDouble(lambda)))
//  }
//  override def toString = f"CheatingNontermProdDistInitializer(lambda=$lambda)"
//}
//
//class UniformTermProdDistInitializer() extends TermProdDistInitializer {
//  def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, TermProd] = {
//    val tagdict = initialTagdict.withWords(bracketedSentences.flatMap(_._1).toSet)
//    val allTermProds = tagdict.allWords.map(TermProd(_))
//    new SimpleConditionalLogProbabilityDistribution[Cat, TermProd](Map.empty[Cat, LogProbabilityDistribution[TermProd]],
//      new LaplaceLogProbabilityDistribution[TermProd](Map(), Some(allTermProds), None, lambda = LogDouble(1.0)))
//  }
//  override def toString = f"UniformTermProdDistInitializer()"
//}
//
//case class EmInitializerTermProdDistInitializer(
//  emissionInitializer: EmissionInitializer[Cat])
//  extends TermProdDistInitializer {
//  def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, TermProd] = {
//    val emissionDist = emissionInitializer(bracketedSentences.map(_._1), initialTagdict)
//    new ConditionalLogProbabilityDistribution[Cat, TermProd] {
//      def apply(x: TermProd, given: Cat) = emissionDist(x.word, given)
//      def sample(given: Cat) = TermProd(emissionDist.sample(given))
//    }
//  }
//  override def toString = f"EmInitializerTermProdDistInitializer($emissionInitializer)"
//}
//
//case class TermProdDistInitializerEmInitializer(
//  termProdDistInitializer: TermProdDistInitializer)
//  extends EmissionInitializer[Cat] {
//  def apply(sentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, Word] = {
//    val termProdDist = termProdDistInitializer.apply(sentences, initialTagdict)
//    new ConditionalLogProbabilityDistribution[Cat, Word] {
//      def apply(x: Word, given: Cat) = termProdDist(TermProd(x), given)
//      def sample(given: Cat) = termProdDist.sample(given).word
//    }
//  }
//  override def toString = f"TermProdDistInitializerEmInitializer($termProdDistInitializer)"
//}
//
//class CheatingTermProdDistInitializer(supervisedCorpus: Vector[CcgTree], lambda: Double = 0.1) extends TermProdDistInitializer {
//  def apply(sentences: Vector[CfgGuideChart], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, TermProd] = {
//    val prodCounts = supervisedCorpus.map(SupervisedPcfgParserTrainer.getProductions).flatten.collect { case (a, p: TermProd) => (a, p) }.groupByKey.mapVals(_.counts.mapVals(LogDouble(_)))
//    val allKnownSupertags = initialTagdict.allTags ++ prodCounts.map(_._1)
//    val tagdict = initialTagdict
//      .withWords(prodCounts.values.flatten.collect { case (TermProd(word), _) => word }.toSet)
//      //.withTags(allKnownCats)
//      .withTags(allKnownSupertags)
//
//    new SimpleConditionalLogProbabilityDistribution(
//      prodCounts.mapt((c, cCounts) => c -> new LaplaceLogProbabilityDistribution(cCounts, None, /*Some[Set[Prod]]((tagdict.entries.keySet -- tagdict.knownWordsForTag.getOrElse(c, Set.empty)).map(TermProd(_)))*/ None, LogDouble(lambda))),
//      new LaplaceLogProbabilityDistribution[TermProd](Map(), None, /*TODO: Some[Set[Prod]](tagdict.entries.keySet.map(TermProd(_)))*/ None, LogDouble(lambda)))
//  }
//  override def toString = f"CheatingTermProdDistInitializer(lambda=$lambda)"
//}
