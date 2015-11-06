//package dhg.ccg.dd
//
//import dhg.util._
//import dhg.util._
//import org.junit.Test
//import org.junit.Assert._
//import dhg.ccg.parse._
//import dhg.ccg.prob._
//import dhg.ccg.cat._
//import dhg.ccg.tagdict.SimpleTagDictionary
//import dhg.ccg.parse.pcfg._
//import dhg.ccg.tag._
//
//class DualParserTrainerTests {
//
//  @Test
//  def test() {
//
//    val S = Cat.startCat
//    val E = Cat.endCat
//    val a = AtomCat("A")
//    val b = AtomCat("B")
//
//    val rules = Vector(FA, BA)
//
//    val tagdict = SimpleTagDictionary[Cat](Map(
//      "1" -> Set(a / b, b),
//      "2" -> Set(b, a \ b)),
//      "<S>", S, "<E>", E)
//
//    val baseTrDist = new SimpleConditionalExpProbabilityDistribution[Cat, Cat](Map(
//      (S) -> new SimpleExpProbabilityDistribution(Map( /*     */ (a / b) -> 0.1, (b) -> 0.9)),
//      (a / b) -> new SimpleExpProbabilityDistribution(Map( /* */ (b) -> 0.1, (a \ b) -> 0.1)),
//      (a \ b) -> new SimpleExpProbabilityDistribution(Map( /* */ E -> 0.1)),
//      (b) -> new SimpleExpProbabilityDistribution(Map( /*     */ (a \ b) -> 0.8, b -> 0.1, E -> 0.1)),
//      (E) -> new SimpleExpProbabilityDistribution(Map())))
//    val baseEmDist = new SimpleConditionalExpProbabilityDistribution[Cat, String](Map(
//      (S) -> new SimpleExpProbabilityDistribution(Map("<S>" -> 1.0)),
//      (a / b) -> new SimpleExpProbabilityDistribution(Map( /**/ "1" -> 0.1)),
//      (a \ b) -> new SimpleExpProbabilityDistribution(Map( /**/ "2" -> 0.9)),
//      (b) -> new SimpleExpProbabilityDistribution(Map( /*    */ "1" -> 0.9, "2" -> 0.1)),
//      (E) -> new SimpleExpProbabilityDistribution(Map("<E>" -> 1.0))))
//
//    val baseRootDist = new SimpleExpProbabilityDistribution[Cat](Map(
//      (a) -> 1.0))
//    //    val baseProdDist = new SimpleConditionalExpProbabilityDistribution[Cat, Prod](Map(
//    //      (a) -> new SimpleExpProbabilityDistribution(Map(
//    //        NontermProd(a / b, b) -> 0.9,
//    //        NontermProd(b, a \ b) -> 0.1)),
//    //      (a / b) -> new SimpleExpProbabilityDistribution(Map(
//    //        TermProd("1") -> 0.5)),
//    //      (a \ b) -> new SimpleExpProbabilityDistribution(Map(
//    //        TermProd("2") -> 0.5)),
//    //      (b) -> new SimpleExpProbabilityDistribution(Map(
//    //        TermProd("1") -> 0.1, TermProd("2") -> 0.1))))
//    val baseNontDist = new SimpleConditionalExpProbabilityDistribution[Cat, NontermProd](Map(
//      (a) -> new SimpleExpProbabilityDistribution(Map(
//        NontermProd(a / b, b) -> 0.9,
//        NontermProd(b, a \ b) -> 0.1))))
//    val baseTermDist = new SimpleConditionalExpProbabilityDistribution[Cat, TermProd](Map(
//      (a / b) -> new SimpleExpProbabilityDistribution(Map(
//        TermProd("1") -> 0.5)),
//      (a \ b) -> new SimpleExpProbabilityDistribution(Map(
//        TermProd("2") -> 0.5)),
//      (b) -> new SimpleExpProbabilityDistribution(Map(
//        TermProd("1") -> 0.1, TermProd("2") -> 0.1))))
//
//    //
//    //
//    //
//
//    val sent1 = "1 2".splitWhitespace
//
//    val guideChartBuilder = new OldToRemoveCfgGuideChartBuilder(rules)
//    val dualTrainer = new DualParserTrainer(
//      new AddLambdaSmoothedPcfgParserTrainer[String](0.1, guideChartBuilder),
//      new AddLambdaSmoothedHmmTaggerTrainer[String, Cat](0.1),
//      maxEmIterations = 5, maxDecompIterations = 10, deltaConst = 1.0,
//      alphaRoots = 1.0, alphaNonts = 1.0, alphaTerms = 1.0, alphaTrans = 1.0, alphaEmiss = 1.0,
//      guideChartBuilder)
//    val dualParser = dualTrainer.train(Vector(sent1), tagdict, new Double2LogProbabilityDistributionAdapter(baseRootDist), new Double2LogConditionalProbabilityDistributionAdapter(baseNontDist), new Double2LogConditionalProbabilityDistributionAdapter(baseTermDist), new Double2LogConditionalProbabilityDistributionAdapter(baseTrDist), new Double2LogConditionalProbabilityDistributionAdapter(baseEmDist))
//    println(dualParser.parse(sent1).get.pretty)
//
//  }
//
//}
