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
//class DualParserTests {
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
//    val tagger = new HmmTagger(
//      transitions = new Double2LogConditionalProbabilityDistributionAdapter(new SimpleConditionalExpProbabilityDistribution[Cat, Cat](Map(
//        (S) -> new SimpleExpProbabilityDistribution(Map( /*     */ (a / b) -> 0.1, (b) -> 0.9)),
//        (a / b) -> new SimpleExpProbabilityDistribution(Map( /* */ (b) -> 0.1, (a \ b) -> 0.1)),
//        (a \ b) -> new SimpleExpProbabilityDistribution(Map( /* */ E -> 0.1)),
//        (b) -> new SimpleExpProbabilityDistribution(Map( /*     */ (a \ b) -> 0.8, b -> 0.1, E -> 0.1)),
//        (E) -> new SimpleExpProbabilityDistribution(Map())))),
//      emissions = new Double2LogConditionalProbabilityDistributionAdapter(new SimpleConditionalExpProbabilityDistribution[Cat, String](Map(
//        (S) -> new SimpleExpProbabilityDistribution(Map("<S>" -> 1.0)),
//        (a / b) -> new SimpleExpProbabilityDistribution(Map( /**/ "1" -> 0.1)),
//        (a \ b) -> new SimpleExpProbabilityDistribution(Map( /**/ "2" -> 0.9)),
//        (b) -> new SimpleExpProbabilityDistribution(Map( /*    */ "1" -> 0.9, "2" -> 0.1)),
//        (E) -> new SimpleExpProbabilityDistribution(Map("<E>" -> 1.0))))),
//      tagdict)
//
//    val parser = PcfgParser(
//      rootDist = new Double2LogProbabilityDistributionAdapter(new SimpleExpProbabilityDistribution(Map(
//        (a) -> 1.0))),
//      prodDist = new Double2LogConditionalProbabilityDistributionAdapter(new SimpleConditionalExpProbabilityDistribution(Map(
//        (a) -> new SimpleExpProbabilityDistribution(Map(
//          NontermProd(a / b, b) -> 0.9,
//          NontermProd(b, a \ b) -> 0.1)),
//        (a / b) -> new SimpleExpProbabilityDistribution(Map(
//          TermProd("1") -> 0.5)),
//        (a \ b) -> new SimpleExpProbabilityDistribution(Map(
//          TermProd("2") -> 0.5)),
//        (b) -> new SimpleExpProbabilityDistribution(Map(
//          TermProd("1") -> 0.1, TermProd("2") -> 0.1))))),
//      tagdict = tagdict,
//      rules = rules)
//
//    //
//    //
//    //
//
//    val sent1 = "1 2".splitWhitespace
//
//    println(tagger.tag(sent1).mkString(" -> "))
//    println(parser.parse(sent1).get.pretty)
//    println
//
//    val guideChartBuilder = new OldToRemoveCfgGuideChartBuilder(rules)
//    val dualParser = new DualParser(parser, tagger, tagdict, guideChartBuilder, maxIterations = 10, deltaConst = 0.25, verbose = true)
//    println(dualParser.parse(sent1).get.pretty)
//
//  }
//
//  //@Test
//  def test1() {
//
//    val S = Cat.startCat
//    val E = Cat.endCat
//    val s = AtomCat("S")
//    val np = AtomCat("NP")
//    val n = AtomCat("N")
//    val pp = AtomCat("PP")
//
//    val rules = Vector(FA, BA)
//
//    val tagdict = SimpleTagDictionary[Cat](Map(
//      "the" -> Set(np / n, np),
//      "dog" -> Set(n, np, (s \ np) / np),
//      "walks" -> Set(s \ np, n, np)),
//      "<S>", S, "<E>", E)
//
//    val tagger = new HmmTagger(
//      transitions = new Double2LogConditionalProbabilityDistributionAdapter(new SimpleConditionalExpProbabilityDistribution[Cat, Cat](Map(
//        (S) -> new SimpleExpProbabilityDistribution(Map( /*     */ (np / n) -> 1.0, (n) -> 0.0, (np) -> 0.0, (s \ np) -> 0.0)),
//        (np / n) -> new SimpleExpProbabilityDistribution(Map( /**/ (np / n) -> 0.1, (n) -> 0.3, (np) -> 0.4, (s \ np) -> 0.1, E -> 0.1)),
//        (n) -> new SimpleExpProbabilityDistribution(Map( /*     */ (np / n) -> 0.1, (n) -> 0.4, (np) -> 0.1, (s \ np) -> 0.1, E -> 0.3)),
//        (np) -> new SimpleExpProbabilityDistribution(Map( /*    */ (np / n) -> 0.1, (n) -> 0.1, (np) -> 0.4, (s \ np) -> 0.1, E -> 0.3)),
//        (s \ np) -> new SimpleExpProbabilityDistribution(Map( /**/ (np / n) -> 0.1, (n) -> 0.1, (np) -> 0.1, (s \ np) -> 0.1, E -> 0.1)),
//        ((s \ np) / np) -> new SimpleExpProbabilityDistribution(Map( /**/ (np / n) -> 0.1, (n) -> 0.1, (np) -> 0.1, (s \ np) -> 0.1, E -> 0.1)),
//        (E) -> new SimpleExpProbabilityDistribution(Map())))),
//      emissions = new Double2LogConditionalProbabilityDistributionAdapter(new SimpleConditionalExpProbabilityDistribution[Cat, String](Map(
//        (S) -> new SimpleExpProbabilityDistribution(Map("<S>" -> 1.0)),
//        (np / n) -> new SimpleExpProbabilityDistribution(Map( /*       */ "the" -> 1.0, "dog" -> 0.0, "walks" -> 0.0)),
//        (n) -> new SimpleExpProbabilityDistribution(Map( /*            */ "the" -> 0.0, "dog" -> 0.5, "walks" -> 0.5)),
//        (np) -> new SimpleExpProbabilityDistribution(Map( /*           */ "the" -> 0.0, "dog" -> 0.5, "walks" -> 0.5)),
//        (s \ np) -> new SimpleExpProbabilityDistribution(Map( /*       */ "the" -> 0.0, "dog" -> 0.2, "walks" -> 0.8)),
//        ((s \ np) / np) -> new SimpleExpProbabilityDistribution(Map( /**/ "the" -> 0.0, "dog" -> 0.2, "walks" -> 0.8)),
//        (E) -> new SimpleExpProbabilityDistribution(Map("<E>" -> 1.0))))),
//      tagdict)
//
//    val parser = PcfgParser(
//      rootDist = new Double2LogProbabilityDistributionAdapter(new SimpleExpProbabilityDistribution(Map(
//        (np / n) -> 0.1,
//        (n) -> 0.1,
//        (np) -> 0.1,
//        (s) -> 0.6,
//        (s \ np) -> 0.1,
//        ((s \ np) / np) -> 0.1))),
//      prodDist = new Double2LogConditionalProbabilityDistributionAdapter(new SimpleConditionalExpProbabilityDistribution(Map(
//        (np / n) -> new SimpleExpProbabilityDistribution(Map(
//          TermProd("the") -> 1.0, TermProd("dog") -> 0.0, TermProd("walks") -> 0.0)),
//        (n) -> new SimpleExpProbabilityDistribution(Map(
//          TermProd("the") -> 0.0, TermProd("dog") -> 0.5, TermProd("walks") -> 0.5)),
//        (np) -> new SimpleExpProbabilityDistribution(Map(
//          NontermProd(np / n, n) -> 4.0,
//          TermProd("the") -> 0.0, TermProd("dog") -> 0.5, TermProd("walks") -> 0.5)),
//        (s) -> new SimpleExpProbabilityDistribution(Map(
//          NontermProd(np, s \ np) -> 0.9,
//          TermProd("the") -> 0.0, TermProd("dog") -> 0.0, TermProd("walks") -> 0.1)),
//        (s \ np) -> new SimpleExpProbabilityDistribution(Map(
//          NontermProd((s \ np) / np, np) -> 0.5,
//          TermProd("the") -> 0.0, TermProd("dog") -> 0.2, TermProd("walks") -> 0.3)),
//        ((s \ np) / np) -> new SimpleExpProbabilityDistribution(Map(
//          TermProd("the") -> 0.0, TermProd("dog") -> 0.2, TermProd("walks") -> 0.8))))),
//      tagdict = tagdict,
//      rules = rules)
//
//    //
//    //
//    //
//
//    val sent1 = "the dog walks".splitWhitespace
//
//    println(tagger.tag(sent1).mkString(" -> "))
//    println(parser.parse(sent1).get.pretty)
//    println
//
//    val guideChartBuilder = new OldToRemoveCfgGuideChartBuilder(rules)
//    val dualParser = new DualParser(parser, tagger, tagdict, guideChartBuilder, maxIterations = 10, verbose = true)
//    println(dualParser.parse(sent1).get.pretty)
//
//  }
//
//  //@Test
//  def test2() {
//
//    val s = AtomCat("S")
//    val np = AtomCat("NP")
//    val n = AtomCat("N")
//    val pp = AtomCat("PP")
//
//    val tagdict = SimpleTagDictionary[Cat](Map(
//      "time" -> Set(n, np, n / n, np / np),
//      "flies" -> Set(np, n, (s \ np) / pp),
//      "like" -> Set((s / np) \ np, pp / np),
//      "an" -> Set(np / n),
//      "arrow" -> Set(n),
//
//      "she" -> Set(n),
//      "eats" -> Set(s \ n, (s \ n) / n, ((s \ n) / pp) / n),
//      "pizza" -> Set(n, n / pp),
//      "without" -> Set(pp / n),
//      "hesitation" -> Set(n),
//      "anchovies" -> Set(n)),
//      "<S>", cat"<S>", "<E>", cat"<E>")
//
//    val tagger = new HmmTagger(
//      transitions = new Double2LogConditionalProbabilityDistributionAdapter(new SimpleConditionalExpProbabilityDistribution[Cat, Cat](Map(), None, None, new LaplaceExpProbabilityDistribution(Map(), Some(tagdict.allTags), None, 1.0))),
//      emissions = new Double2LogConditionalProbabilityDistributionAdapter(new SimpleConditionalExpProbabilityDistribution[Cat, String](Map(), None, None, new LaplaceExpProbabilityDistribution(Map(), Some(tagdict.allWords), None, 1.0))),
//      tagdict)
//
//    val parser = PcfgParser(
//      rootDist = new Double2LogProbabilityDistributionAdapter(new LaplaceExpProbabilityDistribution(Map(
//        s -> 7.8,
//        n -> 0.8,
//        (s \ n) -> 0.8),
//        None, None, 0.2)),
//      prodDist = new Double2LogConditionalProbabilityDistributionAdapter(new SimpleConditionalExpProbabilityDistribution(Map(
//        (s \ n) -> new LaplaceExpProbabilityDistribution(Map(
//          NontermProd((s \ n) / n, n) -> 4.0,
//          NontermProd((s \ n) / pp, pp) -> 1.0),
//          None, None, 0.2)),
//        None, None, new LaplaceExpProbabilityDistribution(Map((null: NontermProd) -> 1.0), None, None, 1.0))),
//      tagdict = tagdict,
//      rules = Vector(FA, BA))
//
//    //
//    //
//    //
//
//    val rules = Vector(FA, BA)
//    val guideChartBuilder = new OldToRemoveCfgGuideChartBuilder(rules)
//    val dualParser = new DualParser(parser, tagger, tagdict, guideChartBuilder, maxIterations = 20)
//
//    println(dualParser.parse("time flies like an arrow".splitWhitespace).get.pretty)
//
//  }
//
//}
