package dhg.ccg.parse.pcfg

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.tag.UnsmoothedHmmTaggerTrainer
import dhg.util._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.parse.pcfg.typesup._
import dhg.ccg.tag.learn.EmUniform
import dhg.ccg.parse.scg.exp.Em2TermProdDist
import dhg.ccg.tagdict.TagDictionary

class EmPcfgTests {

  val s = cat"S".asInstanceOf[AtomCat]
  val np = cat"NP".asInstanceOf[AtomCat]
  val n = cat"N".asInstanceOf[AtomCat]
  val pp = cat"PP".asInstanceOf[AtomCat]

  val A = cat"A".asInstanceOf[AtomCat]
  val B = cat"B".asInstanceOf[AtomCat]
  val C = cat"C".asInstanceOf[AtomCat]
  val D = cat"D".asInstanceOf[AtomCat]
  val E = cat"E".asInstanceOf[AtomCat]
  val F = cat"F".asInstanceOf[AtomCat]

  val startWord = "<S>"
  val startTag = cat"<S>"
  val endWord = "<E>"
  val endTag = cat"<E>"

  @Test
  def test_1 {
    type Word = String
    type Tag = Cat

    val pcfgProductionFinder = new SimplePcfgProductionCounter()
    val pcfgGuideChartProdFinder = new SimplePcfgGuideChartProdFinder()
    val pcfgAlphaPriorMaker = new TrainDataPcfgAlphaPriorMaker(pcfgProductionFinder, pcfgGuideChartProdFinder)
    val pcfgInsideChartBuilder = new SimplePcfgInsideChartBuilder()
    val pcfgParserInstantiater = new ExactPcfgParserInstantiater()

    //    val priorRootDist = new UniformRootDistInitializer().apply(Vector.empty, SimpleTagDictionary.empty(startWord, startTag, endWord, endTag))
    //    val priorBinyDist = new UniformBinaryDistInitializer().apply(Vector.empty, SimpleTagDictionary.empty(startWord, startTag, endWord, endTag))
    //    val priorUnryDist = new UniformUnaryDistInitializer().apply(Vector.empty, SimpleTagDictionary.empty(startWord, startTag, endWord, endTag))
    //    val priorTermDist = Em2TermProdDist(new EmUniform[String, Cat]().fromKnownSupertagSets(Vector.empty, SimpleTagDictionary.empty(startWord, startTag, endWord, endTag)))

    val em = new EmPcfg(
      maxIterations = 10,
      pcfgAlphaPriorMaker,
      alphaRoot = 0.0, alphaBiny = 0.0, alphaUnry = 0.0, alphaTerm = 0.0,
      priorBinyProdMix = 1.0 / 3, priorUnryProdMix = 1.0 / 3, priorTermProdMix = 1.0 / 3,
      alphaProd = 0.0,
      pcfgInsideChartBuilder,
      pcfgParserInstantiater)

    //

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
      Map[Word, Set[Tag]](
        "the" -> Det,
        "big" -> Adj,
        "man" -> N,
        "dog" -> N,
        "dogs" -> N,
        "cats" -> N,
        "telescope" -> N,
        "saw" -> (IV | TV),
        "walked" -> (IV | TV),
        "chase" -> TV,
        "run" -> IV,
        "ran" -> IV,
        "John" -> NNP,
        "Mary" -> NNP,
        "with" -> Prep),
      "<S>", cat"<S>", "<E>", cat"<E>")

    val sentences = Vector(
      //      "the dogs walked",
      //      "the man walked the dog",
      //      "dogs chase cats",
      //      "big dogs run",
      //      "the big dogs run",
      "John saw Mary with the dog")
    //      ,
    //      "John saw Mary with the telescope",
    //      "John saw Mary with the dog with the telescope")

    val gcBuilder = new SimpleCfgGuideChartBuilder(Vector(FA, BA, N2NP), additionalSupertagAdder = new PresentTagdictAdditionalTagAdder, allowTerminalDeletion = false)
    val rawDataGC = sentences.flatMap(s => gcBuilder.build(s.splitWhitespace, None, tagdict))

    def gc2supertagSets(gc: CfgGuideChart, td: TagDictionary[Cat]) = (gc.words zipSafe gc.supertagSets).mapt { (w, tags) => (w, tags & td.entries.getOrElse(w, Set.empty)) }
    val priorRootDist = new UniformRootDistInitializer().apply(rawDataGC, tagdict)
    val priorBinyDist = new UniformBinaryDistInitializer().apply(rawDataGC, tagdict)
    val priorUnryDist = new UniformUnaryDistInitializer().apply(rawDataGC, tagdict)
    val priorTermDist = Em2TermProdDist(new EmUniform[Cat]().fromKnownSupertagSets(rawDataGC.map(gc2supertagSets(_, tagdict)), tagdict))

    println(rawDataGC.only.numPossibleParses)

    {
      val priorProdDist = new ICPDU(priorBinyDist, priorUnryDist, priorTermDist,
        Map().withDefaultValue((LogDouble(1.0/3), LogDouble(1.0/3), LogDouble(1.0/3))))
      val ic = pcfgInsideChartBuilder.buildInsideChart(rawDataGC.only, priorProdDist)
      ic.draw()
    }

    val r = em.trainGuideChartsWithSomeGold(rawDataGC, Vector.empty, priorRootDist, priorBinyDist, priorUnryDist, priorTermDist)
    println(r)

  }

  //  //@Test
  //  def hard_test1() {
  //    /*
  //     *  +-------------+--------------+--------------+
  //     *  | np/n -> 0.8 | np ->        | s ->         |
  //     *  | n/n -> 0.1  | n ->         | n ->         |
  //     *  | n/s -> 0.1  |              |              |
  //     *  +-------------+--------------+--------------+
  //     *                | n -> 0.6     | s ->         |
  //     *                | np -> 0.4    |              |
  //     *                |              |              |
  //     *                +--------------+--------------+
  //     *                               | s\np -> 0.7  |
  //     *                               | s\n -> 0.2   |
  //     *                               | s -> 0.1     |
  //     *                               +--------------+
  //     *
  //     * np/n -> the     : 0.21
  //     * n/n  -> the     : 0.02
  //     * n/s  -> the     : 0.01
  //     * n    -> dogs    : 0.05
  //     * np   -> dogs    : 0.04
  //     * s\np -> run     : 0.06
  //     * s\n  -> run     : 0.01
  //     * s    -> run     : 0.03
  //     * 
  //     * np -> np/n n    : 0.45
  //     * np -> X         : 0.55
  //     * n  -> n/n  n    : 0.25
  //     * n  -> n/s  s    : 0.10
  //     * n  -> X         : 0.65
  //     * s  -> np   s\np : 0.65
  //     * s  -> n    s\n  : 0.15
  //     * s  -> X         : 0.20
  //     * 
  //     * p(s) : 0.7
  //     * p(n) : 0.2
  //     * p(X) : 0.1
  //     * 
  //     * 
  //     * v02(np): p(np -> np/n n) * v01(np/n) * v12(n) = 0.45 * 0.21 * 0.05 = 0.004725
  //     * v02(n):  p(n  -> n/n  n) * v01(n/n)  * v12(n) = 0.25 * 0.02 * 0.05 = 0.000250
  //     * 
  //     * v13(s): p(s -> np s\np) * v12(np) * v23(s\np) = 0.65 * 0.04 * 0.06 = 0.001560
  //     * v13(s): p(s -> n  s\n ) * v12(n)  * v23(s\n)  = 0.15 * 0.05 * 0.01 = 0.000075
  //     * 
  //     * v03(s): p(s -> np s\np) * v02(np) * v23(s\np) = 0.65 * 0.004725 * 0.06 = 0.000184275
  //     * v03(s): p(s -> n  s\n ) * v02(n)  * v23(s\n)  = 0.15 * 0.000250 * 0.01 = 0.000000375
  //     * v03(n): p(n -> n/s  s) * v01(n/s) * v13(s)    = 0.10 * 0.01 * 0.001560 = 0.000001560
  //     * 
  //     * p(s) * v03(s) = 0.7 * 0.000184275 = 0.0001289925
  //     * p(n) * v03(n) = 0.2 * 0.000001560 = 0.0000003120
  //     * 
  //     */
  //
  //    val tagdict = SimpleTagDictionary[Cat](Map(
  //      "the" -> Set(n / n, np / n, n / s),
  //      "dogs" -> Set(np, n),
  //      "run" -> Set(s \ n, s \ np, s)),
  //      "<S>", cat"<S>", "<E>", cat"<E>")
  //
  //    val rootDist = new Double2LogProbabilityDistributionAdapter(new SimpleExpProbabilityDistribution[Cat](Map(
  //      s -> 0.7,
  //      n -> 0.2,
  //      A -> 0.1)))
  //    val prodDist = new Double2LogConditionalProbabilityDistributionAdapter(new SimpleConditionalExpProbabilityDistribution[Cat, Prod](Map(
  //      np -> new DefaultedExpProbabilityDistribution(Map(
  //        NontermProd(np / n, n) -> 0.45,
  //        NontermProd(A, A) -> 0.55,
  //        TermProd("dogs") -> 0.04,
  //        TermProd("X") -> 0.96)),
  //      n -> new DefaultedExpProbabilityDistribution(Map(
  //        NontermProd(n / n, n) -> 0.25,
  //        NontermProd(n / s, s) -> 0.10,
  //        NontermProd(A, A) -> 0.65,
  //        TermProd("dogs") -> 0.05,
  //        TermProd("X") -> 0.95)),
  //      s -> new DefaultedExpProbabilityDistribution(Map(
  //        NontermProd(np, s \ np) -> 0.65,
  //        NontermProd(n, s \ n) -> 0.15,
  //        NontermProd(A, A) -> 0.2,
  //        TermProd("run") -> 0.03,
  //        TermProd("X") -> 0.97)),
  //      (np / n) -> new DefaultedExpProbabilityDistribution(Map(
  //        TermProd("the") -> 0.21,
  //        TermProd("X") -> 0.79)),
  //      (n / n) -> new DefaultedExpProbabilityDistribution(Map(
  //        TermProd("the") -> 0.02,
  //        TermProd("X") -> 0.98)),
  //      (n / s) -> new DefaultedExpProbabilityDistribution(Map(
  //        TermProd("the") -> 0.01,
  //        TermProd("X") -> 0.99)),
  //      (s \ np) -> new DefaultedExpProbabilityDistribution(Map(
  //        TermProd("run") -> 0.06,
  //        TermProd("X") -> 0.94)),
  //      (s \ n) -> new DefaultedExpProbabilityDistribution(Map(
  //        TermProd("run") -> 0.01,
  //        TermProd("X") -> 0.99)))))
  //    val rules = Vector(FA, BA)
  //    val sentences = Vector(
  //      "the dogs run").map(_.splitWhitespace)
  //
  //    val guideChartBuilder = new OldToRemoveCfgGuideChartBuilder(rules)
  //    val hard1 = new HardEmPcfg(2, new UnsmoothedPcfgParserTrainer(guideChartBuilder), guideChartBuilder, convergence = 1e-20).train(sentences, tagdict, rootDist, prodDist)
  //
  //    //    val ot = hard1.parse("the dogs run".split("\\s+").toVector)
  //    //    println(ot)
  //    //    ot.foreach(t => println(t.pretty))
  //  }
  //
  //  @Test
  //  def soft_test1() {
  //    /*
  //     *  +-------------+--------------+--------------+
  //     *  | np/n -> 0.8 | np ->        | s ->         |
  //     *  | n/n -> 0.1  | n ->         | n ->         |
  //     *  | n/s -> 0.1  |              |              |
  //     *  +-------------+--------------+--------------+
  //     *                | n -> 0.6     | s ->         |
  //     *                | np -> 0.4    |              |
  //     *                |              |              |
  //     *                +--------------+--------------+
  //     *                               | s\np -> 0.7  |
  //     *                               | s\n -> 0.2   |
  //     *                               | s -> 0.1     |
  //     *                               +--------------+
  //     *
  //     * np/n -> the     : 0.21
  //     * n/n  -> the     : 0.02
  //     * n/s  -> the     : 0.01
  //     * n    -> dogs    : 0.05
  //     * np   -> dogs    : 0.04
  //     * s\np -> run     : 0.06
  //     * s\n  -> run     : 0.01
  //     * s    -> run     : 0.03
  //     * 
  //     * np -> np/n n    : 0.45
  //     * np -> X         : 0.55
  //     * n  -> n/n  n    : 0.25
  //     * n  -> n/s  s    : 0.10
  //     * n  -> X         : 0.65
  //     * s  -> np   s\np : 0.65
  //     * s  -> n    s\n  : 0.15
  //     * s  -> X         : 0.20
  //     * 
  //     * p(s) : 0.7
  //     * p(n) : 0.2
  //     * p(X) : 0.1
  //     * 
  //     * 
  //     * INSIDE:
  //     *       
  //     * i02(np): p(np -> np/n n) * i01(np/n) * i12(n) = 0.45 * 0.21 * 0.05 = 0.004725
  //     * i02(n):  p(n  -> n/n  n) * i01(n/n)  * i12(n) = 0.25 * 0.02 * 0.05 = 0.000250
  //     * 
  //     * i13(s): p(s -> np s\np) * i12(np) * i23(s\np) = 0.65 * 0.04 * 0.06 = 0.001560
  //     *         p(s -> n  s\n ) * i12(n)  * i23(s\n)  = 0.15 * 0.05 * 0.01 = 0.000075
  //     *                                                                    = 0.001635
  //     * 
  //     * i03(s): p(s -> np s\np) * i02(np) * i23(s\np) = 0.65 * 0.004725 * 0.06 = 0.000184275
  //     *         p(s -> n  s\n ) * i02(n)  * i23(s\n)  = 0.15 * 0.000250 * 0.01 = 0.000000375
  //     *                                                                        = 0.000184650
  //     * i03(n): p(n -> n/s  s) * i01(n/s) * i13(s)    = 0.10 * 0.01 * 0.001635 = 0.000001635
  //     * 
  //     * p(s) * i03(s) = 0.7 * 0.000184650 = 0.0001292550
  //     * p(n) * i03(n) = 0.2 * 0.000001635 = 0.0000003270
  //     * 
  //     * p                                 = 0.0001295820
  //     * 
  //     * 
  //     * OUTSIDE:
  //     * 
  //     * o03(s) = 0.7
  //     * o03(n) = 0.2
  //     * 
  //     * o02(np) = p(s -> np s\np) * i23(s\np) * o03(s) = 0.65 * 0.06 * 0.7 = 0.02730
  //     * o02(n)  = p(s -> n  s\n ) * i23(s\n)  * o03(s) = 0.15 * 0.01 * 0.7 = 0.00105
  //     * 
  //     * o13(s)  = p(n -> n/s s)   * i01(n/s)  * o03(n) = 0.10 * 0.01 * 0.2 = 0.00020
  //     * 
  //     * o01(np/n) = p(np -> np/n n) * i12(n) * o02(np) = 0.45 * 0.05 * 0.02730 = 0.000614250
  //     * o01(n/n)  = p(n  -> n/n  n) * i12(n) * o02(n)  = 0.25 * 0.05 * 0.00105 = 0.000013125
  //     * o01(n/s)  = p(n  -> n/s  s) * i13(s) * o03(n)  = 0.10 * 0.001635 * 0.2 = 0.000032700
  //     * 
  //     * o12(np)   = p(s -> np s\np) * i23(s\np) * o13(s)  = 0.65 * 0.06 * 0.00020 = 0.00000780
  //     * o12(n)    = p(np -> np/n n) * i01(np/n) * o02(np) = 0.45 * 0.21 * 0.02730 = 0.00257985
  //     *           + p(n -> n/n n)   * i01(n/n)  * o02(n)  = 0.25 * 0.02 * 0.00105 = 0.00000525
  //     *           + p(s -> n s\n)   * i23(s\n)  * o13(s)  = 0.15 * 0.01 * 0.00020 = 0.00000030
  //     *                                                                           = 0.00258615
  //     *  
  //     * o23(s\np) = p(s -> np s\np) * i12(np) * o13(s) = 0.65 * 0.04 * 0.00020 = 0.000005200
  //     *           + p(s -> np s\np) * i02(np) * o03(s) = 0.65 * 0.004725 * 0.7 = 0.002149875
  //     *                                                                        = 0.002155075
  //     * o23(s\n)  = p(s -> n s\n)   * i12(n)  * o13(s) = 0.15 * 0.05 * 0.00020 = 0.00000150
  //     *           + p(s -> n s\n)   * i02(n)  * o03(s) = 0.15 * 0.000250 * 0.7 = 0.00002625
  //     *                                                                        = 0.00003150
  //     * 
  //     * 
  //     * EXPECTATIONS:
  //     * 
  //     * 
  //     * 
  //     */
  //
  //    val tagdict = SimpleTagDictionary[Cat](Map(
  //      "the" -> Set(n / n, np / n, n / s),
  //      "dogs" -> Set(np, n),
  //      "run" -> Set(s \ n, s \ np, s)),
  //      "<S>", cat"<S>", "<E>", cat"<E>")
  //
  //    val rootDist = new Double2LogProbabilityDistributionAdapter(new SimpleExpProbabilityDistribution[Cat](Map(
  //      s -> 0.7,
  //      n -> 0.2,
  //      A -> 0.1)))
  //    val prodDist = new Double2LogConditionalProbabilityDistributionAdapter(new SimpleConditionalExpProbabilityDistribution[Cat, Prod](Map(
  //      np -> new DefaultedExpProbabilityDistribution(Map(
  //        NontermProd(np / n, n) -> 0.45,
  //        NontermProd(A, A) -> 0.55,
  //        TermProd("dogs") -> 0.04,
  //        TermProd("X") -> 0.96)),
  //      n -> new DefaultedExpProbabilityDistribution(Map(
  //        NontermProd(n / n, n) -> 0.25,
  //        NontermProd(n / s, s) -> 0.10,
  //        NontermProd(A, A) -> 0.65,
  //        TermProd("dogs") -> 0.05,
  //        TermProd("X") -> 0.95)),
  //      s -> new DefaultedExpProbabilityDistribution(Map(
  //        NontermProd(np, s \ np) -> 0.65,
  //        NontermProd(n, s \ n) -> 0.15,
  //        NontermProd(A, A) -> 0.2,
  //        TermProd("run") -> 0.03,
  //        TermProd("X") -> 0.97)),
  //      (np / n) -> new DefaultedExpProbabilityDistribution(Map(
  //        TermProd("the") -> 0.21,
  //        TermProd("X") -> 0.79)),
  //      (n / n) -> new DefaultedExpProbabilityDistribution(Map(
  //        TermProd("the") -> 0.02,
  //        TermProd("X") -> 0.98)),
  //      (n / s) -> new DefaultedExpProbabilityDistribution(Map(
  //        TermProd("the") -> 0.01,
  //        TermProd("X") -> 0.99)),
  //      (s \ np) -> new DefaultedExpProbabilityDistribution(Map(
  //        TermProd("run") -> 0.06,
  //        TermProd("X") -> 0.94)),
  //      (s \ n) -> new DefaultedExpProbabilityDistribution(Map(
  //        TermProd("run") -> 0.01,
  //        TermProd("X") -> 0.99)))))
  //    val rules = Vector(FA, BA)
  //    val sentences = Vector(
  //      "the dogs run").map(_.splitWhitespace)
  //
  //    val guideChartBuilder = new OldToRemoveCfgGuideChartBuilder(rules)
  //    val soft1 = new SoftEmPcfg(1, new UnsmoothedPcfgParserTrainer(guideChartBuilder), guideChartBuilder, convergence = 1e-20, verbose = true).train(sentences, tagdict, rootDist, prodDist)
  //
  //    //    val ot = hard1.parse("the dogs run".split("\\s+").toVector)
  //    //    println(ot)
  //    //    ot.foreach(t => println(t.pretty))
  //  }
  //
  //  //@Test
  //  def soft_test2() {
  //    val tagdict = SimpleTagDictionary[Cat](Map(
  //      "the" -> Set(np / n, n / n),
  //      "dogs" -> Set(n),
  //      "run" -> Set(s \ np, s, A \ n)),
  //      "<S>", cat"<S>", "<E>", cat"<E>")
  //
  //    val rootDist = new Double2LogProbabilityDistributionAdapter(new SimpleExpProbabilityDistribution[Cat](Map(
  //      s -> 0.7,
  //      n -> 0.2,
  //      A -> 0.1)))
  //    val prodDist = new UniformProdDistInitializer().apply(Vector.empty, tagdict)
  //    val rules = Vector(FA, BA)
  //    val sentences = Vector("the dogs run").map(_.splitWhitespace)
  //    val guideChartBuilder = new OldToRemoveCfgGuideChartBuilder(rules)
  //    val soft1 = new SoftEmPcfg(1, new UnsmoothedPcfgParserTrainer(guideChartBuilder), guideChartBuilder, convergence = 1e-20).train(sentences, tagdict, rootDist, prodDist)
  //  }

}
