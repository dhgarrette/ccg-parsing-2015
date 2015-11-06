//package dhg.ccg.parse.pcfg
//
//import org.junit.Test
//import org.junit.Assert._
//import dhg.ccg.parse._
//import dhg.ccg.prob._
//import dhg.ccg.cat._
//import dhg.ccg.tagdict.SimpleTagDictionary
//import dhg.ccg.tagdict.TagDictionary
//import dhg.util._
//import dhg.util._
//
//class TypesupPcfgTests {
//
//  val s = AtomCat("S")
//  val np = AtomCat("NP")
//  val n = AtomCat("N")
//  val pp = AtomCat("PP")
//
//  val A = AtomCat("A")
//  val B = AtomCat("B")
//  val C = AtomCat("C")
//  val D = AtomCat("D")
//  val E = AtomCat("E")
//  val F = AtomCat("F")
//
//  @Test
//  def test1() {
//
//    val tagdict = SimpleTagDictionary[Cat](Map(
//      "the" -> Set(np / n),
//      "lazy" -> Set(n / n),
//      "man" -> Set(n),
//      "dogs" -> Set(np, n),
//      "cat" -> Set(n),
//      "run" -> Set(s \ np, s),
//      "walks" -> Set(s \ np, (s \ np) / np),
//      "sleeps" -> Set(s \ np)),
//      "<S>", cat"<S>", "<E>", cat"<E>")
//
//    val nontermProdDistInitializer = new NontermProdDistInitializer[String] {
//      def fromBracketed(bracketedSentences: Vector[(Vector[String], Vector[(Int, Int)])], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, NontermProd] = {
//        new SimpleConditionalLogProbabilityDistribution(Map())
//      }
//    }
//
//    val termProdDistInitializer = new TermProdDistInitializer[String] {
//      def fromBracketed(bracketedSentences: Vector[(Vector[String], Vector[(Int, Int)])], initialTagdict: TagDictionary[Cat]): ConditionalLogProbabilityDistribution[Cat, TermProd] = {
//        new SimpleConditionalLogProbabilityDistribution(Map())
//      }
//    }
//
//    val init = new NtTermInterpolatingProdDistInitializer[String](
//      nontermProdDistInitializer,
//      termProdDistInitializer,
//      guideChartBuilder = new OldToRemoveCfgGuideChartBuilder(Vector(FA, BA)),
//      priorInterp = 0.1)
//
//    val d =
//      init.apply(Vector(
//        "the dogs run",
//        "the cat walks",
//        "run",
//        "the man walks the lazy dog")
//        .map(_.splitWhitespace), tagdict)
//        .asInstanceOf[NtTermInterpolatingProdDist[String]]
//
//    println(d.nontermLambdas)
//    println(d.defaultLambda)
//
//  }
//
//}
