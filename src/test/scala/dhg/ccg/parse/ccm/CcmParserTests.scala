//package dhg.ccg.parse.ccm
//
//import org.junit.Test
//import org.junit.Assert._
//
//import dhg.ccg.parse._
//import dhg.ccg.prob._
//import dhg.ccg.cat._
//import dhg.ccg.tagdict.SimpleTagDictionary
//
//class CcmParserTests {
//
//  val s = AtomCat("S")
//  val np = AtomCat("NP")
//  val n = AtomCat("N")
//  val pp = AtomCat("PP")
//
//  val A = AtomCat("A")
//  val B = AtomCat("B")
//  val C = AtomCat("C")
//
//  @Test
//  def test1() {
//    val tagdict = SimpleTagDictionary[Cat](Map(
//      "1" -> Set(A / B, A / C),
//      "2" -> Set(B, C)),
//      "<S>", cat"<S>", "<E>", cat"<E>")
//
//    val ccm = Ccm[String](
//      constituentDist = new LaplaceExpProbabilityDistribution[Span](Map(
//        Span(Vector(A / B, B)) -> 0.9,
//        Span(Vector(A / C, C)) -> 0.1), 
//        None, None, 0.2, 100.0),
//      constituentContextDist = new SimpleConditionalExpProbabilityDistribution[Span, Context](Map(
//        Span(Vector(B)) -> new LaplaceExpProbabilityDistribution(Map(
//          Context(A / B, tagdict.endTag) -> 0.2,
//          Context(A / C, tagdict.endTag) -> 0.00001),
//          None, None, 0.2, 100.0),
//        Span(Vector(A / B, B)) -> new LaplaceExpProbabilityDistribution(Map(
//          Context(tagdict.startTag, tagdict.endTag) -> 0.1),
//          None, None, 0.2, 100.0),
//        Span(Vector(A / C, C)) -> new LaplaceExpProbabilityDistribution(Map(
//          Context(tagdict.startTag, tagdict.endTag) -> 0.9),
//          None, None, 0.2, 100.0)),
//        None, None,
//        new LaplaceExpProbabilityDistribution[Context](Map(), None, None, 0.2, 100.0)),
//      distituentDist = new LaplaceExpProbabilityDistribution[Span](Map(), None, None, 0.2, 100.0),
//      distituentContextDist = new SimpleConditionalExpProbabilityDistribution[Span, Context](Map(), None, None,
//        new LaplaceExpProbabilityDistribution[Context](Map(), None, None, 0.2, 100.0)),
//      terminalDist = new SimpleConditionalExpProbabilityDistribution(Map(),
//        None, None,
//        new LaplaceExpProbabilityDistribution(Map(), Some(tagdict.allWords), None, 1.0)))
//
//    val rules = Vector(FA, BA)
//    val canCombine = new CachingCanCombineCfg(new SimpleCanCombineCfg(rules))
//    val parser = new CcgCcmParser(
//      ccm,
//      tagdict,
//      rules,
//      canCombine,
//      parseThreshold = Int.MaxValue,
//      verbose = true)
//    //parser.parseFromSupertags(Vector("she" -> n, "eats" -> (s \ n)))
//    val ot = parser.parse("1 2".split("\\s+").toVector)
//    println(ot)
//    ot.foreach(t => println(t.pretty))
//
//  }
//
//}
