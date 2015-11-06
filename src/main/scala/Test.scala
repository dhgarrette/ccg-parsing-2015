//import dhg.util._
//import dhg.ccg.data._
//import dhg.ccg.tag.learn._
//import dhg.ccg.tag.learn._
//import dhg.ccg.tag._
//import dhg.ccg.prob._
//import dhg.ccg.cat._
//import dhg.ccg.parse._
//import dhg.ccg.parse.pcfg._
//import org.apache.commons.math3.random.RandomGenerator
//import org.apache.commons.math3.random.AbstractRandomGenerator
//import scalaz._
//import Scalaz._
//import dhg.ccg.parse.inf.SimpleInfCatPrior
//import dhg.ccg.tagdict.SimpleTagDictionary
//
//object Test {
//
//  def main(args: Array[String]): Unit = {
//
//    //    val sents = FeaturedEnglishCcgBankReader.rawCHEATING ++ FeaturedEnglishCcgBankReader.tdData ++ FeaturedEnglishCcgBankReader.devData ++ FeaturedEnglishCcgBankReader.testData
//    //    //sents.flatten.groupByKey.apply("buy").counts.desc foreach println
//    //    //return
//    //    for (s <- sents) {
//    //      val words = s.map(_._1).mkString(" ")
//    //      s.groupByKey.get("buy").map { cats => 
//    //        if(cats.map(_.toString).contains("((S[b]\\NP)/NP)"))
//    //          println(f"$words") }
//    //          //println(f"$words\n${cats.mkString(",  ")}\n") }
//    //    }
//
//    //    val rules = Vector(FA, BA)
//    //    val reader = EnglishCcgTreeBankReader(rules, Vector(FAb, BAb), RebankRules.standard)
//    //    val trees = reader.readTrees(0 to 24).toVector
//
//    //
//
//    val s = cat"S".asInstanceOf[AtomCat]
//    val np = cat"NP".asInstanceOf[AtomCat]
//    val n = cat"N".asInstanceOf[AtomCat]
//
//    val catprior = new SimpleInfCatPrior(
//      tagdict = SimpleTagDictionary(Map(), "<S>", cat"<S>", "<E>", cat"<E>"),
//      allAtoms = Set(s, np, n),
//      pAtom = new SimpleLogProbabilityDistribution[AtomCat](Map(s -> 0.5, np -> 0.3, n -> 0.2).mapVals(LogDouble(_))),
//      pTerm = 0.5,
//      pMod = 0.1,
//      pFwd = 0.55)
//
//    //    def f(x: Cat, y: Cat) {
//    //      println(f"${catprior(x)}  ${catprior(y)}  ${catprior(x)/catprior(y)}")
//    //    }
//    //    f((s\np)/(s\np), (s\np)/(s\n))
//    //    f(n/n, np/n)
//
//    for (((c, cp), cp2) <- catprior.allAbove(LogDouble(0.000001)).mapt((c, cp) => ((c, cp), catprior(c))).desc) {
//      println(f"$c%-50s ${cp.toDouble}%.6f ${cp2.toDouble}%.6f  ${if (cp != cp2) "**" else ""}")
//    }
//
//  }
//
//}
