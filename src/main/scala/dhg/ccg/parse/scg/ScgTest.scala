//package dhg.ccg.parse.scg
//
//import scala.collection.mutable.{ Set => MSet }
//import scala.collection.mutable.{ Map => MMap }
//import dhg.ccg.parse._
//import dhg.ccg.parse.pcfg._
//import dhg.util._
//import scalaz._
//import Scalaz._
//import dhg.ccg.cat._
//import dhg.ccg.prob._
//import dhg.ccg.cat._
//import dhg.ccg.parse._
//import dhg.ccg.parse.pcfg._
//import dhg.ccg.data._
//import dhg.ccg.parse._
//import dhg.util.viz._
//import scala.collection.mutable.{ Map => MMap }
//import scala.collection.mutable.{ Set => MSet }
//import scalaz._
//import Scalaz._
//import scala.collection.breakOut
//import scala.annotation.tailrec
//import dhg.condor.Condor
//import org.apache.commons.math3.random.MersenneTwister
//import dhg.ccg.dd.DualParserTrainer
//import dhg.ccg.dd.DualParser
//import dhg.ccg.tagdict.TagDictionary
//import dhg.ccg.tagdict.SimpleTagDictionary
//import dhg.ccg.tagdict.SimpleTagDictionaryFactory
//
//object ScgTest {
//
//  def main(args: Array[String]): Unit = {
//
//    val rules = Vector(FA, BA)
//    val sentencesToTake = Int.MaxValue
//    val options = Map.empty[String, String]
//
//    val reader = EnglishCcgTreeBankReader(rules, Vector(FAb, BAb), RebankRules.standard)
//    val parseTrainData = time("read train data", reader.readTrees(16 to 18).take(sentencesToTake).toVector)
//    val rawData = parseTrainData.map(_.words)
//    println(f"raw data tokens: ${rawData.sumBy(_.size)}")
//    val supertaggedTrainData = parseTrainData.map(_.tagged)
//    val testData = time("read test data", reader.readTrees(19 to 21).take(sentencesToTake).toVector)
//    println(f"test data tokens: ${testData.sumBy(_.length)}")
//    val tdCutoff = options.get("td-cutoff").fold(0.0)(_.toDouble)
//    val tdData = time("read td data", reader.readTrees(0 to 15).take(sentencesToTake).map(_.tagged).toVector)
//    val requiredTagMappings: Map[String, Set[Cat]] = (testData ++ parseTrainData).flatMap(_.tagged).groupByKey.mapVals(_.toSet)
//    val (fullWordSet, fullSupertagSet) = (tdData.flatten ++ (testData ++ parseTrainData).flatMap(_.tagged)).toSet.unzip
//
//    val tdIncludesTest = options.get("td-includes-test").fold(false)(_.toBoolean)
//
//    val fullTdData = tdData ++ (if (tdIncludesTest) (testData ++ parseTrainData).map(_.tagged) else Vector())
//    println(f"TD data tokens: ${fullTdData.sumBy(_.size)}")
//
//    val unclusteredTagdictWithoutAllRequired: TagDictionary[Cat] = time("make td", new SimpleTagDictionaryFactory(Some(tdCutoff))(fullTdData, "<S>", cat"<S>", "<E>", cat"<E>")).withWords(fullWordSet ++ rawData.flatten).withTags(fullSupertagSet)
//    val unclusteredTagdict = SimpleTagDictionary(
//      unclusteredTagdictWithoutAllRequired.entries.mapt { (w, ts) => w -> (ts | requiredTagMappings.getOrElse(w, Set.empty)) }, // don't narrow down the tag choices, but don't exclude any required tags either. if a word is in the TD, add its required tags.  if it's not found, then don't do anything since the default tag set will contain any required tags. 
//      unclusteredTagdictWithoutAllRequired.startWord, unclusteredTagdictWithoutAllRequired.startTag, unclusteredTagdictWithoutAllRequired.endWord, unclusteredTagdictWithoutAllRequired.endTag, unclusteredTagdictWithoutAllRequired.allWords, unclusteredTagdictWithoutAllRequired.allTags, unclusteredTagdictWithoutAllRequired.excludedTags)
//    val tagdict = unclusteredTagdictWithoutAllRequired
//
//    case class Counts(name: String, var supertagsInTd: Int = 0, var supertagsTotal: Int = 0, var treesParseable: Int = 0, var treesCorrectParseable: Int = 0, var treesTotal: Int = 0) {
//      def print() {
//        println
//        println(f"$name supertags:          ${supertagsInTd / supertagsTotal.toDouble}  ($supertagsInTd/$supertagsTotal)")
//        println(f"$name parseable:          ${treesParseable / treesTotal.toDouble}  ($treesParseable/$treesTotal)")
//        println(f"$name correct parseable:  ${treesCorrectParseable / treesTotal.toDouble}  ($treesCorrectParseable/$treesTotal)")
//      }
//    }
//
//    for (
//      sa <- Vector(
//        None,
//        Some(new FwdBkdModOldToRemoveAdditionalSupertagAdder[String]),
//        Some(new FwdBkdConsumerOldToRemoveAdditionalSupertagAdder[String]),
//        Some(new AllFwdBkdOldToRemoveAdditionalSupertagAdder[String]))
//    ) {
//      val guideChartBuilder = new OldToRemoveCfgGuideChartBuilder(rules, Vector(Some(new NoOpOldToRemoveAdditionalSupertagAdder[String]), sa).flatten)
//      val parseCounter = new OldToRemoveCfgParseCounter[String](guideChartBuilder)
//      val supertagAdder = sa.getOrElse(new NoOpOldToRemoveAdditionalSupertagAdder[String])
//
//      var trainCounts = Counts("train")
//      var testCounts = Counts("test")
//
//      def handle(data: Vector[CcgTree], counts: Counts) {
//        for ((tree, ti) <- data.zipWithIndex) {
//          val sentenceExistingTags = tree.words.map(tagdict(_))
//          val sentenceNewTags = supertagAdder(tree.words zipSafe sentenceExistingTags)
//
//          val tagged = tree.tagged
//          for (((w, t), existingTags, newTags) <- zipSafe(tagged, sentenceExistingTags, sentenceNewTags)) {
//            if ((existingTags ++ newTags)(t)) counts.supertagsInTd += 1
//            counts.supertagsTotal += 1
//          }
//
//          if (zipSafe(tagged, sentenceExistingTags, sentenceNewTags).forall { case ((w, t), existingTags, newTags) => (existingTags ++ newTags)(t) }) counts.treesCorrectParseable += 1
//
//          val chart = guideChartBuilder.build(tree.words, Vector.empty, tagdict)
//          if (chart.exists(c => parseCounter.countParsesFromGuideChart(c) > 0)) counts.treesParseable += 1
//          counts.treesTotal += 1
//        }
//        counts.print()
//      }
//
//      println(f"Supertag Adder: $supertagAdder")
//      time(f"$supertagAdder", handle(parseTrainData, trainCounts))
//      time(f"$supertagAdder", handle(testData, testCounts))
//      println("\n\n")
//    }
//  }
//
//}
//
////  Supertag Adder: dhg.ccg.parse.pcfg.NoOpOldToRemoveAdditionalSupertagAdder@6786091b
////  starting: dhg.ccg.parse.pcfg.NoOpOldToRemoveAdditionalSupertagAdder@6786091b
////  
////  train supertags:          0.9364983164983165  (27814/29700)
////  train parseable:          0.7519125683060109  (1376/1830)
////  train correct parseable:  0.42185792349726775  (772/1830)
////  finished: dhg.ccg.parse.pcfg.NoOpOldToRemoveAdditionalSupertagAdder@6786091b in 46.68 seconds
////  starting: dhg.ccg.parse.pcfg.NoOpOldToRemoveAdditionalSupertagAdder@6786091b
////  
////  test supertags:          0.934149207764315  (23052/24677)
////  test parseable:          0.7648630594522378  (1145/1497)
////  test correct parseable:  0.3921175684702739  (587/1497)
////  finished: dhg.ccg.parse.pcfg.NoOpOldToRemoveAdditionalSupertagAdder@6786091b in 43.448 seconds
////  
////  
////  
////  Supertag Adder: dhg.ccg.parse.pcfg.FwdBkdModOldToRemoveAdditionalSupertagAdder@5e6f5adc
////  starting: dhg.ccg.parse.pcfg.FwdBkdModOldToRemoveAdditionalSupertagAdder@5e6f5adc
////  
////  train supertags:          0.9551178451178451  (28367/29700)
////  train parseable:          0.9912568306010929  (1814/1830)
////  train correct parseable:  0.5409836065573771  (990/1830)
////  finished: dhg.ccg.parse.pcfg.FwdBkdModOldToRemoveAdditionalSupertagAdder@71950f04 in 237.921 seconds
////  starting: dhg.ccg.parse.pcfg.FwdBkdModOldToRemoveAdditionalSupertagAdder@71950f04
////  
////  test supertags:          0.9527090002836649  (23510/24677)
////  test parseable:          0.9933199732798931  (1487/1497)
////  test correct parseable:  0.5143620574482298  (770/1497)
////  finished: dhg.ccg.parse.pcfg.FwdBkdModOldToRemoveAdditionalSupertagAdder@71950f04 in 190.418 seconds
