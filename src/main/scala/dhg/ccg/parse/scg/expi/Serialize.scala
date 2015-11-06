package dhg.ccg.parse.scg.expi

import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.dep._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import dhg.ccg.parse.pcfg.typesup._
import dhg.ccg.parse.scg._
import dhg.ccg.parse.scg.mcmc._
import dhg.ccg.data._
import dhg.util._
import dhg.util.viz._
import dhg.ccg.parse._
import scalaz._
import scalaz.Scalaz._
import dhg.ccg.tagdict.SimpleTagDictionaryFactory
import dhg.ccg.rule._
import dhg.ccg.util.SimpleIndexer
import dhg.ccg.util._

/**
 * target/start dhg.ccg.parse.scg.expi.Serialize
 */
object Serialize {
  type Word = String

  val takeMax = Int.MaxValue
  val maxCatSize = Int.MaxValue // 16

  def serializationArgFilenameString(lang: String, rawTokStr: String, testSentStr: String, tdTokStr: String, maxSentLenStr: String) = {
    f"${lang}_rawtok${rawTokStr}_testsent${testSentStr}_tdtok${tdTokStr}_maxSentLen$maxSentLenStr"
  }

  def getPath(name: String, argFilenameString: String) = {
    val dir = f"data/gcser/$name"
    File(dir).mkdirs()
    f"$dir/$argFilenameString"
  }

  def main(args: Array[String]): Unit = {
    val argString = args.mkString(" ")
    println(argString)

    //    val (arguments, options_) = parseArgs(args)
    //    val options = CommandLineOptions(options_)
    val arguments = args.toVector

    val Vector(name,
      "--lang", lang,
      "--rawTok", rawTokStr,
      "--testSent", testSentStr,
      "--tdTok", tdTokStr,
      "--maxSentLen", maxSentLenStr) = arguments

    val argFilenameString = serializationArgFilenameString(lang, rawTokStr, testSentStr, tdTokStr, maxSentLenStr)
    val path = getPath(name, argFilenameString)

    val reader = getReader(lang, maxSentLenStr)
    val (trainingGcBuilder, parsingGcBuilder, tagdict) = getGcBuilders(reader, tdTokStr)

    //

    val (rawCatIndexer, rawWordIndexer, rawMsg) = {
      val rawData = reader.raw

      val rawTok = rawTokStr match { case UInt(n) => n * 1000; case "inf" | "max" => Int.MaxValue }
      val rawGCsItr: Iterator[CfgGuideChart] = rawData.take(takeMax).zipWithIndex
        .map { case (s, i) => () => time1(f"    raw $i", trainingGcBuilder.build(s, None, tagdict)) }
        .parChunked(20).flatten
      val rawGCsTokLimitingItr: Iterator[CfgGuideChart] = new Iterator[CfgGuideChart] {
        var totalSize = 0
        def next(): CfgGuideChart = {
          val x = rawGCsItr.next
          totalSize += x.length
          //println(f"   -->  ${x.words.mkString(" ")}  ->  ${x.length}  ->  ${totalSize}")
          x
        }
        def hasNext(): Boolean = rawGCsItr.hasNext && totalSize <= rawTok
      }

      val ((rawCatIndexer, rawWordIndexer), (rawTokCount, rawSentCount, rawMaxSentLen)) =
        rawGCsTokLimitingItr.groupedAsVector(1000).zipWithIndex.foldLeft(((new SimpleIndexer[Cat](Vector(StartCat, EndCat, DeleteFromLeftCat, DeleteFromRightCat)), new SimpleIndexer[Word](Vector("<S>", "<E>"))), (0, 0, 0))) {
          case (((catIndexer, wordIndexer), (rawTokCount, rawSentCount, rawMaxSentLen)), (gcGroup, groupNum)) =>
            val newCatIndexer = catIndexer.append(gcGroup.flatMap(_.allCats))
            val newWordIndexer = wordIndexer.append(gcGroup.flatMap(_.words))
            val gciGroup = gcGroup.par.map(gc => Some(CfgGuideChartI.to(gc, newCatIndexer, newWordIndexer)) -> None).seq

            val newTokCount = rawTokCount + gciGroup.sumBy(_._1.get.length)
            val newSentCount = rawSentCount + gciGroup.size
            val newMaxSentLen = rawMaxSentLen max (gciGroup.map(_._1.get.length) match { case Vector() => -1; case xs => xs.max })

            val fn = f"$path-raw-part$groupNum%02d.gciv.gz"
            time(f"serializing raw gciv to $fn", CfgGuideChartI.writeVector(gciGroup, fn))

            ((newCatIndexer, newWordIndexer), (newTokCount, newSentCount, newMaxSentLen))
        }
      val rawMsg = f"raw gc:         $rawTokCount tokens, $rawSentCount sentences, max sent len = $rawMaxSentLen"
      println(rawMsg)
      (rawCatIndexer, rawWordIndexer, rawMsg)
    }

    val ((fullCatIndexer, fullWordIndexer), (testTokCount, testSentCount, testMaxSentLen), (uTstTokCount, uTstSentCount, uTstMaxSentLen)) = {
      val tstData = reader.testData // .devData
      val tstGCsItr: Iterator[() => (Option[CfgGuideChart], CcgTree)] =
        tstData.take(takeMax).zipWithIndex
          .map { case (t, i) => () => (time1(f"    test $i", parsingGcBuilder.build(t.words, None, tagdict)), t) }
          .take(testSentStr match { case UInt(n) => n; case "inf" | "max" => Int.MaxValue })

      val ((catIndexer, wordIndexer), (testTokCount, testSentCount, testMaxSentLen), (uTstTokCount, uTstSentCount, uTstMaxSentLen)) =
        tstGCsItr.groupedAsVector(1000).zipWithIndex.foldLeft(((rawCatIndexer, rawWordIndexer), (0, 0, 0), (0, 0, 0))) {
          case (((catIndexer, wordIndexer), (testTokCount, testSentCount, testMaxSentLen), (uTstTokCount, uTstSentCount, uTstMaxSentLen)), (gcGroup_, groupNum)) =>
            val gcGroup = gcGroup_.map(_.apply)
            val newCatIndexer = catIndexer.append(gcGroup.flatMap(_._2.allCats) ++ gcGroup.flatMap(_._1).flatMap(_.allCats))
            val newWordIndexer = wordIndexer.append(gcGroup.flatMap(_._2.words))
            val gciGroup = gcGroup.par.map { case (gc, t) => gc.map(CfgGuideChartI.to(_, newCatIndexer, newWordIndexer)) -> Some(CcgTreeI.to(t, newCatIndexer, newWordIndexer)) }.seq

            val newTokCount = testTokCount + gciGroup.sumBy(_._2.get.length)
            val newSentCount = testSentCount + gciGroup.size
            val newMaxSentLen = testMaxSentLen max gciGroup.map(_._2.get.length).max

            val newUTokCount = uTstTokCount + gciGroup.flatMap(_._1).sumBy(_.length)
            val newUSentCount = uTstSentCount + gciGroup.count(_._1.isDefined)
            val newUMaxSentLen = uTstMaxSentLen max gciGroup.flatMap(_._1).map(_.length).max

            val fn = f"$path-tst-part$groupNum%02d.gciv.gz"
            time(f"serializing tst gciv to $fn", CfgGuideChartI.writeVector(gciGroup, fn))

            ((newCatIndexer, newWordIndexer), (newTokCount, newSentCount, newMaxSentLen), (newUTokCount, newUSentCount, newUMaxSentLen))
        }

      ((catIndexer, wordIndexer), (testTokCount, testSentCount, testMaxSentLen), (uTstTokCount, uTstSentCount, uTstMaxSentLen))
    }

    println(rawMsg)
    println(f"test gc:        $testTokCount tokens, $testSentCount sentences, max sent len = $testMaxSentLen")
    println(f"usable test gc: $uTstTokCount tokens, $uTstSentCount sentences, max sent len = $uTstMaxSentLen")
    println(f"num words = ${fullWordIndexer.size}, num cats = ${fullCatIndexer.size}")

    time(f"serializing cat indexer to $path-cats.txt", writeUsing(File(f"$path-cats.txt")) { f => fullCatIndexer.objects.foreach(f.wl) })
    time(f"serializing wrd indexer to $path-wrds.txt", writeUsing(File(f"$path-wrds.txt")) { f => fullWordIndexer.objects.foreach(f.wl) })
  }

  def getReader(lang: String, maxSentLenStr: String) = {
    val maxSentLen = maxSentLenStr match { case UInt(n) => n; case "inf" | "max" => Int.MaxValue }
    val standardReader = lang match {
      case "en" =>
        val ccgbank = EnglishCcgTreeBankReader()
        new SeparateTrainTestCorpusReader(
          tdReader = ccgbank,
          rawReader = new DeduplicatingCorpusReader(new MaxLengthRemovingCorpusReader(maxSentLen, new CompositeCorpusReader(Vector(
            ccgbank,
            new QuoteRemovingCorpusReader(new TokenizedRawCorpusReader(Vector("data/nytgiga.100k.spl"))))))),
          testReader = ccgbank)
      case "ch" =>
        val ccgbank = ChineseCcgTreeBankReader()
        new SeparateTrainTestCorpusReader(
          tdReader = ccgbank,
          rawReader = new DeduplicatingCorpusReader(new MaxLengthRemovingCorpusReader(maxSentLen,
            ccgbank)),
          testReader = ccgbank)
      case "it" =>
        val ccgbank = ItalianCcgTreeBankReader()
        new SeparateTrainTestCorpusReader(
          tdReader = ccgbank,
          rawReader = new DeduplicatingCorpusReader(new MaxLengthRemovingCorpusReader(maxSentLen, new CompositeCorpusReader(Vector(
            ccgbank,
            new QuoteRemovingCorpusReader(new TokenizedRawCorpusReader(Vector("data/wacky-italian.100k.spl"))))))),
          testReader = ccgbank)
    }
    val rebankingReader = new RebankingTreeBankReader(new SimpleRebanker(RebankRules.standard), standardReader)
    val dependencyConvertableTreeReader = new RuleViolatedRemovingTreeBankReader(new SimpleRuleViolationFinder(CcgRules.all, allowAllUnary = true), rebankingReader)
    dependencyConvertableTreeReader
  }

  def getGcBuilders(reader: TreeBankReader, tdTokStr: String) = {
    val tdTok = tdTokStr match { case UInt(n) => n; case "inf" | "max" => Int.MaxValue }
    val tdData = time("read td data", reader.tdData.map(_.tagged).takeSub(tdTok * 1000).toVector)
    val tagdict = new SimpleTagDictionaryFactory().apply(tdData.map(_.filter(_._2.size <= maxCatSize)), "<S>", StartCat, "<E>", EndCat)
    println(f"tdData:")
    println(f"    num tokens:    ${tdData.sumBy(_.size)}")
    println(f"    num sentences: ${tdData.size}")
    println(f"    max cat size   ${tagdict.allTags.map(_.size).max}")
    println(f"tagdict:")
    println(f"    num words: ${tagdict.allWords.size}")
    println(f"    num cats:  ${tagdict.allTags.size}")
    println(f"    num single-cat words: ${tagdict.entries.count(_._2.size == 1)}")
    println("\n")

    val rootSet = Set(cat"S[dcl]", cat"NP")
    val rules = CcgRules.nonComp
    val binaryRules = rules.collect { case r: BinaryCcgRule => r }
    val unaryRules = rules.collect { case r: UnaryCcgRule => r }
    val standardNoMergeGcBuilder = new SimpleCfgGuideChartBuilder((rules.toSet - Merge).toVector, additionalSupertagAdder = new StandardTagDictAdditionalTagAdder, rootSet, allowTerminalDeletion = false)
    val standardMergeGcBuilder = new SimpleCfgGuideChartBuilder(rules.toVector, additionalSupertagAdder = new StandardTagDictAdditionalTagAdder, rootSet, allowTerminalDeletion = false)
    val standardMergeNorootsetGcBuilder = new SimpleCfgGuideChartBuilder(rules.toVector, additionalSupertagAdder = new StandardTagDictAdditionalTagAdder, rootSet = UniversalSet(), allowTerminalDeletion = false)
    //val fwdbkdGcBuilder = new SimpleCfgGuideChartBuilder(rules.toVector, additionalSupertagAdder = new StandardTagDictAndFwdBkdAdditionalTagAdder, rootSet)
    val fwdbkdGcBuilder = new SimpleCfgGuideChartBuilder(rules.toVector, additionalSupertagAdder = new StandardTagDictAdditionalTagAdder, rootSet, allowTerminalDeletion = true)
    val fwdbkdNorootsetGcBuilder = new SimpleCfgGuideChartBuilder(rules.toVector, additionalSupertagAdder = new StandardTagDictAdditionalTagAdder, rootSet = UniversalSet(), allowTerminalDeletion = true)

    val trainingGcBuilder = new CascadingAttemptsCfgGuideChartBuilder(Vector(standardMergeGcBuilder))
    val parsingGcBuilder = new CascadingAttemptsCfgGuideChartBuilder(Vector( /*standardMergeGcBuilder,*/ standardMergeNorootsetGcBuilder, /*fwdbkdGcBuilder,*/ fwdbkdNorootsetGcBuilder))
    (trainingGcBuilder, parsingGcBuilder, tagdict)
  }

}

