package dhg.ccg.data

import dhg.util._
import scala.util.Try
import dhg.ccg.cat._
import scala.util.parsing.combinator.RegexParsers
import dhg.ccg.parse._
import scala.annotation.tailrec
import scalaz.{ Tree => _, _ }
import Scalaz._
import dhg.util.viz._
import dhg.ccg.parse.pcfg._

trait CorpusReader {
  type Word = String

  def raw(): Iterator[Vector[Word]]
}

trait TreeBankReader extends CorpusReader {
  def tdData(): Iterator[CcgTree]
  def rawDataDONTUSE: Iterator[CcgTree]
  def raw(): Iterator[Vector[Word]] //= rawDataDONTUSE.map(_.leaves.map(_.word))
  final def rawCHEATING(): Iterator[CcgTree] = rawDataDONTUSE
  def devData(): Iterator[CcgTree]
  def testData(): Iterator[CcgTree]
  
  def readFileTrees(file: File): Iterator[CcgTree]
  def readFileTrees(fileNumber: Int): Option[Iterator[CcgTree]]

  final def fullCorpusDONTUSE: Iterator[CcgTree] = tdData ++ rawDataDONTUSE ++ devData ++ testData

  def startWord: Word
  def startTag: Cat
  def endWord: Word
  def endTag: Cat
  def tagToString: (Cat => String)
  def tagFromString: (String => Cat)
}

class SeparateTrainTestTreeBankReader(
  tdReader: TreeBankReader,
  rawReader: TreeBankReader,
  testReader: TreeBankReader)
    extends TreeBankReader {

  def tdData(): Iterator[CcgTree] = tdReader.tdData
  def rawDataDONTUSE: Iterator[CcgTree] = rawReader.rawDataDONTUSE
  def raw(): Iterator[Vector[Word]] = rawReader.raw
  def devData(): Iterator[CcgTree] = testReader.devData
  def testData(): Iterator[CcgTree] = testReader.testData
  
  def readFileTrees(file: File): Iterator[CcgTree] = ???
  def readFileTrees(fileNumber: Int): Option[Iterator[CcgTree]] = ???

  def startWord: Word = { val a = rawReader.startWord; val b = tdReader.startWord; val c = testReader.startWord; assert(a == b && b == c); a }
  def startTag: Cat = { val a = rawReader.startTag; val b = tdReader.startTag; val c = testReader.startTag; assert(a == b && b == c); a }
  def endWord: Word = { val a = rawReader.endWord; val b = tdReader.endWord; val c = testReader.endWord; assert(a == b && b == c); a }
  def endTag: Cat = { val a = rawReader.endTag; val b = tdReader.endTag; val c = testReader.endTag; assert(a == b && b == c); a }
  def tagToString: (Cat => String) = { val a = rawReader.tagToString; val b = tdReader.tagToString; val c = testReader.tagToString; assert(a == b && b == c); a }
  def tagFromString: (String => Cat) = { val a = rawReader.tagFromString; val b = tdReader.tagFromString; val c = testReader.tagFromString; assert(a == b && b == c); a }

  override def toString = {
    f"SeparateTrainTestTreeBankReader(\n" +
      f"    tdReader=$tdReader\n" +
      f"    rawReader=$rawReader\n" +
      f"    testReader=$testReader)"
  }
}

class SeparateTrainTestCorpusReader(
  tdReader: TreeBankReader,
  rawReader: CorpusReader,
  testReader: TreeBankReader)
    extends TreeBankReader {

  def tdData(): Iterator[CcgTree] = tdReader.tdData
  override def raw(): Iterator[Vector[Word]] = rawReader.raw
  def rawDataDONTUSE = sys.error("SeparateTrainTestCorpusReader.rawDataDONTUSE not supported")
  def devData(): Iterator[CcgTree] = testReader.devData
  def testData(): Iterator[CcgTree] = testReader.testData

  def readFileTrees(file: File): Iterator[CcgTree] = ???
  def readFileTrees(fileNumber: Int): Option[Iterator[CcgTree]] = ???

  def startWord: Word = { val b = tdReader.startWord; val c = testReader.startWord; assert(b == c); b }
  def startTag: Cat = { val b = tdReader.startTag; val c = testReader.startTag; assert(b == c); b }
  def endWord: Word = { val b = tdReader.endWord; val c = testReader.endWord; assert(b == c); b }
  def endTag: Cat = { val b = tdReader.endTag; val c = testReader.endTag; assert(b == c); b }
  def tagToString: (Cat => String) = { val b = tdReader.tagToString; val c = testReader.tagToString; assert(b == c); b }
  def tagFromString: (String => Cat) = { val b = tdReader.tagFromString; val c = testReader.tagFromString; assert(b == c); b }

  override def toString = {
    f"SeparateTrainTestCorpusReader(\n" +
      f"    tdReader=$tdReader\n" +
      f"    rawReader=$rawReader\n" +
      f"    testReader=$testReader)"
  }
}

/**
 * Subclasses must implement:
 *
 *       def modify(trees: Iterator[CcgTree]): Iterator[CcgTree]
 *
 */
abstract class DelegatingTreeBankReader(delegate: TreeBankReader) extends TreeBankReader {
  final def tdData(): Iterator[CcgTree] = modifyTrees(delegate.tdData)
  final def raw(): Iterator[Vector[Word]] = modifyRaw(delegate.raw)
  final def rawDataDONTUSE: Iterator[CcgTree] = modifyTrees(delegate.rawDataDONTUSE)
  final def devData(): Iterator[CcgTree] = modifyTrees(delegate.devData)
  final def testData(): Iterator[CcgTree] = modifyTrees(delegate.testData)

  def modifyTrees(trees: Iterator[CcgTree]): Iterator[CcgTree]
  def modifyRaw(sents: Iterator[Vector[Word]]): Iterator[Vector[Word]]

  def readFileTrees(file: File): Iterator[CcgTree] = modifyTrees(delegate.readFileTrees(file))
  def readFileTrees(fileNumber: Int): Option[Iterator[CcgTree]] = delegate.readFileTrees(fileNumber).map(modifyTrees)

  final def startWord: Word = delegate.startWord
  final def startTag: Cat = delegate.startTag
  final def endWord: Word = delegate.endWord
  final def endTag: Cat = delegate.endTag
  final def tagToString: (Cat => String) = delegate.tagToString
  final def tagFromString: (String => Cat) = delegate.tagFromString
}

class RuleViolatedRemovingTreeBankReader(violationFinder: RuleViolationFinder, delegate: TreeBankReader) extends DelegatingTreeBankReader(delegate) {
  def modifyTrees(trees: Iterator[CcgTree]): Iterator[CcgTree] = {
    trees.filter { t =>
      //println(f"        RuleViolatedRemovingTreeBankReader:  ${violationFinder.violations(t).isEmpty}  $t")
      //for (v <- violationFinder.violations(t)) println(f"          $v")
      violationFinder.violations(t).isEmpty
    }
  }
  def modifyRaw(sents: Iterator[Vector[Word]]): Iterator[Vector[Word]] = sents
  override def toString = f"RuleViolatedRemovingTreeBankReader($violationFinder, $delegate)"
}

class MaxLengthRemovingTreeBankReader(maxLength: Int, delegate: TreeBankReader) extends DelegatingTreeBankReader(delegate) {
  def modifyTrees(trees: Iterator[CcgTree]): Iterator[CcgTree] = {
    trees.filter(_.length <= maxLength)
  }
  def modifyRaw(sents: Iterator[Vector[Word]]): Iterator[Vector[Word]] = {
    sents.filter(_.length <= maxLength)
  }
  override def toString = f"MaxLengthRemovingTreeBankReader(maxLength=$maxLength, $delegate)"
}

class RebankingTreeBankReader(rebanker: Rebanker, delegate: TreeBankReader) extends DelegatingTreeBankReader(delegate) {
  def modifyTrees(trees: Iterator[CcgTree]): Iterator[CcgTree] = {
    trees.map(rebanker)
  }
  def modifyRaw(sents: Iterator[Vector[Word]]): Iterator[Vector[Word]] = sents
  override def toString = f"RebankingTreeBankReader($rebanker, $delegate)"
}

//class PunctSplittingTreeBankReader(delegate: TreeBankReader) extends DelegatingTreeBankReader(delegate) {
//  def modifyTrees(trees: Iterator[CcgTree]): Iterator[CcgTree] = {
//    trees.flatMap(split)
//  }
//
//  /**
//   * Split-worthy tokens:  DEF:   '!'  '?'
//   *                       PROB:  '.' (also appears with S)
//   *                              ',' (also appears with NP once)
//   *                              ';' (also appears with ((NP\NP)/S[dcl]) once)
//   *                       MAYBE: ':'
//   */
//  def split(t: CcgTree): Vector[CcgTree] = {
//    t match {
//      case CcgBinode(cat, left, right) => ???
//      case CcgUnode(cat, sub) => ???
//      case CcgLeaf(cat, word, _) => Vector.empty
//    }
//  }
//
//  override def toString = f"PunctSplittingTreeBankReader($delegate)"
//}

//class FeatureRemovingTreeBankReader(delegate: TreeBankReader) extends DelegatingTreeBankReader(delegate) {
//  def modify(trees: Iterator[CcgTree]): Iterator[CcgTree] = {
//    trees.map { t =>
//      try {
//        replaceInTree(t)
//      }
//      catch {
//        case e: MatchError =>
//          println(f"FeatureRemovingTreeBankReader.modify: ${e}")
//          println(f"  $t")
//          TreeViz.drawTree(t)
//          CcgLeaf(cat"FAKE", "fake")
//      }
//    } //.map { t => println(t); t } 
//  }
//
//  def replaceInTree(t: CcgTree): CcgTree = t match {
//    case CcgBinode(c, l, r) => CcgBinode(replaceInCat(c), replaceInTree(l), replaceInTree(r))
//    case CcgUnode(c, s) => CcgUnode(replaceInCat(c), replaceInTree(s))
//    case CcgLeaf(c, w, _) => CcgLeaf(replaceInCat(c), w)
//  }
//
//  def replaceInCat(c: Cat): Cat = c match {
//    //case ConjCat(c) => replaceInCat(c)
//    case a / b => replaceInCat(a) / replaceInCat(b)
//    case a \ b => replaceInCat(a) \ replaceInCat(b)
//    case a: AtomCat => a.base
//  }
//  
//  override def toString = f"FeatureRemovingTreeBankReader($delegate)"
//}
//
//class N2NPTreeBankReader(delegate: TreeBankReader) extends DelegatingTreeBankReader(delegate) {
//  private[this] val NP = AtomCat("NP")
//  private[this] val N = AtomCat("N")
//
//  def modify(trees: Iterator[CcgTree]): Iterator[CcgTree] = {
//    trees.map(replaceInTree) //.map { t => println(t); t }
//  }
//
//  def replaceInTree(t: CcgTree): CcgTree = t match {
//    case CcgBinode(c, l, r) => CcgBinode(replaceInCat(c), replaceInTree(l), replaceInTree(r))
//    case CcgUnode(c, s) => CcgUnode(replaceInCat(c), replaceInTree(s))
//    case CcgLeaf(c, w, _) => CcgLeaf(replaceInCat(c), w)
//  }
//
//  def replaceInCat(c: Cat): Cat = c match {
//    case a / b => replaceInCat(a) / replaceInCat(b)
//    case a \ b => replaceInCat(a) \ replaceInCat(b)
//    case NP => N
//    case _ => c
//  }
//}

//
//
//

abstract class CcgbankReader(usePos: Boolean) extends TreeBankReader {
  protected[this] val Loc: String
  protected[this] def catmap: CatInterner

  protected[this] val AtomicRe = """[^\\/]+""".r

  val FN_RE = """wsj_(\d\d)(\d\d)\.auto""".r

  final def raw(): Iterator[Vector[Word]] = rawDataDONTUSE.map(_.leaves.map(_.word))

  def readTrees(section: Int): Iterator[CcgTree] = {
    readTrees(section to section)
  }

  def readTrees(sections: Range): Iterator[CcgTree] = {
    for {
      sectionDir <- File(Loc).ls.iterator
      dirNum = sectionDir.name.toInt
      if (sections.contains(dirNum))
      file <- sectionDir.ls
      tree <- readFileTrees(file)
    } yield tree
  }

  def readFileTrees(fileNumber: Int): Option[Iterator[CcgTree]] = {
    val fnString = f"$fileNumber%04d"
    val file = File(Loc, fnString.take(2), filename(fileNumber))
    file.exists.option(readFileTrees(file))
  }
  def filename(fileNumber: Int): String

  def readFileTrees(file: File): Iterator[CcgTree] = {
    for {
      Seq(header, line) <- file.readLines.grouped(2)
      _ = assert(header.startsWith("ID="))
      tree <- try {
        val t = getTree(line)
        Some(t)
      }
      catch {
        case e: RuntimeException =>
          println(line)
          println(e)
          println
          //TreeViz.drawTree(t)
          None
      }
    } yield tree
  }

  private[this] def readTreeLines(sections: Range): Iterator[String] = {
    for {
      sectionDir <- File(Loc).ls.iterator
      dirNum = sectionDir.name.toInt
      if (sections.contains(dirNum))
      file <- sectionDir.ls
      Seq(header, content) <- file.readLines.grouped(2)
    } yield {
      assert(header.startsWith("ID="))
      content
    }
  }

  def getTree(line: String): CcgTree = {
    object CcgbankLineParser extends RegexParsers {
      private[this] val TerminalRe = """<L ([\S]+) ([\S]+) ([\S]+) ([\S]+) ([\S]+)>""".r // <L (NP\NP)/NP IN IN of (NP_99\NP_99)/NP_100>
      private[this] val NonTerminalRe = """<T ([\S]+) ([\S]+) ([\S]+)>""".r

      private def term: Parser[CcgTree] =
        ("(" ~> TerminalRe <~ ")").map {
          case TerminalRe(_, pos, _, token, Category(cat)) =>
            //case TerminalRe(Category(cat), pos, _, token, _) => // Old, non-indexed category version
            CcgLeaf(if (usePos) Category(pos) else cat, token, pos)
        }
      private def unary: Parser[CcgTree] =
        ("(" ~> NonTerminalRe ~ tree <~ ")").map {
          case NonTerminalRe(Category(cat), _, _) ~ sub =>
            CcgUnode(cat, sub)
        }
      private def binary: Parser[CcgTree] =
        ("(" ~> NonTerminalRe ~ tree ~ tree <~ ")").map {
          case NonTerminalRe(Category(cat), _, _) ~ left ~ right =>
            CcgBinode(cat, left, right)
        }
      private def tree = (term | binary | unary)
      def parse(str: String): CcgTree = parseAll(tree, str) match {
        case Success(result: CcgTree, _) => result
        case _ => sys.error("Could not parse the input string: " + str)
      }
    }

    CcgbankLineParser.parse(line)
  }

  object Category {
    def apply(s: String) = {
      val ConjRe = """(.*)\[conj\]""".r
      val (a, isConj) = s match {
        case ConjRe(x) => (x, true)
        case _ => (s, false)
      }
      val b = catmap(a match {
        case "((S[b]\\NP)/NP)/" => "((S[b]\\NP)/NP)" // error in wsj_0595.15
        case "((S[b]\\NP_199)/NP_200)/_201" => "((S[b]\\NP_199)/NP_200)" // error in wsj_0595.15
        case "((S[b]\\NP_266)/NP_267)/_268" => "((S[b]\\NP_266)/NP_267)" // error in wsj_0595.15
        case "(S[dcl]\\NP)/(S[dcl]\\NP)~SB" => "((S[dcl]\\NP)/(S[dcl]\\NP))" // errors in chinese ccgbank
        case "LRB/LRB" => "LRB" //                                              chinese ccgbank issue                       
        case "((S\\NP)/(S\\NP))/:" => "((S\\NP)/(S\\NP))" //           wsj_0576.12
        case "(((S\\NP)/(S\\NP))/:)/PP" => "(((S\\NP)/(S\\NP))/PP)" // wsj_0576.12
        case ":\\NP" => "(NP\\NP)" //                     wsj_1493.43
        case "(:\\NP)/PP" => "((NP\\NP)/PP)" //           wsj_1493.43
        case "((:\\NP)/PP)/NP" => "(((NP\\NP)/PP)/NP)" // wsj_1493.43
        //case "." => """(S\S)"""
        //case "," => """(NP\NP)"""
        //case ";" => """((S\S)/S)"""
        case s @ AtomicRe() => s
        case s => s"($s)"
      })
      if (isConj) catmap.conj(b) else b
    }
    def unapply(s: String) = Some(apply(s))
  }

  override final val startWord = "<S>"
  override final val endWord = "<E>"
}

//
//
//

////
////
////

object TreeBankReaderRunner {
  def main(args: Array[String]): Unit = {

    //println("English"); EnglishCcgTreeBankReader().fullCorpusDONTUSE.zipWithIndex.foreach { case (t,i) => }//println(f"  $i") }
    println("Chinese"); ChineseCcgTreeBankReader().fullCorpusDONTUSE.zipWithIndex.foreach { case (t, i) => } //println(f"  $i") }
    println("Italian"); ItalianCcgTreeBankReader().fullCorpusDONTUSE.zipWithIndex.foreach { case (t, i) => } //println(f"  $i") }

    return

    val line1 = """
       (<T S[dcl] _ _>
         (<T S[dcl] _ _>
           (<L NP _ _ They _>)
           (<T S[dcl]\NP _ _>
             (<L (S[dcl]\NP)/NP _ _ operate _>) 
             (<T NP _ _>
               (<T N _ _>
                 (<L N _ _ ships _>)
                 (<T N[conj] _ _> 
                   (<L conj _ _ and _>)
                   (<L N _ _ banks _>)
                 ) 
               ) 
             ) 
           ) 
         ) 
         (<L . _ _ . _>) 
       )
     """
    val line2 = """(<T S[dcl] 0 2> (<T S[dcl] 1 2> (<T S/S 0 2> (<T NP 1 2> (<L NP[nb]/N DT DT No NP[nb]_167/N_167>) (<L N NNS NNS dummies N>) ) (<L , , , , ,>) ) (<T S[dcl] 1 2> (<T NP 1 2> (<L NP[nb]/N DT DT the NP[nb]_160/N_160>) (<L N NNS NNS drivers N>) ) (<T S[dcl]\NP 0 2> (<T (S[dcl]\NP)/S[dcl] 0 2> (<L (S[dcl]\NP)/S[dcl] VBD VBD pointed (S[dcl]\NP_68)/S[dcl]_69>) (<L (S\NP)\(S\NP) RP RP out (S_81\NP_76)_81\(S_81\NP_76)_81>) ) (<T S[dcl] 1 2> (<L NP PRP PRP they NP>) (<T S[dcl]\NP 1 2> (<L (S\NP)/(S\NP) RB RB still (S_151\NP_146)_151/(S_151\NP_146)_151>) (<T S[dcl]\NP 0 2> (<L (S[dcl]\NP)/NP VBD VBD had (S[dcl]\NP_88)/NP_89>) (<T NP 0 2> (<T NP 0 2> (<T NP 0 1> (<L N NN NN space N>) ) (<T NP\NP 0 2> (<L (NP\NP)/NP IN IN on (NP_100\NP_100)/NP_101>) (<T NP 1 2> (<L NP[nb]/N PRP$ PRP$ their NP[nb]_108/N_108>) (<L N NNS NNS machines N>) ) ) ) (<T NP\NP 0 2> (<L (NP\NP)/NP IN IN for (NP_116\NP_116)/NP_117>) (<T NP 0 2> (<T NP 1 2> (<T NP[nb]/N 1 2> (<T NP 1 2> (<L NP[nb]/N DT DT another NP[nb]_134/N_134>) (<L N NN NN sponsor N>) ) (<L (NP[nb]/N)\NP POS POS 's (NP[nb]_126/N_126)\NP_127>) ) (<L N NN NN name N>) ) (<T NP[conj] 1 2> (<L conj CC CC or conj>) (<T NP 0 1> (<L N CD CD two N>) ) ) ) ) ) ) ) ) ) ) ) (<L . . . . .>) )"""
    val line3 = """(<T NP\NP 0 2> (<L (NP\NP)/NP IN IN from (NP_136\NP_136)/NP_137>) (<T NP 0 2> (<T NP 1 2> (<L NP[nb]/N DT DT the NP[nb]_158/N_158>) (<T N 1 2> (<L N/N NNP NNP National N_153/N_153>) (<T N 1 2> (<L N/N NNP NNP Cancer N_146/N_146>) (<L N NNP NNP Institute N>) ) ) ) (<T NP[conj] 1 2> (<L conj CC CC and conj>) (<T NP 0 2> (<T NP 1 2> (<L NP[nb]/N DT DT the NP[nb]_172/N_172>) (<T N 1 2> (<L N/N JJ JJ medical N_167/N_167>) (<L N NNS NNS schools N>) ) ) (<T NP\NP 0 2> (<L (NP\NP)/NP IN IN of (NP_180\NP_180)/NP_181>) (<T NP 0 2> (<T NP 0 1> (<T N 1 2> (<L N/N NNP NNP Harvard N_190/N_190>) (<L N NNP NNP University N>) ) ) (<T NP[conj] 1 2> (<L conj CC CC and conj>) (<T NP 0 1> (<T N 1 2> (<L N/N NNP NNP Boston N_200/N_200>) (<L N NNP NNP University N>) ) ) ) ) ) ) ) ) )"""
    val line4 = """(<T (S\NP)\(S\NP) 1 2> (<T NP 0 2> (<T NP 0 2> (<T NP 0 1> (<T N 1 2> (<L N/N JJ JJ many N_197/N_197>) (<L N NNS NNS practices N>) ) ) (<T NP\NP 0 1> (<T S[pss]\NP 1 2> (<L (S\NP)/(S\NP) RB RB historically (S_242\NP_237)_242/(S_242\NP_237)_242>) (<T S[pss]\NP 0 2> (<T S[pss]\NP 0 2> (<L (S[pss]\NP)/PP VBN VBN accepted (S[pss]\NP_205)/PP_206>) (<T PP 0 2> (<L PP/(S[adj]\NP) IN IN as PP/(S[adj]_213\NP_211)_213>) (<L S[adj]\NP JJ JJ normal S[adj]\NP_218>) ) ) (<L (S\NP)\(S\NP) RB RB here (S_230\NP_225)_230\(S_230\NP_225)_230>) ) ) ) ) (<T NP\NP 0 2> (<T NP\NP 1 2> (<L : : : -- :>) (<T NP\NP 1 2> (<L (NP\NP)/(NP\NP) JJ JJ such (NP_333\NP_327)_333/(NP_333\NP_327)_333>) (<T NP\NP 0 2> (<T (NP\NP)/(S[ng]\NP) 0 2> (<L ((NP\NP)/(S[ng]\NP))/NP IN IN as ((NP_259\NP_259)/(S[ng]_260\NP_255:B)_260)/NP_255>) (<T NP 0 1> (<L N NNS NNS politicians N>) ) ) (<T S[ng]\NP 0 2> (<T S[ng]\NP 0 2> (<T S[ng]\NP 0 2> (<L (S[ng]\NP)/NP VBG VBG accepting (S[ng]\NP_271)/NP_272>) (<T NP 0 1> (<T N 1 2> (<L N/N JJ JJ substantial N_281/N_281>) (<L N NNS NNS gifts N>) ) ) ) (<T (S\NP)\(S\NP) 0 2> (<L ((S\NP)\(S\NP))/NP IN IN from ((S_295\NP_290)_295\(S_295\NP_290)_295)/NP_296>) (<T NP 0 1> (<L N NNS NNS businessmen N>) ) ) ) (<T S[ng]\NP[conj] 1 2> (<L conj CC CC or conj>) (<T S[ng]\NP 0 2> (<L (S[ng]\NP)/NP VBG VBG having (S[ng]\NP_306)/NP_307>) (<T NP 0 1> (<T N 1 2> (<L N/N JJ JJ extramarital N_316/N_316>) (<L N NNS NNS affairs N>) ) ) ) ) ) ) ) ) (<L : : : -- :>) ) ) (<T ((S\NP)\(S\NP))\NP 0 2> (<L (((S\NP)\(S\NP))\NP)/(S[ng]\NP) IN IN are (((S_150\NP_142)_150\(S_150\NP_142)_150)\NP_151)/(S[ng]_152\NP_146)_152>) (<T S[ng]\NP 0 2> (<L S[ng]\NP VBG VBG coming S[ng]\NP_157>) (<T (S\NP)\(S\NP) 0 2> (<L ((S\NP)\(S\NP))/NP IN IN under ((S_170\NP_165)_170\(S_170\NP_165)_170)/NP_171>) (<T NP 0 1> (<T N 1 2> (<L N/N JJ JJ close N_187/N_187>) (<T N 1 2> (<L N/N JJ JJ ethical N_180/N_180>) (<L N NN NN scrutiny N>) ) ) ) ) ) ) ) """
    val line5 = """(<T NP 0 2> (<T NP 1 2> (<L NP[nb]/N DT DT any NP[nb]_167/N_167>) (<T N 1 2> (<L N/N NN NN asbestos N_162/N_162>) (<L N NNS NNS workers N>) ) ) (<T NP\NP 0 1> (<T S[pss]\NP 0 2> (<L S[pss]\NP VBN VBN studied S[pss]\NP_172>) (<T (S\NP)\(S\NP) 0 2> (<L ((S\NP)\(S\NP))/NP IN IN in ((S_185\NP_180)_185\(S_185\NP_180)_185)/NP_186>) (<T NP 0 1> (<T N 1 2> (<L N/N JJ JJ Western N_202/N_202>) (<T N 1 2> (<L N/N VBN VBN industrialized N_195/N_195>) (<L N NNS NNS countries N>) ) ) ) ) ) ) )"""
    val line6 = """(<T S[dcl] 0 2> (<T S[dcl] 0 2> (<T S[dcl] 1 2> (<T NP 1 2> (<L NP[nb]/N DT DT The NP[nb]_174/N_174>) (<T N 1 2> (<L N/N JJ JJ 30-day N_169/N_169>) (<T N 1 2> (<L N/N JJ JJ simple N_162/N_162>) (<L N NN NN yield N>) ) ) ) (<T S[dcl]\NP 0 2> (<T S[dcl]\NP 0 2> (<L S[dcl]\NP VBD VBD fell S[dcl]\NP_94>) (<T (S\NP)\(S\NP) 0 2> (<L ((S\NP)\(S\NP))/NP TO TO to ((S_107\NP_102)_107\(S_107\NP_102)_107)/NP_108>) (<T NP 1 2> (<L NP[nb]/N DT DT an NP[nb]_129/N_129>) (<T N 1 2> (<L N/N JJ JJ average N_124/N_124>) (<T N 1 2> (<L N/N CD CD 8.19 N_117/N_117>) (<L N NN NN % N>) ) ) ) ) ) (<T (S\NP)\(S\NP) 0 2> (<L ((S\NP)\(S\NP))/NP IN IN from ((S_142\NP_137)_142\(S_142\NP_137)_142)/NP_143>) (<T NP 0 1> (<T N 1 2> (<L N/N CD CD 8.22 N_152/N_152>) (<L N NN NN % N>) ) ) ) ) ) (<T S[dcl][conj] 1 2> (<L ; ; : ; ;>) (<T S[dcl] 1 2> (<T NP 1 2> (<L NP[nb]/N DT DT the NP[nb]_259/N_259>) (<T N 1 2> (<L N/N JJ JJ 30-day N_254/N_254>) (<T N 1 2> (<L N/N NN NN compound N_247/N_247>) (<L N NN NN yield N>) ) ) ) (<T S[dcl]\NP 0 2> (<T S[dcl]\NP 0 2> (<L S[dcl]\NP VBD VBD slid S[dcl]\NP_179>) (<T (S\NP)\(S\NP) 0 2> (<L ((S\NP)\(S\NP))/NP TO TO to ((S_192\NP_187)_192\(S_192\NP_187)_192)/NP_193>) (<T NP 1 2> (<L NP[nb]/N DT DT an NP[nb]_214/N_214>) (<T N 1 2> (<L N/N JJ JJ average N_209/N_209>) (<T N 1 2> (<L N/N CD CD 8.53 N_202/N_202>) (<L N NN NN % N>) ) ) ) ) ) (<T (S\NP)\(S\NP) 0 2> (<L ((S\NP)\(S\NP))/NP IN IN from ((S_227\NP_222)_227\(S_227\NP_222)_227)/NP_228>) (<T NP 0 1> (<T N 1 2> (<L N/N CD CD 8.56 N_237/N_237>) (<L N NN NN % N>) ) ) ) ) ) ) ) (<L . . . . .>) )"""
    val line7 = """(<T S[dcl] 1 2> (<T S/S 0 2> (<L (S/S)/S[dcl] WRB WRB When (S_367/S_367)/S[dcl]_368>) (<T S[dcl] 1 2> (<L NP PRP PRP we NP>) (<T S[dcl]\NP 0 2> (<L (S[dcl]\NP)/(S[ng]\NP) VBD VBD evaluated (S[dcl]\NP_377)/(S[ng]_378\NP_377:B)_378>) (<T S[ng]\NP 0 2> (<L (S[ng]\NP)/NP VBG VBG raising (S[ng]\NP_385)/NP_386>) (<T NP 1 2> (<L NP[nb]/N PRP$ PRP$ our NP[nb]_393/N_393>) (<L N NN NN bid N>) ) ) ) ) ) (<T S[dcl] 1 2> (<L , , , , ,>) (<T S[dcl] 0 2> (<T S[dcl] 0 2> (<T S[dcl] 1 2> (<T NP 1 2> (<L NP[nb]/N DT DT the NP[nb]_312/N_312>) (<L N NNS NNS risks N>) ) (<T S[dcl]\NP 0 2> (<T S[dcl]\NP 0 2> (<L (S[dcl]\NP)/(S[adj]\NP) VBD VBD seemed (S[dcl]\NP_257)/(S[adj]_258\NP_257:B)_258>) (<T S[adj]\NP 0 2> (<L S[adj]\NP JJ JJ substantial S[adj]\NP_263>) (<T S[adj]\NP[conj] 1 2> (<L conj CC CC and conj>) (<L S[adj]\NP JJ JJ persistent S[adj]\NP_268>) ) ) ) (<T (S\NP)\(S\NP) 0 2> (<L ((S\NP)\(S\NP))/NP IN IN over ((S_283\NP_278)_283\(S_283\NP_278)_283)/NP_284>) (<T NP 1 2> (<L NP[nb]/N DT DT the NP[nb]_305/N_305>) (<T N 1 2> (<L N/N JJ JJ next N_300/N_300>) (<T N 1 2> (<L N/N CD CD five N_293/N_293>) (<L N NNS NNS years N>) ) ) ) ) ) ) (<T S[dcl][conj] 1 2> (<L , , , , ,>) (<T S[dcl][conj] 1 2> (<L conj CC CC and conj>) (<T S[dcl] 1 2> (<T NP 1 2> (<L NP[nb]/N DT DT the NP[nb]_351/N_351>) (<L N NNS NNS rewards N>) ) (<T S[dcl]\NP 0 2> (<L (S[dcl]\NP)/(S[adj]\NP) VBD VBD seemed (S[dcl]\NP_321)/(S[adj]_322\NP_321:B)_322>) (<T S[adj]\NP 1 2> (<T NP 1 2> (<L NP[nb]/N DT DT a NP[nb]_344/N_344>) (<T N 1 2> (<L N/N JJ JJ long N_339/N_339>) (<L N NN NN way N>) ) ) (<L (S[adj]\NP)\NP IN IN out (S[adj]\NP_329)\NP_330>) ) ) ) ) ) ) (<L . . . . .>) ) ) )"""
    val line8 = """(<T S[dcl] 0 2> (<T S[dcl] 1 2> (<T NP 0 1> (<T N 1 2> (<L N/N NNP NNP Mr. N_142/N_142>) (<L N NNP NNP Vinken N>) ) ) (<T S[dcl]\NP 0 2> (<L (S[dcl]\NP)/NP VBZ VBZ is (S[dcl]\NP_87)/NP_88>) (<T NP 0 2> (<T NP 0 1> (<L N NN NN chairman N>) ) (<T NP\NP 0 2> (<L (NP\NP)/NP IN IN of (NP_99\NP_99)/NP_100>) (<T NP 0 2> (<T NP 0 1> (<T N 1 2> (<L N/N NNP NNP Elsevier N_109/N_109>) (<L N NNP NNP N.V. N>) ) ) (<T NP[conj] 1 2> (<L , , , , ,>) (<T NP 1 2> (<L NP[nb]/N DT DT the NP[nb]_131/N_131>) (<T N 1 2> (<L N/N NNP NNP Dutch N_126/N_126>) (<T N 1 2> (<L N/N VBG VBG publishing N_119/N_119>) (<L N NN NN group N>) ) ) ) ) ) ) ) ) ) (<L . . . . .>) )"""
    val line9 = """(<T NP 0 2> (<T NP 1 2> (<L NP[nb]/N DT DT this NP[nb]_87/N_87>) (<T N 1 2> (<L N/N NNP NNP U.S. N_82/N_82>) (<T N 1 2> (<L N/N NNS NNS sales N_75/N_75>) (<T N 1 2> (<L conj CC CC and conj>) (<T N 1 2> (<L N/N NN NN marketing N_66/N_66>) (<L N NN NN arm N>) ) ) ) ) ) (<T NP\NP 0 2> (<L (NP\NP)/NP IN IN of (NP_95\NP_95)/NP_96>) (<T NP 0 1> (<T N 1 2> (<L N/N JJ JJ Japanese N_133/N_133>) (<T N 1 2> (<L N/N NN NN auto N_126/N_126>) (<T N 1 2> (<L N/N NN NN maker N_119/N_119>) (<T N 1 2> (<L N/N NNP NNP Mazda N_112/N_112>) (<T N 1 2> (<L N/N NNP NNP Motor N_105/N_105>) (<L N NNP NNP Corp N>) ) ) ) ) ) ) ) ) """
    val line10 = """(<T S[dcl] 0 2> (<T S[dcl] 1 2> (<T S/S 0 2> (<L S/S RB RB Obviously S_127/S_127>) (<L , , , , ,>) ) (<T S[dcl] 1 2> (<L NP[expl] PRP PRP it NP[expl]>) (<T S[dcl]\NP[expl] 0 2> (<T (S[dcl]\NP[expl])/S[em] 0 2> (<T ((S[dcl]\NP[expl])/S[em])/PP 0 2> (<L (((S[dcl]\NP[expl])/S[em])/PP)/(S[adj]\NP) VBZ VBZ 's (((S[dcl]\NP[expl]_90)/S[em]_91)/PP)/(S[adj]_14\NP_10)_14>) (<L S[adj]\NP VBN VBN lost S[adj]\NP_119>) ) (<T PP 1 2> (<L PP/NP IN IN on PP_111/NP_111>) (<L NP PRP PRP you NP>) ) ) (<T S[em] 0 2> (<L S[em]/S[dcl] IN IN that S[em]/S[dcl]_132>) (<T S[dcl] 1 2> (<T NP 1 2> (<T NP/NP 1 2> (<L (NP/NP)/(NP/NP) IN IN about (NP_249/NP_249)_250/(NP_249/NP_249)_250>) (<L NP/NP CD CD 75 NP_144/NP_144>) ) (<T NP 1 2> (<L NP/PP NN NN % NP/PP_650>) (<T PP 1 2> (<L PP/NP IN IN of PP_111/NP_111>) (<T NP 1 2> (<L NP/N DT DT the NP_636/N_636>) (<T N 1 2> (<L N/N JJ JJ American N_107/N_107>) (<L N NNS NNS people N>) ) ) ) ) ) (<T S[dcl]\NP 1 2> (<T (S\NP)/(S\NP) 1 2> (<L ((S\NP)/(S\NP))/N DT DT these ((S_288\NP_289)_288/(S_288\NP_289)_288)/N_286>) (<L N NNS NNS days N>) ) (<T S[dcl]\NP 1 2> (<T (S\NP)/(S\NP) 0 2> (<T ((S\NP)/(S\NP))/: 0 2> (<T (((S\NP)/(S\NP))/:)/PP 0 2> (<T (((S\NP)/(S\NP))/:)/PP 0 2> (<L (((S\NP)/(S\NP))/:)/PP LRB -LRB- -LRB- (((S_183\NP_178)_183/(S_183\NP_178)_183)/:_184)/PP>) (<L ((S\NP)/(S\NP))\((S\NP)/(S\NP)) CC CC and ((S_490\NP_491)_490/(S_490\NP_491)_490)_488\((S_490\NP_491)_490/(S_490\NP_491)_490)_488>) ) (<T ((S\NP)/(S\NP))\((S\NP)/(S\NP)) 0 2> (<L (((S\NP)/(S\NP))\((S\NP)/(S\NP)))/NP IN IN in (((S_507\NP_508)_507/(S_507\NP_508)_507)_505\((S_507\NP_508)_507/(S_507\NP_508)_507)_505)/NP_504>) (<T NP 0 1> (<L N NN NN fact N>) ) ) ) (<T PP 1 2> (<T PP/PP 1 2> (<L (PP/PP)/N DT DT the (PP/PP)/N_258>) (<L N NN NN president N>) ) (<T PP 1 2> (<L PP/NP IN IN of PP_111/NP_111>) (<T NP 1 2> (<L NP/N DT DT the NP_636/N_636>) (<T N 1 2> (<L N/N NNP NNP United N_107/N_107>) (<L N NNPS NNPS States N>) ) ) ) ) ) (<L : RRB -RRB- -RRB- :>) ) (<T S[dcl]\NP 0 2> (<T (S[dcl]\NP)/NP 0 2> (<L ((S[dcl]\NP)/NP)/NP VBP VBP consider ((S[dcl]\NP_194)/NP_195)/NP_192>) (<L NP PRP PRP themselves NP>) ) (<T NP 0 1> (<L N NNS NNS environmentalists N>) ) ) ) ) ) ) ) ) ) (<L . . . . .>))"""
    val line11 = """(<T : 1 2> (<L : : : : :>) (<T : 1 2> (<T : 1 2> (<T NP 0 2> (<T NP 0 2> (<T NP 0 1> (<T N 1 2> (<T N/N 1 2> (<L (N/N)/(N/N) NNP NNP American (N_121/N_121)_122/(N_121/N_121)_122>) (<T N/N 1 2> (<L (N/N)/(N/N) NNP NNP Suzuki (N_121/N_121)_122/(N_121/N_121)_122>) (<L N/N NNP NNP Motor N_107/N_107>) ) ) (<L N NNP NNP Corp. N>) ) ) (<L , , , , ,>) ) (<T NP\NP 0 2> (<T NP\NP 0 2> (<L NP\NP NNP NNP Brea NP_139\NP_139>) (<T NP\NP[conj] 1 2> (<L , , , , ,>) (<L NP\NP NNP NNP Calif. NP_139\NP_139>) ) ) (<L , , , , ,>) ) ) (<T :\NP 0 2> (<T (:\NP)/PP 0 2> (<L ((:\NP)/PP)/NP VBD VBD awarded ((:\NP_17)/PP_18)/NP_19>) (<T NP 1 2> (<L NP/(N/PP) PRP$ PRP$ its NP_681/(N_681/PP{_*})_681>) (<T N/PP 1 2> (<L N/N VBN VBN estimated N_107/N_107>) (<T N/PP 1 2> (<T N/N 0 2> (<T N/N 0 2> (<L (N/N)/N[num] $ $ $ (N_592/N_592)/N[num]_593>) (<T N[num] 1 2> (<L N/N CD CD 10 N_107/N_107>) (<L N[num] CD CD million N[num]>) ) ) (<T (N/N)\(N/N) 0 2> (<L ((N/N)\(N/N))/N TO TO to ((N_376/N_376)_377\(N_376/N_376)_377)/N_374>) (<T N 0 2> (<L N/N[num] $ $ $ N/N[num]_591>) (<T N[num] 1 2> (<L N/N CD CD 30 N_107/N_107>) (<L N[num] CD CD million N[num]>) ) ) ) ) (<L N/PP NN NN account N/PP_543>) ) ) ) ) (<T PP 1 2> (<L PP/NP TO TO to PP_111/NP_111>) (<T NP 0 1> (<T N 0 2> (<L N NNP NNP Asher\/Gould N>) (<T N[conj] 1 2> (<L , , , , ,>) (<T N 1 2> (<L N/N NNP NNP Los N_107/N_107>) (<L N NNP NNP Angeles N>) ) ) ) ) ) ) ) (<L . . . . .>) ) )"""

    val r = EnglishCcgTreeBankReader() // (CcgRules.standard, RebankRules.standard)
    TreeViz.drawTree(r.getTree(line11))

    //    val a = r.readTreeLines(0 to 24).take(2).toVector.last
    //    val b = r.getTree(a).getOrElse(sys.error("Could not parse the input string: " + a))
    //    TreeViz.drawTree(r.assignRules(b))
    //    val c = r.rebank(b)
    //    TreeViz.drawTree(r.assignRules(c))
    //
    //    return
    //
    //    val t = r.getTree(line9.replaceAll("\\s+", " ")).get
    //    println(t.pretty)
    //    TreeViz.drawTree(r.assignRules(t))
    //    val u = r.rebank(t)
    //    println(u.pretty)
    //    TreeViz.drawTree(r.assignRules(u))
    //    r.convertToTree(u).foreach { v =>
    //      println(v.pretty)
    //      //TreeViz.drawTree(v)
    //    }

  }
}
