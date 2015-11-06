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

abstract class TutReader(usePos: Boolean) extends TreeBankReader {
  protected[this] val Loc: String
  protected[this] def catmap: CatInterner

  final def raw(): Iterator[Vector[Word]] = rawDataDONTUSE.map(_.leaves.map(_.word))
  
  def readTrees(section: String): Iterator[CcgTree] = {
    val file = File(Loc).ls.collect { case f if f.name.lsplit("\\.").head == section => f }.only
    for {
      group <- file.readLines.map(_.trim).split("")
      _ = assert(group.head.startsWith("ccg("))
      _ = assert(group.last.endsWith(")."))
      tree = getTree(group.mkString("\n"))
    } yield tree
  }
  
  def readFileTrees(file: File): Iterator[CcgTree] = ???
  def readFileTrees(fileNumber: Int): Option[Iterator[CcgTree]] = ???

  def getTree(line: String): CcgTree = {

    //   ba(t,
    //        fa(n,
    //             ba((n/n),
    //                  t(n,'832','832','NUMR'),
    //                  t(((n/n)\n),'.','.','PUNCT-SEPARATOR')),

    object TutTreeParser extends RegexParsers {
      private[this] val TextRe = """('[^\s']+'|[^\s,']+)""".r
      private[this] val SingleQuotedRe = """'(.+)'""".r
      private[this] val X = cat"X".asInstanceOf[AtomCat]

      private def cleanText(text: String) = {
        text.replace("<APOSTROPHE>", "'") match {
          case SingleQuotedRe(t) => t
          case _ => text
        }
      }
      private def makeCat(cat: String) = {
        catmap(cat)
      }

      private def term: Parser[CcgTree] =
        ("t(" ~> "[^\\s,]+".r ~ "," ~ TextRe ~ "," ~ TextRe ~ "," ~ "'[^\\s']+'".r <~ ")").map {
          case cat ~ _ ~ token ~ _ ~ lemma ~ _ ~ pos =>
            CcgLeaf(makeCat(if (usePos) pos else cat), cleanText(token), pos)
        }
      private def unary: Parser[CcgTree] =
        ("(tc)\\(".r ~> "[^\\s,]+".r ~ "," ~ "[^\\s,]+".r ~ "," ~ tree <~ ")").map {
          case cat ~ _ ~ _ ~ _ ~ sub =>
            CcgUnode(makeCat(cat), sub)
        }
      private def binary: Parser[CcgTree] =
        ("(fa|ba)".r ~ "(" ~ "[^\\s,]+".r ~ "," ~ tree ~ "," ~ tree <~ ")").map {
          case "fa" ~ _ ~ cat ~ _ ~ (left @ CcgTree(_ / X)) ~ _ ~ right => //                   ((X/P[par])/X)    (N\N)    ->    ((N\N)/P[par])
            CcgBinode(makeCat(cat), withCat(left, replace(left.cat, X, right.cat)), right)
          case "fa" ~ _ ~ cat ~ _ ~ (left @ CcgTree(X / _)) ~ _ ~ right =>
            CcgBinode(makeCat(cat), withCat(left, replace(left.cat, X, makeCat(cat))), right)
          case "ba" ~ _ ~ cat ~ _ ~ left ~ _ ~ (right @ CcgTree(_ \ X)) =>
            CcgBinode(makeCat(cat), left, withCat(right, replace(right.cat, X, left.cat)))
          case "ba" ~ _ ~ cat ~ _ ~ left ~ _ ~ (right @ CcgTree(X \ _)) => //                   (N/P[quo])    (X\(X/P[quo]))    ->    N
            CcgBinode(makeCat(cat), left, withCat(right, replace(right.cat, X, makeCat(cat))))
          case rule ~ _ ~ cat ~ _ ~ left ~ _ ~ right =>
            CcgBinode(makeCat(cat), left, right)
        }
      private def tree: Parser[CcgTree] = (term | binary | unary)
      private def ccgExp: Parser[CcgTree] =
        ("ccg(" ~> "\\d+".r ~ "," ~ tree <~ ").").map {
          case num ~ _ ~ exp => exp
        }
      def parse(str: String): CcgTree = parseAll(ccgExp | tree, str.splitlines.map(_.split("%%%").head.trim).mkString(" ").replace("\\'", "<APOSTROPHE>")) match {
        case Success(result: CcgTree, _) => result
        case _ => sys.error("Could not parse the input string: " + str)
      }
    }

    val t = TutTreeParser.parse(line)
    check(t)
    t
  }

  def replace(c: Cat, A: Cat, B: Cat): Cat = c match {
    case A => B
    case x: AtomCat => x
    case x: PuncCat => x
    case x / y => replace(x, A, B).asInstanceOf[NonPuncCat] / replace(y, A, B).asInstanceOf[NonPuncCat]
    case x \ y => replace(x, A, B).asInstanceOf[NonPuncCat] \ replace(y, A, B).asInstanceOf[NonPuncCat]
  }

  def withCat(t: CcgTree, c: Cat) = t match {
    case CcgBinode(_, l, r) => CcgBinode(c, l, r)
    case CcgUnode(_, s) => CcgUnode(c, s)
    case CcgLeaf(_, w, p) => CcgLeaf(c, w, p)
  }

  def check(t: CcgTree): Unit = t match {
    case CcgBinode(c, l, r) =>
      assert(!c.toString.contains('X'), f"found cat `$c`")
      check(l)
      check(r)
    case CcgUnode(c, s) =>
      assert(!c.toString.contains('X'), f"found cat `$c`")
      check(s)
    case CcgLeaf(c, w, _) =>
      assert(!c.toString.contains('X'), f"found cat `$c`")
  }

  override final val startWord = "<S>"
  override final val endWord = "<E>"
}

case class ItalianCcgTreeBankReader(usePos: Boolean = false) extends TutReader(usePos) {
  protected[this] val Loc = "data/ccgbank-italian/pro"
  override protected[this] final val catmap: CatInterner = TutCatInterner

  final def tdData = readTrees("civil_law").drop(350) ++ readTrees("jrc_acquis") ++ readTrees("newspaper").drop(350)
  final def rawDataDONTUSE = readTrees("civil_law").slyce(0 until 150) ++ readTrees("newspaper").slyce(0 until 150)
  final def devData = readTrees("civil_law").slyce(150 until 250) ++ readTrees("newspaper").slyce(150 until 250)
  final def testData = readTrees("civil_law").slyce(250 until 350) ++ readTrees("newspaper").slyce(250 until 350)

  override final val startTag = Cat.startCat(catmap)
  override final val endTag = Cat.endCat(catmap)
  override final val tagToString: (Cat => String) = _.toString
  override final lazy val tagFromString: (String => Cat) = catmap.fromString _
}

//

object ItalianTreebankReader {

  def main(args: Array[String]): Unit = {

    //    val y6 = """
    //                                             t((n/n),'0,2','0,2','NUMR')
    //		"""
    //
    //    val y = """
    //ccg(170,   %%% 'ALB-201'
    //     ba(t,
    //          ba(s:dcl,
    //               fa(np,
    //                    t((np/n),'L\'','l\'','ART~DE'),
    //                    t(n,'Irs',irs,'NOU~PR')),
    //               fa((s:dcl\np),
    //                    fa(((s:dcl\np)/np),
    //                         t((((s:dcl\np)/np)/((s:pap\np)/np)),ha,ha,'VAU~RE'),
    //                         t(((s:pap\np)/np),stimato,stimato,'VMA~PA')),
    //                    fa(np,
    //                         t((np/n),un,un,'ART~IN'),
    //                         fa(n,
    //                              ba((n/pp),
    //                                   ba((n/pp),
    //                                        t((n/pp),aumento,aumento,'NOU~CS'),
    //                                        t(((n/pp)\(n/pp)),tendenziale,tendenziale,'ADJ~QU')),
    //                                   fa(((n/pp)\(n/pp)),
    //                                        t((((n/pp)\(n/pp))/n),dello,dello,'DEFPREP'),
    //                                        fa(n,
    //                                             t((n/n),'0,2','0,2','NUMR'),
    //                                             t(n,'%','%','SPECIAL-ARG-PERCENT')))),
    //                              fa(pp,
    //                                   t((pp/n),della,della,'DEFPREP'),
    //                                   ba(n,
    //                                        ba(n,
    //                                             t(n,produzione,produzione,'NOU~CS'),
    //                                             t((n\n),media,media,'ADJ~QU')),
    //                                        t((n\n),giornaliera,giornaliera,'ADJ~QU'))))))),
    //          t((t\s:dcl),;,;,'PUNCT-END'))).
    //      """
    //
    //    val y5 =
    //      """
    //		  ccg(1,   %%% 'CODICECIVILE-1'
    //           ba(t,
    //                fa(n,
    //                     ba((n/n),
    //                          t(n,'832','832','NUMR'),
    //                          t(((n/n)\n),'.','.','PUNCT-SEPARATOR')),
    //                     ba(n,
    //                          t(n,'Contenuto\'s',contenuto,'NOU~CS'),
    //                          fa((n\n),
    //                               t(((n\n)/n),del,del,'DEFPREP'),
    //                               t(n,diritto,diritto,'NOU~CS')))),
    //                t((t\n),'.','.','PUNCT-END'))).
    //      """
    //
    //    val y1 = "t(n,'832','832','NUMR')"
    //    val y2 = """ba((n/n),
    //                    t(n,'832','832','NUMR'),
    //                    t(((n/n)\n),'.','.','PUNCT-SEPARATOR'))"""
    //    val y3 = """ba(n,
    //                    t(n,'Contenutos',contenuto,'NOU~CS'),
    //                    t(((n\n)/n),del,del,'DEFPREP'))"""
    //    val y4 = """ba(n,
    //                    t(n,'Contenutos',contenuto,'NOU~CS'),
    //                    fa((n\n),
    //                         t(((n\n)/n),del,del,'DEFPREP'),
    //                         t(n,diritto,diritto,'NOU~CS')))"""
    //
    //    //    val r = FeaturelessItalianCcgTreeBankReader
    //    //    println(r.readTree(y1).pretty)
    //    //    println(r.readTree(y2).pretty)
    //    //    println(r.readTree(y3).pretty)
    //    //    println(r.readTree(y4).pretty)
    //    //    println(r.readTree(y5).pretty)
    //    //    println(r.readTree(y6).pretty)
    //    //    println(r.readTree(y).pretty)
    //    //    //    return
    //    //
    //    //    ///
    //    //
    //    //    val reader = FeaturelessItalianCcgTreeBankReader
    //    //
    //    //    var good = 0
    //    //    var bad = 0
    //    //    for (x <- reader.allTreeOpts) {
    //    //      if (x.isDefined) good += 1
    //    //      else bad += 1
    //    //    }
    //    //    println(f"good = $good")
    //    //    println(f"bad  = $bad")
    //    //    println(f"pct  = ${good / (good + bad).toDouble}")

    val y1 = """fa((b:c\a),
                    t(((X\a)/X),'1','1','1'),
                    t(b:c,'2','2','2'))"""
    val t1 = ItalianCcgTreeBankReader().getTree(y1)
    println(t1.pretty)
    val y2 = """ba((b:c/a),
                    t(b:c,'1','1','1'),
                    t(((X/a)\X),'2','2','2'))"""
    val t2 = ItalianCcgTreeBankReader().getTree(y2)
    println(t2.pretty)
    val y3 = """fa(b:c,
                    t((X/(X\a)),'1','1','1'),
                    t((b:c\a),'2','2','2'))"""
    val t3 = ItalianCcgTreeBankReader().getTree(y3)
    println(t3.pretty)
    val y4 = """ba(b:c,
                    t((b:c/a),'1','1','1'),
                    t((X\(X/a)),'2','2','2'))"""
    val t4 = ItalianCcgTreeBankReader().getTree(y4)
    println(t4.pretty)

  }

}
