package dhg.ccg.data

import dhg.util._
import scala.util.Try
import dhg.ccg.cat._

trait TagBankReader[Tag] {
  type Word = String
  def tdData(): Vector[Vector[(Word, Tag)]]
  protected[this] def rawData: Vector[Vector[(Word, Tag)]]
  final def raw(): Vector[Vector[Word]] = rawData.map(_.map(_._1))
  final def rawCHEATING(): Vector[Vector[(Word, Tag)]] = rawData
  def devData(): Vector[Vector[(Word, Tag)]]
  def testData(): Vector[Vector[(Word, Tag)]]

  def startWord: Word
  def startTag: Tag
  def endWord: Word
  def endTag: Tag
  def tagToString: (Tag => String)
  def tagFromString: (String => Tag)
}

abstract class CcgTagBankReader[Tag] extends TagBankReader[Tag] {
  protected[this] val Loc: String

  protected[this] val TerminalRe = """<L ([\S]+) ([\S]+) ([\S]+) ([\S]+) ([\S]+)>""".r // <L (NP\NP)/NP IN IN of (NP_99\NP_99)/NP_100>
  protected[this] val NonTerminalRe = """<T ([\S]+) ([\S]+) ([\S]+)>""".r
  protected[this] val AtomicRe = """[^\\/]+""".r

  /**
   * section -> sentence -> token
   */
  protected[this] def doGetStrings(): Vector[Vector[Vector[(String, String, String)]]] = {
    File(Loc).ls("\\d+".r).map { sectionDir =>
      for (file <- sectionDir.ls; Seq(header, content) <- file.readLines.grouped(2)) yield {
          assert(header.startsWith("ID="))
          TerminalRe.allGroups(content).map {
            case Seq(supertag, pos, _, token, _) =>
              val cleanSupertag = supertag match {
                case "((S[b]\\NP)/NP)/" => "((S[b]\\NP)/NP)" // error in wsj_0595.15
                case "(S[dcl]\\NP)/(S[dcl]\\NP)~SB" => "((S[dcl]\\NP)/(S[dcl]\\NP))" // errors in chinese ccgbank
                case "." => """(S\S)"""
                case "," => """(NP\NP)"""
                case ";" => """((S\S)/S)"""
                case s @ AtomicRe() => s
                case s => s"($s)"
              }
              (token, cleanSupertag, pos)
            case x => sys.error("Failure in reading CcgBank.  \nTerminal \"%s\"  \nfrom content line:%s\n".format(x, content))
          }.toVector
        }
      }
  }

  protected[this] def data: Vector[Vector[Vector[(String, Tag)]]]

  override final val startWord = "<S>"
  override final val endWord = "<E>"
}

abstract class EnglishCcgTagBankReader[Tag] extends CcgTagBankReader[Tag] {
  protected[this] val Loc = "data/ccgbank/AUTO"

  override final def tdData = data.slyce(0 to 15).flatten
  override final protected[this] def rawData = data.slyce(16 to 18).flatten
  override final def devData = data.slyce(19 to 21).flatten
  override final def testData = data.slyce(22 to 24).flatten
}

object PosEnglishCcgTagBankReader extends EnglishCcgTagBankReader[String] {
  override /*protected[this]*/ val data = doGetStrings().map(_.map(_.mapt((w, s, p) => (w, p))))
  override val startTag = "<S>"
  override val endTag = "<E>"
  override val tagToString: (String => String) = identity
  override val tagFromString: (String => String) = identity
}

abstract class CatEnglishCcgTagBankReader extends EnglishCcgTagBankReader[Cat] {
  protected[this] def catmap: CatInterner
  override /*protected[this]*/ lazy val data = doGetStrings().map(_.map(_.mapt((w, s, p) => (w, catmap(s)))))
  override final val startTag = Cat.startCat(catmap)
  override final val endTag = Cat.endCat(catmap)
  override final val tagToString: (Cat => String) = _.toString
  override final lazy val tagFromString: (String => Cat) = catmap.fromString _
}

object FeaturelessEnglishCcgTagBankReader extends CatEnglishCcgTagBankReader {
  override protected[this] val catmap: CatInterner = ??? // new StandardCatInterner(CatInterner.CcgBankPuncRe, CatInterner.CcgBankAtomRe, removeFeatures = true)
}

object FeaturedEnglishCcgTagBankReader extends CatEnglishCcgTagBankReader {
  override protected[this] val catmap: CatInterner = ??? // new StandardCatInterner(CatInterner.CcgBankPuncRe, CatInterner.CcgBankAtomRe, removeFeatures = false)
}

//
//
//

abstract class TutCcgTagBankReader extends TagBankReader[Cat] {
  private[this] val Loc = "data/ccgbank-italian/pro"

  // newspaper has 807 sentences
  // jrc_acquis has 132
  // civil_law has 898

  protected[this] def catmap: CatInterner
  override final def tdData = data("civil_law").drop(350) ++ data("jrc_acquis") ++ data("newspaper").drop(350)
  override final protected[this] def rawData = data("civil_law").slyce(0 until 150) ++ data("newspaper").slyce(0 until 150)
  override final def devData = data("civil_law").slyce(150 until 250) ++ data("newspaper").slyce(150 until 250)
  override final def testData = data("civil_law").slyce(250 until 350) ++ data("newspaper").slyce(250 until 350)

  private[this] val data: Map[String, Vector[Vector[(String, Cat)]]] = {
    File(Loc).ls.map { f =>
      f.name.split("\\.").head ->
        f.readLines.map(_.trim).split("").map { group =>
          assert(group.head.startsWith("ccg("))
          assert(group.last.endsWith(")."))
          for (line <- group if line.startsWith("t(")) yield parseTokenLine(line)
        }.toVector
    }.toMap
  }

  private[this] def parseTokenLine(line: String): (String, Cat) = {
    assert(line.startsWith("t("))
    var token: Option[String] = None
    var cat: Option[String] = None

    var tokenBuilder = new StringBuilder
    var catBuilder = new StringBuilder
    var i = 2
    while (cat.isEmpty) {
      line(i) match {
        case ',' =>
          val parts = catBuilder.toString.replaceAll("""([()\\/:])""", " $1 ").trim.splitWhitespace
          val b = new StringBuilder
          var j = 0
          while (j < parts.size) {
            parts(j) match {
              case ":" =>
                b ++= f"[${parts(j + 1)}]"
                j += 1
              case x => b ++= x.toUpperCase
            }
            j += 1
          }
          cat = Some(b.toString)
        case c => catBuilder += c
      }
      i += 1
    }

    if (line(i) == '\'') { // quoted token
      i += 1
      while (token.isEmpty) {
        line(i) match {
          case '\'' =>
            token = Some(tokenBuilder.toString)
          case '\\' =>
            tokenBuilder += line(i + 1)
            i += 1
          case c =>
            tokenBuilder += c
        }
        i += 1
      }
    }
    else { // unquoted token
      while (token.isEmpty) {
        line(i) match {
          case ',' =>
            token = Some(tokenBuilder.toString)
          case '\\' =>
            tokenBuilder += line(i + 1)
            i += 1
          case c =>
            tokenBuilder += c
        }
        i += 1
      }
    }

    assert(token.isDefined && token.get.nonEmpty)
    assert(cat.isDefined && cat.get.nonEmpty)
    (token.get, catmap(cat.get))
  }

  override final val startWord = "<S>"
  override final val endWord = "<E>"
  override final val startTag = Cat.startCat(catmap)
  override final val endTag = Cat.endCat(catmap)
  override final val tagToString: (Cat => String) = _.toString
  override final lazy val tagFromString: (String => Cat) = catmap.fromString _
}

object FeaturelessTutCcgTagBankReader extends TutCcgTagBankReader {
  override protected[this] val catmap: CatInterner = ??? // new StandardCatInterner(CatInterner.TutBankAtomRe, removeFeatures = true)
}

object FeaturedTutCcgTagBankReader extends TutCcgTagBankReader {
  override protected[this] val catmap: CatInterner = ??? // new StandardCatInterner(CatInterner.TutBankAtomRe, removeFeatures = false)
}

//
//
//

abstract class ChineseCcgTagBankReader[Tag] extends CcgTagBankReader[Tag] {
  protected[this] val Loc = "data/ccgbank-chinese/AUTO"

  override final def tdData = data.slyce(0 to 11).flatten // 00-11
  override final protected[this] def rawData = data.slyce(12 to 16).flatten // 20-24
  override final def devData = data.slyce(17 to 19).flatten // 25-27
  override final def testData = data.slyce(20 to 24).flatten // 28-31
}

abstract class CatChineseCcgTagBankReader extends ChineseCcgTagBankReader[Cat] {
  protected[this] def catmap: CatInterner
  override /*protected[this]*/ lazy val data = {
    doGetStrings().map { section =>
      section.flatMap { sentence =>
        try {
          Some(sentence.mapt((w, s, p) => (w, catmap(s))))
        }
        catch {
          case e: RuntimeException =>
            //println(f"FAILED SENTENCE: $sentence")
            None
        }
      }
    }
  }
  override final val startTag = Cat.startCat(catmap)
  override final val endTag = Cat.endCat(catmap)
  override final val tagToString: (Cat => String) = _.toString
  override final lazy val tagFromString: (String => Cat) = catmap.fromString _
}

object FeaturelessChineseCcgTagBankReader extends CatChineseCcgTagBankReader {
  override protected[this] val catmap: CatInterner = ??? // new StandardCatInterner(CatInterner.CcgBankPuncRe, CatInterner.CcgBankAtomRe, removeFeatures = true)
}

object FeaturedChineseCcgTagBankReader extends CatChineseCcgTagBankReader {
  override protected[this] val catmap: CatInterner = ??? // new StandardCatInterner(CatInterner.CcgBankPuncRe, CatInterner.CcgBankAtomRe, removeFeatures = false)
}

//
//
//

object TestTagBankReader extends TagBankReader[String] {
  val Loc = "data/test/AUTO"

  val TokRe = """([^|\s]+)\|([^|\s]+)""".r

  /**
   * @return	section -> file -> sentence
   */
  val data = {
    for {
      section <- (0 to 24).toVector
    } yield {
      Option(File(Loc, f"$section%02d").listFiles).map { sectionDir =>
        for {
          file <- sectionDir.iterator
          line <- file.readLines.map(_.trim)
          if line.nonEmpty
        } yield {
          line.split("\\s+").map { case TokRe(w, t) => (w, t) }.toVector
        }
      }.fold(Vector[Vector[(String, String)]]())(_.toVector)
    }
  }

  final def tdData = data.slyce(0 to 15).flatten
  final protected[this] def rawData = data.slyce(16 to 18).flatten
  final def devData = data.slyce(19 to 21).flatten
  final def testData = data.slyce(22 to 24).flatten

  override val startWord = "<S>"
  override val startTag = "<S>"
  override val endWord = "<E>"
  override val endTag = "<E>"
  override val tagToString: (String => String) = identity
  override val tagFromString: (String => String) = identity
}
