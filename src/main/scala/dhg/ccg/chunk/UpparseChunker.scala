package dhg.ccg.chunk

import dhg.util._
import net.ponvert.upparse.model.{ Chunker => UppChunker, _ }
import net.ponvert.upparse.corpus._
import net.ponvert.upparse.cli.{ Main => UpparseMain }
import dhg.ccg.data.EnglishCcgTreeBankReader
import dhg.ccg.cat._
import scala.collection.JavaConverters._
import java.io.ByteArrayOutputStream
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import dhg.gfl._
import dhg.util.viz._
import scala.util.control.Breaks._
import dhg.ccg.parse.gfl.AnnotationTree
import dhg.ccg.parse.gfl.ChunkerAnnoExtractor

class UpparseChunker(trainedUppChunker: SequenceModelChunker, upparse: UpparseMain, name: String = "upp-chunker") extends UpparseTool() with Chunker with Serializable {
  private val serialVersionUID = 1L

  def chunkAll(sentences: Vector[Vector[Word]]): Vector[AnnotationTree] = {
    //println("UpparseChunker.chunkAll"); for (s <- sentences) println(f"  ${s.mkString(" ")}")
    val uppChunker: UppChunker = trainedUppChunker.getCurrentChunker;
    //val chunkerOutput: ChunkedSegmentedCorpus = tryChunk(sentences, uppChunker)
    val inputCorpusFile = f"temp/$name.spl"; writeUsing(File(inputCorpusFile)) { f => sentences.foreach(s => f.wl(s.mkString(" "))) }
    val inputCorpus = upparse.getStopSegmentCorpus(Array(inputCorpusFile), CorpusType.SPL, -1, -1)
    val chunkerOutput: ChunkedSegmentedCorpus = uppChunker.getChunkedCorpus(inputCorpus)
    val chunkerOutputStrings = asString(chunkerOutput)
    assert(sentences.size == chunkerOutputStrings.size, {
      val b = new StringBuilder
      b.append("failed in chunkAll")
      b.append("  sentences:")
      for (s <- sentences) b.append(f"    [${s}]")
      b.append("  chunkerOutputStrings:")
      for (s <- chunkerOutputStrings) b.append(f"    [${s}]")
      b.toString
    })
    val trees = (sentences zipSafe chunkerOutputStrings).mapt { (originalSentence, chunkedString) =>
      UpparseTool.toAnnotationTree(originalSentence, chunkedString)
    }
    //for (s <- trees) println(f"  ${s}")
    trees
  }

  //  private[this] def tryChunk(sentences: Vector[Vector[Word]], uppChunker: UppChunker): ChunkedSegmentedCorpus = {
  //    try {
  //      val inputCorpusFile = "temp/upp-input-corpus.spl"
  //      writeUsing(File(inputCorpusFile)) { f => sentences.foreach(s => f.wl(s.mkString(" "))) }
  //      val inputCorpus = upparse.getStopSegmentCorpus(Array(inputCorpusFile), CorpusType.SPL, -1, -1)
  //      val chunkerOutput: ChunkedSegmentedCorpus = uppChunker.getChunkedCorpus(inputCorpus)
  //      chunkerOutput
  //    }
  //    catch {
  //      case e: ArrayIndexOutOfBoundsException =>
  //        println(f"failed to chunk: ${sentences.map(_.mkString(" ")).mkString("\n")}")
  //        if (sentences.size == 1 && sentences.only.size > 0) {
  //          tryChunk(Vector(sentences.only.drop(1)), uppChunker)
  //        }
  //        else throw e
  //    }
  //  }

  override def toString = f"UpparseChunker()"
}

class UpparseParser(trainedUppChunker: SequenceModelChunker, upparse: UpparseMain) extends UpparseTool() with PartialParser with Serializable {
  private val serialVersionUID = 1L

  def parseAll(sentences: Vector[Vector[Word]]): Vector[AnnotationTree] = ???

  override def toString = f"UpparseParser()"
}

abstract class UpparseTool() {

  protected[this] def asString(chunkerOutput: ChunkedSegmentedCorpus): Vector[String] = {
    val b = new ByteArrayOutputStream()
    chunkerOutput.writeToWithPunc(new BufferedWriter(new OutputStreamWriter(b)), null, "{|{", "}|}")
    b.toString("UTF8").splitlines
  }

}

object UpparseTool {

  def toAnnotationTree(chunkedString: String): AnnotationTree = {
    toAnnotationTree(chunkedString.replace("{|{", " ").replace("}|}", " ").trim.splitWhitespace, chunkedString)
  }

  def toAnnotationTree(originalSentence: Vector[String], chunkedString: String): AnnotationTree = {
    val chunkedTokens = chunkedString.replace("{|{", " {|{ ").replace("}|}", " }|} ").trim.splitWhitespace
    val tokens = originalSentence.zipWithIndex.mapt((t, i) => Token(t, i))
    val tokenNodes = tokens.map(tok => WordNode(f"${tok.token}_${tok.index}", tok))

    //println(originalSentence.mkString(" "))
    //println(chunkedTokens.filterNot(Set("(", ")")).mkString(" "))
    //println(chunkedTokens.mkString(" "))

    var i = 0 // index in the chunked string
    var o = 0 // index in the original string (parens removed)
    def toTree(): AnnotationTree = {
      val startIdx = o
      var trees = Vector[AnnotationTree]()
      breakable {
        while (i < chunkedTokens.length) {
          chunkedTokens(i) match {
            case "{|{" =>
              i += 1
              trees = trees :+ toTree()
            case "}|}" =>
              i += 1
              break
            case t =>
              if (o < tokenNodes.length) { // because UPP adds punctuation at the end of a sentence if it doesn't have one
                trees = trees :+ AnnotationTree(tokenNodes(o), cat"<fake category>", Vector.empty)
              }
              else if (!(o == tokenNodes.length && KeepStop.isStoppingPunc(t))) {
                println(originalSentence.mkString(" "))
                println(chunkedTokens.filterNot(Set("(", ")")).mkString(" "))
                println(chunkedTokens.mkString(" "))
                println(chunkedTokens.zipWithIndex.mapt((w, i) => f"${i + 1}:$w").mkString(" "))
              }

              i += 1
              o += 1
          }
        }
      }
      AnnotationTree(
        FeNode(f"FE_${startIdx}_${o}"), cat"<fake category>",
        if (trees.size == 1) trees.only.subtrees else trees) // if there is only one subtree, just promote its children
    }

    val newS = toTree()
    //println(newS)
    //TreeViz.drawTree(newS)
    //println
    newS
  }
}

//

class UpparseChunkerTrainer(name: String = "upp-chunker") extends UpparserTrainer with UnsupervisedChunkerTrainer with Serializable {
  private val serialVersionUID = 1L

  def trainChunker(rawTrainData: TraversableOnce[Vector[Word]]) = {
    val trainCorpusFile = "temp/upp-train-corpus.spl"
    val upparse = new UpparseMain(f"chunk -train $trainCorpusFile -trainFileType SPL".split("\\s+"))
    new UpparseChunker(train(rawTrainData, upparse, trainCorpusFile), upparse, name)
  }

  override def toString = f"UpparseChunkerTrainer()"
}

class UpparsePartialParserTrainer extends UpparserTrainer with UnsupervisedPartialParserTrainer with Serializable {
  private val serialVersionUID = 1L

  def trainParser(rawTrainData: TraversableOnce[Vector[Word]]) = {
    val trainCorpusFile = "temp/upp-train-corpus.spl"
    val upparse: UpparseMain = ??? // new UpparseMain(f"chunk -train $trainCorpusFile -trainFileType SPL".split("\\s+"))
    new UpparseParser(train(rawTrainData, upparse, trainCorpusFile), upparse)
  }

  override def toString = f"UpparsePartialParserTrainer()"
}

abstract class UpparserTrainer {
  protected[this] def train(rawTrainData: TraversableOnce[Vector[String]], upparse: UpparseMain, trainCorpusFile: String) = {
    writeUsing(File(trainCorpusFile)) { f => rawTrainData.foreach { s => f.wl(s.mkString(" ")) } }
    val trainableChunker = upparse.getTrainableChunker()
    upparse.trainChunker(trainableChunker)
    trainableChunker
  }
}

object UpparseChunker {
  def main(args: Array[String]): Unit = {
    //    val rawTrainData: Iterator[Vector[String]] = EnglishCcgTreeBankReader().raw().take(100)
    //    val chunker = new UpparseChunkerTrainer().trainChunker(rawTrainData)
    //    val inputData: Vector[Vector[String]] = EnglishCcgTreeBankReader().raw().drop(1000).take(10).toVector
    //    val chunked = chunker.chunkAll(inputData)
    //    chunked foreach TreeViz.drawTree
    //    TreeViz.drawTree(chunker.chunk("the company ( 1 ) and the ( 2 ) know him".splitWhitespace))

    val a = UpparseTool.toAnnotationTree("(((LIMITED RISK FUNDS) :))".replace("(", "{|{").replace(")", "}|}"))
    TreeViz.drawTree(a)
    val (tokens, tokenNodes) = ChunkerAnnoExtractor.makeTokens("LIMITED RISK FUNDS :".splitWhitespace)
    val f = a.toFudgSentence(tokens, tokenNodes)
    TreeViz.drawTree(f.fudgTree)
  }
}
