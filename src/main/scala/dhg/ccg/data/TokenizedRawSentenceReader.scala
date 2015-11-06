package dhg.ccg.data

import dhg.util._
import scala.collection.mutable.{ Set => MSet }

class TokenizedRawCorpusReader(paths: Vector[String], maxSentLen: Int = Int.MaxValue) extends CorpusReader {

  def raw(): Iterator[Vector[Word]] = {
    for {
      path <- paths.iterator
      line <- File(path).readLines
      if !line.startsWith("OPTIONAL")
      if !line.contains("STORY CAN END HERE")
    } yield {
      line.splitWhitespace
    }
  }

}

class QuoteRemovingCorpusReader(delegate: CorpusReader) extends CorpusReader {
  def raw(): Iterator[Vector[Word]] = delegate.raw.map(_.filterNot(Set("``", "''")))
}

class DeduplicatingCorpusReader(delegate: CorpusReader) extends CorpusReader {
  def raw(): Iterator[Vector[Word]] = {
    val seen = MSet.empty[Set[Word]]
    delegate.raw.filter { s =>
      val sSet = s.toSet
      if (seen(sSet)) {
        false
      }
      else {
        seen += sSet
        true
      }
    }
  }
}

class CompositeCorpusReader(delegates: Vector[CorpusReader]) extends CorpusReader {
  def raw(): Iterator[Vector[Word]] = delegates.iterator.flatMap(_.raw)
}

class MaxLengthRemovingCorpusReader(maxlen: Int, delegate: CorpusReader) extends CorpusReader {
  def raw(): Iterator[Vector[Word]] = delegate.raw.filter(_.size <= maxlen)
}

object TokenizedRawCorpusReader {
  def main(args: Array[String]): Unit = {
    new DeduplicatingCorpusReader(new TokenizedRawCorpusReader(Vector("gc.txt"))).raw
      .foreach(s => println(s.mkString(" ")))

    val enCcgbank = EnglishCcgTreeBankReader()
    val r = new SeparateTrainTestCorpusReader(
      tdReader = enCcgbank,
      rawReader = new DeduplicatingCorpusReader(new CompositeCorpusReader(Vector(
        enCcgbank,
        new QuoteRemovingCorpusReader(new TokenizedRawCorpusReader(Vector("data/nytgiga.100k.spl")))))),
      testReader = enCcgbank)
    writeUsing(File("test.out.txt")) { f =>
      for (s <- r.raw.take(10000)) {
        f.writeLine(s.mkString(" "))
      }
    }
  }
}

