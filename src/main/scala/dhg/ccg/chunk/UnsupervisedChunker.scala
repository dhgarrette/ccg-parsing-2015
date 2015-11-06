package dhg.ccg.chunk

import dhg.util._
import dhg.ccg.parse.gfl.AnnotationTree

trait Chunker {
  type Word = String
  def chunkAll(sentences: Vector[Vector[Word]]): Vector[AnnotationTree]
  final def chunk(tokens: Vector[Word]): AnnotationTree = chunkAll(Vector(tokens)).only
}

trait UnsupervisedChunkerTrainer {
  type Word = String
  def trainChunker(corpus: TraversableOnce[Vector[Word]]): Chunker
}

//

trait PartialParser {
  type Word = String
  def parseAll(sentences: Vector[Vector[Word]]): Vector[AnnotationTree]
  final def parse(tokens: Vector[Word]): AnnotationTree = parseAll(Vector(tokens)).only
}

trait UnsupervisedPartialParserTrainer {
  type Word = String
  def trainParser(corpus: TraversableOnce[Vector[Word]]): PartialParser
}
