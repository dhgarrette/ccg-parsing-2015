package dhg.ccg.tagdict

import dhg.util._
import scalaz._
import Scalaz._
import dhg.ccg.util.Indexer
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.util._
import scala.collection.immutable.BitSet

object TagDictionaryI {
  type Word = String
  def to[A](td: TagDictionary[A], tagIndexer: Indexer[A], wordIndexer: Indexer[Word]): IndirectSparseVec[BitSet] = {
    val tdEntries = td.entries
    IndirectSparseVec(wordIndexer.objects.zipWithIndex.flatMap {
      case (w, wi) =>
        tdEntries.get(w).map(tags => wi -> tags.map(tagIndexer).toBitSet)
    }, wordIndexer.size)
  }
}

