package dhg.ccg.parse.pcfg

import dhg.ccg.cat.Cat
import dhg.ccg.util.Indexer

sealed trait ProdI {
  def isNt: Boolean
  def isTerm: Boolean
}

sealed trait NontProdI extends ProdI {
  final def isNt = true
  final def isTerm = false
}

case class BinaryProdI(left: Int, right: Int) extends NontProdI {
  override def toString: String = f"[$left $right]"
}

case class UnaryProdI(sub: Int) extends NontProdI {
  override def toString: String = f"[$sub]"
}

case class TermProdI(word: Int) extends ProdI {
  override def isNt = false
  override def isTerm = true
  override def toString: String = f"$word"
}

object ProdI {
  type Word = String
  def to(p: Prod, catIndexer: Indexer[Cat], wordIndexer: Indexer[Word]): ProdI = p match {
    case BinaryProd(l, r) => BinaryProdI(catIndexer(l), catIndexer(r))
    case UnaryProd(s) => UnaryProdI(catIndexer(s))
    case TermProd(w) => TermProdI(wordIndexer(w))
  }
  def from(p: ProdI, catIndexer: Indexer[Cat], wordIndexer: Indexer[Word]): Prod = p match {
    case BinaryProdI(l, r) => BinaryProd(catIndexer.obj(l), catIndexer.obj(r))
    case UnaryProdI(s) => UnaryProd(catIndexer.obj(s))
    case TermProdI(w) => TermProd(wordIndexer.obj(w))
  }
}

