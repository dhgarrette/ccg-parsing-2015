package dhg.ccg.parse.pcfg

import dhg.ccg.cat.Cat

sealed trait Prod extends Serializable {
  def isNt: Boolean
  def isTerm: Boolean
}

sealed trait NontProd extends Prod {
  final def isNt = true
  final def isTerm = false
}

case class BinaryProd(left: Cat, right: Cat) extends NontProd {
  override def toString: String = f"[$left $right]"
}

case class UnaryProd(sub: Cat) extends NontProd {
  override def toString: String = f"[$sub]"
}

case class TermProd(word: String) extends Prod {
  override def isNt = false
  override def isTerm = true
  override def toString: String = word
}
