package dhg.ccg.gen

import scala.util.matching.Regex
import dhg.util._

trait LpFeature

case class TypeLpFeature(word: String) extends LpFeature

case class TokenLpFeature(word: String, sentIdx: Int, tokenIdx: Int) extends LpFeature

case class PrevWordLpFeat(prevWord: Option[String], startWord: String) extends LpFeature {
  override def toString = f"PREV_${prevWord.getOrElse(startWord)}"
}

case class NextWordLpFeat(nextWord: Option[String], endWord: String) extends LpFeature {
  override def toString = f"NEXT_${nextWord.getOrElse(endWord)}"
}

case class PrefixLpFeat(prefix: String, n: Int) extends LpFeature {
  override def toString = f"PREF_${prefix}_${n}"
}

case class SuffixLpFeat(suffix: String, n: Int) extends LpFeature {
  override def toString = f"SUFF_${suffix}_${n}"
}

case class DictposLpFeat(dictpos: String) extends LpFeature {
  override def toString = f"DICT_${dictpos}"
}

case class MorphLpFeat(morph: String) extends LpFeature {
  override def toString = f"MORPH_${morph}"
}
