package dhg.ccg.cat

import dhg.ccg.tagdict.StartEndTags

object `package` {

  val NonRemovingCcgBankCatInterner = new StandardCatInterner(CatInterner.CcgBankPuncRe, CatInterner.CcgBankAtomRe, removeFeatures = false)
  implicit class CatInternerStringInterpolationHelper(val sc: StringContext) extends AnyVal {
    def cat(args: Any*): Cat = NonRemovingCcgBankCatInterner.fromString(sc.standardInterpolator(identity, args))
  }

  val FeatRemovingCcgBankCatInterner = new SeparateCacheCatInterner(
    new StandardCatInterner(CatInterner.CcgBankPuncRe, CatInterner.CcgBankAtomRe, removeFeatures = true),
    NonRemovingCcgBankCatInterner)
  val TutCatInterner = new SeparateCacheCatInterner(
    new StandardCatInterner(CatInterner.CcgBankPuncRe, CatInterner.TutBankAtomRe, removeFeatures = false, uppercaseAtoms = true),
    NonRemovingCcgBankCatInterner)

  implicit object CatStartEndTags extends StartEndTags[Cat] {
    def startTag = StartCat
    def endTag = EndCat
  }

  implicit object CatOrdering extends Ordering[Cat] {
    def compare(a: Cat, b: Cat): Int = (a, b) match {
      case (StartCat, _) => -1
      case (_, StartCat) => 1
      case (EndCat, _) => -1
      case (_, EndCat) => 1
      case (BadCat, _) => -1
      case (_, BadCat) => 1
      case _ if (a.size < b.size) => -1
      case _ if (a.size > b.size) => 1
      case (a: PuncCat, b: PuncCat) => if (a.punc < a.punc) -1 else 1
      case (a: PuncCat, _) => -1
      case (_, b: PuncCat) => 1
      case (AtomCat(x, xf, xi), AtomCat(y, yf, yi)) => Ordering[(String, Option[String], Option[Int])].compare((x, xf, xi), (y, yf, yi))
      case (a: FCat, b: BCat) => -1
      case (a: BCat, b: FCat) => 1
      case (w || x, y || z) => Ordering[(Cat, Cat)].compare((w, x), (y, z))
    }
  }

}
