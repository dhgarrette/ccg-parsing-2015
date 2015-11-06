package dhg.ccg.rule

import dhg.util._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import scalaz._
import Scalaz._
import dhg.ccg.tagdict.TagDictionary

trait CcgRule {
  def isBinary: Boolean
  def isUnary: Boolean
}

trait BinaryCcgRule extends CcgRule {
  def apply(left: Cat, right: Cat): Option[Cat]
  def inferRight(parent: Cat, left: Cat): Option[Cat]
  def inferLeft(parent: Cat, right: Cat): Option[Cat]
  final val isBinary = true
  final val isUnary = false
}

trait UnaryCcgRule extends CcgRule {
  def apply(sub: Cat): Option[Cat] = (sub == child).option(parent)
  def child: Cat
  def parent: Cat
  //def inferSub(parent: Cat): Option[Cat]
  final val isBinary = false
  final val isUnary = true
}

/**
 * FA,BA,
 * FC,BX,
 * GFC,GBX
 *
 * We place the following constraint on backward crossed
 * composition (for all models): the Y category in
 *     Y/Z  X\Y  =>Bx  X/Z
 * cannot be an N or NP category. We
 * also place a similar constraint on backward composition. Both constraints reduce the
 * size of the charts considerably with no impact on coverage or accuracy
 *
 * combinator for removing any punctuation to the right, and
 *        one for removing open-brackets and open-quotes to the left
 *   --> We also remove LHS comma and semicolon
 *
 * --> Use  "X X => X" merging rule
 *
 * the [nb] feature is ignored
 */
object CcgRules {
  val nonComp = Vector[CcgRule](
    // Full English CcgBank: parseable=85.62  parseableLen=22.57  allLen=23.50
    // Full Chinese CcgBank: parseable=49.69  parseableLen=21.25  allLen=27.99
    // Full Italian CcgBank: parseable=70.49  parseableLen=20.88  allLen=22.91

    // With both FC2+BX2 ablated:
    //      English CcgBank: parseable=85.15  -0.47
    //      Chinese CcgBank: parseable=49.44  -0.25
    //      Italian CcgBank: parseable=70.49  -0.00

    FA,
    BA,
    //    FC, //  When ablated: 80.35
    //BC,  // When added: 85.72
    //    BX, //  When ablated: 66.52
    //    FC2, // When ablated: 85.59
    //    BX2, // When ablated: 85.18

    N2NP,

    TR1,
    TR2,
    TR3,

    RRC1,
    RRC2,
    RRC3,
    RRC4,
    RRC5,
    RRC6,

    VPSM1,
    VPSM2,
    VPSM3,

    //    PunctRight,
    //    PunctLeft,
    LeftBracketPunctRight,
    RightBracketPunctRight,
    CommaPunctRight,
    SemicolonPunctRight,
    ColonPunctRight,
    FullstopPunctRight,
    LeftBracketPunctLeft,

    Merge)

  val firstOrderComp = Vector[CcgRule](FC, BX)
  val firstOrder = nonComp ++ firstOrderComp
  val secondOrder = Vector[CcgRule](FC2, BX2)
  val all = firstOrder ++ secondOrder
  //val consumer = Vector(ConsumeRight, ConsumeLeft)

}

//
// Binary Rules
//

/**
 *  Forward Application
 *  X/Y  Y  =>  X
 */
case object FA extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat): Option[Cat] = {
    left match {
      case x / y if (y u right) => Some(x)
      case _ => None
    }
  }
  def inferRight(parent: Cat, left: Cat): Option[Cat] = {
    left match {
      case x / y if (x u parent) => Some(y)
      case _ => None
    }
  }
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = {
    (parent, right) match {
      case (p: NonPuncCat, r: NonPuncCat) => Some(p / r)
      case _ => None
    }
  }
}

/**
 *  Forward Harmonic Composition
 *  X/Y  Y/Z  =>  X/Z
 */
case object FC extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat) = {
    (left, right) match {
      case (x / y1, y2 / z) if (y1 u y2) => Some(x / z)
      case _ => None
    }
  }
  def inferRight(parent: Cat, left: Cat): Option[Cat] = {
    (parent, left) match {
      case (x1 / z, x2 / y) if (x1 u x2) => Some(y / z)
      case _ => None
    }
  }
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = {
    (parent, right) match {
      case (x / z1, y / z2) if (z1 u z2) => Some(x / y)
      case _ => None
    }
  }
}

/**
 *  Forward Harmonic Composition 2
 *  X/Y  (Y/Z)|W  =>  (X/Z)|W
 */
case object FC2 extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat) = {
    (left, right) match {
      case (x / y1, (y2 / z) / w) if (y1 u y2) => Some((x / z) / w)
      case (x / y1, (y2 / z) \ w) if (y1 u y2) => Some((x / z) \ w)
      case _ => None
    }
  }
  def inferRight(parent: Cat, left: Cat): Option[Cat] = ???
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = ???
}

/**
 *  Forward Crossed Composition
 *  X/Y  Y\Z  =>  X\Z
 */
case object FX extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat) = {
    (left, right) match {
      case (x / y1, y2 \ z) if y1 u y2 => Some(x \ z)
      case _ => None
    }
  }
  def inferRight(parent: Cat, left: Cat): Option[Cat] = {
    (parent, left) match {
      case (x1 \ z, x2 / y) if (x1 u x2) => Some(y \ z)
      case _ => None
    }
  }
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = {
    (parent, right) match {
      case (x \ z1, y \ z2) if (z1 u z2) => Some(x / y)
      case _ => None
    }
  }
}

/**
 *  Forward Crossed Composition 2
 *  X/Y  (Y\Z)|W  =>  (X\Z)|W
 */
case object FX2 extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat) = {
    (left, right) match {
      case (x / y1, (y2 \ z) / w) if y1 u y2 => Some((x \ z) / w)
      case (x / y1, (y2 \ z) \ w) if y1 u y2 => Some((x \ z) \ w)
      case _ => None
    }
  }
  def inferRight(parent: Cat, left: Cat): Option[Cat] = ???
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = ???
}

//

/**
 *  Backward Application
 *  Y  X\Y  =>  X
 */
case object BA extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat) = {
    right match {
      case x \ y if (left u y) => Some(x)
      case _ => None
    }
  }
  def inferRight(parent: Cat, left: Cat): Option[Cat] = {
    (parent, left) match {
      case (p: NonPuncCat, l: NonPuncCat) => Some(p \ l)
      case _ => None
    }
  }
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = {
    right match {
      case x \ y if (x u parent) => Some(y)
      case _ => None
    }
  }
}

/**
 *  Backward Harmonic Composition
 *  Y\Z  X\Y  =>  X\Z
 *
 * Blocked where Y is an N or NP
 */
case object BC extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat) = {
    (left, right) match {
      case (_, _ \ AtomCat(y, _, _)) if Set("N", "NP")(y) => None
      case (y1 \ z, x \ y2) if (y1 u y2) => Some(x \ z)
      case _ => None
    }
  }
  def inferRight(parent: Cat, left: Cat): Option[Cat] = {
    (parent, left) match {
      case (_, AtomCat(y, _, _) \ _) if Set("N", "NP")(y) => None
      case (x \ z1, y \ z2) if (z1 u z2) => Some(x \ y)
      case _ => None
    }
  }
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = {
    (parent, right) match {
      case (_, _ \ AtomCat(y, _, _)) if Set("N", "NP")(y) => None
      case (x1 \ z, x2 \ y) if (x1 u x2) => Some(y \ z)
      case _ => None
    }
  }
}

/**
 *  Backward Harmonic Composition 2
 *  (Y\Z)|W  X\Y  =>  (X\Z)|W
 *
 * Blocked where Y is an N or NP
 */
case object BC2 extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat) = {
    (left, right) match {
      case (_, x \ AtomCat(y, _, _)) if Set("N", "NP")(y) => None
      case ((y1 \ z) / w, x \ y2) if (y1 u y2) => Some((x \ z) / w)
      case ((y1 \ z) \ w, x \ y2) if (y1 u y2) => Some((x \ z) \ w)
      case _ => None
    }
  }
  def inferRight(parent: Cat, left: Cat): Option[Cat] = ???
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = ???
}

/**
 *  Backward Crossed Composition
 *  Y/Z  X\Y  =>  X/Z
 *
 * Blocked where Y is an N or NP
 */
case object BX extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat) = {
    (left, right) match {
      case (_, _ \ AtomCat(y, _, _)) if Set("N", "NP")(y) => None
      case (y1 / z, x \ y2) if (y1 u y2) => Some(x / z)
      case _ => None
    }
  }
  def inferRight(parent: Cat, left: Cat): Option[Cat] = {
    (parent, left) match {
      case (_, AtomCat(y, _, _) / _) if Set("N", "NP")(y) => None
      case (x / z1, y / z2) if (z1 u z2) => Some(x \ y)
      case _ => None
    }
  }
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = {
    (parent, right) match {
      case (_, _ \ AtomCat(y, _, _)) if Set("N", "NP")(y) => None
      case (x1 / z, x2 \ y) if (x1 u x2) => Some(y / z)
      case _ => None
    }
  }
}

/**
 *  Backward Crossed Composition 2
 *  (Y/Z)|W  X\Y  =>  (X/Z)|W
 *
 * Blocked where Y is an N or NP
 */
case object BX2 extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat) = {
    (left, right) match {
      case (_, _ \ AtomCat(y, _, _)) if Set("N", "NP")(y) => None
      case ((y1 / z) / w, x \ y2) if (y1 u y2) => Some((x / z) / w)
      case ((y1 / z) \ w, x \ y2) if (y1 u y2) => Some((x / z) \ w)
      case _ => None
    }
  }
  def inferRight(parent: Cat, left: Cat): Option[Cat] = ???
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = ???
}

//
//
//

/**
 *  Forward Application: N -> NP unary conversion
 *  X/NP  N  =>  X
 */
case object FAn2np extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat) = {
    (left, right) match {
      case (x / (y @ AtomCat("NP", f1, _)), AtomCat("N", f2, _)) if (f1 == f2 || f1.isEmpty || f2.isEmpty) => Some(x)
      case _ => None
    }
  }
  def inferRight(parent: Cat, left: Cat): Option[Cat] = {
    left match {
      case x / y if (x u parent) => Some(y)
      case _ => None
    }
  }
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = {
    (parent, right) match {
      case (p: NonPuncCat, r: NonPuncCat) => Some(p / r)
      case _ => None
    }
  }
}

/**
 *  Backward Application: N -> NP unary conversion
 *  N  X\NP  =>  X
 */
case object BAn2np extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat) = {
    (left, right) match {
      case (AtomCat("N", f2, _), x \ (y @ AtomCat("NP", f1, _))) if (f1 == f2 || f1.isEmpty || f2.isEmpty) => Some(x)
      case _ => None
    }
  }
  def inferRight(parent: Cat, left: Cat): Option[Cat] = {
    (parent, left) match {
      case (p: NonPuncCat, l: NonPuncCat) => Some(p \ l)
      case _ => None
    }
  }
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = {
    right match {
      case x \ y if (x u parent) => Some(y)
      case _ => None
    }
  }
}

//
// TOP rewrites
//

case object NP2Top extends UnaryCcgRule { val child: Cat = cat"NP"; val parent: Cat = TopCat }
case object S2Top extends UnaryCcgRule { val child: Cat = cat"S"; val parent: Cat = TopCat }

//
// Bare noun phrases
//

/** N => NP */
case object N2NP extends UnaryCcgRule { val child: Cat = cat"N"; val parent: Cat = cat"NP" }

//
// Type raising
//

/** NP => S/(S\NP) */
case object TR1 extends UnaryCcgRule { val child: Cat = cat"NP"; val parent: Cat = cat"""(S/(S\NP))""" }
/** NP => (S\NP)/((S\NP)/NP) */
case object TR2 extends UnaryCcgRule { val child: Cat = cat"NP"; val parent: Cat = cat"""((S\NP)/((S\NP)/NP))""" }
/** NP => (S\NP)/((S\NP)/NP) */
case object TR3 extends UnaryCcgRule { val child: Cat = cat"PP"; val parent: Cat = cat"""((S\NP)/((S\NP)/PP))""" }

//
// Reduced relative clauses 
//

/** Spss\NP => NP\NP */
case object RRC1 extends UnaryCcgRule { val child: Cat = cat"""(S[pss]\NP)"""; val parent: Cat = cat"""(NP\NP)""" }
/** Sng\NP => NP\NP */
case object RRC2 extends UnaryCcgRule { val child: Cat = cat"""(S[ng]\NP)"""; val parent: Cat = cat"""(NP\NP)""" }
/** Sadj\NP => NP\NP */
case object RRC3 extends UnaryCcgRule { val child: Cat = cat"""(S[adj]\NP)"""; val parent: Cat = cat"""(NP\NP)""" }
/** Sto\NP => NP\NP */
case object RRC4 extends UnaryCcgRule { val child: Cat = cat"""(S[to]\NP)"""; val parent: Cat = cat"""(NP\NP)""" }
/** Sto\NP => NP\NP */
case object RRC5 extends UnaryCcgRule { val child: Cat = cat"""(S[to]\NP)"""; val parent: Cat = cat"""(N\N)""" }
/** Sdcl/NP => NP\NP */
case object RRC6 extends UnaryCcgRule { val child: Cat = cat"""(S[dcl]/NP)"""; val parent: Cat = cat"""(NP\NP)""" }

//
// Reduced relative clauses 
//

/** Spss\NP => S/S */
case object VPSM1 extends UnaryCcgRule { val child: Cat = cat"""(S[pss]\NP)"""; val parent: Cat = cat"""(S/S)""" }
/** Sng\NP => S/S */
case object VPSM2 extends UnaryCcgRule { val child: Cat = cat"""(S[ng]\NP)"""; val parent: Cat = cat"""(S/S)""" }
/** Sto\NP => S/S */
case object VPSM3 extends UnaryCcgRule { val child: Cat = cat"""(S[to]\NP)"""; val parent: Cat = cat"""(S/S)""" }

//

/**
 *  Merging rule
 *  X  X  =>  X
 */
case object Merge extends BinaryCcgRule {
  override def apply(left: Cat, right: Cat) = {
    (left == right).option(left)
  }
  def inferRight(parent: Cat, left: Cat): Option[Cat] = (parent == left).option(parent)
  def inferLeft(parent: Cat, right: Cat): Option[Cat] = (parent == right).option(parent)
}

///** Consume anything on the right: X  Y  =>  X */
//case object ConsumeRight extends BinaryCcgRule { override def apply(left: Cat, right: Cat) = Some(left) }
///** Consume anything on the left: Y  X  =>  X */
//case object ConsumeLeft extends BinaryCcgRule { override def apply(left: Cat, right: Cat) = Some(right) }

object CcgRule {

  def allDerivable(rules: Set[CcgRule], known: Set[Cat]): Set[Cat] = {
    val bRules = rules.collect { case r: BinaryCcgRule => r }
    val uRules = rules.collect { case r: UnaryCcgRule => r }

    val newCatsB = for { u <- known; v <- known; r <- bRules; t <- r(u, v) if !known(t) } yield t
    val newCatsU = for { u <- known; /*       */ r <- uRules; t <- r(u) if !known(t) } yield t
    val newCats = newCatsB | newCatsU

    if (newCats.nonEmpty)
      allDerivable(rules, known | newCats)
    else
      known
  }

  def allDerivableProds(rules: Set[CcgRule], tagdict: TagDictionary[Cat]): (Map[Cat, Set[BinaryProd]], Map[Cat, Set[UnaryProd]], Map[Cat, Set[TermProd]]) = {
    val bRules = rules.collect { case r: BinaryCcgRule => r }
    val uRules = rules.collect { case r: UnaryCcgRule => r }
    val allCats = allDerivable(rules, tagdict.allTags)

    val prodsB = for { u <- allCats; v <- allCats; r <- bRules; t <- r(u, v) } yield BinaryProd(u, v)
    val prodsU = for { u <- allCats; /*         */ r <- uRules; t <- r(u) } yield UnaryProd(u)
    val prodsT = for { w <- tagdict.allWords } yield TermProd(w)

    val cacheB = prodsB.mapTo(identity).toMap // object sharing
    val cacheU = prodsU.mapTo(identity).toMap // object sharing
    val cacheT = prodsT.mapTo(identity).toMap // object sharing

    val tprodsB = for { u <- allCats; v <- allCats; r <- bRules; t <- r(u, v) } yield t -> cacheB(BinaryProd(u, v))
    val tprodsU = for { u <- allCats; /*         */ r <- uRules; t <- r(u) } yield t -> cacheU(UnaryProd(u))
    val tprodsT = for { w <- tagdict.allWords; /*         */ t <- tagdict(w) } yield t -> cacheT(TermProd(w))

    (tprodsB.groupByKey, tprodsU.groupByKey, tprodsT.groupByKey)
  }

}

