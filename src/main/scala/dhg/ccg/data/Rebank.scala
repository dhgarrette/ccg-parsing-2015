package dhg.ccg.data

import dhg.util._
import dhg.ccg.parse._
import dhg.ccg.data._
import dhg.ccg.cat._
import scalaz.{ Tree => _, \/ => _, _ }
import Scalaz._
import scala.annotation.tailrec
import dhg.ccg.data.Rebank._

trait Rebanker extends (CcgTree => CcgTree) {
  def apply(t: CcgTree): CcgTree
}

class SimpleRebanker(rebankRules: Vector[RebankRule]) extends Rebanker {
  def apply(t: CcgTree): CcgTree = {
    val (u, uc) =
      rebankRules.foldLeft((t, false)) {
        case ((u, false), rule) => repeatedlyApplyRebankRule(u, rule)
        case ((u, true), _) => (u, true)
      }
    if (uc) apply(u)
    else u
  }

  private[this] def repeatedlyApplyRebankRule(t: CcgTree, rule: RebankRule): (CcgTree, Boolean) = {
    @tailrec def inner(t: CcgTree, rule: RebankRule): CcgTree = {
      val (u, uc) = rebankRecursively(t, rule)
      if (uc) inner(u, rule)
      else u
    }
    val (u, uc) = rebankRecursively(t, rule)
    if (uc) inner(u, rule) -> true
    else u -> false
  }

  private[this] def rebankRecursively(t: CcgTree, rule: RebankRule): (CcgTree, Boolean) = {
    val (u, uc) =
      t match {
        case CcgBinode(cat, left, right) =>
          val (l, lc) = rebankRecursively(left, rule)
          val (r, rc) = rebankRecursively(right, rule)
          CcgBinode(cat, l, r) -> (lc || rc)
        case CcgUnode(cat, sub) =>
          val (s, sc) = rebankRecursively(sub, rule)
          CcgUnode(cat, s) -> sc
        case CcgLeaf(cat, word, _) => t -> false
      }
    if (uc) u -> true
    else {
      rule(u) match {
        case Some(v) => v -> true
        case None => u -> false
      }
    }
  }
}

//
//

sealed trait RebankRule { // extends (CcgTree => Option[CcgTree]) {
  final def apply(t: CcgTree): Option[CcgTree] = this.unapply(t)
  def unapply(t: CcgTree): Option[CcgTree]
}

case object SelfUnary extends RebankRule {
  def unapply(t: CcgTree): Option[CcgTree] = t match {
    case CcgUnode(x1, sub @ CcgTree(x2)) if (x1 == x2) => Some(sub)
    case _ => None
  }
}

case object ConjSimple extends RebankRule {
  val Punct = Set(",", ";", ":", ".", "LRB", "RRB", "LQU", "RQU")
  val Conj = cat"conj"
  val ConjTags = Set("conj") | Punct

  def unapply(t: CcgTree): Option[CcgTree] = t match {

    /**
     *      X(nc)                        X
     *     /  \                         / \
     *   Y    Z1[conj]                Y    z\z
     *       /   \                        /   \
     *      ,     Z2[conj]       (z\z)/(z\z)  z\z
     *           /   \                        /   \
     *          ,     Z3[conj]       (z\z)/(z\z)  z\z
     *               /   \                       /   \
     *            conj    W(nc)             (z\z)/z   W
     */
    case CcgBinode(x,
      left @ CcgTree(y),
      CcgBinode(ConjCat(z1 @ (z, zi)),
        conj1 @ CcgTree(AtomCat(conjAtom1, None, None)),
        CcgBinode(ConjCat(z2 @ (_, _)),
          conj2 @ CcgTree(AtomCat(conjAtom2, None, None)),
          CcgBinode(ConjCat(z3 @ (_, _)),
            conj3 @ CcgTree(AtomCat(conjAtom3, None, None)),
            right @ CcgTree(w))))) //
            if (z1 == z2) && (z2 == z3) &&
      !x.isInstanceOf[ConjCat] && !w.isInstanceOf[ConjCat] &&
      ConjTags(conjAtom1) &&
      ConjTags(conjAtom2) &&
      ConjTags(conjAtom3) =>
      Some(
        CcgBinode(x,
          left,
          CcgBinode(z \ z,
            withCat(conj1, (z \ z) / (z \ z)),
            CcgBinode(z \ z,
              withCat(conj2, (z \ z) / (z \ z)),
              CcgBinode(z \ z,
                withCat(conj3, (z \ z) / z),
                right)))))

    /**
     *      X(nc)                        X
     *     /  \                         / \
     *   Y    Z1[conj]                Y    z\z
     *       /   \                        /   \
     *      ,     Z2[conj]       (z\z)/(z\z)  z\z
     *           /   \                       /   \
     *        conj    W(nc)             (z\z)/z   W
     */
    case CcgBinode(x,
      left @ CcgTree(y),
      CcgBinode(ConjCat(z1 @ (z, zi)),
        conj1 @ CcgTree(AtomCat(conjAtom1, None, None)),
        CcgBinode(ConjCat(z2 @ (_, _)),
          conj2 @ CcgTree(AtomCat(conjAtom2, None, None)),
          right @ CcgTree(w)))) //
          if (z1 == z2) &&
      !x.isInstanceOf[ConjCat] && !w.isInstanceOf[ConjCat] &&
      ConjTags(conjAtom1) &&
      ConjTags(conjAtom2) =>
      Some(
        CcgBinode(x,
          left,
          CcgBinode(z \ z,
            withCat(conj1, (z \ z) / (z \ z)),
            CcgBinode(z \ z,
              withCat(conj2, (z \ z) / z),
              right))))

    /**
     *       X(non-conj)                          X
     *     /   \                                /   \
     *   Y    Z[conj]                        Y      Z\Z
     *         /   \                               /   \
     *      conj    W(non-conj)                (Z\Z)/Z   W
     */
    case CcgBinode(x,
      left @ CcgTree(y),
      CcgBinode(ConjCat(z, i),
        conj @ CcgTree(AtomCat(conjAtom, None, None)),
        right @ CcgTree(w))) //
        if !x.isInstanceOf[ConjCat] && !w.isInstanceOf[ConjCat] &&
      ConjTags(conjAtom) =>
      Some(
        CcgBinode(x,
          left,
          CcgBinode(z \ z,
            withCat(conj, (z \ z) / z),
            right)))

    //    /**
    //     * Chinese CcgBank
    //     *
    //     *       X(non-conj)                          X
    //     *     /   \                                /   \
    //     *   Y    Z[conj]                        Y      X\Y
    //     *         /   \                               /   \
    //     *      Z/Z    W(non-conj)                (X\Y)/W   W
    //     */
    //    case CcgBinode(x,
    //      left @ CcgTree(y),
    //      CcgBinode(ConjCat(z),
    //        conj @ CcgTree(z2 / w2),
    //        right @ CcgTree(w))) //
    //        if !x.isInstanceOf[ConjCat] && !w.isInstanceOf[ConjCat] &&
    //      (z u w2) && (z u z2) && (w u w2) =>
    //      Some(
    //        CcgBinode(x,
    //          left,
    //          CcgBinode(x \ y,
    //            withCat(conj, (x \ y) / w),
    //            right)))

    /**
     * Chinese CcgBank
     *
     *       X(non-conj)                          X
     *     /   \                                /   \
     *   Y    Z[conj]                        Y      X\Y
     *          ...                                 ...
     */
    case CcgBinode(x,
      left @ CcgTree(y),
      right @ CcgTree(ConjCat(z, i))) //
      if !x.isInstanceOf[ConjCat] && !y.isInstanceOf[ConjCat] && x.isInstanceOf[NonPuncCat] && y.isInstanceOf[NonPuncCat] =>
      Some(CcgBinode(x, left, withCat(right, x.asInstanceOf[NonPuncCat] \ y.asInstanceOf[NonPuncCat])))

    /**
     * Chinese CcgBank
     *
     *       X[conj]                         X
     *     /   \                           /   \
     *   ,      Y                        X/Y    Y
     *         ...                             ...
     */
    case CcgBinode(ConjCat(x, i),
      left @ CcgTree(AtomCat(conj, None, None)),
      right @ CcgTree(y)) //
      if ConjTags(conj) && !y.isInstanceOf[ConjCat] && x.isInstanceOf[NonPuncCat] && y.isInstanceOf[NonPuncCat] =>
      Some(CcgBinode(x, withCat(left, x.asInstanceOf[NonPuncCat] / y.asInstanceOf[NonPuncCat]), right))

    /**
     * Chinese CcgBank
     *
     *       X(non-conj)                          X
     *     /   \                                /   \
     *   Y    Z[conj]                        Y      X\Y
     *           |
     *          X\Y
     */
    case CcgBinode(x,
      left @ CcgTree(y),
      CcgUnode(ConjCat(z, i),
        right @ CcgTree(x2 \ y2))) //
        if !x.isInstanceOf[ConjCat] &&
      (x u x2) && (y u y2) =>
      Some(CcgBinode(x, left, right))

    /**
     *                                                 e.g.       N
     *                                                          /   \
     *        Z(non-conj)        =>            Z              N/N     N
     *         /   \                         /   \                  /   \
     *      conj    W(non-conj)            Z/W    W             conj    N
     *                                                                 /   \
     *                                                               N/N     N
     */
    case CcgBinode(z,
      conj @ CcgTree(Conj),
      right @ CcgTree(w)) //
      if !z.isInstanceOf[ConjCat] && !w.isInstanceOf[ConjCat] &&
      !hasConj(z) && !hasConj(w) &&
      z.isInstanceOf[NonPuncCat] && w.isInstanceOf[NonPuncCat] =>
      Some(CcgBinode(z, withCat(conj, z.asInstanceOf[NonPuncCat] / w.asInstanceOf[NonPuncCat]), right))

    /**
     *                                                     e.g.        x
     *                                                               /   \
     *     (non-conj)Z              =>       Z                      x     y
     *             /   \                   /   \                  /   \  when
     *  (non-conj)W    conj               W    Z\W               x    conj
     *                                                           if   and
     */
    case CcgBinode(z,
      left @ CcgTree(w),
      conj @ CcgTree(Conj)) //
      if !z.isInstanceOf[ConjCat] && !w.isInstanceOf[ConjCat] &&
      !hasConj(z) && !hasConj(w) &&
      z.isInstanceOf[NonPuncCat] && w.isInstanceOf[NonPuncCat] =>
      Some(CcgBinode(z, left, withCat(conj, z.asInstanceOf[NonPuncCat] \ w.asInstanceOf[NonPuncCat])))

    //    /**
    //     *       X(non-conj)                    X          e.g.       N
    //     *     /   \                          /   \                 /   \
    //     *   Y    Z(non-conj)        =>     Y      Z              N/N     N
    //     *         /   \                         /   \                  /   \
    //     *      conj    W(non-conj)            Z/W   W               conj    N
    //     *                                                                 /   \
    //     *                                                               N/N     N
    //     */
    //    case CcgBinode(x,
    //      left @ CcgTree(y),
    //      CcgBinode(z,
    //        conj @ CcgTree(Conj),
    //        right @ CcgTree(w))) //
    //        if !x.isInstanceOf[ConjCat] && !z.isInstanceOf[ConjCat] && !w.isInstanceOf[ConjCat] =>
    //      Some(
    //        CcgBinode(x,
    //          left,
    //          CcgBinode(z,
    //            withCat(conj, z / w),
    //            right)))
    //
    //    /**
    //     *        (non-conj)X                       X          e.g.        x
    //     *                /   \                   /   \                  /   \
    //     *     (non-conj)Z     Y        =>       Z      Y               x     y
    //     *             /   \                   /   \                  /   \  when
    //     *  (non-conj)W    conj               W    Z\W               x    conj
    //     *                                                           if   and
    //     */
    //    case CcgBinode(x,
    //      CcgBinode(z,
    //        left @ CcgTree(w),
    //        conj @ CcgTree(Conj)),
    //      right @ CcgTree(y)) //
    //      if !x.isInstanceOf[ConjCat] && !z.isInstanceOf[ConjCat] && !w.isInstanceOf[ConjCat] =>
    //      Some(
    //        CcgBinode(x,
    //          CcgBinode(z,
    //            left,
    //            withCat(conj, z \ w)),
    //          right))
    //
    //    /**
    //     *        (non-conj)X                       X
    //     *                /   \                   /   \
    //     *     (non-conj)Z     Y        =>       Z      Y
    //     *             /   \                   /   \
    //     *          conj    W(non-conj)      Z/W    W
    //     */
    //    case CcgBinode(x,
    //      CcgBinode(z,
    //        conj @ CcgTree(Conj),
    //        left @ CcgTree(w)),
    //      right @ CcgTree(y)) //
    //      if !x.isInstanceOf[ConjCat] && !z.isInstanceOf[ConjCat] && !w.isInstanceOf[ConjCat] =>
    //      Some(
    //        CcgBinode(x,
    //          CcgBinode(z,
    //            withCat(conj, z / w),
    //            left),
    //          right))
    //
    //    /**
    //     *             X(non-conj)                  X
    //     *           /   \                        /   \
    //     *          Y    Z(non-conj)      =>     Y     Z
    //     *             /   \                         /   \
    //     *  (non-conj)W    conj                     W    Z\W
    //     */
    //    case CcgBinode(x,
    //      left @ CcgTree(y),
    //      CcgBinode(z,
    //        conj @ CcgTree(Conj),
    //        right @ CcgTree(w))) //
    //        if !x.isInstanceOf[ConjCat] && !z.isInstanceOf[ConjCat] && !w.isInstanceOf[ConjCat] =>
    //      Some(
    //        CcgBinode(x,
    //          left,
    //          CcgBinode(z,
    //            right,
    //            withCat(conj, z \ w))))

    //    /**
    //     *            X                   X
    //     *          /   \        =>     /   \
    //     *   conj/conj   conj         X/X    X
    //     */
    //    case CcgBinode(x,
    //      left @ CcgTree(Conj / Conj),
    //      right @ CcgTree(Conj)) //
    //      if x != Conj &&
    //      !x.isInstanceOf[ConjCat] =>
    //      Some(CcgBinode(x, withCat(left, x / x), withCat(right, x)))
    //
    //    /**
    //     *        X                     X
    //     *      /   \          =>     /   \
    //     *   conj  conj\conj         X    X\X
    //     */
    //    case CcgBinode(x,
    //      left @ CcgTree(Conj),
    //      right @ CcgTree(Conj \ Conj)) //
    //      if x != Conj &&
    //      !x.isInstanceOf[ConjCat] =>
    //      Some(CcgBinode(x, withCat(left, x), withCat(right, x \ x)))

    /**
     *        X                     X
     *      /   \          =>     /   \
     *   conj  conj\conj         X    X\X
     */
    case CcgBinode(x,
      left @ CcgTree(c1),
      right @ CcgTree(c2)) //
      if x != Conj && !x.isInstanceOf[ConjCat] &&
      isAllConj(c1) && isAllConj(c2) =>
      Some(CcgBinode(x, frConj(left, x), frConj(right, x)))

    case _ => None
  }

  private[this] def frConj(t: CcgTree, use: Cat): CcgTree = t match {
    case CcgBinode(cat, left, right) =>
      if (isAllConj(cat)) CcgBinode(replaceConj(cat, use), frConj(left, use), frConj(right, use))
      else t
    case CcgUnode(cat, sub) =>
      if (isAllConj(cat)) CcgUnode(replaceConj(cat, use), frConj(sub, use))
      else t
    case CcgLeaf(cat, word, pos) =>
      if (isAllConj(cat)) CcgLeaf(replaceConj(cat, use), word, pos)
      else t
  }
  private[this] def isAllConj(cat: Cat): Boolean = cat match {
    case c: ConjCat => false
    case c: PuncCat => false
    case a: AtomCat => a == Conj
    case l / r => isAllConj(l) && isAllConj(r)
    case l \ r => isAllConj(l) && isAllConj(r)
  }
  private[this] def hasConj(cat: Cat): Boolean = cat match {
    case ConjCat(c, _) => hasConj(c)
    case c: PuncCat => false
    case a: AtomCat => a == Conj
    case l / r => hasConj(l) || hasConj(r)
    case l \ r => hasConj(l) || hasConj(r)
  }
  private[this] def replaceConj(cat: Cat, use: Cat): Cat = cat match {
    case ConjCat(c, _) => replaceConj(c, use)
    case Conj => use
    case l / r => replaceConj(l, use).asInstanceOf[NonPuncCat] / replaceConj(r, use).asInstanceOf[NonPuncCat]
    case l \ r => replaceConj(l, use).asInstanceOf[NonPuncCat] \ replaceConj(r, use).asInstanceOf[NonPuncCat]
  }
}

//case object PunctMod extends RebankRule {
//  def unapply(t: CcgTree): Option[CcgTree] = t match {
//    //    /**
//    //     *      X(non-conj)    X
//    //     *    /   \          /   \
//    //     *   X   PUNCT      X    X\X
//    //     */
//    //    case CcgBinode(x1,
//    //      left @ CcgTree(x2),
//    //      right @ CcgLeaf(AtomCat(a, None), _)) //
//    //      if (x1 u x2) && Punct(a)
//    //      && !x1.isInstanceOf[ConjCat] =>
//    //      Some(CcgBinode(x1, left, withCat(right, x1 \ x2)))
//    //
//    //    /**
//    //     *      X(non-conj)    X
//    //     *    /   \          /   \
//    //     * PUNCT   X       X/X    X
//    //     */
//    //    case CcgBinode(x1,
//    //      left @ CcgLeaf(AtomCat(a, None), _),
//    //      right @ CcgTree(x2)) //
//    //      if (x1 u x2) && Punct(a)
//    //      && !x1.isInstanceOf[ConjCat] =>
//    //      Some(CcgBinode(x1, withCat(left, x1 / x2), right))
//
//    /**
//     * Weird ones:
//     *   241:  ,    NP    ->    ((S\NP)\(S\NP))
//     *   188:  (S[dcl]/S[dcl])    ,    ->    ((S\NP)/(S\NP))
//     *   92:   (S[dcl]/S[dcl])    ,    ->    ((S\NP)\(S\NP))
//     *   61:   NP    ,    ->    (S/S)
//     *   59:   (S[dcl]/S[dcl])    ,    ->    (S\S)
//     *   40:   (S[dcl]\S[dcl])    ,    ->    (S/S)
//     *   20:   S[dcl]    ,    ->    (S/S)
//     *   18:   (S[dcl]\S[dcl])    ,    ->    ((S\NP)/(S\NP))
//     *   17:   (S[dcl]/S[dcl])    ,    ->    (NP\NP)
//     *   12:   (S[dcl]\S[dcl])    ,    ->    ((S\NP)\(S\NP))
//     *   ...
//     */
//    case CcgBinode(x1,
//      left @ CcgTree(x2),
//      right @ CcgLeaf(AtomCat(a, None), _)) //
//      if Punct(a)
//      && !x1.isInstanceOf[ConjCat] =>
//      Some(CcgBinode(x1, left, withCat(right, x1 \ x2)))
//    case CcgBinode(x1,
//      left @ CcgLeaf(AtomCat(a, None), _),
//      right @ CcgTree(x2)) //
//      if Punct(a)
//      && !x1.isInstanceOf[ConjCat] =>
//      Some(CcgBinode(x1, withCat(left, x1 / x2), right))
//
//    case _ => None
//  }
//}
//
//case object NegContraction2Mod extends RebankRule {
//  /*
//   *  ((S[dcl]\NP)/(S[b  ]\NP))    ((S\NP)\(S\NP))    ->    ((S[dcl]\NP)/(S[b  ]\NP))
//   *  ((S[dcl]\NP)/(S[pss]\NP))    ((S\NP)\(S\NP))    ->    ((S[dcl]\NP)/(S[pss]\NP))
//   *                                     n't
//   */
//  private[this] val S = AtomCat("S")
//  private[this] val NP = AtomCat("NP")
//  private[this] val SdclNP = S("dcl") \ NP
//  private[this] val SNP = S \ NP
//  def unapply(t: CcgTree): Option[CcgTree] = t match {
//    case CcgBinode(x @ (SdclNP / (AtomCat("S", f1) \ NP)),
//      left @ CcgTree(y @ (SdclNP / (AtomCat("S", f2) \ NP))),
//      right @ CcgTree(SNP \ SNP)) //
//      if (f1 == f2) =>
//      Some(CcgBinode(x, left, withCat(right, x \ y)))
//    case _ => None
//  }
//}

object Rebank {
  def withCat(t: CcgTree, c: Cat): CcgTree = t match {
    case CcgBinode(_, left, right) => CcgBinode(c, left, right)
    case CcgUnode(_, sub) => CcgUnode(c, sub)
    case CcgLeaf(_, word, pos) => CcgLeaf(c, word, pos)
  }

  object NorNP { def unapply(c: Cat): Option[Option[String]] = c match { case AtomCat(x, f, _) if Set("NP", "N")(x) => Some(f); case _ => None } }
}

object RebankRules {
  val standard: Vector[RebankRule] = Vector(SelfUnary, ConjSimple)
}
