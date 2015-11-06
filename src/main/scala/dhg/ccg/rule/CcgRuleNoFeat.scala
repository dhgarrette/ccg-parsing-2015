package dhg.ccg.rule

import dhg.util._
import dhg.ccg.cat._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import scalaz._
import Scalaz._
import dhg.ccg.tagdict.TagDictionary

object CcgRuleNoFeat {

  val binaryRules = Vector[BinaryCcgRule](
    FA,
    BA,
    //    FCnf,
    //    BCnf, 
    //    BXnf,
    //    FC2nf,
    //    BX2nf,

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

  val unaryRules = Vector[UnaryCcgRule](
    N2NP,

    TR1,
    TR2,
    TR3,

    RRC1nf,
    RRC2nf,

    VPSM1nf)

  val nonComp: Vector[CcgRule] = binaryRules ++ unaryRules

  val firstOrderComp = Vector[CcgRule](FC, BX)
  val firstOrder = nonComp ++ firstOrderComp
  val secondOrder = Vector[CcgRule](FC2, BX2)
  val all = firstOrder ++ secondOrder
  //val consumer = Vector(ConsumeRight, ConsumeLeft)

}

//
// Binary Rules
//

//
// Reduced relative clauses 
//

/** Sto\NP => NP\NP */
case object RRC1nf extends UnaryCcgRule { val child: Cat = cat"""(S\NP)"""; val parent: Cat = cat"""(NP\NP)""" }
/** Sto\NP => NP\NP */
case object RRC2nf extends UnaryCcgRule { val child: Cat = cat"""(S\NP)"""; val parent: Cat = cat"""(N\N)""" }

//
// Reduced relative clauses 
//

/** Spss\NP => S/S */
case object VPSM1nf extends UnaryCcgRule { val child: Cat = cat"""(S\NP)"""; val parent: Cat = cat"""(S/S)""" }

