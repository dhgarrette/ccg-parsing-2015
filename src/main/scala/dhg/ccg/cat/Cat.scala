package dhg.ccg.cat

sealed trait Cat extends Serializable {
  def size: Int
  def complexity: Int
  def u(o: Cat): Boolean
  def isModifier: Boolean
  def noFeat: Cat
  def noIndices: Cat
  //override final lazy val hashCode: Int = toString.hashCode
}

final class PuncCat private[cat] (private val in: PuncCat.PuncCatInner, private val ci: CatInterner) extends Cat {
  def punc = in.punc
  def size = 1
  def complexity = 1
  def u(o: Cat): Boolean = (this eq o) || (o match {
    case o: PuncCat => (in.punc == o.in.punc)
    case _ => false
  })
  def isModifier = false
  def noFeat = this
  def noIndices = this
  override def equals(o: Any) = o match {
    case o: PuncCat =>
      if (ci eq o.ci)
        (this eq o)
      else
        (in == o.in)
    case _ => false
  }
  override def hashCode() = in.hashCode
  override val toString = in.punc //+ f"=${Integer.toHexString(System.identityHashCode(this))}"
}
object PuncCat {
  private[cat] case class PuncCatInner(punc: String)
  def apply(punc: String)(implicit ci: CatInterner): PuncCat = ci.cachedVersion(new PuncCat(PuncCatInner(punc), JunkCatInterner)).asInstanceOf[PuncCat]
  def unapply(a: PuncCat) = Some(a.in.punc)
}

sealed trait NonPuncCat extends Cat {
  def /(b: NonPuncCat): FCat
  def \(b: NonPuncCat): BCat
  def index: Option[Int]
  def noFeat: NonPuncCat
  def noIndices: NonPuncCat
  //override final lazy val hashCode: Int = toString.hashCode
}

object / { def unapply(c: NonPuncCat) = c match { case FCat(a, b, _) => Some((a, b)); case _ => None } }
object \ { def unapply(c: NonPuncCat) = c match { case BCat(a, b, _) => Some((a, b)); case _ => None } }
object || { def unapply(c: NonPuncCat) = c match { case a / b => Some((a, b)); case a \ b => Some((a, b)); case _ => None } }

final class AtomCat private[cat] (private val in: AtomCat.AtomCatInner, private val ci: CatInterner) extends NonPuncCat {
  def atom = in.atom
  def feature = in.feature
  def index = in.index
  def size = 1
  def complexity = 1
  def /(b: NonPuncCat) = FCat(this, b)(ci)
  def \(b: NonPuncCat) = BCat(this, b)(ci)
  def u(o: Cat): Boolean = (this eq o) || (o match {
    case o: AtomCat => (in.atom == o.in.atom) && (in.feature == o.in.feature || in.feature.isEmpty || o.in.feature.isEmpty)
    case _ => false
  })
  def isModifier = false
  def apply(f: String) = { assert(in.feature.isEmpty); AtomCat(in.atom, Some(f))(ci) }
  def noFeat = if (in.feature.isEmpty) this else AtomCat(in.atom, None, in.index)(ci)
  def noIndices = if (in.index.isEmpty) this else AtomCat(in.atom, in.feature, None)(ci)
  override def equals(o: Any) = o match {
    case o: AtomCat =>
      if (ci eq o.ci)
        (this eq o)
      else
        (in == o.in);
    case _ =>
      false
  }
  override def hashCode() = in.hashCode
  override val toString = in.atom + in.feature.fold("")(f => "[" + f + "]") + in.index.fold("")(i => f"_$i")
}
object AtomCat {
  private[cat] case class AtomCatInner(atom: String, feature: Option[String], index: Option[Int])
  def apply(atom: String, feature: Option[String] = None, index: Option[Int] = None)(implicit ci: CatInterner): AtomCat = ci.cachedVersion(new AtomCat(AtomCatInner(atom, feature, index), JunkCatInterner)).asInstanceOf[AtomCat]
  def unapply(a: AtomCat) = Some((a.in.atom, a.in.feature, a.in.index))
}

trait ComplexCat extends NonPuncCat

final class FCat private[cat] (private val in: FCat.FCatInner, private val ci: CatInterner) extends ComplexCat {
  def left = in.left
  def right = in.right
  def index = in.index
  val size = in.left.size + in.right.size
  def complexity = 1 + in.left.complexity + in.right.complexity
  def /(b: NonPuncCat) = FCat(this, b)(ci)
  def \(b: NonPuncCat) = BCat(this, b)(ci)
  def u(o: Cat): Boolean = (this eq o) || (o match {
    case o: FCat => (in.left u o.in.left) && (in.right u o.in.right)
    case _ => false
  })
  def isModifier = in.left == in.right
  def noFeat = FCat(in.left.noFeat, in.right.noFeat)(ci)
  def noIndices = FCat(in.left.noIndices, in.right.noIndices)(ci)
  override def equals(o: Any) = o match {
    case o: FCat =>
      if (ci eq o.ci)
        (this eq o)
      else
        (in == o.in);
    case _ => false
  }
  override def hashCode() = in.hashCode
  override final val toString = "(" + in.left + """/""" + in.right + ")" + in.index.fold("")(i => f"_$i")
}
object FCat {
  private[cat] case class FCatInner(left: NonPuncCat, right: NonPuncCat, index: Option[Int])
  def apply(left: NonPuncCat, right: NonPuncCat, index: Option[Int] = None)(implicit ci: CatInterner): FCat = ci.cachedVersion(new FCat(FCatInner(left, right, index), JunkCatInterner)).asInstanceOf[FCat]
  def unapply(a: FCat) = Some((a.in.left, a.in.right, a.in.index))
}

final class BCat private[cat] (private val in: BCat.BCatInner, private val ci: CatInterner) extends ComplexCat {
  def left = in.left
  def right = in.right
  def index = in.index
  val size = in.left.size + in.right.size
  def complexity = 1 + in.left.complexity + in.right.complexity
  def /(b: NonPuncCat) = FCat(this, b)(ci)
  def \(b: NonPuncCat) = BCat(this, b)(ci)
  def u(o: Cat): Boolean = (this eq o) || (o match {
    case o: BCat => (in.left u o.in.left) && (in.right u o.in.right)
    case _ => false
  })
  def isModifier = in.left == in.right
  def noFeat = BCat(in.left.noFeat, in.right.noFeat)(ci)
  def noIndices = BCat(in.left.noIndices, in.right.noIndices)(ci)
  override def equals(o: Any) = o match {
    case o: BCat =>
      if (ci eq o.ci)
        (this eq o)
      else
        (in == o.in);
    case _ => false
  }
  override def hashCode() = in.hashCode
  override final val toString = "(" + in.left + """\""" + in.right + ")" + in.index.fold("")(i => f"_$i")
}
object BCat {
  private[cat] case class BCatInner(left: NonPuncCat, right: NonPuncCat, index: Option[Int])
  def apply(left: NonPuncCat, right: NonPuncCat, index: Option[Int] = None)(implicit ci: CatInterner): BCat = ci.cachedVersion(new BCat(BCatInner(left, right, index), JunkCatInterner)).asInstanceOf[BCat]
  def unapply(a: BCat) = Some((a.in.left, a.in.right, a.in.index))
}

// TODO: Remove?
final class ConjCat private[cat] (private val in: ConjCat.ConjCatInner, private val ci: CatInterner) extends NonPuncCat {
  def c = in.c
  def index = ???
  def size: Int = 10000 + in.c.size
  def complexity: Int = 10000
  def /(b: NonPuncCat) = FCat(this, b)(ci)
  def \(b: NonPuncCat) = BCat(this, b)(ci)
  def u(o: Cat): Boolean = (this eq o) || (o match {
    case o: ConjCat => (in.c u o.in.c)
    case _ => false
  })
  override def isModifier = false
  def noFeat = ConjCat(in.c.noFeat, in.c.index)(ci)
  def noIndices = ConjCat(in.c, None)(ci)
  override def equals(o: Any) = o match {
    case o: ConjCat =>
      if (ci eq o.ci)
        (this eq o)
      else
        (in == o.in);
    case _ => false
  }
  override def hashCode() = in.hashCode
  override final val toString = in.c + "[conj]" + in.index.fold("")(i => f"_$i")
}
object ConjCat {
  private[cat] case class ConjCatInner(c: NonPuncCat, index: Option[Int])
  def apply(c: NonPuncCat, index: Option[Int] = None)(implicit ci: CatInterner): ConjCat = ci.cachedVersion(new ConjCat(ConjCatInner(c, index), JunkCatInterner)).asInstanceOf[ConjCat]
  def unapply(a: ConjCat) = Some((a.in.c, a.in.index))
}

object Cat {
  def startCat(implicit ci: CatInterner) = AtomCat("<S>")
  def endCat(implicit ci: CatInterner) = AtomCat("<E>")
}

object TopCat extends Cat {
  def size: Int = 1
  def complexity: Int = 1
  def u(o: Cat): Boolean = this eq o
  def isModifier: Boolean = false
  def noFeat = this
  def noIndices = this
  //override final lazy val hashCode: Int = toString.hashCode
  override val toString = "TOP"
}

object StartCat extends Cat {
  def size: Int = 1
  def complexity: Int = 1
  def u(o: Cat): Boolean = this eq o
  def isModifier: Boolean = false
  def noFeat = this
  def noIndices = this
  //override final lazy val hashCode: Int = toString.hashCode
  override val toString = "<S>"
}

object EndCat extends Cat {
  def size: Int = 1
  def complexity: Int = 1
  def u(o: Cat): Boolean = this eq o
  def isModifier: Boolean = false
  def noFeat = this
  def noIndices = this
  //override final lazy val hashCode: Int = toString.hashCode
  override val toString = "<E>"
}

object DeleteFromLeftCat extends Cat {
  def size: Int = 1
  def complexity: Int = 1
  def u(o: Cat): Boolean = this eq o
  def isModifier: Boolean = false
  def noFeat = this
  def noIndices = this
  //override final lazy val hashCode: Int = toString.hashCode
  override val toString = "<DFL>"
}

object DeleteFromRightCat extends Cat {
  def size: Int = 1
  def complexity: Int = 1
  def u(o: Cat): Boolean = this eq o
  def isModifier: Boolean = false
  def noFeat = this
  def noIndices = this
  //override final lazy val hashCode: Int = toString.hashCode
  override val toString = "<DFR>"
}

object BadCat extends Cat {
  def /(b: Cat) = ???
  def \(b: Cat) = ???
  def size: Int = ???
  def complexity: Int = ???
  def u(b: Cat): Boolean = ???
  def isModifier: Boolean = ???
  def noFeat: Cat = ???
  def noIndices = ???
  override def toString = ""
}

