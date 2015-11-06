package dhg.ccg.parse.inf

import scala.annotation.tailrec
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Set => MSet }
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.prob._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.tagdict.TagDictionary
import dhg.util._
import scalaz._
import Scalaz._
import dhg.ccg.parse._
import dhg.ccg.parse.pcfg._
import dhg.ccg.parse.pcfg.mcmc._
import org.apache.commons.math3.random.RandomGenerator
import dhg.ccg.math._
import dhg.ccg.math.Util._
import dhg.ccg.tagdict.StartEndTags
import dhg.ccg.tagdict.SimpleStartEndTags
import scala.collection.parallel.immutable.ParVector
import dhg.ccg.math.DirichletSampler
import dhg.ccg.parse.dep.ParserEvaluator
import dhg.gfl.FudgSentence
import dhg.ccg.math.SimpleDirichletSampler
import breeze.stats.distributions.Rand
import scala.util.control.Breaks._
import org.apache.commons.math3.random.MersenneTwister
import org.apache.commons.math3.random.SynchronizedRandomGenerator
import dhg.util.viz._


/** 
 * Resample using the restricted supertag sets.  Do not use any additional tag adder-ing since:
 *   a) every token has a carefully selected supertag set
 *   b) the existing tree will still be possible, so no new tags are needed to "make it work"
 */
class SupertagsOnlyInfTreeResampler(
  rules: Vector[CcgRule],
  pcfgTreeSampler: PcfgTreeSampler,
  infCatPrior: InfCatPrior,
  rootSet: Set[Cat],
  rand: RandomGenerator)
    extends InfTreeResampler {

  private[this] val gcb = new SimpleCfgGuideChartBuilder(rules, new NoOpAdditionalTagAdder(), rootSet, allowTerminalDeletion = false)

  private[this] val Z: Cat = cat"<Z>"
  private[this] val BiZProd = BinaryProd(Z, Z)
  private[this] val UnZProd = UnaryProd(Z)
  private[this] val TeZProd = TermProd("Z")

  def resampleTrees(
    currentTrees: Vector[CcgTree], // ParVector[CcgTree],    
    knownRootProds: Map[Cat, LogDouble],
    knownBinyProds: Map[Cat, Map[BinaryProd, LogDouble]],
    knownUnryProds: Map[Cat, Map[UnaryProd, LogDouble]],
    rootDist: LogProbabilityDistribution[Cat],
    prodDist: ConditionalLogProbabilityDistribution[Cat, Prod],
    prodMixes: Map[Cat, (LogDouble, LogDouble, LogDouble)], // binyProdMix: LogDouble, unryProdMix: LogDouble, termProdMix: LogDouble, // guaranteed to sum to 1
    infCatPriorsDesc: Vector[(Cat, LogDouble)],
    binyScaleFactors: Map[Cat, LogDouble],
    unryScaleFactors: Map[Cat, LogDouble],
    sentences: Vector[Vector[Word]],
    annotations: Vector[Option[FudgSentence]]): Vector[CcgTree] = {
    val (qTrees, minQs) = currentTrees.map(sampleQs(_, knownBinyProds, knownUnryProds, prodMixes, binyScaleFactors, unryScaleFactors, rand)).unzip
    for ((ws, as, qtree) <- zipSafe(sentences, annotations, qTrees) /*.PARALLEL*/ ) yield {
      val sts = getSupertagSets(qtree, infCatPriorsDesc, knownRootProds, knownBinyProds, knownUnryProds, prodMixes, binyScaleFactors, unryScaleFactors, rand)
      val Some(gc) = gcb.buildFromSupertagSetSentence(ws zipSafe sts, as, null)
      pcfgTreeSampler.sample(gc, rootDist, prodDist)
    }
  }

  /**
   * Traverse the current CcgTree and sample q-values for each node.
   * Return a tree of q-values, as well as the minimum overall q.
   *
   * We need the minimum overall q, but also the maximum overall
   * scaling factor.
   */
  private[this] def sampleQs(ccgTree: CcgTree,
    knownBinyProds: Map[Cat, Map[BinaryProd, LogDouble]],
    knownUnryProds: Map[Cat, Map[UnaryProd, LogDouble]],
    prodMixes: Map[Cat, (LogDouble, LogDouble, LogDouble)], // binyProdMix: LogDouble, unryProdMix: LogDouble, termProdMix: LogDouble, // guaranteed to sum to 1
    binyScaleFactors: Map[Cat, LogDouble],
    unryScaleFactors: Map[Cat, LogDouble],
    rand: RandomGenerator) = {
    def f(t: CcgTree): (QTree, LogDouble) = t match {
      case CcgBinode(cat, l, r) =>
        val q = LogDouble(rand.nextDouble()) * knownBinyProds(cat)(BinaryProd(l.cat, r.cat)) * prodMixes(cat)._1
        val (fl, lm) = f(l)
        val (fr, rm) = f(r)
        (BiQTree(cat, q, fl, fr), (q * binyScaleFactors(cat)) min lm min rm)
      case CcgUnode(cat, s) =>
        val q = LogDouble(rand.nextDouble()) * knownUnryProds(cat)(UnaryProd(s.cat)) * prodMixes(cat)._2
        val (fs, sm) = f(s)
        (UnQTree(cat, q, fs), (q * unryScaleFactors(cat)) min sm)
      case CcgLeaf(cat, word, _) =>
        (LeQTree(cat), LogDouble.one)
    }
    f(ccgTree)
  }

  private[this] def getSupertagSets(qTree: QTree,
    infCatPriorsDesc: Vector[(Cat, LogDouble)],
    knownRootProbs: Map[Cat, LogDouble],
    knownBinyProds: Map[Cat, Map[BinaryProd, LogDouble]],
    knownUnryProds: Map[Cat, Map[UnaryProd, LogDouble]],
    prodMixes: Map[Cat, (LogDouble, LogDouble, LogDouble)], // binyProdMix: LogDouble, unryProdMix: LogDouble, termProdMix: LogDouble, // guaranteed to sum to 1
    binyScaleFactors: Map[Cat, LogDouble],
    unryScaleFactors: Map[Cat, LogDouble],
    rand: RandomGenerator) = {

    def f(qTree: QTree, tsToUse: Set[Cat]): Vector[Set[Cat]] = {
      qTree match {

        case BiQTree(_, q, l, r) =>
          //println(f"    p(${qTree.cat} -> [${l.cat},${r.cat}])=${knownBinyProds(qTree.cat)(BinaryProd(l.cat, r.cat)).logValue}%1.3e, q=${q.logValue}%1.3e =>")
          val prodsToUse =
            tsToUse.flatMap { t =>
              //println(f"      t=$t")
              val binyProdMix = prodMixes(t)._1

              // Observed categories (directly sampled probabilities)
              var knownProdsToUse: Vector[BinaryProd] =
                for {
                  probs <- knownBinyProds.get(t).toVector
                  (uvProd @ BinaryProd(u, v), uvp) <- probs
                  if uvProd != BiZProd
                  //_ = { println(f"        $uvProd => ${(uvp * binyProdMix).logValue}%1.3e > ${q.logValue}%1.3e => ${uvp * binyProdMix > q}") }
                  if uvp * binyProdMix > q
                } yield {
                  uvProd
                }
              //println(f"        observed categories: $knownProdsToUse")

              // Unobserved categories
              val scaleFactor = binyScaleFactors.getOrElse(t, LogDouble.one)
              breakable {
                for ((u, uProdPrior) <- infCatPriorsDesc) {
                  if (uProdPrior * scaleFactor * binyProdMix > q) {
                    breakable {
                      for ((v, vProdPrior) <- infCatPriorsDesc) { // TODO: Replace this with `v <- rules.flatMap(_.inferRight(t,u)); vProdPrior = infCatPrior(v)`
                        //println(f"      $t -> [${u},${v}] => ${rules.collect { case r: BinaryCcgRule => r(u, v) }.flatten}")
                        if (rules.collect { case r: BinaryCcgRule => r(u, v) }.flatten.contains(t)) { // make sure it's a valid rule application
                          val uvProd = BinaryProd(u, v)
                          if (knownBinyProds.get(t).forall(!_.contains(uvProd))) {
                            val p = uProdPrior * vProdPrior * scaleFactor * binyProdMix
                            //println(f"        $uvProd => ${p.logValue}%1.3e > ${q.logValue}%1.3e => ${p > q}")
                            if (p > q) knownProdsToUse :+= uvProd
                            else break
                          }
                        }
                      }
                    }
                  }
                  else break
                }
              }

              //println(f"        all categories: $knownProdsToUse")
              knownProdsToUse
            }

          val (usToUse, vsToUse) = prodsToUse.map { case BinaryProd(u, v) => (u, v) }.unzip
          f(l, usToUse) ++ f(r, vsToUse)

        case UnQTree(_, q, s) =>
          val prodsToUse =
            tsToUse.flatMap { t =>
              val unryProdMix = prodMixes(t)._2

              var knownProdsToUse: Vector[UnaryProd] =
                for {
                  probs <- knownUnryProds.get(t).toVector
                  (uProd @ UnaryProd(u), up) <- probs
                  if uProd != UnZProd
                  if up * unryProdMix > q
                } yield {
                  uProd
                }

              val scaleFactor = unryScaleFactors.getOrElse(t, LogDouble.one)
              for ((u, uProdPrior) <- infCatPriorsDesc) { // TODO: Replace this with `u <- rules.flatMap(_.inferSub(t)); up = infCatPrior(u)`
                if (rules.collect { case r: UnaryCcgRule => r(u) }.contains(t)) { // make sure it's a valid rule application
                  val uProd = UnaryProd(u)
                  if (knownUnryProds.get(t).forall(!_.contains(uProd))) {
                    val p = uProdPrior * scaleFactor * unryProdMix
                    if (p > q) knownProdsToUse :+= uProd
                  }
                }
              }

              knownProdsToUse
            }

          val usToUse = prodsToUse.map { case UnaryProd(u) => u }
          f(s, usToUse)

        case LeQTree(_) =>
          Vector(tsToUse)
      }
    }

    //println(f"  handle $qTree")
    val qRoot = LogDouble(rand.nextDouble()) * knownRootProbs(qTree.cat)
    val rootObservedCats = knownRootProbs.keySet - Z
    val rootUnobservedPriorSum = LogDouble.one - rootObservedCats.sumBy(infCatPrior)
    val rootsToUse = rootSet.filter { s =>
      val p = knownRootProbs.getOrElse(s, knownRootProbs(Z) * infCatPrior(s) / rootUnobservedPriorSum)
      p >= qRoot
    }
    val supertagSets = f(qTree, rootsToUse)
    //println("    supertag sets:"); for ((original, newSet) <- qTree.supertags zipSafe supertagSets) println(f"      $original%-10s ${newSet(original)}%-5s -> $newSet")
    //TreeViz.drawTree(qTree)
    //throw new RuntimeException("STOP!")
    supertagSets
  }
}

trait QTree extends VizTree { def cat: Cat; def supertags: Vector[Cat]; def label = cat.toString }
case class BiQTree(cat: Cat, q: LogDouble, left: QTree, right: QTree) extends QTree { def supertags = left.supertags ++ right.supertags; override def toString() = f"BiQTree($cat, ${f"${q.logValue}%1.3e".replaceAll("e-0*", "e-")}, $left, $right"; def children = Vector(left, right) }
case class UnQTree(cat: Cat, q: LogDouble, sub: QTree) extends QTree { def supertags = sub.supertags; override def toString() = f"UnQTree($cat, ${f"${q.logValue}%1.3e".replaceAll("e-0*", "e-")}, $sub"; def children = Vector(sub) }
case class LeQTree(cat: Cat) extends QTree { def supertags = Vector(cat); def children = Vector.empty }
