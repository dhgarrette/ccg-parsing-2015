package dhg.ccg.gen

import dhg.ccg.tagdict.TagDictionary
import dhg.util._
import collection.mutable
import collection.breakOut
import math.{ log, exp }
import scalaz._
import Scalaz._
import dhg.ccg.prob._
import scala.annotation.tailrec
import com.typesafe.scalalogging.slf4j.{ StrictLogging => Logging }

trait SoftToHardTagger[Tag] {
  type Word = String
  type Token = (Word, Map[Tag, Double])

  def tagFromSoftTaggedSentences(
    softTaggedSentences: Vector[Vector[Token]],
    taggedSentences: Vector[Vector[(Word, Tag)]],
    initialTagdict: TagDictionary[Tag]): Vector[Vector[(Word, Tag)]]
}

final class ModelMinSoftToHardTagger[Tag]() extends SoftToHardTagger[Tag] with Logging {

  override def tagFromSoftTaggedSentences(
    softTaggedSentences: Vector[Vector[Token]],
    taggedSentences: Vector[Vector[(Word, Tag)]],
    initialTagdict: TagDictionary[Tag]) = {
    val tagdict = initialTagdict.withWords(softTaggedSentences.flatten.map(_._1).toSet | taggedSentences.flatten.map(_._1).toSet).withTags(softTaggedSentences.flatten.flatMap(_._2.keys).toSet | taggedSentences.flatten.map(_._2).toSet)

    // Starting here, sentences will always have start/end tags appended 
    val completedSoftTaggedSentences: List[Vector[Token]] = softTaggedSentences.map(completeTagging(_, tagdict))(breakOut)
    val completedHardTaggedSentences: List[Vector[Token]] = taggedSentences.map(_.mapVals(t => Map(t -> 1.0))).map(completeTagging(_, tagdict))(breakOut)

    val trellis = completedSoftTaggedSentences // ++ completedHardTaggedSentences // exclude hard b/c then we wont' be able to separate them out at the end...
    val selectedEdges = cover(trellis, Set(tagdict.startTag -> tagdict.startTag, tagdict.endTag -> tagdict.endTag))
    fill(trellis, selectedEdges, tagdict).toVector
  }

  /**
   * 1) Filter invalid tags from tokens.
   * 2) If token has no associated tags, associate all tags from the word's TD entry, with uniform weights
   * 3) Append start/end tags
   */
  def completeTagging(sentence: Vector[Token], tagdict: TagDictionary[Tag]): Vector[Token] = {
    val completedSentence =
      sentence.mapt { (word, weightedTags) =>
        val tdTags = tagdict(word)
        val filteredTags = weightedTags.filterKeys(tdTags)
        val completedTags = if (filteredTags.isEmpty) tdTags.mapToVal(1.0 / tdTags.size).toMap else filteredTags
        word -> completedTags
      }
    (tagdict.startWord, Map(tagdict.startTag -> 1.0)) +: completedSentence :+ (tagdict.endWord, Map(tagdict.endTag -> 1.0))
  }

  def cover(sentencesToDo: List[Vector[Token]], selectedEdges: Set[(Tag, Tag)]): Set[(Tag, Tag)] = {
    coverRecurse(sentencesToDo, List.empty, selectedEdges)
  }

  @tailrec def coverRecurse(sentencesToDo: List[Vector[Token]], coveredSentences: List[Vector[Token]], selectedEdges: Set[(Tag, Tag)]): Set[(Tag, Tag)] = {
    var allCoveredSentences: List[Vector[Token]] = coveredSentences
    var uncoveredSentences: List[Vector[Token]] = List.empty

    val coverWeights =
      sentencesToDo.foldLeft(Map.empty[(Tag, Tag), Double]) {
        case (weightMap, sentence) =>
          val weights = edgeCoverWeights(sentence, selectedEdges)
          if (weights.isEmpty) {
            logger.debug(f"  Covered: ${sentence.map(_._1)}")
            allCoveredSentences ::= sentence
            weightMap // fully-covered sentences won't be used to choose next edge (weights is empty anyway, so adding wouldn't actually hurt)
          }
          else {
            uncoveredSentences ::= sentence
            weightMap |+| weights
          }
      }

    if (uncoveredSentences.isEmpty) { // no uncovered => all covered
      logger.info(f"All Sentences Covered!")
      selectedEdges
    }
    else { // still more to do.  pick a next edge and try to parse remaining sentences.
      logger.debug(f"Choices: ${coverWeights.desc.take(5).map { case ((a, b), w) => f"($a,$b) -> $w%.2f" }.mkString(", ")}")
      val newEdge = coverWeights.maxBy(_._2)._1
      logger.info(f"  New Edge ${selectedEdges.size + 1} (${uncoveredSentences.size} sentences remain): $newEdge")
      coverRecurse(uncoveredSentences, allCoveredSentences, selectedEdges + newEdge)
    }
  }

  def fill(sentencesToDo: List[Vector[Token]], selectedEdges: Set[(Tag, Tag)], tagdict: TagDictionary[Tag]): List[Vector[(Word, Tag)]] = {
    // Ensure that there is at least one start and one end edge, since it is impossible to solve any sentence without one of each
    val needsStart = !selectedEdges.exists { case (a, b) => a == tagdict.startTag && b != tagdict.startTag }
    val needsEnd = !selectedEdges.exists { case (a, b) => b == tagdict.endTag && a != tagdict.endTag }
    val newSelectedEdges =
      if (needsStart || needsEnd) { // if there's no selected start/end tag edge, pick one
        val holeWeights = sentencesToDo.map(holeFillWeights(_, selectedEdges)).reduce(_ |+| _)
        val start = needsStart.option(holeWeights.filter(_._1._1 == tagdict.startTag).maxBy(_._2)._1)
        start.foreach { s => logger.info(f"Prepick Edge: $s") }
        val end = needsEnd.option(holeWeights.filter(_._1._2 == tagdict.endTag).maxBy(_._2)._1)
        end.foreach { e => logger.info(f"Prepick Edge: $e") }
        selectedEdges ++ start ++ end
      }
      else selectedEdges

    fillRecurse(sentencesToDo, List.empty, newSelectedEdges, tagdict)
  }

  @tailrec def fillRecurse(sentencesToDo: List[Vector[Token]], solvedSentences: List[Vector[(Word, Tag)]], selectedEdges: Set[(Tag, Tag)], tagdict: TagDictionary[Tag]): List[Vector[(Word, Tag)]] = {
    var allSolvedSentences: List[Vector[(Word, Tag)]] = solvedSentences
    var unsolvedSentences: List[Vector[Token]] = List.empty

    val holeWeights =
      sentencesToDo.foldLeft(Map.empty[(Tag, Tag), Double]) {
        case (weightMap, sentence) =>
          val weights = holeFillWeights(sentence, selectedEdges)
          tag(sentence, selectedEdges, tagdict) match {
            case Some(taggedSentence) =>
              logger.debug(f"  Solved: $taggedSentence")
              allSolvedSentences ::= taggedSentence
              weightMap // fully-covered sentences won't be used to choose next edge (weights is empty anyway, so adding wouldn't actually hurt)
            case None =>
              unsolvedSentences ::= sentence
              weightMap |+| weights
          }
      }

    if (unsolvedSentences.isEmpty) { // no unsolved => all solved
      logger.info(f"All Sentences Solved!")
      allSolvedSentences
    }
    else { // still more to do.  pick a next edge and try to parse remaining sentences.
      logger.debug(f"Choices: ${holeWeights.desc.take(5).map { case ((a, b), w) => f"($a,$b) -> $w%.2f" }.mkString(", ")}")
      if (holeWeights.isEmpty) {
        println(f"RAN OUT OF EDGES!!  ${unsolvedSentences.size}/${unsolvedSentences.size + allSolvedSentences.size} remain unsolved!")
        //for (s <- unsolvedSentences) println("\n" + s.map { case (w, ts) => f"$w|(${ts.toVector.map(_._1).map(_.toString).sorted.mkString(",")})" }.mkString(" ")); println
        allSolvedSentences
      }
      else {
        val newEdge = holeWeights.maxBy(_._2)._1
        logger.info(f"  New Edge ${selectedEdges.size + 1} (${unsolvedSentences.size} sentences remain): $newEdge")
        fillRecurse(unsolvedSentences, allSolvedSentences, selectedEdges + newEdge, tagdict)
      }
    }
  }

  def tag(sentence: Vector[Token], selectedEdges: Set[(Tag, Tag)], tagdict: TagDictionary[Tag]): Option[Vector[(Word, Tag)]] = {
    type Forward = Map[Tag, (Double, Tag)] // each potential tag for the word mapped to it's best previous tag
    val initialForward = Map(tagdict.startTag -> (0.0, tagdict.startTag))

    var forwards = List(initialForward) // build the list of forwards backwards
    for ((currWord, currTags) <- sentence.drop(1)) {
      val prevV: Forward = forwards.head
      forwards ::=
        currTags.flatMap {
          case (k, kProb) =>
            val scores = // scores for each previous tag reaching this tag
              prevV.flatMap {
                case (kprime, (kprimeScore, _)) =>
                  selectedEdges(kprime -> k).option(kprime -> kprimeScore) // None if that transition is not among the available (selected) edges
              }
            if (scores.isEmpty) None
            else {
              val (bestKprime, bestKprimeScore) = scores.maxBy(_._2) // best transition to this tag
              Some(k -> (kProb * bestKprimeScore, bestKprime))
            }
        }
    }

    // Follow backpointers
    // TODO: The Option shouldn't be necessary since if the sentence has no holes, then it should be taggable!
    forwards.foldLeft(Option(List(tagdict.endTag))) {
      case (Some(nextTag :: tags), v) =>
        v.get(nextTag).map { case (_, t) => t :: nextTag :: tags }
      case (None, _) => None
      case _ => sys.error("impossible")
    }.map { tags => // if a backward path could be found (it's Some)
      (sentence.map(_._1) zipSafe tags.tail).slyce(1, -1) // Zip with words and drop start/end tags
    }
  }

  /**
   * Calculate the total "weight" of uncovered tokens that would be covered by each potential edge.
   * The start/end tokens are "already covered", so this just focuses on the real tokens.
   * If the returned Map is empty, then the sentence is fully covered!
   */
  def edgeCoverWeights(sentence: Vector[Token], alreadySelectedEdges: Set[(Tag, Tag)]): Map[(Tag, Tag), Double] = {
    (for {
      ((p, prevTags), (c, currTags), (n, nextTags)) <- sentence.sliding3
      potentialEdges = {
        val prevEdges = (for ((prev, prevWeight) <- prevTags; (curr, currWeight) <- currTags) yield (prev, curr) -> (currWeight))
        val nextEdges = (for ((curr, currWeight) <- currTags; (next, nextWeight) <- nextTags) yield (curr, next) -> (currWeight))
        prevEdges |+| nextEdges
      }
      //_ = logger.trace(f"$p -> $c -> $n: ${potentialEdges.keySet}:  intersection: ${potentialEdges.keySet & alreadySelectedEdges}")
      if (potentialEdges.keySet & alreadySelectedEdges).isEmpty // none of the potential edges has been selected yet
      (edge, edgeWeight) <- potentialEdges
    } yield edge -> edgeWeight).groupByKey.mapVals(_.sum.toDouble)
  }

  /**
   * Calculate the total "weight" of path holes that would be filled by each potential edge.  For any "hole" (an edge
   * a->b such that there is an edge going into a and an edge going out of b), its score is:
   *
   *   w(a) + w(b)
   *    + sum_{p | p->a in selectedEdges} w(p)
   *    + sum_{n | b->n in selectedEdges} w(n)
   *
   * This takes into account the number of paths that introducing a->b would connect, and the weights of those paths.
   *
   * The start/end tokens are "already touched".
   */
  def holeFillWeights(sentence: Vector[Token], alreadySelectedEdges: Set[(Tag, Tag)]): Map[(Tag, Tag), Double] = {
    (for {
      ((_, prevTags), (x, aTags), (y, bTags), (_, nextTags)) <- (sentence.head +: sentence :+ sentence.last).sliding4 // add extra start/end so that the true start/end tokens are included in counts
      //
      //_ = println(f"$x -> $y")
      (a, aw) <- aTags
      (b, bw) <- bTags
      if !alreadySelectedEdges(a, b) // for every potential hole a->b
      pws = (for ((p, pw) <- prevTags if alreadySelectedEdges(p -> a)) yield pw) if pws.nonEmpty // for every p leading into a
      nws = (for ((n, nw) <- nextTags if alreadySelectedEdges(b -> n)) yield nw) if nws.nonEmpty // for every n leading out of b
    } yield {
      //println(f"    ($a, $b) -> (${pws.mkString("+")}) + $aw + $bw + (${nws.mkString("+")})")
      (a, b) -> (pws.sum + aw + bw + nws.sum)
    }).groupByKey.mapVals(_.sum.toDouble)
  }

}

/**
 * Always choose the tag [randomly from the set of tags] with the highest weight.
 */
class HighProbSoftToHardTagger[Tag]() extends SoftToHardTagger[Tag] {
  def tagFromSoftTaggedSentences(
    softTaggedSentences: Vector[Vector[Token]],
    taggedSentences: Vector[Vector[(Word, Tag)]],
    initialTagdict: TagDictionary[Tag]): Vector[Vector[(Word, Tag)]] = {

    softTaggedSentences.map { s =>
      s.mapVals { ts =>
        if (ts.nonEmpty)
          ts.toVector.shuffle.maxBy(_._2)._1
        else
          initialTagdict.allTags.toVector.shuffle.head
      }
    }
  }
}


