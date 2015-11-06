package dhg.ccg.gen

import java.util.ArrayList
import dhg.ccg.tagdict.TagDictionary
import dhg.util._
import junto.algorithm.ModifiedAdsorption
import junto.config.Edge
import junto.config.GraphBuilder
import junto.config.Label
import junto.graph.GraphIo
import junto.util.{ Constants => JuntoConstants }
import com.typesafe.scalalogging.slf4j.{ StrictLogging => Logging }

trait SoftTagger[Tag] {
  type Word = String

  /**
   * Soft-tag and return the raw sentences
   */
  def tagFromAnnotations(
    rawSentences: Vector[Vector[Word]],
    taggedSentences: Vector[Vector[(Word, Tag)]],
    initialTagdict: TagDictionary[Tag]): Vector[Vector[(Word, Map[Tag, Double])]]
}

class JuntoTagger[Tag](
  lpTaggingGraphBuilder: LpTaggingGraphBuilder[Tag, String, String, String],
  maxIterations: Int = 200,
  threshold: Double,
  //
  tagToString: (Tag => String),
  tagFromString: (String => Tag))
    extends SoftTagger[Tag]
    with Logging {

  override def tagFromAnnotations(
    rawSentences: Vector[Vector[Word]],
    taggedSentences: Vector[Vector[(Word, Tag)]],
    initialTagdict: TagDictionary[Tag]): Vector[Vector[(Word, Map[Tag, Double])]] = {
    val tagdict = initialTagdict.withWords(rawSentences.flatten.toSet | taggedSentences.flatten.map(_._1).toSet).withTags(taggedSentences.flatten.map(_._2).toSet)

    logger.info("Make Junto edges")
    val edges =
      lpTaggingGraphBuilder.makeType2TokenEdges(taggedSentences, rawSentences, tagdict) ++
        lpTaggingGraphBuilder.makeFeatureEdges(taggedSentences, rawSentences, tagdict)

    logger.info("Make Junto seeds")
    val seeds =
      lpTaggingGraphBuilder.makeTypeSeeds(taggedSentences, rawSentences, tagdict) ++
        lpTaggingGraphBuilder.makeTokenSeeds(taggedSentences, rawSentences, tagdict)

    logger.info("Build the Junto graph")
    val graph = GraphBuilder(
      makeWeightedEdges(edges).map { case (node1, node2, weight) => new Edge(node1, node2, weight) },
      seeds.map { case (word, tag) => new Label(word, f"TAG_${tagToString(tag)}", 1.0) },
      testLabels = List[Label](),
      beta = 2.0,
      maxNeighbors = Int.MaxValue,
      maxSeedsPerClass = Int.MaxValue,
      setGaussianWeights = false,
      sigmaFactor = 0.0,
      pruneThreshold = "0",
      isDirected = true)

    val result = new ArrayList[Map[String, Double]]
    val propagator = new ModifiedAdsorption(graph, keepTopKLabels = Int.MaxValue, mu1 = 1.0, mu2 = 0.01, mu3 = 0.01)
    logger.info("Run Junto propagator")
    propagator.run(maxIter = maxIterations, useBipartiteOptimization = false, verbose = false, resultList = result)

    logger.info("Save Junto output to file")
    val outputFile = mktemp("output_file")
    GraphIo.saveEstimatedScores(graph, outputFile.path)
    //println(outputFile.getAbsolutePath)

    val TagRe = """TAG_(.+)""".r

    val NodeRE = """([^_]+)_(.+)""".r
    val DummyLabel = JuntoConstants.GetDummyLabel // "__DUMMY__"

    logger.info("Extract Junto output from file")
    val extractedOutput: Iterator[(String, Map[Tag, Double])] = {
      for {
        line <- outputFile.readLines
        Vector(node @ NodeRE(nodetype, _), gold, injected, estimated, isTestNode, mrr) = line.lsplit("\t")
        labelProbs = estimated
          .lsplit(" ").grouped(2).toVector
          .map { case Vector(label, prob) => (label, prob.toDouble) }
          .filter(_._1 != DummyLabel)
          .map { case (TagRe(tag), prob) => (tagFromString(tag), prob.toDouble) }
          .toMap
          .normalizeValues
          .filter(_._2 >= threshold)
          .normalizeValues
        if labelProbs.nonEmpty
      } yield (node, labelProbs)
    }

    outputFile.delete()

    logger.info("Extract token labels from Junto output")
    lpTaggingGraphBuilder.extractTaggedTokens(extractedOutput, rawSentences)
  }

  /**
   * (thing -> feature)  1 / (# things that have the feature)
   * (feature -> thing)  1 / (# things that have the feature)
   *
   * Edges between things and their features are weighted uniformly, with each
   * feature getting a (1/#things) portion of the mass.  Less frequent features
   * will push their labels more forcefully toward the things that have them
   * since they are "very indicative" of those things.
   */
  private[this] def makeWeightedEdges(thingsByFeature: Iterator[(String, Vector[String])]) = {
    thingsByFeature.flatMap {
      case (feature, things) =>
        val numThings = things.size
        val pThing = 1.0 / numThings
        things.flatMap { thing =>
          Seq(
            (thing, feature, pThing),
            (feature, thing, pThing))
        }
    }
  }

}

class UniformSoftTagger[Tag]() extends SoftTagger[Tag] {
  override def tagFromAnnotations(
    rawSentences: Vector[Vector[Word]],
    taggedSentences: Vector[Vector[(Word, Tag)]],
    initialTagdict: TagDictionary[Tag]): Vector[Vector[(Word, Map[Tag, Double])]] = {

    rawSentences.map { s =>
      s.mapTo { w =>
        val ts = initialTagdict(w)
        val p = 1.0 / ts.size
        ts.mapToVal(p).toMap
      }
    }
  }
}

