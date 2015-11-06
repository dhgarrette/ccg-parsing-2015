package dhg.ccg.tag

import dhg.util._
import dhg.ccg.prob.ConditionalLogProbabilityDistribution
import dhg.ccg.tagdict.TagDictionary
import dhg.ccg.tagdict.SimpleTagDictionary

trait Tagger[Tag] {
  type Word = String

  /**
   * Compute the probability of the tagged sentence.
   */
  def sentenceProb(sentence: Vector[(Word, Tag)]): LogDouble

  /**
   * Accepts a sentence of word tokens and returns a sequence of
   * tags corresponding to each of those words.
   */
  final def tag(sentence: Vector[Word]): Vector[Tag] = tagAndProb(sentence)._1

  /**
   * Accepts a sentence of word tokens and returns a sequence of
   * tags corresponding to each of those words, along with the
   * probability of the tagged sentence.
   */
  def tagAndProb(sentence: Vector[Word]): (Vector[Tag], LogDouble)

}

trait WeightedTagger[Tag] extends Tagger[Tag] {
  override final def sentenceProb(sentence: Vector[(Word, Tag)]): LogDouble = sentenceProbWithWeights(sentence, us = Vector.fill(sentence.size)(Map.empty))
  override final def tagAndProb(sentence: Vector[Word]): (Vector[Tag], LogDouble) = tagAndProbWithWeights(sentence, us = Vector.fill(sentence.size)(Map.empty))

  def sentenceProbWithWeights(sentence: Vector[(Word, Tag)], us: Vector[Map[Tag, LogDouble]]): LogDouble
  final def tagWithWeights(sentence: Vector[Word], us: Vector[Map[Tag, LogDouble]]): Vector[Tag] = tagAndProbWithWeights(sentence, us)._1
  def tagAndProbWithWeights(sentence: Vector[Word], us: Vector[Map[Tag, LogDouble]]): (Vector[Tag], LogDouble)
  def tagAndProbWithWeightsFromTagSet(sentence: Vector[(Word, Set[Tag])], us: Vector[Map[Tag, LogDouble]]): (Vector[Tag], LogDouble)
}

//
//
//

trait SupervisedTaggerTrainer[Tag] {
  type Word = String
  def train(taggedSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Tag]): Tagger[Tag]
}

trait SemisupervisedTaggerTrainer[Tag] {
  type Word = String
  final def train(
    rawSentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Tag],
    baseTrDist: ConditionalLogProbabilityDistribution[Tag, Tag], baseEmDist: ConditionalLogProbabilityDistribution[Tag, Word]): Tagger[Tag] = {
    trainWithSomeGold(rawSentences, Vector.empty, initialTagdict, baseTrDist, baseEmDist)
  }

  final def trainWithTagsets(
    rawSentencesWithTokenTags: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag],
    transitions: ConditionalLogProbabilityDistribution[Tag, Tag], emissions: ConditionalLogProbabilityDistribution[Tag, Word]): Tagger[Tag] = {
    trainWithTagsetsAndSomeGold(rawSentencesWithTokenTags, Vector.empty, initialTagdict, transitions, emissions)
  }

  final def trainWithSomeGold(
    rawSentences: Vector[Vector[Word]], goldLabeledSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Tag],
    transitions: ConditionalLogProbabilityDistribution[Tag, Tag], emissions: ConditionalLogProbabilityDistribution[Tag, Word]) = {
    trainWithTagsetsAndSomeGold(rawSentences.map(_.mapTo(initialTagdict)), goldLabeledSentences, initialTagdict, transitions, emissions)
  }

  def trainWithTagsetsAndSomeGold(
    rawSentencesWithTokenTags: Vector[Vector[(Word, Set[Tag])]], goldLabeledSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Tag],
    transitions: ConditionalLogProbabilityDistribution[Tag, Tag], emissions: ConditionalLogProbabilityDistribution[Tag, Word]): Tagger[Tag]
}

trait TypeSupervisedTaggerTrainer[Tag] {
  type Word = String
  def typesupTrain(rawSentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Tag]): Tagger[Tag]
}

trait NoisilySupervisedTaggerTrainer[Tag] {
  type Word = String
  final def noisySupTrain(noisilyLabeledSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Tag]): Tagger[Tag] = {
    noisySupTrainWithSomeGold(noisilyLabeledSentences, Vector.empty, initialTagdict)
  }
  def noisySupTrainWithSomeGold(noisilyLabeledSentences: Vector[Vector[(Word, Tag)]], goldLabeledSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Tag]): Tagger[Tag]
}
