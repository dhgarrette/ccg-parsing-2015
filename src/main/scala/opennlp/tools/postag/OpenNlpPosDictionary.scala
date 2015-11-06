package opennlp.tools.postag

object OpenNlpPosDictionary {

  def apply(validWords: Set[String], validTags: Set[String], partialMapping: Map[String, Set[String]]) = {
    val td = new POSDictionary
    for (word <- validWords)
      td(word) = partialMapping.get(word).fold(validTags)(_ & validTags)
    td
  }

  implicit class ExtendedPOSDictionary(val self: POSDictionary) extends AnyVal {
    def apply(word: String): Set[String] = {
      self.getTags(word).toSet
    }

    def update(word: String, tags: Set[String]): Unit = {
      self.addTags(word, tags.toSeq: _*)
    }

    def updated(word: String, tags: Set[String]): POSDictionary = {
      self(word) = tags
      self
    }

    def append(word: String, additionalTags: Set[String]) = {
      self.updated(word, self(word) | additionalTags)
    }
  }

}
