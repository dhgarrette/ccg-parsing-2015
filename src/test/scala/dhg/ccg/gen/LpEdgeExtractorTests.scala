package dhg.ccg.gen

import org.junit.Test
import dhg.util._
import dhg.util.TestUtil._
import org.junit.Assert._

class LpEdgeExtractorTests {

  @Test
  def test_Type2TokenLpEdgeExtractor {
    val raw = Vector(
      "the dog walks",
      "the dog runs",
      "the man walks the dog")
      .map(_.lsplit("\\s+"))

    val e = new Type2TokenLpEdgeExtractor()
    val result = e(raw, Set("a", "on", "the", "bird", "sings", "silent", "works", "running")).toMap

    def get(word: String) = result(TypeLpFeature(word)).sortBy(t => (t.sentIdx, t.tokenIdx))

    assertEquals(5, result.size)
    assertEquals(Vector(TokenLpFeature("the", 0, 0), TokenLpFeature("the", 1, 0), TokenLpFeature("the", 2, 0), TokenLpFeature("the", 2, 3)), get("the"))
    assertEquals(Vector(TokenLpFeature("dog", 0, 1), TokenLpFeature("dog", 1, 1), TokenLpFeature("dog", 2, 4)), get("dog"))
    assertEquals(Vector(TokenLpFeature("walks", 0, 2), TokenLpFeature("walks", 2, 2)), get("walks"))
    assertEquals(Vector(TokenLpFeature("runs", 1, 2)), get("runs"))
    assertEquals(Vector(TokenLpFeature("man", 2, 1)), get("man"))
  }

  @Test
  def test_TokenPrevLpEdgeExtractor {
    val raw = Vector(
      "the dog walks",
      "the dog runs",
      "the man walks the dog")
      .map(_.lsplit("\\s+"))

    val e = new TokenPrevLpEdgeExtractor("<S>")
    val result = e(raw, Set("a", "on", "the", "bird", "sings", "silent", "works", "running")).toMap

    def get(word: Option[String]) = result(PrevWordLpFeat(word, "<S>")).sortBy(t => (t.sentIdx, t.tokenIdx))

    assertEquals(6, result.size)
    assertEquals(Vector(TokenLpFeature("the", 0, 0), TokenLpFeature("the", 1, 0), TokenLpFeature("the", 2, 0)), get(None))
    assertEquals(Vector(TokenLpFeature("dog", 0, 1), TokenLpFeature("dog", 1, 1), TokenLpFeature("man", 2, 1), TokenLpFeature("dog", 2, 4)), get(Some("the")))
    assertEquals(Vector(TokenLpFeature("walks", 0, 2), TokenLpFeature("runs", 1, 2)), get(Some("dog")))
    assertEquals(Vector(TokenLpFeature("the", 2, 3)), get(Some("walks")))
    assertEquals(Vector(), get(Some("runs")))
    assertEquals(Vector(TokenLpFeature("walks", 2, 2)), get(Some("man")))
  }

  @Test
  def test_TokenNextLpEdgeExtractor {
    val raw = Vector(
      "the dog walks",
      "the dog runs",
      "the man walks the dog")
      .map(_.lsplit("\\s+"))

    val e = new TokenNextLpEdgeExtractor("<E>")
    val result = e(raw, Set("a", "on", "the", "bird", "sings", "silent", "works", "running")).toMap

    def get(word: Option[String]) = result(NextWordLpFeat(word, "<E>")).sortBy(t => (t.sentIdx, t.tokenIdx))

    assertEquals(6, result.size)
    assertEquals(Vector(TokenLpFeature("walks", 2, 2)), get(Some("the")))
    assertEquals(Vector(TokenLpFeature("the", 0, 0), TokenLpFeature("the", 1, 0), TokenLpFeature("the", 2, 3)), get(Some("dog")))
    assertEquals(Vector(TokenLpFeature("dog", 0, 1), TokenLpFeature("man", 2, 1)), get(Some("walks")))
    assertEquals(Vector(TokenLpFeature("dog", 1, 1)), get(Some("runs")))
    assertEquals(Vector(TokenLpFeature("the", 2, 0)), get(Some("man")))
    assertEquals(Vector(TokenLpFeature("walks", 0, 2), TokenLpFeature("runs", 1, 2), TokenLpFeature("dog", 2, 4)), get(None))
  }

  @Test
  def test_TokenPrefixLpEdgeExtractor {
    val raw = Vector(
      "the dog walks",
      "the dog runs",
      "the man walks the dog")
      .map(_.lsplit("\\s+"))

    val e = new WordPrefixLpEdgeExtractor(3)
    val result = e(raw, Set("a", "on", "the", "bird", "sings", "silent", "works", "running")).toMap

    def get(fix: String, n: Int) = result(PrefixLpFeat(fix, n)).sortBy(_.word)

    //    a
    //    bird
    //    dog
    //    man
    //    on
    //    running
    //    runs
    //    silent
    //    sings
    //    the  
    //    walks
    //    works

    assertEquals(27, result.size)

    assertEquals(Vector(TypeLpFeature("a")), get("a", 1))

    assertEquals(Vector(TypeLpFeature("bird")), get("b", 1))
    assertEquals(Vector(TypeLpFeature("bird")), get("bi", 2))
    assertEquals(Vector(TypeLpFeature("bird")), get("bir", 3))

    assertEquals(Vector(TypeLpFeature("dog")), get("d", 1))
    assertEquals(Vector(TypeLpFeature("dog")), get("do", 2))
    assertEquals(Vector(TypeLpFeature("dog")), get("dog", 3))

    assertEquals(Vector(TypeLpFeature("man")), get("m", 1))
    assertEquals(Vector(TypeLpFeature("man")), get("ma", 2))
    assertEquals(Vector(TypeLpFeature("man")), get("man", 3))

    assertEquals(Vector(TypeLpFeature("on")), get("o", 1))
    assertEquals(Vector(TypeLpFeature("on")), get("on", 2))

    assertEquals(Vector(TypeLpFeature("running"), TypeLpFeature("runs")), get("r", 1))
    assertEquals(Vector(TypeLpFeature("running"), TypeLpFeature("runs")), get("ru", 2))
    assertEquals(Vector(TypeLpFeature("running"), TypeLpFeature("runs")), get("run", 3))

    assertEquals(Vector(TypeLpFeature("silent"), TypeLpFeature("sings")), get("s", 1))
    assertEquals(Vector(TypeLpFeature("silent"), TypeLpFeature("sings")), get("si", 2))
    assertEquals(Vector(TypeLpFeature("silent")), get("sil", 3))
    assertEquals(Vector(TypeLpFeature("sings")), get("sin", 3))

    assertEquals(Vector(TypeLpFeature("the")), get("t", 1))
    assertEquals(Vector(TypeLpFeature("the")), get("th", 2))
    assertEquals(Vector(TypeLpFeature("the")), get("the", 3))

    assertEquals(Vector(TypeLpFeature("walks"), TypeLpFeature("works")), get("w", 1))
    assertEquals(Vector(TypeLpFeature("walks")), get("wa", 2))
    assertEquals(Vector(TypeLpFeature("walks")), get("wal", 3))
    assertEquals(Vector(TypeLpFeature("works")), get("wo", 2))
    assertEquals(Vector(TypeLpFeature("works")), get("wor", 3))
  }

  @Test
  def test_TokenSuffixLpEdgeExtractor {
    val raw = Vector(
      "the dog walks",
      "the dog runs",
      "the man walks the dog")
      .map(_.lsplit("\\s+"))

    val e = new WordSuffixLpEdgeExtractor(3)
    val result = e(raw, Set("a", "on", "the", "bird", "sings", "silent", "works", "running")).toMap

    def get(fix: String, n: Int) = result(SuffixLpFeat(fix, n)).sortBy(_.word)

    //          a
    //       bird
    //        the
    //    running
    //        dog
    //        man
    //         on
    //      sings
    //      walks
    //      works
    //       runs
    //     silent

    assertEquals(27, result.size)

    assertEquals(Vector(TypeLpFeature("a")), get("a", 1))

    assertEquals(Vector(TypeLpFeature("bird")), get("d", 1))
    assertEquals(Vector(TypeLpFeature("bird")), get("rd", 2))
    assertEquals(Vector(TypeLpFeature("bird")), get("ird", 3))

    assertEquals(Vector(TypeLpFeature("the")), get("e", 1))
    assertEquals(Vector(TypeLpFeature("the")), get("he", 2))
    assertEquals(Vector(TypeLpFeature("the")), get("the", 3))

    assertEquals(Vector(TypeLpFeature("dog"), TypeLpFeature("running")), get("g", 1))
    assertEquals(Vector(TypeLpFeature("running")), get("ng", 2))
    assertEquals(Vector(TypeLpFeature("running")), get("ing", 3))
    assertEquals(Vector(TypeLpFeature("dog")), get("og", 2))
    assertEquals(Vector(TypeLpFeature("dog")), get("dog", 3))

    assertEquals(Vector(TypeLpFeature("man"), TypeLpFeature("on")), get("n", 1))
    assertEquals(Vector(TypeLpFeature("man")), get("an", 2))
    assertEquals(Vector(TypeLpFeature("man")), get("man", 3))
    assertEquals(Vector(TypeLpFeature("on")), get("on", 2))

    assertEquals(Vector(TypeLpFeature("runs"), TypeLpFeature("sings"), TypeLpFeature("walks"), TypeLpFeature("works")), get("s", 1))
    assertEquals(Vector(TypeLpFeature("sings")), get("gs", 2))
    assertEquals(Vector(TypeLpFeature("sings")), get("ngs", 3))
    assertEquals(Vector(TypeLpFeature("walks"), TypeLpFeature("works")), get("ks", 2))
    assertEquals(Vector(TypeLpFeature("walks")), get("lks", 3))
    assertEquals(Vector(TypeLpFeature("works")), get("rks", 3))
    assertEquals(Vector(TypeLpFeature("runs")), get("ns", 2))
    assertEquals(Vector(TypeLpFeature("runs")), get("uns", 3))

    assertEquals(Vector(TypeLpFeature("silent")), get("t", 1))
    assertEquals(Vector(TypeLpFeature("silent")), get("nt", 2))
    assertEquals(Vector(TypeLpFeature("silent")), get("ent", 3))
  }

}
