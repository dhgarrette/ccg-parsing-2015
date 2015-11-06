package dhg.ccg.tagdict

import org.junit.Test
import dhg.util._
import org.junit.Assert._

class TagDictionaryTests {

  @Test
  def test_SimpleTagDictionary {

    val map = Map("a" -> Set(1), "b" -> Set(2), "c" -> Set(3), "d" -> Set(1, 2))
    val td = SimpleTagDictionary.apply(map, "0", 0, "9", 9, Set("a", "c", "e"), Set(1, 5), excludedTags = Set(2, 4))
    assertEquals(Set(0), td("0"))
    assertEquals(Set(1), td("a"))
    assertEquals(Set(1, 3, 5), td("b"))
    assertEquals(Set(3), td("c"))
    assertEquals(Set(1), td("d"))
    assertEquals(Set(1, 3, 5), td("e"))
    assertEquals(Set(1, 3, 5), td("f"))
    assertEquals(Set(1, 3, 5), td("g"))
    assertEquals(Set(9), td("9"))

    assertEquals(Map("0" -> Set(0), "a" -> Set(1), "c" -> Set(3), "d" -> Set(1), "9" -> Set(9)), td.entries)
    assertEquals(Map(0 -> Set("0"), 1 -> Set("a", "d"), 3 -> Set("c"), 5 -> Set(), 9 -> Set("9")), td.knownWordsForTag)

    val td2 = td.withWords(Set("c", "e", "f"))
    assertEquals(Set("a", "b", "c", "d", "e", "f"), td2.allWords)
    assertEquals(Set("a", "b", "c", "d", "e", "f", "0", "9"), td2.allWordsSE)
    assertEquals(td.allTags, td2.allTags)
    assertEquals(td.startWord, td2.startWord)
    assertEquals(td.startTag, td2.startTag)
    assertEquals(td.endWord, td2.endWord)
    assertEquals(td.endTag, td2.endTag)
    assertEquals(td.excludedTags, td2.excludedTags)
    assertEquals(Set(0), td2("0"))
    assertEquals(Set(1), td2("a"))
    assertEquals(Set(1, 3, 5), td2("b"))
    assertEquals(Set(3), td2("c"))
    assertEquals(Set(1), td2("d"))
    assertEquals(Set(1, 3, 5), td2("e"))
    assertEquals(Set(1, 3, 5), td2("f"))
    assertEquals(Set(1, 3, 5), td2("g"))
    assertEquals(Set(9), td2("9"))

    val td3 = td.withTags(Set(3, 4, 6))
    assertEquals(td.allWords, td3.allWords)
    assertEquals(Set(1, 3, 5, 6), td3.allTags)
    assertEquals(Set(1, 3, 5, 6, 0, 9), td3.allTagsSE)
    assertEquals(td.startWord, td3.startWord)
    assertEquals(td.startTag, td3.startTag)
    assertEquals(td.endWord, td3.endWord)
    assertEquals(td.endTag, td3.endTag)
    assertEquals(td.excludedTags, td3.excludedTags)
    assertEquals(Set(0), td3("0"))
    assertEquals(Set(1), td3("a"))
    assertEquals(Set(1, 3, 5, 6), td3("b"))
    assertEquals(Set(3), td3("c"))
    assertEquals(Set(1), td3("d"))
    assertEquals(Set(1, 3, 5, 6), td3("e"))
    assertEquals(Set(1, 3, 5, 6), td3("f"))
    assertEquals(Set(1, 3, 5, 6), td3("g"))
    assertEquals(Set(9), td3("9"))

    val td4 = td.withExcludedTags(Set(1, 4, 6))
    assertEquals(td.allWords, td4.allWords)
    assertEquals(td.allTags, td4.allTags)
    assertEquals(td.startWord, td4.startWord)
    assertEquals(td.startTag, td4.startTag)
    assertEquals(td.endWord, td4.endWord)
    assertEquals(td.endTag, td4.endTag)
    assertEquals(Set(1, 2, 4, 6), td4.excludedTags)
    assertEquals(Set(0), td4("0"))
    assertEquals(Set(3, 5), td4("a"))
    assertEquals(Set(3, 5), td4("b"))
    assertEquals(Set(3), td4("c"))
    assertEquals(Set(3, 5), td4("d"))
    assertEquals(Set(3, 5), td4("e"))
    assertEquals(Set(3, 5), td4("f"))
    assertEquals(Set(3, 5), td4("g"))
    assertEquals(Set(9), td4("9"))
  }

  @Test
  def test_SimpleTagDictionaryFactory {
    val cutoff = Some(0.2)
    val f = new SimpleTagDictionaryFactory[Char](cutoff)
    val sentences = Vector(Vector(
      "1" -> 'a',
      "1" -> 'a',
      "1" -> 'a',
      "1" -> 'a',
      "1" -> 'b',
      "1" -> 'b',
      "1" -> 'b',
      "1" -> 'c',
      "2" -> 'b',
      "2" -> 'b',
      "2" -> 'b',
      "2" -> 'b',
      "2" -> 'b',
      "2" -> 'z',
      "7" -> 'a',
      "7" -> 'c'))
    val td = f(sentences, "0", 'A', "9", 'Z', Set("7", "8"), Set('x', 'y'))

    assertEquals(Set("1", "2", "7", "8"), td.allWords)
    assertEquals(Set("1", "2", "7", "8", "0", "9"), td.allWordsSE)
    assertEquals(Set('a', 'b', 'c', 'x', 'y'), td.allTags)
    assertEquals(Set('a', 'b', 'c', 'x', 'y', 'A', 'Z'), td.allTagsSE)

    assertEquals(Set('A'), td("0"))
    assertEquals(Set('a', 'b'), td("1"))
    assertEquals(Set('b'), td("2"))
    assertEquals(Set('a', 'b', 'c', 'x', 'y'), td("3"))
    assertEquals(Set('a', 'c'), td("7"))
    assertEquals(Set('a', 'b', 'c', 'x', 'y'), td("8"))
    assertEquals(Set('Z'), td("9"))
  }

  @Test
  def test_StartEndSwappedTagDictionary {
    val map = Map("a" -> Set(1), "b" -> Set(2), "c" -> Set(3), "d" -> Set(1, 2))
    val td = new StartEndSwappedTagDictionary(SimpleTagDictionary(map, "0", 0, "9", 9, Set("a", "c", "e"), Set(1, 5), excludedTags = Set(2, 4)))
    
    assertEquals(Set(0), td("0"))
    assertEquals(Set(1), td("a"))
    assertEquals(Set(1, 3, 5), td("b"))
    assertEquals(Set(3), td("c"))
    assertEquals(Set(1), td("d"))
    assertEquals(Set(1, 3, 5), td("e"))
    assertEquals(Set(1, 3, 5), td("f"))
    assertEquals(Set(1, 3, 5), td("g"))
    assertEquals(Set(9), td("9"))
    assertEquals("9", td.startWord)
    assertEquals(9, td.startTag)
    assertEquals("0", td.endWord)
    assertEquals(0, td.endTag)

    assertEquals(Map("0" -> Set(0), "a" -> Set(1), "c" -> Set(3), "d" -> Set(1), "9" -> Set(9)), td.entries)
    assertEquals(Map(0 -> Set("0"), 1 -> Set("a", "d"), 3 -> Set("c"), 5 -> Set(), 9 -> Set("9")), td.knownWordsForTag)

    val td2 = td.withWords(Set("c", "e", "f"))
    assertEquals(Set("a", "b", "c", "d", "e", "f"), td2.allWords)
    assertEquals(Set("a", "b", "c", "d", "e", "f", "0", "9"), td2.allWordsSE)
    assertEquals(td.allTags, td2.allTags)
    assertEquals(td.startWord, td2.startWord)
    assertEquals(td.startTag, td2.startTag)
    assertEquals(td.endWord, td2.endWord)
    assertEquals(td.endTag, td2.endTag)
    assertEquals(td.excludedTags, td2.excludedTags)
    assertEquals(Set(0), td2("0"))
    assertEquals(Set(1), td2("a"))
    assertEquals(Set(1, 3, 5), td2("b"))
    assertEquals(Set(3), td2("c"))
    assertEquals(Set(1), td2("d"))
    assertEquals(Set(1, 3, 5), td2("e"))
    assertEquals(Set(1, 3, 5), td2("f"))
    assertEquals(Set(1, 3, 5), td2("g"))
    assertEquals(Set(9), td2("9"))

    val td3 = td.withTags(Set(3, 4, 6))
    assertEquals(td.allWords, td3.allWords)
    assertEquals(Set(1, 3, 5, 6), td3.allTags)
    assertEquals(Set(1, 3, 5, 6, 0, 9), td3.allTagsSE)
    assertEquals(td.startWord, td3.startWord)
    assertEquals(td.startTag, td3.startTag)
    assertEquals(td.endWord, td3.endWord)
    assertEquals(td.endTag, td3.endTag)
    assertEquals(td.excludedTags, td3.excludedTags)
    assertEquals(Set(0), td3("0"))
    assertEquals(Set(1), td3("a"))
    assertEquals(Set(1, 3, 5, 6), td3("b"))
    assertEquals(Set(3), td3("c"))
    assertEquals(Set(1), td3("d"))
    assertEquals(Set(1, 3, 5, 6), td3("e"))
    assertEquals(Set(1, 3, 5, 6), td3("f"))
    assertEquals(Set(1, 3, 5, 6), td3("g"))
    assertEquals(Set(9), td3("9"))

    val td4 = td.withExcludedTags(Set(1, 4, 6))
    assertEquals(td.allWords, td4.allWords)
    assertEquals(td.allTags, td4.allTags)
    assertEquals(td.startWord, td4.startWord)
    assertEquals(td.startTag, td4.startTag)
    assertEquals(td.endWord, td4.endWord)
    assertEquals(td.endTag, td4.endTag)
    assertEquals(Set(1, 2, 4, 6), td4.excludedTags)
    assertEquals(Set(0), td4("0"))
    assertEquals(Set(3, 5), td4("a"))
    assertEquals(Set(3, 5), td4("b"))
    assertEquals(Set(3), td4("c"))
    assertEquals(Set(3, 5), td4("d"))
    assertEquals(Set(3, 5), td4("e"))
    assertEquals(Set(3, 5), td4("f"))
    assertEquals(Set(3, 5), td4("g"))
    assertEquals(Set(9), td4("9"))
  }

}
