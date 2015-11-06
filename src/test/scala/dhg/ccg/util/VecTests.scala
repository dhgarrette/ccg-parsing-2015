package dhg.ccg.util

import org.junit.Test
import org.junit.Assert._
import dhg.util.TestUtil._

class VecTests {

  @Test
  def test_IndirectSparseVec_companion_isSortedUnique {
    assertTrue(IndirectSparseVec.isSortedUnique(Array[Int](), 0))
    assertTrue(IndirectSparseVec.isSortedUnique(Array[Int](2), 1))
    assertTrue(IndirectSparseVec.isSortedUnique(Array[Int](2, 3), 2))
    assertFalse(IndirectSparseVec.isSortedUnique(Array[Int](3, 2), 2))
    assertTrue(IndirectSparseVec.isSortedUnique(Array[Int](2, 3, 5, 7), 4))
    assertFalse(IndirectSparseVec.isSortedUnique(Array[Int](2, 3, 8, 7), 4))
    assertFalse(IndirectSparseVec.isSortedUnique(Array[Int](8, 2, 3, 7), 4))
    assertFalse(IndirectSparseVec.isSortedUnique(Array[Int](2, 3, 3, 7), 4))

    assertTrue(IndirectSparseVec.isSortedUnique(Array[Int](-1), 0))
    assertTrue(IndirectSparseVec.isSortedUnique(Array[Int](2, -1), 1))
    assertTrue(IndirectSparseVec.isSortedUnique(Array[Int](2, 3, -1), 2))
    assertFalse(IndirectSparseVec.isSortedUnique(Array[Int](3, 2, -1), 2))
    assertTrue(IndirectSparseVec.isSortedUnique(Array[Int](2, 3, 5, 7, -1), 4))
    assertFalse(IndirectSparseVec.isSortedUnique(Array[Int](2, 3, 8, 7, -1), 4))
    assertFalse(IndirectSparseVec.isSortedUnique(Array[Int](8, 2, 3, 7, -1), 4))
    assertFalse(IndirectSparseVec.isSortedUnique(Array[Int](2, 3, 3, 7, -1), 4))
  }

  @Test
  def test_IndirectSparseVec_apply {
    val a0 = IndirectSparseVec[String](Array[Int](), 9)
    assertExceptionAny(a0(0))

    assertExceptionAny(IndirectSparseVec[String](Array(5, 3, 8), 9))

    //    val a1 = IndirectSparseVec[String](Array(3, 5, 8))
    //    a1(5) = "b"
    //    a1(3) = "a"
    //    a1(8) = "c"
    //    assertEquals("a", a1(3))
    //    assertEquals("b", a1(5))
    //    assertEquals("c", a1(8))
    //    assertExceptionAny(a1(4))

    val a2 = IndirectSparseVec(Array(3, 5, 8), Array("a", "b", "c"), 9)
    assertEquals("a", a2(3))
    assertEquals("b", a2(5))
    assertEquals("c", a2(8))
    assertExceptionAny(a2(4))
  }

  @Test
  def test_IndirectSparseVec_get {
    val a0 = IndirectSparseVec[String](Array[Int](), 9)
    assertEquals(None, a0.get(0))

    assertExceptionAny(IndirectSparseVec[String](Array(5, 3, 8), 9))

    //    val a1 = IndirectSparseVec[String](Array(3, 5, 8), 9)
    //    a1(5) = "b"
    //    a1(3) = "a"
    //    a1(8) = "c"
    //    assertEquals(Some("a"), a1.get(3))
    //    assertEquals(Some("b"), a1.get(5))
    //    assertEquals(Some("c"), a1.get(8))
    //    assertEquals(None, a1.get(4))

    val a2 = IndirectSparseVec(Array(3, 5, 8), Array("a", "b", "c"), 9)
    assertEquals(Some("a"), a2.get(3))
    assertEquals(Some("b"), a2.get(5))
    assertEquals(Some("c"), a2.get(8))
    assertEquals(None, a2.get(4))
  }

  @Test
  def test_IndirectSparseVec_keysValuesSize {
    val a0Keys = Array[Int]()
    val a0Data = Array[String]()
    val a0 = IndirectSparseVec[String](a0Keys, a0Data, 9)
    assertSame(a0Keys, a0.activeKeys)
    assertSame(a0Data, a0.activeValues)
    assertEquals(0, a0.activeCount)

    val a2Keys = Array(3, 5, 8)
    val a2Data = Array("a", "b", "c")
    val a2 = IndirectSparseVec[String](a2Keys, a2Data, 9)
    assertSame(a2Keys, a2.activeKeys)
    assertSame(a2Data, a2.activeValues)
    assertEquals(3, a2.activeCount)
    assertEqualsArray(Array((3, "a"), (5, "b"), (8, "c")), a2.activePairs)
  }

  @Test
  def test_IndirectSparseVec_withDefaultValue {
    val a0Keys = Array[Int]()
    val a0Data = Array[String]()
    val itr0 = Iterator("a0xx")
    val a0 = IndirectSparseVec[String](a0Keys, a0Data, 9).withDefaultValue(itr0.next)
    assertEquals("a0xx", a0(3))
    assertSame(a0Keys, a0.activeKeys)
    assertSame(a0Data, a0.activeValues)
    assertEquals(0, a0.activeCount)
    assertEquals("a0xx", a0(4))
    assertEquals(Some("a0xx"), a0.get(5))
    assertEquals("a0xx", a0.getOrElse(10, "no"))

    val a2Keys = Array(3, 5, 8)
    val a2Data = Array("a", "b", "c")
    val itr2 = Iterator("a2xx")
    val a2 = IndirectSparseVec[String](a2Keys, a2Data, 9).withDefaultValue(itr2.next)
    assertEquals("a2xx", a2(2))
    assertEquals("b", a2(5))
    assertEquals("a2xx", a2(6))
    assertSame(a2Keys, a2.activeKeys)
    assertSame(a2Data, a2.activeValues)
    assertEquals("a2xx", a2(7))
    assertEquals("c", a2(8))
    assertEquals("a2xx", a2(9))
    assertEquals(Some("a2xx"), a2.get(1))
    assertEquals("a2xx", a2.getOrElse(10, "no"))
  }

  @Test
  def test_DirectSparseVec_companion_isSortedUnique {
    assertTrue(DirectSparseVec.isSortedUnique(Array[Int](), 0))
    assertTrue(DirectSparseVec.isSortedUnique(Array[Int](2), 1))
    assertTrue(DirectSparseVec.isSortedUnique(Array[Int](2, 3), 2))
    assertFalse(DirectSparseVec.isSortedUnique(Array[Int](3, 2), 2))
    assertTrue(DirectSparseVec.isSortedUnique(Array[Int](2, 3, 5, 7), 4))
    assertFalse(DirectSparseVec.isSortedUnique(Array[Int](2, 3, 8, 7), 4))
    assertFalse(DirectSparseVec.isSortedUnique(Array[Int](8, 2, 3, 7), 4))
    assertFalse(DirectSparseVec.isSortedUnique(Array[Int](2, 3, 3, 7), 4))

    assertTrue(DirectSparseVec.isSortedUnique(Array[Int](-1), 0))
    assertTrue(DirectSparseVec.isSortedUnique(Array[Int](2, -1), 1))
    assertTrue(DirectSparseVec.isSortedUnique(Array[Int](2, 3, -1), 2))
    assertFalse(DirectSparseVec.isSortedUnique(Array[Int](3, 2, -1), 2))
    assertTrue(DirectSparseVec.isSortedUnique(Array[Int](2, 3, 5, 7, -1), 4))
    assertFalse(DirectSparseVec.isSortedUnique(Array[Int](2, 3, 8, 7, -1), 4))
    assertFalse(DirectSparseVec.isSortedUnique(Array[Int](8, 2, 3, 7, -1), 4))
    assertFalse(DirectSparseVec.isSortedUnique(Array[Int](2, 3, 3, 7, -1), 4))
  }

  @Test
  def test_DirectSparseVec_apply {
    val a0 = DirectSparseVec[String](Array[Int](), 9)
    //assertExceptionAny(a0(0))

    assertExceptionAny(DirectSparseVec[String](Array(5, 3, 8), 9))

    //    val a1 = DirectSparseVec[String](Array(3, 5, 8))
    //    a1(5) = "b"
    //    a1(3) = "a"
    //    a1(8) = "c"
    //    assertEquals("a", a1(3))
    //    assertEquals("b", a1(5))
    //    assertEquals("c", a1(8))
    //    assertExceptionAny(a1(4))

    //                                             0     1     2     3    4     5    6     7     8
    val a2 = DirectSparseVec(Array(3, 5, 8), Array(null, null, null, "a", null, "b", null, null, "c"), 9)
    assertEquals("a", a2(3))
    assertEquals("b", a2(5))
    assertEquals("c", a2(8))
    //assertExceptionAny(a2(4))
    assertNull(a2(4))
  }

  @Test
  def test_DirectSparseVec_get {
    val a0 = DirectSparseVec[String](Array[Int](), 9)
    assertEquals(None, a0.get(0))

    assertExceptionAny(DirectSparseVec[String](Array(5, 3, 8), 9))

    //    val a1 = DirectSparseVec[String](Array(3, 5, 8), 9)
    //    a1(5) = "b"
    //    a1(3) = "a"
    //    a1(8) = "c"
    //    assertEquals(Some("a"), a1.get(3))
    //    assertEquals(Some("b"), a1.get(5))
    //    assertEquals(Some("c"), a1.get(8))
    //    assertEquals(None, a1.get(4))

    //                                             0     1     2     3    4     5    6     7     8
    val a2 = DirectSparseVec(Array(3, 5, 8), Array(null, null, null, "a", null, "b", null, null, "c"), 9)
    assertEquals(Some("a"), a2.get(3))
    assertEquals(Some("b"), a2.get(5))
    assertEquals(Some("c"), a2.get(8))
    assertEquals(None, a2.get(4))
  }

  @Test
  def test_DirectSparseVec_keysValuesSize {
    val a0Keys = Array[Int]()
    val a0Data = new Array[String](9)
    val a0 = DirectSparseVec[String](a0Keys, a0Data, 9)
    assertSame(a0Keys, a0.activeKeys)
    assertEqualsArray(Array[String](), a0.activeValues)
    assertEquals(0, a0.activeCount)

    val a2Keys = Array(3, 5, 8)
    //                 0     1     2     3    4     5    6     7     8
    val a2Data = Array(null, null, null, "a", null, "b", null, null, "c")
    val a2 = DirectSparseVec[String](a2Keys, a2Data, 9)
    assertSame(a2Keys, a2.activeKeys)
    assertEqualsArray(Array("a", "b", "c"), a2.activeValues)
    assertEquals(3, a2.activeCount)
    assertEqualsArray(Array((3, "a"), (5, "b"), (8, "c")), a2.activePairs)
  }

  @Test
  def test_DirectSparseVec_withDefaultValue {
    val a0Keys = Array[Int]()
    val a0Data = new Array[String](9)
    val itr0 = Iterator("a0xx")
    val a0 = DirectSparseVec[String](a0Keys, a0Data, 9).withDefaultValue(itr0.next)
    assertEquals("a0xx", a0(3))
    assertSame(a0Keys, a0.activeKeys)
    assertEqualsArray(Array[String](), a0.activeValues)
    assertEquals(0, a0.activeCount)
    assertEquals("a0xx", a0(4))
    assertEquals(Some("a0xx"), a0.get(5))
    assertEquals("a0xx", a0.getOrElse(10, "no"))

    val a2Keys = Array(3, 5, 8)
    //                 0     1     2     3    4     5    6     7     8
    val a2Data = Array(null, null, null, "a", null, "b", null, null, "c")
    val itr2 = Iterator("a2xx")
    val a2 = DirectSparseVec[String](a2Keys, a2Data, 9).withDefaultValue(itr2.next)
    assertEquals("a2xx", a2(2))
    assertEquals("b", a2(5))
    assertEquals("a2xx", a2(6))
    assertSame(a2Keys, a2.activeKeys)
    assertEqualsArray(Array("a", "b", "c"), a2.activeValues)
    assertEquals("a2xx", a2(7))
    assertEquals("c", a2(8))
    assertEquals("a2xx", a2(9))
    assertEquals(Some("a2xx"), a2.get(1))
    assertEquals("a2xx", a2.getOrElse(10, "no"))
  }

  @Test
  def test_OrderedIndirectSparseVec_apply {
    val a0 = OrderedIndirectSparseVec[String](Array[Int](), 9)
    assertExceptionAny(a0(0))

    val a2 = OrderedIndirectSparseVec(Array(5, 3, 8), Array("b", "a", "c"), 9)
    assertEquals("a", a2(3))
    assertEquals("b", a2(5))
    assertEquals("c", a2(8))
    assertExceptionAny(a2(4))

    //   val ax = new IndirectSparseVec(Array(5, 3, 8), Array("b", "a", "c"), 3)
    //    assertEquals("a", ax(3))
    //    assertEquals("b", ax(5))
    //    assertEquals("c", ax(8))
    //    assertExceptionAny(ax(4))
  }

  @Test
  def test_OrderedIndirectSparseVec_get {
    val a0 = OrderedIndirectSparseVec[String](Array[Int](), 9)
    assertEquals(None, a0.get(0))

    val a2 = OrderedIndirectSparseVec(Array(5, 3, 8), Array("b", "a", "c"), 9)
    assertEquals(Some("a"), a2.get(3))
    assertEquals(Some("b"), a2.get(5))
    assertEquals(Some("c"), a2.get(8))
    assertEquals(None, a2.get(4))
  }

  @Test
  def test_OrderedIndirectSparseVec_keysValuesSize {
    val a0Keys = Array[Int]()
    val a0Data = Array[String]()
    val a0 = OrderedIndirectSparseVec[String](a0Keys, a0Data, 9)
    assertSame(a0Keys, a0.activeKeys)
    assertSame(a0Data, a0.activeValues)
    assertEquals(0, a0.activeCount)

    val a2Keys = Array(5, 3, 8)
    val a2Data = Array("b", "a", "c")
    val a2 = OrderedIndirectSparseVec[String](a2Keys, a2Data, 9)
    assertSame(a2Keys, a2.activeKeys)
    assertSame(a2Data, a2.activeValues)
    assertEquals(3, a2.activeCount)
  }

}
