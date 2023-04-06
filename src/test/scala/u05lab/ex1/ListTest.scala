package u05lab.ex1

import org.junit.Assert.{assertEquals, assertThrows}
import org.junit.Test
import u05lab.ex1.List.{::, Nil}

class ListTest:

  val list: List[Int] = 10 :: 20 :: 30 :: Nil()

  @Test def testZipRight(): Unit =
    assertEquals(Nil(), Nil().zipRight)
    assertEquals(List((10, 0), (20, 1), (30, 2)), list.zipRight)

  @Test def testFoldLeftRight(): Unit =
    assertEquals(0, list.foldLeftRight(0)(a => a)(_ - _)((a, _, c) => a + c))
    assertEquals(-40, list.foldLeftRight(0)(a => a)(_ + _)((a, _, c) => a - c))

  @Test def testZipRightWithDoubleFolding(): Unit =
    assertEquals(Nil(), Nil().zipRightDoubleFolding)
    assertEquals(List((10, 0), (20, 1), (30, 2)), list.zipRightDoubleFolding)

  @Test def testPartition(): Unit =
    assertEquals((Nil(), Nil()), Nil().partition(_ => true))
    assertEquals((List(30), List(10, 20)), list.partition(_ > 20))

  @Test def testSpan(): Unit =
    assertEquals((Nil(), Nil()), Nil().span(_ => true))
    assertEquals((List(10), List(20, 30)), list.span(_ == 20))
    assertEquals((List(10, 20, 30), Nil()), list.span(_ == 40))

  @Test def testReduce(): Unit =
    assertThrows(UnsupportedOperationException().getClass, () => Nil().reduce((a, _) => a))
    assertEquals(10, List(10).reduce(_ + _))
    assertEquals(60, list.reduce(_ + _))

  @Test def testTakeRight(): Unit =
    assertEquals(Nil(), list.takeRight(0))
    assertEquals(List(20, 30), list.takeRight(2))

  @Test def testCollect(): Unit =
    assertEquals(List("20", "30"), list.collect { case n if n > 10 => s"$n" })