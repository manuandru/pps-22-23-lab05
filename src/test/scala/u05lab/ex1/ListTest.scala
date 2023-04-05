package u05lab.ex1

import org.junit.Assert.assertEquals
import org.junit.Test
import u05lab.ex1.List.{::, Nil}

class ListTest {

  val list: List[Int] = 10 :: 20 :: 30 :: Nil()

  @Test def testZipRight(): Unit =
    assertEquals(Nil(), Nil().zipRight)
    assertEquals(List((10, 0), (20, 1), (30, 2)), list.zipRight)

  @Test def testFoldLeftRight(): Unit =
    assertEquals(0, list.foldLeftRight(0)(a => a)(_ - _)((a, _, c) => a + c))
    assertEquals(-40, list.foldLeftRight(0)(a => a)(_ + _)((a, _, c) => a - c))

  @Test def testPartition(): Unit =
    assertEquals((Nil(), Nil()), Nil().partition(_ => true))
    assertEquals((List(30), List(10, 20)), list.partition(_ > 20))

}
