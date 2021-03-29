package u05lab.code

import org.junit.jupiter.api.Assertions.{assertArrayEquals, assertEquals, assertThrows}
import org.junit.jupiter.api.Test

class Ex1Tests {

  val testList: List[Int] = List(1, 2, 3, 4)

  @Test def testZipRight(): Unit = {
    assertEquals(List.nil, List.nil.zipRight)
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), testList.zipRight)
  }

  @Test def testPartition(): Unit = {
    assertEquals((List.nil, List.nil), List.nil[Int].partition(_ => true))
    assertEquals((List(1, 3), List(2, 4)), testList.partition(_ % 2 != 0))
  }

  @Test def testSpan(): Unit = {
    assertEquals((List.nil, List.nil), List.nil[Int].span(_ => true))
    assertEquals((testList, List.nil), testList.span(_ => true))
    assertEquals((List.nil, testList), testList.span(_ => false))
    assertEquals((List(1), List(2, 3, 4)), testList.span(_ % 2 != 0))
  }

  @Test def testReduce(): Unit = {
    assertThrows(classOf[UnsupportedOperationException], () => List.nil[Int].reduce(_ + _))
    assertEquals(10, testList.reduce(_ + _))
  }

  @Test def testTakeRight(): Unit = {
    assertEquals(List.nil, List.nil.takeRight(1))
    assertEquals(List.nil, List.nil.takeRight(-1))
    assertEquals(List.nil, testList.takeRight(-1))
    assertEquals(List.nil, testList.takeRight(0))
    assertEquals(List(4), testList.takeRight(1))
    assertEquals(List(3, 4), testList.takeRight(2))
    assertEquals(List(2, 3, 4), testList.takeRight(3))
    assertEquals(testList, testList.takeRight(4))
    assertEquals(testList, testList.takeRight(5))
  }

  @Test def testCollect(): Unit = {
    val addOneToEvenNumber: PartialFunction[Int, Int] = { case n if n % 2 == 0 => n + 1 }
    assertEquals(List.nil, List.nil.collect(addOneToEvenNumber))
    assertEquals(List(3, 5), testList.collect(addOneToEvenNumber))
    assertEquals(List.nil, List(1, 3).collect(addOneToEvenNumber))
  }
}
