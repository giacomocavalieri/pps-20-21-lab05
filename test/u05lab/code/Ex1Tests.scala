package u05lab.code

import org.junit.jupiter.api.Assertions.assertEquals
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
}
