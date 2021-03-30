package u05lab.code

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u05lab.code.Ex4.sequence

class Ex4Tests {
  val listWithSome: List[Option[Int]] = List(Some(1), Some(2), Some(3))
  val listWithNone: List[Option[Int]] = List(Some(1), None, Some(3))

  @Test def testSequence(): Unit = {
    assertEquals(Option(List.nil[Option[Int]]), sequence(List.nil[Option[Int]]))
    assertEquals(Option(List(1, 2, 3)), sequence(listWithSome))
    assertEquals(None, sequence(listWithNone))
  }
}
