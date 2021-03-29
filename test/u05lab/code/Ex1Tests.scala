package u05lab.code

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class Ex1Tests {

  @Test
  def testZipRight() {
    val list = List("a", "b", "c")

    assertEquals(List.nil, List.nil.zipRight)
    assertEquals(List(("a", 0), ("b", 1), ("c", 2)), list.zipRight)
  }
}
