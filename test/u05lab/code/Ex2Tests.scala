package u05lab.code

import org.junit.jupiter.api.Assertions.{assertEquals, assertThrows}
import org.junit.jupiter.api.Test

class Ex2Tests {
  var examsManager: ExamsManager = ExamsManager()

  @Test def testEvaluationCannotBeGraterThan30(): Unit = {
    assertThrows(classOf[IllegalArgumentException], () => Succeeded(31))
  }

  @Test def testEvaluationCannotBeLessThan18(): Unit = {
    assertThrows(classOf[IllegalArgumentException], () => Succeeded(17))
  }

  private def prepareExams(): Unit = {
    examsManager = ExamsManager()
    examsManager.createNewCall("jan")
    examsManager.createNewCall("feb")
    examsManager.addStudentResult("jan", "rossi", Succeeded(18))
    examsManager.addStudentResult("jan", "bianchi", Succeeded(30, cumLaude = true))
    examsManager.addStudentResult("feb", "rossi", Succeeded(30))
    examsManager.addStudentResult("feb", "verdi", Retired())
  }

  @Test def cannotCreateCallTwice(): Unit = {
    prepareExams()
    assertThrows(classOf[IllegalArgumentException], () => examsManager.createNewCall("jan"))
  }

  @Test def cannotRegisterResultTwice(): Unit = {
    prepareExams()
    assertThrows(classOf[IllegalArgumentException], () => examsManager.addStudentResult("jan", "rossi", Retired()))
  }

  @Test def testGetAllStudentsFromCall(): Unit = {
    prepareExams()
    assertEquals(Set("rossi", "bianchi"), examsManager.getAllStudentsFromCall("jan"))
    assertEquals(Set("rossi", "verdi"), examsManager.getAllStudentsFromCall("feb"))
    assertThrows(classOf[IllegalArgumentException], () => examsManager.getAllStudentsFromCall("mar"))
  }

  @Test def testGetEvaluationMapFromCall(): Unit = {
    prepareExams()
    assertEquals(Map("rossi" -> 18, "bianchi" -> 30), examsManager.getEvaluationsMapFromCall("jan"))
    assertEquals(Map("rossi" -> 30), examsManager.getEvaluationsMapFromCall("feb"))
    assertThrows(classOf[IllegalArgumentException], () => examsManager.getEvaluationsMapFromCall("mar"))
  }

  @Test def testGetResultsMapFromStudents(): Unit = {
    prepareExams()
    assertEquals(Map("jan" -> Succeeded(18), "feb" -> Succeeded(30)), examsManager.getResultsMapFromStudent("rossi"))
    assertEquals(Map("jan" -> Succeeded(30, cumLaude = true)), examsManager.getResultsMapFromStudent("bianchi"))
    assertEquals(Map("feb" -> Retired()), examsManager.getResultsMapFromStudent("verdi"))
    assertEquals(Map(), examsManager.getResultsMapFromStudent("viola"))
  }

  @Test def testGetBestResultFromStudent(): Unit = {
    prepareExams()
    assertEquals(Some(30), examsManager.getBestResultFromStudent("rossi"))
    assertEquals(Some(30), examsManager.getBestResultFromStudent("bianchi"))
    assertEquals(None, examsManager.getBestResultFromStudent("verdi"))
    assertEquals(None, examsManager.getBestResultFromStudent("viola"))
  }
}
