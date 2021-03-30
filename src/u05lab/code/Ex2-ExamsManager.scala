package u05lab.code

sealed trait ExamResult
case class Retired() extends ExamResult { override def toString: String = "RETIRED" }
case class Failed() extends ExamResult { override def toString: String = "FAILED" }
case class Succeeded(evaluation: Int, cumLaude: Boolean = false) extends ExamResult {
  val minGrade = 18
  val maxGrade = 30
  require(minGrade to maxGrade contains evaluation, s"Evaluation must be in range $minGrade-$maxGrade for an exam to succeed")
  require(!cumLaude || evaluation == maxGrade, s"Evaluation must be $maxGrade to get L")
  override def toString: String = s"SUCCEEDED($evaluation${if (cumLaude) "L" else ""})"
}

trait ExamsManager {
  def createNewCall(call: String): Unit
  def addStudentResult(call: String, student: String, result: ExamResult): Unit
  def getAllStudentsFromCall(call: String): Set[String]
  def getEvaluationsMapFromCall(call: String): Map[String, Int]
  def getResultsMapFromStudent(student: String): Map[String, ExamResult]
  def getBestResultFromStudent(student: String): Option[Int]
}

object ExamsManager {
  def apply() = new ExamsManagerImpl()

  class ExamsManagerImpl extends ExamsManager {
    private var calls: Map[String, Map[String, ExamResult]] = Map()

    override def createNewCall(call: String): Unit = {
      require(!(calls contains call), s"call $call was already added")
      calls = calls + (call -> Map())
    }

    override def addStudentResult(call: String, student: String, result: ExamResult): Unit = {
      require(calls contains call, s"$call is not a call")
      require(!(calls(call) contains student), s"student $student already has a result for call $call")
      calls = calls + (call -> (calls(call) + (student -> result)))
    }

    override def getAllStudentsFromCall(call: String): Set[String] = {
      require(calls contains call, s"$call is not a call")
      calls(call).keySet
    }

    override def getEvaluationsMapFromCall(call: String): Map[String, Int] = {
      require(calls contains call, s"$call is not a call")
      calls(call) collect { case (student, Succeeded(e, _)) => student -> e }
    }

    override def getResultsMapFromStudent(student: String): Map[String, ExamResult] =
      calls collect { case (call, results) if results contains student => call -> results(student) }

    override def getBestResultFromStudent(student: String): Option[Int] =
      getResultsMapFromStudent(student) collect { case (_, Succeeded(e, _)) => e } maxByOption identity
  }
}
