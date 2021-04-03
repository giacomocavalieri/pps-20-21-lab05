package u05lab.code

import u05lab.code.PerformanceUtils.measureTime

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.collection.immutable.{HashSet, List, TreeSet}
import scala.collection.mutable.{ArrayBuffer, HashSet => MutableHashSet, TreeSet => MutableTreeSet}

object PerformanceUtils {
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]] {
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)
  }

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] = {
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime()-startTime, TimeUnit.NANOSECONDS)
    if(msg nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)
  }

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

  def measureTime[T](expr: => T): FiniteDuration = measure("")(expr).duration
}

class CollectionsLogger(val collSize: Int = 1_000_000, val repetitions: Int = 5) {
  var opsResults: Map[String, Map[String, FiniteDuration]] = Map()
  private val collElems = 1 to collSize toList
  private val presentElem = collSize
  private val missingElem = collSize + 1

  import LoggerUtils._

  def testIndexed[A <: Iterable[Int]](collName: String, create: Iterable[Int] => A, find: (A, Int) => Boolean,
                                      remove: (A, Int) => Unit, append: (A, Int) => A, prepend: (A, Int) => A,
                                      access: (A, Int) => Int): Unit = {
    testCommonOperations(collName, create, find, remove)
    test(collName, create,
      ("append", append(_: A, missingElem)),
      ("prepend", prepend(_: A, missingElem)),
      ("access first", access(_: A, 0)),
      ("access last", access(_: A, collSize - 1)),
      ("access middle", access(_: A, collSize/2)))
  }

  def testUnordered[A <: Iterable[Int]](collName: String, create: Iterable[Int] => A, find: (A, Int) => Boolean,
                                         remove: (A, Int) => Unit, add: (A, Int) => A): Unit = {
    testCommonOperations(collName, create, find, remove)
    test(collName, create,
      ("add present", add(_: A, presentElem)),
      ("add missing", add(_: A, missingElem)))
  }

  private object LoggerUtils {
    def testCommonOperations[A <: Iterable[Int]](collName: String, create: Iterable[Int] => A, find: (A, Int) => Boolean,
                                                 remove: (A, Int) => Unit): Unit = {
      test(collName, create,
        ("create", (_: A) => create(collElems)),
        ("size", (_: A).size),
        ("find present", find(_: A, presentElem)),
        ("find missing", find(_: A, missingElem)),
        ("remove", remove(_: A, presentElem)))
    }

    def test[A <: Iterable[Int]](collName: String, create: Iterable[Int] => A, ops: (String, A => Any)*): Unit = {
      ops foreach { case (opName, op) => testOperation(collName, opName, create, op) }
    }

    def testOperation[A <: Iterable[Int]](collName: String, opName: String, create: Iterable[Int] => A, op: A => Any): Unit = {
      val collResult = collName -> average(create, op)
      opsResults += opName -> (opsResults.getOrElse(opName, Map()) + collResult)
    }

    def average[A <: Iterable[Int]](create: Iterable[Int] => A, op: A => Any): FiniteDuration = {
      val durations = (1 to repetitions) map (_ => { val coll = create(collElems); measureTime(op(coll)); })
      (durations reduce (_ + _)) / repetitions
    }
  }
}

object Ex3prove extends App {
  val logger = new CollectionsLogger()
  logger.testIndexed[List[Int]]("(I) List", List.from(_), _ contains _, (l, e) => l filter (_ != e), _ :+ _, _ prepended  _, _ apply _)
  logger.testIndexed[Vector[Int]]("(I) Vector", Vector.from(_), _ contains _, (l, e) => l filter (_ != e), _ :+ _, _ prepended _, _ apply _)
  logger.testIndexed[ArrayBuffer[Int]]("(M) ArrayBuffer", ArrayBuffer.from(_), _ contains _, _ -= _, _ append _, _ prepend _, _ apply _)
  logger.testUnordered[HashSet[Int]]("(I) HashSet", HashSet.from(_), _ contains _, _ - _, _ + _)
  logger.testUnordered[MutableHashSet[Int]]("(M) HashSet", MutableHashSet.from(_), _ contains _, _ -= _, _ += _)
  logger.testUnordered[TreeSet[Int]]("(I) TreeSet", TreeSet.from(_), _ contains _, _ - _, _ + _)
  logger.testUnordered[MutableTreeSet[Int]]("(M) TreeSet", MutableTreeSet.from(_), _ contains _, _ -= _, _ += _)
  printingUtils.printTimes(logger.opsResults)

  private object printingUtils {
    def printTimes(results: Map[String, Map[String, FiniteDuration]]): Unit = {
      println(s"(Collections' size: ${logger collSize})\n(Times averaged from ${logger repetitions} tests)\n")
      results foreach { case (opName, opResults) => printOperationTimes(opName, opResults) }
    }

    def printOperationTimes(opName: String, opResults: Map[String, FiniteDuration]): Unit = {
      println(opName toUpperCase)
      val sortedDurations = opResults.toList sortWith { case ((_, d1), (_, d2)) => d1 < d2 }
      for ((collName, duration) <- sortedDurations) {
        println("\t- %-15s %,10d µs".format(collName, duration.toMicros))
      }
      println()
    }
  }
}
