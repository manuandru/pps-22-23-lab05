package u05lab.ex3

import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils:
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]]:
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] =
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

import PerformanceUtils.*

/* Linear sequences: List, ListBuffer */
@main def checkListPerformance(): Unit =
  // ListBuffer creation is faster the second time!
  //  List.from(1 to 1)
  //  ListBuffer.from(1 to 1)
  var immutableList = List.from(1 to 10_000_000)
  val mutableList = ListBuffer.from(1 to 10_000_000)
  assert(
    measure("List immutable ops")(() =>
      immutableList = immutableList.map(_ * 2)
      immutableList = immutableList.takeRight(9_000_000)
      immutableList = immutableList.patch(1_000_000, 1 to 5_000_000, 5_000_000)
      immutableList = immutableList.filter(_ > 2_000_000)
    ) >
      measure("List mutable inplace ops")(() =>
        mutableList.mapInPlace(_ * 2)
        mutableList.takeRightInPlace(9_000_000)
        mutableList.patchInPlace(1_000_000, 1 to 5_000_000, 5_000_000)
        mutableList.filterInPlace(_ > 2_000_000)
      )
  )


/* Sets */
@main def checkSetPerformance(): Unit = ???

/* Maps */
@main def checkMapPerformance(): Unit = ???


/* List vs Vector */
@main def checkListVsVectorPerformance(): Unit =
  val lst = (1 to 10000000).toList
  val vec = (1 to 10000000).toVector
  assert(measure("lst last")(lst.last) > measure("vec last")(vec.last))

/* Indexed sequences: Vector, Array, ArrayBuffer */
// La prima query su Array dura di più
@main def checkArrayPerformance(): Unit =
  val array = (1 to 10_000_000).toArray
  val vector = (1 to 10_000_000).toVector
  val arrayBuffer = (1 to 10_000_000).toBuffer

  measure("first query array")(array.last)
  measure("first query vector")(vector.last)
  measure("first query array buffer")(arrayBuffer.last)

  measure("array")(array.last)
  measure("vector")(vector.last)
  measure("array buffer")(arrayBuffer.last)
