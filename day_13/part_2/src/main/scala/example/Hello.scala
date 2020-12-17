package example

import scala.io.Source

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList
  val schedule = lines(1).split(",")
  val busIds = schedule.filter(_ != "x").map(_.toLong)

  val maxTimestamp = busIds.foldLeft(1.toLong)((acc, v) => acc * v)


  val applicableSchedule = schedule.zipWithIndex.toList.filter{ case (sch, idx) => sch != "x"}.map{ case (sch, idx) => (sch.toLong, idx)}

  val applicableScheduleSortedByBusIdDecreasing = applicableSchedule.sortBy(_._1)(Ordering[Long].reverse)
  applicableScheduleSortedByBusIdDecreasing.foreach(println)

  val maxBusId = applicableScheduleSortedByBusIdDecreasing(0)._1

  println(maxTimestamp)
  println(maxBusId)
  println(maxTimestamp / maxBusId)
  
  // val initialPossibleValues = Range.Long(applicableScheduleSortedByBusIdDecreasing(0)._1, maxTimestamp, applicableScheduleSortedByBusIdDecreasing(0)._1)
  val initialPossibleValues = maxBusId to maxTimestamp by maxBusId

  // println("initialPossibleValues lenght")
  // println(initialPossibleValues.length)

  // val result = sieve(schedule.zipWithIndex.toList, initialPossibleValues)

  def sieve(schedule: List[(String, Int)], possibleValues: Seq[Long]): Seq[Long] = {
    // printf("possibleValues length: %d\n", possibleValues.length)
    printf("schedules left: %d\n", schedule.length)
    if (schedule.length == 0) possibleValues
    else {
      val (nextSchedule, nextScheduleIdx) = schedule.head

      val filteredPossibleValues = nextSchedule match {
        case "x" => possibleValues
        case sch => {
          possibleValues.filter(v => ((v + nextScheduleIdx) % sch.toLong) == 0)
        }
      }

      sieve(schedule.tail, filteredPossibleValues)
    }
  }

  // result.foreach(println)

  val predicates = schedule.zipWithIndex.flatMap{case (scheduleEntry, idx) => {
    if (scheduleEntry == "x") List.empty
    else {
      val predicate: Long => Boolean = (timestamp: Long) => {
        ((timestamp + idx) % scheduleEntry.toLong) == 0
      }
      List(predicate)
    }
  }}

  // def timestamps(n: Long): Stream[Long] = n #:: timestamps(n + schedule(0).toLong)

  // def checkSchedules(schedules: List[(Long, Int)], timestamp: Long, increment: Long): Long = {
  //   // printf("checking %d\n", t)
  //   val (nextSchedule, nextScheduleIdx) = schedules.head
  //   val predicatesResults = predicates.map(predicate => {
  //     predicate(nextSchedule)
  //   })
  //   val allTrue = predicatesResults.forall(_ == true)
  //   // println(allTrue)
  //   if (allTrue) nextSchedule
  //   else {
  //     val newIncrement = increment * t
  //     checkTimestamp(t + newIncrement, newIncrement)
  //   }
  // }

  // val firstScheduleId = schedule(0).toLong

  // println("start: checkTimestamp")

  // val resultTimestamp = checkSchedules(applicableScheduleSortedByBusIdDecreasing, applicableScheduleSortedByBusIdDecreasing(0))



  // val timestampsStream: Stream[Long] = timestamps(schedule(0).toLong)

  // println("start: predicatesMatches")
  // val predicatesMatches = timestampsStream.map(t => {
  //   val predicatesResults = predicates.map(predicate => {
  //     predicate(t)
  //   })
  //   predicatesResults.forall(_ == true)
  // })

  // println("start: find predicateMatch")
  // val predicateMatch = predicatesMatches.zip(timestampsStream).find{case (m, idx) => {
  //   printf("%d: %s\n", idx, m)
  //   m == true
  // }}
  
  // println(resultTimestamp)

  // val resultTimestamp = timestamps(schedule(0).toLong).dropWhile(t => {
  //   val predicatesResults = predicates.map(predicate => {
  //     predicate(t)
  //   })
  //   printf("predicatesResults for %d:\n", t)
  //   predicatesResults.foreach(println)
  //   val notAllTrue = !predicatesResults.forall(_ == true)
  //   println("notAllTrue")
  //   println(notAllTrue)
  //   notAllTrue
  // })

  // println("resultTimestamp")
  // println(resultTimestamp(0))

  // val (timeWaiting, earliestBusIdIdx) = minutesOfWaiting.zipWithIndex.minBy{_._1}
  // val busId = availableBusIds(earliestBusIdIdx)

  // val result = busId * timeWaiting
  // println("result")
  // println(result)
}