package example

import scala.io.Source

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList
  val earliestTimestamp = lines(0).toInt
  val availableBusIds = lines(1).split(",").filter(_ != "x").map(_.toInt)
  val minutesOfWaiting = availableBusIds.map(busId => {
    val arrivalAfterLastBusDeparture = earliestTimestamp % busId
    val minutesOfWaiting = busId - arrivalAfterLastBusDeparture
    minutesOfWaiting
  })

  val (timeWaiting, earliestBusIdIdx) = minutesOfWaiting.zipWithIndex.minBy{_._1}
  val busId = availableBusIds(earliestBusIdIdx)

  val result = busId * timeWaiting
  println("result")
  println(result)
}