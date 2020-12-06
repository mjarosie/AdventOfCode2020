package example

import scala.io.Source

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList

  val seats = lines.map(line => {
    val row = partition('F', 'B')(0, 127, line.take(7))
    val column = partition('L', 'R')(0, 7, line.drop(7).take(3))
    (row, column)
  })

  val ids = seats.map{ case (row, column) => row * 8 + column }

  val sortedIds = ids.sorted(Ordering[Int])

  val (spot, _) = sortedIds.drop(1).take(sortedIds.length - 2).foldLeft(None: Option[Int], sortedIds(0))((acc, value) => {
    val mySpot = acc._1
    val previousValue = acc._2
    mySpot match {
      case Some(spot) => (Some(spot), value)
      case None => if (value - previousValue == 2) (Some(value - 1), value) else (None, value)
    }
  })

  println(spot)

  def partition(lowerHalfSign: Char, upperHalfSign: Char) (low: Int, high: Int, inputs: String) : Int = {
    if (inputs.length() == 0) {
      return low
    }
    else {
      val nextSign = inputs.head
      val restOfSigns = inputs.tail
      val boundary = (low.toFloat + high) / 2
      nextSign match {
        case x if x == lowerHalfSign => partition(lowerHalfSign, upperHalfSign)(low, boundary.floor.toInt, restOfSigns)
        case x if x == upperHalfSign => partition(lowerHalfSign, upperHalfSign)(boundary.ceil.toInt, high, restOfSigns)
      }
    }
  }
}