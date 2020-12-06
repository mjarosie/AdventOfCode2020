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

  val highestId = ids.sorted(Ordering[Int]).last

  println(highestId)

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