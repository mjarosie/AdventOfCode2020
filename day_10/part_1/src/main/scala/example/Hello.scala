package example

import scala.io.Source

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList

  val numbers = lines.map(_.toInt)
  val deviceJoltage = numbers.max + 3
  val numbersSorted = 0 :: numbers.sorted.appended(deviceJoltage)
  val joltDiffs = numbersSorted.zipWithIndex.drop(1).foldLeft((0, 0, 0))((acc, numAndIdx) => {
    val value = numAndIdx._1
    val idx = numAndIdx._2
    val joltDiff = value - numbersSorted(idx - 1)
    joltDiff match {
      case 1 => (acc._1 + 1, acc._2, acc._3)
      case 2 => (acc._1, acc._2 + 1, acc._3)
      case 3 => (acc._1, acc._2, acc._3 + 1)
    }
  })
  println(joltDiffs._1 * (joltDiffs._3))
}