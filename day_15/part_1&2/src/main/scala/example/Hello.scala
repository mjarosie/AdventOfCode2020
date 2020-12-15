package example

import scala.io.Source

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList

  val resultNumberSpokenIndex = 2020
  // val resultNumberSpokenIndex = 30000000

  val numbers = lines(0).split(",").map(_.toInt)

  val result = processNumber(
    numbers.last,
    Map.from(numbers.take(numbers.length - 1).zipWithIndex),
    numbers.length
  ).take(resultNumberSpokenIndex - numbers.length).last

  println(result)

  def processNumber(
      previous: Int,
      numbersLastSpoken: Map[Int, Int],
      currentTurn: Int
  ): LazyList[Int] = {
    val nextSpokenNumber =
      if (numbersLastSpoken.contains(previous))
        currentTurn - 1 - numbersLastSpoken.get(previous).get
      else 0

    LazyList.cons(
      nextSpokenNumber,
      processNumber(
        nextSpokenNumber,
        numbersLastSpoken.updated(previous, currentTurn - 1),
        currentTurn + 1
      )
    )
  }

}
