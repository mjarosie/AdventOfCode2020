package example

import scala.io.Source

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList

  val numbers = lines.map(_.toLong)

  val k = 25

  val result = getFirstValueNotSumOfPrevious(numbers, k)

  println(result)

  def getFirstValueNotSumOfPrevious(
      numbers: List[Long],
      windowSize: Int
  ): Long = {
    val sumOfEachNumberWithFollowing =
      numbers.zipWithIndex.take(numbers.length - k).map {
        case (value, index) => {
          val nextNumbers = numbers.drop(index + 1).take(k - 1)
          val sums = nextNumbers.flatMap(n =>
            if (n != value) { List(n + value) }
            else List.empty
          )
          sums
        }
      }

    val numbersSummingUpToPrevious = numbers.zipWithIndex
      .drop(k)
      .map {
        case (value, index) => {
          val previousNumbersSums =
            sumOfEachNumberWithFollowing.drop(index - k).take(k)
          val atLeastOnePairSumsToCurrentValue =
            previousNumbersSums
              .map(sums => sums.contains(value))
              .exists(_ == true)
          atLeastOnePairSumsToCurrentValue
        }
      }

    val indexOfFirstNotSummingUp = numbersSummingUpToPrevious.indexOf(false) + k

    numbers(indexOfFirstNotSummingUp)
  }
}
