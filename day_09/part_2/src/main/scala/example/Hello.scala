package example

import scala.io.Source

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList

  val numbers = lines.map(_.toLong)

  val k = 25
  val invalidNumber = 1639024365
  // val invalidNumber = 127

  val range = getFirstRangeSummingTo(numbers, invalidNumber)
  val result = range.min + range.max
  println(result)

  def getFirstRangeSummingTo(
      numbers: List[Long],
      target: Int
  ): List[Long] = {
    getFirstRangeSummingToRec(numbers, target, 0, 1, true)
  }

  def getFirstRangeSummingToRec(
      numbers: List[Long],
      target: Int,
      startIdx: Int,
      endIdx: Int,
      endIdxRising: Boolean
  ): List[Long] = {
    val values = numbers.drop(startIdx).take(endIdx - startIdx)
    val sum = values.sum

    if (sum == target) {
      values
    } else {
      if (endIdxRising && sum < target) {
        getFirstRangeSummingToRec(
          numbers,
          target,
          startIdx,
          endIdx + 1,
          endIdxRising
        )
      } else if (!endIdxRising && sum > target) {
        getFirstRangeSummingToRec(
          numbers,
          target,
          startIdx,
          endIdx - 1,
          endIdxRising
        )
      } else {
        getFirstRangeSummingToRec(
          numbers,
          target,
          startIdx + 1,
          endIdx,
          !endIdxRising
        )
      }
    }
  }
}
