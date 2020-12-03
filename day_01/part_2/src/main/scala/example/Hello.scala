package example

import scala.io.Source

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList
  val numbers = lines.map(rawInput => rawInput.toInt)
  val length = numbers.length
  // println(length)

  val indicesCombinations = combine((0 to length - 1).toList, 3)
  // println(indicesCombinations)

  val elementsCombinations = indicesCombinations.map { idxs =>
    idxs.map(i => numbers(i))
  }
  // println(elementsCombinations)

  val summingTo2020 = elementsCombinations.filter{ values => values.foldLeft(0)(_+_) == 2020}
  val resultValues = summingTo2020.head
  println(resultValues)
  val resultValuesSum = resultValues.foldLeft(1)(_*_)
  println(resultValuesSum)

  // Combination of length n of a list consists of a concatenation of each individual element of this list
  // with each combination of length n-1 of items following this element.
  def combine(input: List[Int], n: Int = 2): List[List[Int]] = {
    if (n == 0) {
      return List(List.empty)
    } else {
      input.zipWithIndex.flatMap { case (e, i) =>
        val followingElements = input.drop(i + 1)
        val combinationsOfFollowingElements = combine(followingElements, n - 1)
        val firstElementWithAllCombinationsOfFollowingElements =
          combinationsOfFollowingElements.map(c => e :: c)
        firstElementWithAllCombinationsOfFollowingElements
      }
    }
  }
}
