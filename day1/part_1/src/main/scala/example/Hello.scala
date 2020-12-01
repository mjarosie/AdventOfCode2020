package example

import scala.io.Source

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList
  val numbers = lines.map(rawInput => rawInput.toInt)
  val length = numbers.length
  val indicesCombinations = getCombinations((0 to length - 1).toList)
  val elementsCombinations = indicesCombinations.map{case (i, j) => (numbers(i), numbers(j))}
  val summingTo2020 = elementsCombinations.filter{case (a, b) => a + b == 2020}
  val pair = summingTo2020.head
  val result = pair._1 * pair._2
  println(result)

  def getCombinations(input: List[Int]): List[(Int, Int)] = {
    if (input.length == 0) 
      return List.empty
    else {
      val headWithRestCombinations = combineHeadWithTail(input)
      val tailCombinations = getCombinations(input.tail)
      return headWithRestCombinations ::: tailCombinations
    }
  }

  def combineHeadWithTail(input: List[Int]): List[(Int, Int)] = {
    val hd::tail = input
    return List.fill(tail.length)(hd).zip(tail)
  }
}
