package example

import scala.io.Source

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList

  val numbers = lines.map(_.toInt)
  val deviceJoltage = numbers.max + 3
  val numbersSorted = 0 :: numbers.sorted.appended(deviceJoltage)
  val possibleIndexJumps = getPossibleJoltageIndexJumps(numbersSorted)
  val possibleIndexJumpsReversed = possibleIndexJumps.reverse.drop(1)
  val result = possibleIndexJumpsReversed.foldLeft((1.toLong, 1.toLong, 1.toLong))((acc, possibleIndexJumpsForCurrentPosition) => {
    val minusOneAcc = acc._1
    val minusTwoAcc = acc._2
    val minusThreeAcc = acc._3

    val currentPositionPossibleJumpsAccs = possibleIndexJumpsForCurrentPosition.map(jump => {
      jump match {
        case 1 => minusOneAcc
        case 2 => minusTwoAcc
        case 3 => minusThreeAcc
      }
    })

    val currentAcc = currentPositionPossibleJumpsAccs.sum

    (currentAcc, minusOneAcc, minusTwoAcc)
  })

  println(result._1)

  def getPossibleJoltageIndexJumps(joltages: List[Int]): List[List[Int]] = {
    joltages.zipWithIndex.map{case (currentJoltage, currentIdx) => {
      val nextPossibleJoltages = joltages.zipWithIndex.drop(currentIdx + 1).takeWhile {case (j, idx) => j <= currentJoltage + 3}
      nextPossibleJoltages.map{ case (v, idx) => idx - currentIdx }
    }}
  }
}
