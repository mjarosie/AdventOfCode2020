package example

import scala.io.Source

case class Rule(val firstPos: Int, val secondPos: Int, character: Char)

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList
  val rulesAndPasswords = lines.map(rawInput => {
    val tokens = rawInput.split(" ")
    val positions = tokens(0).split("-")
    val fstPos = positions(0).toInt
    val sndPos = positions(1).toInt
    val character = tokens(1).toCharArray()(0)
    val password = tokens(2)
    (new Rule(fstPos, sndPos, character), password)
  })
  
  val pwdsMatchesRule = rulesAndPasswords.map{case (rule, pwd) => {
    val firstPosMatch = pwd(rule.firstPos - 1) == rule.character
    val secondPosMatch = pwd(rule.secondPos - 1) == rule.character
    val bothPosMatch = firstPosMatch && secondPosMatch
    (firstPosMatch || secondPosMatch) && !(bothPosMatch)
  }}

  val matchingPwdsCount = pwdsMatchesRule.count(x => x)
  println(matchingPwdsCount)
}