package example

import scala.io.Source

case class Rule(val min: Int, val max: Int, character: Char)

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList
  val rulesAndPasswords = lines.map(rawInput => {
    val tokens = rawInput.split(" ")
    val minMax = tokens(0).split("-")
    val min = minMax(0).toInt
    val max = minMax(1).toInt
    val character = tokens(1).toCharArray()(0)
    val password = tokens(2)
    (new Rule(min, max, character), password)
  })

  val pwdsMatchesRule = rulesAndPasswords.map{case (rule, pwd) => {
    val characterCount = pwd.count(_ == rule.character)
    rule.min <= characterCount && characterCount <= rule.max
  }}

  val matchingPwdsCount = pwdsMatchesRule.count(x => x)
  println(matchingPwdsCount)
}