package example

import scala.io.Source
import scala.util.matching.Regex

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList

  val bagCapacityRules = lines.map(extractBagCapacityRules).toMap
  val shinyGoldBagsInside = getNumberOfBagInside(bagCapacityRules, "shiny gold")
  println(shinyGoldBagsInside)

  def extractBagCapacityRules(rule: String): (String, Map[String, Int]) = {
    val tokens = rule.split(" bags contain ")
    val bagType = tokens(0)
    val bagMaxContentDescr = tokens.drop(1).mkString(" ")
    val pattern = new Regex("(\\d) (\\w* \\w*) bags?")
    val limitPerBagType = pattern.findAllMatchIn(bagMaxContentDescr).map(m => {
      val limit = m.group(1).toInt
      val bagType = m.group(2)
      (bagType, limit)
    }).toList
    (bagType, limitPerBagType.toMap)
  }

  def getNumberOfBagInside(rules: Map[String, Map[String, Int]], bagTypeToFitIn: String): Int = {
    val bagsAndTheirCounts = rules(bagTypeToFitIn)
    val bagsCount = bagsAndTheirCounts.foldLeft(0)((count, kv) => {
      count + kv._2
    })
    val indirectBagCount = bagsAndTheirCounts.map{ case (k, v) =>
      v * getNumberOfBagInside(rules, k)
    }.sum
    bagsCount + indirectBagCount
  }

}