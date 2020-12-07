package example

import scala.io.Source
import scala.util.matching.Regex

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList

  val bagCapacityRules = lines.map(extractBagCapacityRules).toMap
  val matchingShinyGold = getAllMatchingBagTypes(bagCapacityRules, "shiny gold")
  println(matchingShinyGold.size)

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

  def matchingBagTypes(rules: Map[String, Map[String, Int]], bagTypeToFitIn: String): Set[String] = {
    val bagsFittingThisBag = rules.foldLeft(List[String]())((matchingBagsSoFar, bagRule) => {
      val currentBag = bagRule._1
      val currentBagRules = bagRule._2

      if (currentBagRules.keySet.contains(bagTypeToFitIn)) currentBag :: matchingBagsSoFar else matchingBagsSoFar
    }).toSet

    bagsFittingThisBag
  }

  def getAllMatchingBagTypes(rules: Map[String, Map[String, Int]], bagTypeToFitIn: String): Set[String] = {
    val directBagMatches = matchingBagTypes(rules, bagTypeToFitIn)
    val indirectBagMatches = directBagMatches.flatMap(m => getAllMatchingBagTypes(rules, m))
    directBagMatches.union(indirectBagMatches)
  }
}