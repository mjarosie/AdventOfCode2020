package example

import scala.io.Source

object Hello extends App {
  val filename = "input.txt"
  val peopleAnswers = Source.fromFile(filename).getLines().toList

  val answersPerGroup = extractAnswersPerGroup(peopleAnswers)

  val combinedAnswersPerGroup = answersPerGroup.map((groupAnswers: List[String]) => {
    groupAnswers.foldLeft("")((allGroupAnswers, personAnswers) => personAnswers ++ allGroupAnswers)
  })

  val uniqueAnswersPerGroup = combinedAnswersPerGroup.map(_.toSet)

  val numberOfUniqueAnswersPerGroup = uniqueAnswersPerGroup.map(_.size)

  val sumOfGroupsUniqueAnswers = numberOfUniqueAnswersPerGroup.sum
  println(sumOfGroupsUniqueAnswers)

  def extractAnswersPerGroup(peopleAnswers: List[String]): List[List[String]] = {
    val (lastGroupAnswers, answers) = peopleAnswers.foldLeft((List.empty: List[String], List.empty: List[List[String]]))((acc, v: String) => {
      val currentGroupAnswers = acc._1
      val groupAnswersSoFar = acc._2
      val nextAccumulatorValue = v match {
        case "" => (List.empty: List[String], currentGroupAnswers :: groupAnswersSoFar)
        case x => (x :: currentGroupAnswers, groupAnswersSoFar)
      }
      nextAccumulatorValue
    })

    lastGroupAnswers :: answers
  }
}