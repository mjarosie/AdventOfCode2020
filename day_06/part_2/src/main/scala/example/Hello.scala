package example

import scala.io.Source

object Hello extends App {
  val filename = "input.txt"
  val peopleAnswers = Source.fromFile(filename).getLines().toList

  val answersPerGroup = extractAnswersPerGroup(peopleAnswers)

  val unanimousAnswersPerGroup = answersPerGroup.map((groupAnswers: List[String]) => {
    groupAnswers.foldLeft(groupAnswers(0).toSet)((equivocalGroupAnswersSoFar, personAnswers) => {
      equivocalGroupAnswersSoFar.intersect(personAnswers.toSet)
    })
  })

  val numberOfUnanimousAnswersPerGroup = unanimousAnswersPerGroup.map(_.size)

  val sumOfGroupsUnanimousAnswers = numberOfUnanimousAnswersPerGroup.sum
  
  println(sumOfGroupsUnanimousAnswers)

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