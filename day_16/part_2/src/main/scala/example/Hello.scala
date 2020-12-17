package example

import scala.io.Source
import scala.annotation.meta.field

case class Ticket(values: List[Int])

case class Range(min: Int, max: Int)

case class TicketFieldRule(name: String, ranges: List[Range])

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList

  val ticketFieldRulesLines = lines.takeWhile(!_.isEmpty())
  val ticketFieldRules = ticketFieldRulesLines.map(parseTicketFieldRuleLine)

  val myTicketLine = lines.drop(ticketFieldRulesLines.length + 2)(0)
  val myTicket = parseTicket(myTicketLine)

  val otherTicketsLines =
    lines.drop(ticketFieldRulesLines.length + 5).takeWhile(!_.isEmpty())
  val otherTickets = otherTicketsLines.map(parseTicket)

  val validTickets =
    myTicket :: getOnlyValidTickets(otherTickets, ticketFieldRules)

  val fieldNames = getTicketFieldNames(validTickets, ticketFieldRules)

  val departureFieldsIndices = fieldNames.zipWithIndex
    .filter {
      case (fieldName, fieldIdx) => {
        fieldName.startsWith("departure")
      }
    }
    .map(_._2)

  val departureFieldValues =
    departureFieldsIndices.map(idx => myTicket.values(idx))

  val result = departureFieldValues.foldLeft(1L)(_ * _)
  println(result)

  def getOnlyValidTickets(
      tickets: List[Ticket],
      rules: List[TicketFieldRule]
  ): List[Ticket] = {
    tickets.filter(ticket => {
      val ticketValuesNotAdheringToAnyRule =
        ticket.values.flatMap(ticketValue => {
          val findResult = rules.find(fieldRule => {
            fieldRule.ranges.exists(range => {
              range.min <= ticketValue && ticketValue <= range.max
            })
          })

          findResult match {
            case None        => List(ticketValue)
            case Some(value) => List.empty
          }
        })
      if (ticketValuesNotAdheringToAnyRule.length == 0) true
      else false
    })
  }

  def getTicketFieldNames(
      tickets: List[Ticket],
      rules: List[TicketFieldRule]
  ): List[String] = {
    val numberOfFieldsPerTicket = tickets(0).values.length
    val uniqueFieldNames = ticketFieldRules.map(rule => rule.name).toSet

    val rulesAdheringToFields = (0 to numberOfFieldsPerTicket - 1)
      .map(fieldIdx => {
        // println("fieldIdx")
        // println(fieldIdx)

        val ticketsFieldValues = tickets.map(ticket => ticket.values(fieldIdx))

        // println("ticketsFieldValues")
        // println(ticketsFieldValues)

        val rulesAdheringToTheField = rules.filter(rule => {
          ticketsFieldValues.forall(ticketFieldValue => {
            rule.ranges.exists(range => {
              range.min <= ticketFieldValue && ticketFieldValue <= range.max
            })
          })
        })

        // printf("rulesAdheringToTheField %d\n", fieldIdx)
        // println(rulesAdheringToTheField)

        (fieldIdx, rulesAdheringToTheField.toSet)
      })
      .toMap[Int, Set[TicketFieldRule]]

    val prunedRules = pruneRules(rulesAdheringToFields)

    prunedRules.toSeq.sortBy(_._1).map(prunedRule => prunedRule._2.name).toList
  }

  def pruneRules(
      possibleRules: Map[Int, Set[TicketFieldRule]]
  ): Map[Int, TicketFieldRule] = {
    // println("possibleRules")
    // println(possibleRules)
    if (possibleRules.forall(ruleSet => ruleSet._2.size == 1)) {
      // println("possibleRules finished!")
      possibleRules.map { case (k, v) => (k, v.head) }
    } else {
      val firstFieldWithSinglePossibleRule = possibleRules.find {
        case (fieldIdx, possibleRuleSet) => {
          possibleRuleSet.size == 1
        }
      }.get

      val singleRuleIdx = firstFieldWithSinglePossibleRule._1
      val singleRule = firstFieldWithSinglePossibleRule._2.head

      val possibleRulesWithoutSingleRuleAppearingElsewhere =
        possibleRules.removed(singleRuleIdx).map {
          case (k, v) => {
            (k, v - singleRule)
          }
        }

      val otherRulesPruned = pruneRules(
        possibleRulesWithoutSingleRuleAppearingElsewhere
      )

      otherRulesPruned.updated(singleRuleIdx, singleRule)
    }
  }

  def parseTicketFieldRuleLine(line: String): TicketFieldRule = {
    val tokens = line.split(":")
    val ticketFieldName = tokens(0)

    val ranges = tokens(1).substring(1).split(" or ")

    val firstRange = ranges(0).split("-")
    val firstRangeMin = firstRange(0).toInt
    val firstRangeMax = firstRange(1).toInt
    val secondRange = ranges(1).split("-")
    val secondRangeMin = secondRange(0).toInt
    val secondRangeMax = secondRange(1).toInt
    TicketFieldRule(
      ticketFieldName,
      List(
        Range(firstRangeMin, firstRangeMax),
        Range(secondRangeMin, secondRangeMax)
      )
    )
  }

  def parseTicket(line: String): Ticket = {
    Ticket(line.split(",").map(_.toInt).toList)
  }
}
