package example

import scala.io.Source
import scala.annotation.meta.field

case class Ticket(values: List[Int])

case class TicketFieldRule(name: String, min: Int, max: Int)

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList

  val ticketFieldRulesLines = lines.takeWhile(!_.isEmpty())
  val ticketFieldRules = ticketFieldRulesLines.flatMap(parseTicketFieldRuleLine)

  val otherTicketsLines = lines.drop(ticketFieldRulesLines.length + 5).takeWhile(!_.isEmpty())
  val otherTickets = otherTicketsLines.map(parseTicket)

  val invalidTicketValues = extractInvalidTicketValues(otherTickets, ticketFieldRules)

  val result = invalidTicketValues.fold(0)(_ + _)
  println(result)

  def extractInvalidTicketValues(tickets: List[Ticket], rules: List[TicketFieldRule]): List[Int] = {
    tickets.flatMap(ticket => {
      val ticketValuesNotAdheringToAnyRule = ticket.values.flatMap(ticketValue => {
        val findResult = rules.find(fieldRule => {
          if (fieldRule.min <= ticketValue && ticketValue <= fieldRule.max) true
          else false
        })

        findResult match {
          case None => List(ticketValue)
          case Some(value) => List.empty
        }
      })
      ticketValuesNotAdheringToAnyRule
    })
  }
  
  def parseTicketFieldRuleLine(line: String): List[TicketFieldRule] = {
    val tokens = line.split(":")
    val ticketFieldName = tokens(0)

    val ranges = tokens(1).substring(1).split(" or ")

    val firstRange = ranges(0).split("-")
    val firstRangeMin = firstRange(0).toInt
    val firstRangeMax = firstRange(1).toInt
    val secondRange = ranges(1).split("-")
    val secondRangeMin = secondRange(0).toInt
    val secondRangeMax = secondRange(1).toInt
    List(
      TicketFieldRule(ticketFieldName, firstRangeMin, firstRangeMax),
      TicketFieldRule(ticketFieldName, secondRangeMin, secondRangeMax)
    )
  }

  def parseTicket(line: String): Ticket = {
    Ticket(line.split(",").map(_.toInt).toList)
  }
}