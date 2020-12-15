package example

import scala.io.Source

sealed trait NavigationInstruction

case class North(units: Int) extends NavigationInstruction
case class South(units: Int) extends NavigationInstruction
case class East(units: Int) extends NavigationInstruction
case class West(units: Int) extends NavigationInstruction
case class TurnLeft(degrees: Int) extends NavigationInstruction
case class TurnRight(degrees: Int) extends NavigationInstruction
case class GoForward(units: Int) extends NavigationInstruction

case class Ship(
    positionX: Int,
    positionY: Int,
    waypointHorizontalDistance: Int,
    waypointVerticalDistance: Int
) {
  def navigateWithInstructions(
      instructions: List[NavigationInstruction]
  ): Ship = {
    if (instructions.isEmpty) this
    else {
      val nextInstruction = instructions.head
      val nextShipState = nextInstruction match {
        case North(units) =>
          Ship(
            positionX,
            positionY,
            waypointHorizontalDistance,
            waypointVerticalDistance + units
          )
        case South(units) =>
          Ship(
            positionX,
            positionY,
            waypointHorizontalDistance,
            waypointVerticalDistance - units
          )
        case East(units) =>
          Ship(
            positionX,
            positionY,
            waypointHorizontalDistance + units,
            waypointVerticalDistance
          )
        case West(units) =>
          Ship(
            positionX,
            positionY,
            waypointHorizontalDistance - units,
            waypointVerticalDistance
          )
        case GoForward(units) =>
          Ship(
            positionX + (units * waypointHorizontalDistance),
            positionY + (units * waypointVerticalDistance),
            waypointHorizontalDistance,
            waypointVerticalDistance
          )
        case TurnLeft(degrees) => {
          degrees match {
            case 90 =>
              Ship(
                positionX,
                positionY,
                -waypointVerticalDistance,
                waypointHorizontalDistance
              )
            case 180 =>
              Ship(
                positionX,
                positionY,
                -waypointHorizontalDistance,
                -waypointVerticalDistance
              )
            case 270 =>
              Ship(
                positionX,
                positionY,
                waypointVerticalDistance,
                -waypointHorizontalDistance
              )
          }
        }
        case TurnRight(degrees) => {
          degrees match {
            case 90 =>
              Ship(
                positionX,
                positionY,
                waypointVerticalDistance,
                -waypointHorizontalDistance
              )
            case 180 =>
              Ship(
                positionX,
                positionY,
                -waypointHorizontalDistance,
                -waypointVerticalDistance
              )
            case 270 =>
              Ship(
                positionX,
                positionY,
                -waypointVerticalDistance,
                waypointHorizontalDistance
              )
          }
        }
      }
      nextShipState.navigateWithInstructions(instructions.tail)
    }
  }

  def floorMod(a: Int, b: Int): Int = {
    a % b + (if (a < 0) b else 0)
  }
}

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList

  val instructions = lines.map(parseLine)

  val shipStart = new Ship(0, 0, 10, 1)
  val shipFinish = shipStart.navigateWithInstructions(instructions)
  val dist = shipFinish.positionX.abs + shipFinish.positionY.abs
  println("dist")
  println(dist)

  def parseLine(line: String): NavigationInstruction = {
    line(0) match {
      case 'N' => North(line.drop(1).toInt)
      case 'S' => South(line.drop(1).toInt)
      case 'E' => East(line.drop(1).toInt)
      case 'W' => West(line.drop(1).toInt)
      case 'L' => TurnLeft(line.drop(1).toInt)
      case 'R' => TurnRight(line.drop(1).toInt)
      case 'F' => GoForward(line.drop(1).toInt)
    }
  }
}
