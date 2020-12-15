package example

import scala.io.Source

sealed trait NavigationInstruction
sealed trait Direction

case class North(units: Int) extends NavigationInstruction with Direction
case class South(units: Int) extends NavigationInstruction with Direction
case class East(units: Int) extends NavigationInstruction with Direction
case class West(units: Int) extends NavigationInstruction with Direction
case class TurnLeft(degrees: Int) extends NavigationInstruction
case class TurnRight(degrees: Int) extends NavigationInstruction
case class GoForward(units: Int) extends NavigationInstruction

case class Ship(azimuth: Int) {
  def toAzimuthAndDirections(
      instructions: List[NavigationInstruction]
  ): (Ship, List[Direction]) = {
    val result =
      instructions.foldLeft((azimuth, List[Direction]()))(
        (acc, currInstruction) => {
          val currentAzimuth = acc._1
          val directionsSoFar = acc._2
          currInstruction match {
            case TurnLeft(degrees) =>
              (floorMod(currentAzimuth - degrees, 360), directionsSoFar)
            case TurnRight(degrees) => {
              (floorMod(currentAzimuth + degrees, 360), directionsSoFar)
            }
            case GoForward(units) => {
              currentAzimuth match {
                case 0 =>
                  (currentAzimuth, directionsSoFar.appended(North(units)))
                case 90 =>
                  (currentAzimuth, directionsSoFar.appended(East(units)))
                case 180 =>
                  (currentAzimuth, directionsSoFar.appended(South(units)))
                case 270 =>
                  (currentAzimuth, directionsSoFar.appended(West(units)))
              }
            }
            case absoluteDirection: Direction =>
              (currentAzimuth, directionsSoFar.appended(absoluteDirection))
          }
        }
      )

    (new Ship(result._1), result._2)
  }

  def floorMod(a: Int, b: Int): Int = {
    a % b + (if (a < 0) b else 0)
  }
}

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList

  val directions = lines.map(parseLine)
  val shipStart = new Ship(90)
  val (shipFinish, absoluteDirections) =
    shipStart.toAzimuthAndDirections(directions)
  val dist = toManhattanDistance(absoluteDirections)
  println("dist")
  println(dist)

  def toManhattanDistance(directions: List[Direction]): Int = {
    val (horizontalDist, verticalDist) =
      directions.foldLeft((0, 0))((distSoFar, direction) => {
        val horizontalDistSoFar = distSoFar._1
        val verticalDistSoFar = distSoFar._2
        direction match {
          case East(units)  => (horizontalDistSoFar + units, verticalDistSoFar)
          case West(units)  => (horizontalDistSoFar - units, verticalDistSoFar)
          case North(units) => (horizontalDistSoFar, verticalDistSoFar + units)
          case South(units) => (horizontalDistSoFar, verticalDistSoFar - units)
        }
      })

    horizontalDist.abs + verticalDist.abs
  }

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
