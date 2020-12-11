package example

import scala.io.Source

trait Position

case class EmptySeat() extends Position {
  override def toString(): String = "L"
}
case class OccupiedSeat() extends Position {
  override def toString(): String = "#"
}
case class Floor() extends Position {
  override def toString(): String = "."
}

sealed trait Direction

case class Left() extends Direction
case class Right() extends Direction
case class Above() extends Direction
case class Below() extends Direction
case class AboveLeft() extends Direction
case class AboveRight() extends Direction
case class BelowLeft() extends Direction
case class BelowRight() extends Direction

case class SeatLayout(positions: Vector[Vector[Position]]) {

  def iterateUntilStable(): SeatLayout = {
    def iterateUntilStableRec(layout: SeatLayout): SeatLayout = {
      val newLayout = layout.iteration()
      if (newLayout != layout) iterateUntilStableRec(newLayout)
      else newLayout
    }

    iterateUntilStableRec(this)
  }

  def iteration(): SeatLayout = {
    val newPositions = positions.zipWithIndex.map {
      case (positionsRow, y) => {
        positionsRow.zipWithIndex.map {
          case (position, x) => {
            val adjacentPositions =
              getNearestPossiblyOccupiedAdjacentPositions(x, y)

              val occupiedAdjacentPositionsCount = adjacentPositions.count(p => p match {
                case OccupiedSeat() => true
                case _ => false
              })

            position match {
              case EmptySeat() if (occupiedAdjacentPositionsCount == 0) =>
                OccupiedSeat()
              case OccupiedSeat() if (occupiedAdjacentPositionsCount >= 5) =>
                EmptySeat()
              case x => x
            }
          }
        }
      }
    }

    SeatLayout(newPositions)
  }

  def getOccupiedSeatsCount(): Int = {
    positions.foldLeft(0)((occupiedCount, positionsRow) => {
      val currentRowCount = positionsRow.count(p => {
        p match {
          case OccupiedSeat() => true
          case _              => false
        }
      })

      occupiedCount + currentRowCount
    })
  }

  def getPosition(x: Int, y: Int): Position = {
    positions(y)(x)
  }

  def getNearestPossiblyOccupiedAdjacentPositions(x: Int, y: Int): List[Position] = {
    List(
      getNearestPossiblyOccupiedPosition(x, y, Above()),
      getNearestPossiblyOccupiedPosition(x, y, Below()),
      getNearestPossiblyOccupiedPosition(x, y, Left()),
      getNearestPossiblyOccupiedPosition(x, y, Right()),
      getNearestPossiblyOccupiedPosition(x, y, AboveLeft()),
      getNearestPossiblyOccupiedPosition(x, y, AboveRight()),
      getNearestPossiblyOccupiedPosition(x, y, BelowLeft()),
      getNearestPossiblyOccupiedPosition(x, y, BelowRight())
    )
  }

  def getNearestPossiblyOccupiedPosition(
      x: Int,
      y: Int,
      dir: Direction
  ): Position = {
    val nextX = dir match {
      case Left() | AboveLeft() | BelowLeft()    => x - 1
      case Right() | AboveRight() | BelowRight() => x + 1
      case _                                     => x
    }
    val nextY = dir match {
      case Above() | AboveLeft() | AboveRight() => y - 1
      case Below() | BelowRight() | BelowLeft() => y + 1
      case _                                    => y
    }

    if (
      nextX < 0 || nextX >= positions(0).length ||
      nextY < 0 || nextY >= positions.length
    ) Floor()
    else {
      val nextPos = getPosition(nextX, nextY)
      nextPos match {
        case Floor() => getNearestPossiblyOccupiedPosition(nextX, nextY, dir)
        case _ => nextPos
      }
    }
  }
}

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList
  val positions = lines
    .map(line => {
      line.map(readPosition).toVector
    })
    .toVector

  val finalLayout = SeatLayout(positions).iterateUntilStable()
  val seatsOccupied = finalLayout.getOccupiedSeatsCount()
  println(seatsOccupied)

  def readPosition(symbol: Char): Position = {
    symbol match {
      case 'L' => EmptySeat()
      case '#' => OccupiedSeat()
      case '.' => Floor()
    }
  }
}
