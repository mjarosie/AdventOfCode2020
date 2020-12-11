package example

import scala.io.Source

trait Position

case class EmptySeat() extends Position {
  override def toString() : String = "L"
}
case class OccupiedSeat() extends Position {
  override def toString() : String = "#"
}
case class Floor() extends Position {
  override def toString() : String = "."
}

case class SeatLayout(positions: Vector[Vector[Position]]) {

  def iterateUntilStable(): SeatLayout = {
    def iterateUntilStableRec(layout: SeatLayout) : SeatLayout = {
      val newLayout = layout.iteration()
      if (newLayout != layout) iterateUntilStableRec(newLayout)
      else newLayout
    }

    iterateUntilStableRec(this)
  }

  def iteration(): SeatLayout = {
    val newPositions = positions.zipWithIndex.map { case (positionsRow, y) => {
        positionsRow.zipWithIndex.map {case (position, x) => {
          val adjacentPositions = getAdjacentPositions(x, y)
          val occupiedAdjacentPositions = adjacentPositions.count(p => {
            p match {
              case OccupiedSeat() => true
              case _ => false
            }
          })

          position match {
            case EmptySeat() if (occupiedAdjacentPositions == 0) =>  OccupiedSeat()
            case OccupiedSeat() if (occupiedAdjacentPositions >= 4) => EmptySeat()
            case x => x
          }
        }}
    }}

    SeatLayout(newPositions)
  }
 
  def getOccupiedSeatsCount(): Int = {
    positions.foldLeft(0)((occupiedCount, positionsRow) => {
      val currentRowCount = positionsRow.count(p => {
        p match {
          case OccupiedSeat() => true
          case _ => false
        }
      })

      occupiedCount + currentRowCount
    })
  }

  def getPosition(x: Int, y: Int): Position = {
    positions(y)(x)
  }

  def getAdjacentPositions(x: Int, y: Int): List[Position] = {
    val adjacentIndices = for {
      i <- x - 1 to x + 1;
      j <- y - 1 to y + 1
    } yield { (i, j) }

    val adjacentIndicesWithinBoundaries = adjacentIndices.filter {
      case (i, j) => {
        i >= 0 && i < positions(0).length &&
        j >= 0 && j < positions.length &&
        !(i == x && j == y)
      }
    }

    adjacentIndicesWithinBoundaries.map {
      case (i, j) => {
        positions(j)(i)
      }
    }.toList
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
