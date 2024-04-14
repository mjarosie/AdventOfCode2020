package example

import scala.io.Source

sealed trait State

case class Active() extends State
case class InActive() extends State

case class PocketDimension(states: Map[(Int, Int, Int), State])

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList
  val initialYSize = lines.length
  val initialXSize = lines(0).length

  val xs = Range(0, initialXSize).map(x => x - initialXSize / 2).toList
  val ys = Range(0, initialYSize).map(y=> y - initialYSize / 2).toList

  val xD = ys.map(y => xs.map(x => (x, y, 0))).flatten
  xD.foreach(println)

  def simulateNextStep(current: PocketDimension) : PocketDimension = {
    val newStates = current.states.keySet.map{ case (x, y, z) => {
      val surroundingXs = Range(-1, 1).map(i => i - x).toList
      val surroundingYs = Range(-1, 1).map(j => j - y).toList
      val surroundingZs = Range(-1, 1).map(k => k - z).toList

      val surroundingIndices = surroundingZs.map(z => {
        surroundingYs.map(y => {
          surroundingXs.map(x => {
            (x, y, z)
          })
        }).flatten
      }).flatten

      surroundingIndices.map{ case (x, y, z) => {

      }}

    }}

    PocketDimension(newStates)
  }
}