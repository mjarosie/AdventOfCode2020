package example

import scala.io.Source

object Hello extends App {
  val filename = "input.txt"
  val treeRows = Source.fromFile(filename).getLines().toList

  val results = List( treesCount(treeRows, 1, 1)
      , treesCount(treeRows, 3, 1)
      , treesCount(treeRows, 5, 1)
      , treesCount(treeRows, 7, 1)
      , treesCount(treeRows, 1, 2))

  println(results.fold(1)(_*_))

  def treesCount(treeRows: List[String], tobogganSlopeRight: Int, tobogganSlopeDown: Int): Int = {
    val rowsTobogganPassedThrough = treeRows.zipWithIndex.filter{case (treeRow, treeRowIdx) => ((treeRowIdx % tobogganSlopeDown) == 0)}
    val fieldsTobogganWentThrough = rowsTobogganPassedThrough.map{case (treeRow, treeRowIdx) => {
      val tobogganIdx = (treeRowIdx * tobogganSlopeRight) % treeRow.length()
      val fieldValue = treeRow.charAt(tobogganIdx)
      fieldValue
    }}
    
    fieldsTobogganWentThrough.count(_ == '#')
  }
}