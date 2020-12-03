package example

import scala.io.Source

object Hello extends App {
  val filename = "input.txt"
  val treeRows = Source.fromFile(filename).getLines().toList

  val fieldsTobogganWentThrough = treeRows.zipWithIndex.map{case (treeRow, treeRowIdx) => {
    val tobogganIdx = (treeRowIdx * 3) % treeRow.length()
    val fieldValue = treeRow.charAt(tobogganIdx)
    fieldValue
  }}
  
  val treesCount = fieldsTobogganWentThrough.count(_ == '#')

  println(treesCount)
}