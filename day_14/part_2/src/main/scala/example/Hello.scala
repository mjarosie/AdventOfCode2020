package example

import scala.io.Source
import java.util.HashMap
import scala.collection.immutable.Map.WithDefault
import java.math.BigInteger

sealed trait Instruction

case class SetMask(value: String) extends Instruction
case class SetMemory(index: Long, value: Long) extends Instruction

case class FerryDockingProgram(
    currentMask: String,
    instructions: List[Instruction],
    memory: scala.collection.immutable.HashMap[Long, Long]
) {

  def iterate(): FerryDockingProgram = {
    val nextInstruction = instructions.head

    nextInstruction match {
      case SetMask(value) =>
        FerryDockingProgram(value, instructions.tail, memory)
      case SetMemory(address, value) => {
        val addressesAfterApplyingMask = applyMask(currentMask, address)
        val newMemory = addressesAfterApplyingMask.foldLeft(memory)(
          (memory, addressAfterApplyingMask) => {
            memory.updated(addressAfterApplyingMask, value)
          }
        )
        FerryDockingProgram(currentMask, instructions.tail, newMemory)
      }
    }
  }

  def carryOutInstructions(): FerryDockingProgram = {
    if (instructions.length == 0) this
    else {
      this.iterate().carryOutInstructions()
    }
  }

  def applyMask(mask: String, value: Long): List[Long] = {

    def getBitAt(value: Long, bitIdx: Int): Char = {
      if (((1L << bitIdx) & value) != 0) '1' else '0'
    }

    val valueMaskedWithFloatingBits =
      mask.reverse.zipWithIndex.foldLeft("")((valueSoFar, maskBit) => {
        val maskEntry = maskBit._1
        val maskIndex = maskBit._2
        maskEntry match {
          case '0' => {
            val bit = getBitAt(value, maskIndex)
            valueSoFar.prepended(bit)
          }
          case ch => valueSoFar.prepended(ch)
        }
      })

    getAllFloatingBitCombinations(valueMaskedWithFloatingBits)
  }

  def getAllFloatingBitCombinations(address: String): List[Long] = {
    val nextFloatingBitIdx = address.indexOf('X')
    nextFloatingBitIdx match {
      case -1 => List(new BigInteger(address, 2).longValue())
      case idx => {
        getAllFloatingBitCombinations(
          address.updated(idx, '0')
        ) ::: getAllFloatingBitCombinations(address.updated(idx, '1'))
      }
    }
  }

}

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList

  val instructions = lines.map(parseInstruction)

  val ferryStart = FerryDockingProgram(
    "",
    instructions,
    new scala.collection.immutable.HashMap[Long, Long]()
  )

  val ferryFinished = ferryStart.carryOutInstructions()

  val result = ferryFinished.memory.values.fold(0L)(_ + _)

  println(result)

  def parseInstruction(line: String): Instruction = {
    val keyValue = line.split(" = ")
    val key = keyValue(0)
    val value = keyValue(1)
    key match {
      case "mask" => {
        SetMask(value)
      }
      case mem => {
        val memIdx = mem.drop(4).substring(0, mem.length() - 5).toLong
        SetMemory(memIdx, value.toLong)
      }
    }
  }
}
