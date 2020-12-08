package example

import scala.io.Source

trait Instruction

case class Nop(value: Int) extends Instruction
case class Acc(diff: Int) extends Instruction
case class Jmp(offset: Int) extends Instruction

case class Console(instructions: List[Instruction], accumulator: Int, instructionPointer: Int, invokedInstructions: Set[Int], inInfiniteLoop: Boolean) {
  def invokeNextInstruction(): Console = {
    val nowInInfiniteLoop = if (invokedInstructions.contains(instructionPointer)) true else false
    instructions(instructionPointer) match {
      case Nop(value) => new Console(instructions = instructions, accumulator, instructionPointer + 1, invokedInstructions.incl(instructionPointer), nowInInfiniteLoop)
      case Acc(diff) => new Console(instructions, accumulator + diff, instructionPointer + 1, invokedInstructions.incl(instructionPointer), nowInInfiniteLoop)
      case Jmp(offset) => new Console(instructions, accumulator, instructionPointer + offset, invokedInstructions.incl(instructionPointer), nowInInfiniteLoop)
    }
  }

  def terminated: Boolean = {
    instructionPointer == instructions.length
  }
}

object Hello extends App {
  val filename = "input.txt"
  val lines = Source.fromFile(filename).getLines().toList
  val instructions = lines.map(parseInstruction)

  val repeatedInstructions = List.fill(instructions.length)(instructions)

  val instructionsMutations = repeatedInstructions.zipWithIndex.flatMap{ case (instructions, changeIdx) => {
    instructions(changeIdx) match {
      case Nop(value) => List(instructions.updated(changeIdx, Jmp(value)))
      case Jmp(value) => List(instructions.updated(changeIdx, Nop(value)))
      case Acc(diff) => List.empty
    }
  }}

  val consoleStates = (instructions :: instructionsMutations).map(instructions => {
    val initialConsoleState = new Console(instructions, 0, 0, Set.empty, false)
    runUntilTerminatedOrAfterEndingUpInLoop(initialConsoleState)
  })

  val fixedConsole = consoleStates.filter(c => !c.inInfiniteLoop).head
  println(fixedConsole.accumulator)

  def parseInstruction(line: String): Instruction = {
    val tokens = line.split(" ")
    val rawOp = tokens(0)
    val rawValue = tokens(1)
    val value = if (rawValue(0) == '-') -(rawValue.drop(1).toInt) else rawValue.drop(1).toInt
    val op = rawOp match {
      case "nop" => Nop(value)
      case "acc" => Acc(value)
      case "jmp" => Jmp(value)
    }
    op
  }

  def runUntilTerminatedOrAfterEndingUpInLoop(console: Console): Console = {
    val newConsoleState = console.invokeNextInstruction()
    if (newConsoleState.inInfiniteLoop || newConsoleState.terminated) newConsoleState
    else runUntilTerminatedOrAfterEndingUpInLoop(newConsoleState)
  }
}