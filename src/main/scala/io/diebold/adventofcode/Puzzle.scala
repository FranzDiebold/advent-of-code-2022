package io.diebold.adventofcode

trait Puzzle[I, O] {
  def readInput(): Iterator[I]

  def solveOne(input: Iterator[I]): O

  def partOne(): O = {
    solveOne(readInput())
  }

  def solveTwo(input: Iterator[I]): O

  def partTwo(): O = {
    solveTwo(readInput())
  }

  def main(args: Array[String]): Unit = {
    println("Puzzle:")

    val resultOne = partOne()
    println(f"The result for part 1 is: $resultOne")

    val resultTwo = partTwo()
    println(f"The result for part 2 is: $resultTwo")
  }
}
