package io.diebold.adventofcode

import io.diebold.util.FileReader

trait Puzzle[I, O] {
  val day = this.getClass.getSimpleName().stripSuffix("$")

  def readRawInput(): Iterator[String] = {
    FileReader.readInput(day)
  }

  def readInput(): I

  def solveOne(input: I): O

  def partOne(): O = {
    solveOne(readInput())
  }

  def solveTwo(input: I): O

  def partTwo(): O = {
    solveTwo(readInput())
  }

  def main(args: Array[String]): Unit = {
    println(day)

    val resultOne = partOne()
    println(f"The result for part 1 is: $resultOne")

    val resultTwo = partTwo()
    println(f"The result for part 2 is: $resultTwo")
  }
}
