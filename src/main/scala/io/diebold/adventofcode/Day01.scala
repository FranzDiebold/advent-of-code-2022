package io.diebold.adventofcode

import io.diebold.util.FileReader

object Day01 {
  def splitIterator[T](iter: Iterator[T], separator: T) = new Iterator[Seq[T]] {
    def hasNext = iter.hasNext
    def next()  = iter.takeWhile(_ != separator).toSeq
  }

  def readInput(): Iterator[Seq[Int]] = {
    val rawInput = FileReader
      .readInput(1)
      .map(_.toIntOption)

    splitIterator(rawInput, None)
      .map(_.flatten)
  }

  def solveOne(input: Iterator[Seq[Int]]): Int = {
    input
      .map(_.sum)
      .max
  }

  def partOne(): Int = {
    solveOne(readInput())
  }

  def solveTwo(input: Iterator[Seq[Int]]): Int = {
    input
      .map(_.sum)
      .toSeq
      .sortWith(_ > _)
      .take(3)
      .sum
  }

  def partTwo(): Int = {
    solveTwo(readInput())
  }

  def main(args: Array[String]): Unit = {
    println("Day 1:")

    val resultOne = partOne()
    println(f"The result for part 1 is: $resultOne")

    val resultTwo = partTwo()
    println(f"The result for part 2 is: $resultTwo")
  }
}
