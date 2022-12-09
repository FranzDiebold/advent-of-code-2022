package io.diebold.adventofcode

import io.diebold.adventofcode.Puzzle

object Day01 extends Puzzle[Iterator[Seq[Int]], Int] {
  def splitIterator[T](iter: Iterator[T], separator: T) = new Iterator[Seq[T]] {
    def hasNext = iter.hasNext
    def next()  = iter.takeWhile(_ != separator).toSeq
  }

  def readInput(): Iterator[Seq[Int]] = {
    val rawInput = readRawInput()
      .map(_.toIntOption)

    splitIterator(rawInput, None)
      .map(_.flatten)
  }

  def solveOne(input: Iterator[Seq[Int]]): Int = {
    input
      .map(_.sum)
      .max
  }

  def solveTwo(input: Iterator[Seq[Int]]): Int = {
    input
      .map(_.sum)
      .toSeq
      .sortWith(_ > _)
      .take(3)
      .sum
  }
}
