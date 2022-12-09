package io.diebold.adventofcode

import io.diebold.adventofcode.Puzzle
import io.diebold.util.SplitIterator

object Day01 extends Puzzle[Iterator[Seq[Int]], Int] {
  def readInput(): Iterator[Seq[Int]] = {
    val rawInput = readRawInput()
      .map(_.toIntOption)

    SplitIterator
      .split(rawInput, None)
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
