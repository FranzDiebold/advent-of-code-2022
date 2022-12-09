package io.diebold.adventofcode

import io.diebold.adventofcode.Puzzle

object Day03 extends Puzzle[String, Int] {
  def readInput(): Iterator[String] = {
    readRawInput()
  }

  def getPriority(item: Char): Int = item.toInt - (if (item.isUpper) 38 else 96)

  def solveOne(input: Iterator[String]): Int = {
    input
      .map((rucksackItems: String) =>
        (
          rucksackItems.slice(0, rucksackItems.length / 2),
          rucksackItems.slice(rucksackItems.length / 2, rucksackItems.length)
        )
      )
      .map((firstCompartmentString: String, secondCompartmentString: String) =>
        firstCompartmentString.toSet.intersect(secondCompartmentString.toSet)
      )
      .map(_.head)
      .map(getPriority)
      .sum
  }

  def solveTwo(input: Iterator[String]): Int = {
    input
      .grouped(3)
      .map((rucksackGroup: Seq[String]) =>
        rucksackGroup.map(_.toSet).reduceLeft((a: Set[Char], b: Set[Char]) => a.intersect(b))
      )
      .map(_.head)
      .map(getPriority)
      .sum
  }
}
