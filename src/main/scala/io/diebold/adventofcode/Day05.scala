package io.diebold.adventofcode

import scala.collection.mutable.Stack

import io.diebold.adventofcode.Puzzle
import io.diebold.util.SplitIterator

object Day05 extends Puzzle[(Seq[Stack[Char]], Seq[(Int, Int, Int)]), String] {
  def readInput(): (Seq[Stack[Char]], Seq[(Int, Int, Int)]) = {
    val rawInput = readRawInput()

    val inputParts = SplitIterator
      .split(rawInput, "")

    val initialStacksInput = inputParts.next
    val initialStacksWidth = initialStacksInput.last.length
    val crateLocations     = Seq.range(1, initialStacksWidth, 4)
    val initialStacks: Seq[Stack[Char]] = initialStacksInput
      .dropRight(1)
      .reverse
      .map((stacksLayer: String) =>
        crateLocations
          .map((crateLocation: Int) => stacksLayer(crateLocation))
      )
      .transpose
      .map((stackList: Seq[Char]) => stackList.filterNot(_ == ' '))
      .map((stackList: Seq[Char]) => Stack[Char]().pushAll(stackList))

    val rearrangementProcedure: Seq[(Int, Int, Int)] = inputParts.next
      .map((step: String) =>
        step match
          case s"move ${count} from ${source} to ${target}" =>
            (count.toInt, source.toInt, target.toInt)
      )

    (initialStacks, rearrangementProcedure)
  }

  def solveOne(input: (Seq[Stack[Char]], Seq[(Int, Int, Int)])): String = {
    val stacks                 = input(0)
    val rearrangementProcedure = input(1)

    rearrangementProcedure
      .foreach((count: Int, source: Int, target: Int) =>
        1.to(count)
          .foreach(_ => stacks(target - 1).push(stacks(source - 1).pop()))
      )

    stacks
      .map(_.top)
      .mkString
  }

  def solveTwo(input: (Seq[Stack[Char]], Seq[(Int, Int, Int)])): String = {
    val stacks                 = input(0)
    val rearrangementProcedure = input(1)

    rearrangementProcedure
      .foreach((count: Int, source: Int, target: Int) =>
        val cratesToMove = 1
          .to(count)
          .map(_ => stacks(source - 1).pop())
        stacks(target - 1).pushAll(cratesToMove.reverse)
      )

    stacks
      .map(_.top)
      .mkString
  }
}
