package io.diebold.adventofcode

import io.diebold.adventofcode.Puzzle
import io.diebold.util.FileReader

object Day04 extends Puzzle[((Int, Int), (Int, Int)), Int] {
  def readInput(): Iterator[((Int, Int), (Int, Int))] = {
    FileReader
      .readInput(4)
      .map(_.split(",").map(_.split("-").map(_.toInt)))
      .map((sectionAssignments: Array[Array[Int]]) =>
        (
          (sectionAssignments(0)(0), sectionAssignments(0)(1)),
          (sectionAssignments(1)(0), sectionAssignments(1)(1))
        )
      )
  }

  def solveOne(input: Iterator[((Int, Int), (Int, Int))]): Int = {
    def isCompletelyOverlapping(
        sectionAssignment1: (Int, Int),
        sectionAssignment2: (Int, Int)
    ): Boolean = {
      (sectionAssignment1(0) >= sectionAssignment2(0) && sectionAssignment1(
        1
      ) <= sectionAssignment2(1))
      ||
      (sectionAssignment1(0) <= sectionAssignment2(0) && sectionAssignment1(
        1
      ) >= sectionAssignment2(1))
    }

    input
      .filter(isCompletelyOverlapping)
      .length
  }

  def solveTwo(input: Iterator[((Int, Int), (Int, Int))]): Int = {
    def isOverlapping(
        sectionAssignment1: (Int, Int),
        sectionAssignment2: (Int, Int)
    ): Boolean = {
      !(sectionAssignment1(1) < sectionAssignment2(0) || sectionAssignment1(0) > sectionAssignment2(
        1
      ))
    }

    input
      .filter(isOverlapping)
      .length
  }
}
