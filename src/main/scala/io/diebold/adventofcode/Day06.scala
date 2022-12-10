package io.diebold.adventofcode

import io.diebold.adventofcode.Puzzle

object Day06 extends Puzzle[Seq[Char], Int] {
  def readInput(): Seq[Char] = {
    readRawInput().next.toSeq
  }

  def findMarker(input: Seq[Char], markerSize: Int): Option[Int] = {
    val marker = input
      .sliding(markerSize)
      .zipWithIndex
      .find((possibleMarkerSeq: Seq[Char], idx: Int) => possibleMarkerSeq.toSet.size == markerSize)

    marker match
      case Some(actualMarker) => Some(actualMarker(1) + markerSize)
      case None               => None
  }

  def solveOne(input: Seq[Char]): Int = {
    findMarker(input, 4).getOrElse(-1)
  }

  def solveTwo(input: Seq[Char]): Int = {
    findMarker(input, 14).getOrElse(-1)
  }
}
