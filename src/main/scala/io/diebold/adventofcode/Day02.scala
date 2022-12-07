package io.diebold.adventofcode

import io.diebold.adventofcode.Puzzle
import io.diebold.util.FileReader

enum RockPaperScissorsShape:
  case Rock, Paper, Scissors

object Day02 extends Puzzle[(RockPaperScissorsShape, String), Int] {
  def decodeFirstColumn(encodedShape: String): RockPaperScissorsShape = {
    encodedShape match
      case "A" => RockPaperScissorsShape.Rock
      case "B" => RockPaperScissorsShape.Paper
      case "C" => RockPaperScissorsShape.Scissors
  }

  def readInput(): Iterator[(RockPaperScissorsShape, String)] = {
    FileReader
      .readInput(2)
      .map(_.split(" "))
      .map((encodedColumns: Array[String]) =>
        (decodeFirstColumn(encodedColumns(0)), encodedColumns(1))
      )
  }

  def getResponseScore(responseShape: RockPaperScissorsShape): Int = {
    responseShape match
      case RockPaperScissorsShape.Rock     => 1
      case RockPaperScissorsShape.Paper    => 2
      case RockPaperScissorsShape.Scissors => 3
  }

  def getOutcomeScore(
      opponentShape: RockPaperScissorsShape,
      responseShape: RockPaperScissorsShape
  ): Int = {
    (opponentShape, responseShape) match
      case (RockPaperScissorsShape.Rock, RockPaperScissorsShape.Paper)     => 6
      case (RockPaperScissorsShape.Rock, RockPaperScissorsShape.Scissors)  => 0
      case (RockPaperScissorsShape.Paper, RockPaperScissorsShape.Rock)     => 0
      case (RockPaperScissorsShape.Paper, RockPaperScissorsShape.Scissors) => 6
      case (RockPaperScissorsShape.Scissors, RockPaperScissorsShape.Rock)  => 6
      case (RockPaperScissorsShape.Scissors, RockPaperScissorsShape.Paper) => 0
      case _                                                               => 3
  }

  def getScore(
      opponentShape: RockPaperScissorsShape,
      responseShape: RockPaperScissorsShape
  ): Int = {
    getResponseScore(responseShape) + getOutcomeScore(opponentShape, responseShape)
  }

  def solveOne(input: Iterator[(RockPaperScissorsShape, String)]): Int = {
    def decodeSecondColumn(encodedShape: String): RockPaperScissorsShape = {
      encodedShape match
        case "X" => RockPaperScissorsShape.Rock
        case "Y" => RockPaperScissorsShape.Paper
        case "Z" => RockPaperScissorsShape.Scissors
    }

    input
      .map((opponentShape: RockPaperScissorsShape, encodedResponseShape: String) =>
        (opponentShape, decodeSecondColumn(encodedResponseShape))
      )
      .map(getScore)
      .sum
  }

  def solveTwo(input: Iterator[(RockPaperScissorsShape, String)]): Int = {
    def getResponseShape(
        opponentShape: RockPaperScissorsShape,
        encodedSecondColumn: String
    ): RockPaperScissorsShape = {
      // X: loose
      // Y: draw
      // Z: win
      (opponentShape, encodedSecondColumn) match
        case (RockPaperScissorsShape.Rock, "X")     => RockPaperScissorsShape.Scissors
        case (RockPaperScissorsShape.Rock, "Z")     => RockPaperScissorsShape.Paper
        case (RockPaperScissorsShape.Paper, "X")    => RockPaperScissorsShape.Rock
        case (RockPaperScissorsShape.Paper, "Z")    => RockPaperScissorsShape.Scissors
        case (RockPaperScissorsShape.Scissors, "X") => RockPaperScissorsShape.Paper
        case (RockPaperScissorsShape.Scissors, "Z") => RockPaperScissorsShape.Rock
        case _                                      => opponentShape
    }

    input
      .map((opponentShape: RockPaperScissorsShape, encodedSecondColumn: String) =>
        (opponentShape, getResponseShape(opponentShape, encodedSecondColumn))
      )
      .map(getScore)
      .sum
  }
}
