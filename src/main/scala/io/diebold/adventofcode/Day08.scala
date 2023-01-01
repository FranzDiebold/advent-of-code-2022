package io.diebold.adventofcode

import scala.annotation.tailrec
import scala.math.max
import scala.runtime.Arrays

import io.diebold.adventofcode.Puzzle

object Day08 extends Puzzle[Array[Array[Int]], Int] {
  def readInput(): Array[Array[Int]] = {
    readRawInput()
      .map((line: String) => line.map((heightChar: Char) => heightChar.toInt).toArray)
      .toArray
  }

  def solveOne(input: Array[Array[Int]]): Int = {
    @tailrec
    def checkVisibilityRowCol(
        heightMap: Array[Array[Int]],
        x: Int,
        y: Int,
        dx: Int,
        dy: Int,
        visibilityMap: Array[Array[Boolean]],
        maxHeight: Int = -1
    ): Array[Array[Boolean]] = {
      val xSize = heightMap.length
      val ySize = heightMap(0).length

      if x < 0 || y < 0 || x >= xSize || y >= ySize then return visibilityMap
      end if

      val currentHeight = heightMap(x)(y)
      val isVisible     = maxHeight < currentHeight
      visibilityMap(x)(y) = visibilityMap(x)(y) || isVisible

      checkVisibilityRowCol(
        heightMap,
        x + dx,
        y + dy,
        dx,
        dy,
        visibilityMap,
        max(maxHeight, currentHeight)
      )
    }

    def checkVisibility(heightMap: Array[Array[Int]]): Array[Array[Boolean]] = {
      val xSize = heightMap.length
      val ySize = heightMap(0).length

      val rowColDirections = (0)
        .to(xSize - 1)
        .flatMap((x: Int) => Vector((x, 0, 0, 1), (x, ySize - 1, 0, -1)))
      ++
      (0)
        .to(ySize - 1)
        .flatMap((y: Int) => Vector((0, y, 1, 0), (xSize - 1, y, -1, 0)))

      val initialVisibilityMap = Array.ofDim[Boolean](xSize, ySize)

      rowColDirections
        .foldLeft(initialVisibilityMap) {
          (visibilityMap: Array[Array[Boolean]], rowColDirection: (Int, Int, Int, Int)) =>
            checkVisibilityRowCol(
              heightMap,
              rowColDirection(0),
              rowColDirection(1),
              rowColDirection(2),
              rowColDirection(3),
              visibilityMap
            )
        }
    }

    def countVisibles(visibilityMap: Array[Array[Boolean]]): Int = {
      visibilityMap
        .map((row: Array[Boolean]) => row.count(_ == true))
        .sum
    }

    countVisibles(checkVisibility(input))
  }

  def solveTwo(input: Array[Array[Int]]): Int = {
    @tailrec
    def getViewingDistance(
        heightMap: Array[Array[Int]],
        height: Int,
        x: Int,
        y: Int,
        dx: Int,
        dy: Int,
        viewingDistance: Int = 1
    ): Int = {
      val xSize = heightMap.length
      val ySize = heightMap(0).length

      if x < 0 || x >= xSize || y < 0 || y >= ySize then return viewingDistance - 1
      end if

      val currentHeight = heightMap(x)(y)
      if currentHeight >= height then return viewingDistance
      end if

      getViewingDistance(
        heightMap,
        height,
        x + dx,
        y + dy,
        dx,
        dy,
        viewingDistance + 1
      )
    }

    def getViewingDistances(
        heightMap: Array[Array[Int]],
        x: Int,
        y: Int
    ): (Int, Int, Int, Int) = {
      val distances = Array(
        (1, 0),
        (0, 1),
        (-1, 0),
        (0, -1)
      )
        .map((d: (Int, Int)) => {
          val (dx, dy) = d
          getViewingDistance(
            heightMap,
            heightMap(x)(y),
            x + dx,
            y + dy,
            dx,
            dy
          )
        })

      (
        distances(0),
        distances(1),
        distances(2),
        distances(3)
      )
    }

    def getScenicScore(heightMap: Array[Array[Int]], x: Int, y: Int): Int = {
      getViewingDistances(heightMap, x, y) match
        case (right: Int, bottom: Int, left: Int, top: Int) => right * bottom * left * top
    }

    val xSize = input.length
    val ySize = input(0).length
    val scenicScoresMap: Seq[Seq[Int]] = (0)
      .to(xSize - 1)
      .map((x: Int) =>
        (0)
          .to(ySize - 1)
          .map((y: Int) => getScenicScore(input, x, y))
      )

    scenicScoresMap
      .map(_.max)
      .max
  }
}
