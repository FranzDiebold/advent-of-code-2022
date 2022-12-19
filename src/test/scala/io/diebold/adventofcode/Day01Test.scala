package io.diebold.adventofcode

import org.scalatest.funsuite.AnyFunSuite

class Day01Test extends AnyFunSuite {
  test("Day01.partOne") {
    assert(Day01.partOne() === 24000)
  }

  test("Day01.partTwo") {
    assert(Day01.partTwo() === 45000)
  }
}
