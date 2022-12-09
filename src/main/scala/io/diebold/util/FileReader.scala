package io.diebold.util

import scala.io.Source

object FileReader {
  def readInput(day: String): Iterator[String] = {
    Source.fromResource(f"$day.txt").getLines
  }
}
