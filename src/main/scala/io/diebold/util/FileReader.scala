package io.diebold.util

import scala.io.Source

object FileReader {
  def readInput(day: Int): Iterator[String] = {
    Source.fromResource(f"Day$day%02d.txt").getLines
  }
}
