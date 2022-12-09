package io.diebold.util

object SplitIterator {
  def split[T](iter: Iterator[T], separator: T) = new Iterator[Seq[T]] {
    def hasNext = iter.hasNext
    def next()  = iter.takeWhile(_ != separator).toSeq
  }
}
