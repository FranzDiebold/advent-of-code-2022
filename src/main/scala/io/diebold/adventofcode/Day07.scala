package io.diebold.adventofcode

import scala.util.matching.Regex
import scala.annotation.tailrec
import scala.collection.mutable.HashMap

import io.diebold.adventofcode.Puzzle

case class File(name: String, size: Int)
case class Directory(
    name: String,
    parentDirectory: Option[Directory],
    childDirectories: HashMap[String, Directory],
    files: HashMap[String, File],
    var totalSize: Int
)

object Day07 extends Puzzle[Iterator[String], Int] {
  val rootDirectoryName = "/"

  val changeDirectoryRootRegex: Regex = """^\$ cd /$""".r
  val changeDirectoryInRegex: Regex   = """^\$ cd (\w+)$""".r
  val changeDirectoryOutRegex: Regex  = """^\$ cd \.\.$""".r
  var listRegex: Regex                = """^\$ ls$""".r
  var fileRegex: Regex                = """^(\d+) ([\w.]+)$""".r
  var directoryRegex: Regex           = """^dir (\d+)$""".r

  def readInput(): Iterator[String] = {
    readRawInput()
  }

  @tailrec
  def updateTotalDirectorySize(directory: Option[Directory], sizeToAdd: Int): Unit = {
    directory match
      case Some(currentDirectory) => {
        currentDirectory.totalSize += sizeToAdd
        updateTotalDirectorySize(currentDirectory.parentDirectory, sizeToAdd)
      }
      case None =>
  }

  @tailrec
  def buildFileSystemTreeRec(
      input: Iterator[String],
      rootDirectory: Directory,
      currentDirectory: Directory
  ): Directory = {
    if input.hasNext then
      input.next match
        case changeDirectoryRootRegex() =>
          buildFileSystemTreeRec(input, rootDirectory, rootDirectory)
        case changeDirectoryInRegex(directoryName) => {
          if currentDirectory.childDirectories.contains(directoryName) then
            buildFileSystemTreeRec(
              input,
              rootDirectory,
              currentDirectory.childDirectories(directoryName)
            )
          else
            val childDirectory = Directory(
              directoryName,
              Some(currentDirectory),
              HashMap[String, Directory](),
              HashMap[String, File](),
              0
            )
            currentDirectory.childDirectories.addOne(directoryName, childDirectory)
            buildFileSystemTreeRec(input, rootDirectory, childDirectory)
          end if
        }
        case changeDirectoryOutRegex() =>
          buildFileSystemTreeRec(
            input,
            rootDirectory,
            currentDirectory.parentDirectory.getOrElse(rootDirectory)
          )
        case listRegex() => buildFileSystemTreeRec(input, rootDirectory, currentDirectory)
        case fileRegex(fileSizeString, fileName) => {
          if !currentDirectory.files.contains(fileName) then
            val fileSize = fileSizeString.toInt
            val file     = File(fileName, fileSize)
            currentDirectory.files.addOne((fileName, file))
            updateTotalDirectorySize(Some(currentDirectory), fileSize)
          end if
          buildFileSystemTreeRec(input, rootDirectory, currentDirectory)
        }
        case directoryRegex(directoryName) => {
          if !currentDirectory.childDirectories.contains(directoryName) then
            val childDirectory = Directory(
              directoryName,
              Some(currentDirectory),
              HashMap[String, Directory](),
              HashMap[String, File](),
              0
            )
            currentDirectory.childDirectories.addOne(directoryName, childDirectory)
          end if
          buildFileSystemTreeRec(input, rootDirectory, currentDirectory)
        }
        case null | _ => buildFileSystemTreeRec(input, rootDirectory, currentDirectory)
    else rootDirectory
    end if
  }

  def buildFileSystemTree(input: Iterator[String]): Directory = {
    val rootDirectory =
      Directory(
        rootDirectoryName,
        None,
        HashMap[String, Directory](),
        HashMap[String, File](),
        0
      )

    buildFileSystemTreeRec(
      input,
      rootDirectory,
      rootDirectory
    )
  }

  def getDirectories(currentDirectory: Directory): Seq[Directory] = {
    Seq(currentDirectory) ++ currentDirectory.childDirectories.values.flatMap(getDirectories)
  }

  def solveOne(input: Iterator[String]): Int = {
    val fileSystemTree = buildFileSystemTree(input)
    getDirectories(fileSystemTree)
      .filter((directory: Directory) => directory.totalSize <= 100000)
      .map(_.totalSize)
      .sum
  }

  def solveTwo(input: Iterator[String]): Int = {
    val totalDiskSpace  = 70000000
    val neededFreeSpace = 30000000

    val fileSystemTree = buildFileSystemTree(input)
    val totalSize      = fileSystemTree.totalSize

    val minSpaceToBeDeleted = neededFreeSpace - (totalDiskSpace - totalSize)

    getDirectories(fileSystemTree)
      .filter((directory: Directory) => directory.totalSize >= minSpaceToBeDeleted)
      .sortBy(_.totalSize)
      .take(1)(0)
      .totalSize
  }
}
