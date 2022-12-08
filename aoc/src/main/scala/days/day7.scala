package advent

import scala.annotation.tailrec
import scala.io.Source

val data7: List[String] = Source.fromResource("day7.csv").getLines.toList

case class Dir(name: String)

@tailrec
def dirIterator7(
    instructionsToGo: List[String],
    currentPath: List[Dir],
    dirMap: List[(Dir, Int, String)]
): List[(Dir, Int, String)] = {
  if (instructionsToGo.isEmpty) dirMap
  else {
    val instruction = instructionsToGo.head
    if (instruction == "$ cd ..")
      dirIterator7(
        instructionsToGo.tail,
        currentPath.tail,
        dirMap
      )
    else if (instruction == "$ ls")
      dirIterator7(instructionsToGo.tail, currentPath, dirMap)
    else if (instruction.startsWith("$ cd ")) {
      val newDir = Dir(instruction.replace("$ cd ", ""))
      dirIterator7(
        instructionsToGo.tail,
        newDir :: currentPath,
        dirMap
      )
    } else if (instruction.startsWith("dir "))
      dirIterator7(instructionsToGo.tail, currentPath, dirMap)
    else {
      val splitString = instruction.split(" ")
      val fileSize: Int = splitString(0).toInt
      val fileName: String = splitString(1)
      val updatedDirMap =
        List.concat(dirMap, currentPath.map(d => (d, fileSize, fileName)))
      println(instruction)
      dirIterator7(instructionsToGo.tail, currentPath, updatedDirMap)
    }
  }
}

def day7a(): Unit = {
  val dirSizes = dirIterator7(
    data7,
    List.empty,
    List.empty
  ).toSet
    .groupBy(_._1)
    .map { case (k, lv) => lv.map(_._2).sum}
    .toList
//  println(
//    dirIterator7(
//      data7,
//      List.empty,
//      List.empty
//    ).length
//  )
  println(dirSizes.filter(_ < 100000).sum)
}

def day7b: Unit = {

  day7a()
}
//    .map { case (k, lv) =>
//      (k, lv.map(_._2).foldLeft(BigInt(0)) { case (acc, x) => acc + BigInt(x) })
//    }
