package advent

import scala.io.Source

val inputData: List[Array[Int]] = Source
  .fromResource("day4.csv")
  .getLines
  .toList
  .map(_.split("[-,]").map(_.toInt))

def day4a(): Int = {
  def isContained(a: Array[Int]): Boolean = {
    ((a(0) >= a(2)) & (a(1) <= a(3))) | ((a(0) <= a(2)) & (a(1) >= a(3)))
  }
  inputData.filter(isContained).length
}

def day4b(): Int = {
  def isOverlapping(a: Array[Int]): Boolean = {
    (a(0) <= a(3)) & (a(1) >= a(2))
  }
  inputData.filter(isOverlapping).length
}

def day4: Unit = {
  println(day4a())
  println(day4b())
}
