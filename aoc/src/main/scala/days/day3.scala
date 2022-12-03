package advent

import scala.annotation.tailrec
import scala.io.Source

def bool2int(b: Boolean) = if (b) 1 else 0

val data3: List[String] = Source.fromResource("day3.csv").getLines.toList

def charScore(c: Char): Int = c.toInt - 'a'.toInt + 1 + 58 * bool2int(c.isUpper)

def findDuplicate(s1: String, s2: String): Char = {
  s1.toSet.intersect(s2.toSet).head
}

def parseString(s: String): Int = {
  val mid: Int = s.length / 2
  val c: Char = findDuplicate(s.substring(0, mid), s.substring(mid))
  charScore(c)
}

def day3b: Int = {

  // for a set of backpacks, find the common character
  def findBadge(vInput: List[(Int, Set[Char])]): Char = {
    val v = vInput.map(_._2)
    v.tail.foldLeft(v.head)((acc, s) => acc.intersect(s)).head
  }

  // group all backpacks in sets of 3
  val indexedData =
    data3.zipWithIndex.map((c, idx) => (idx - idx % 3, c.toSet)).groupBy(_._1)

  indexedData.values.map(backpacks => charScore(findBadge(backpacks))).sum
}

def day3a: Int = {
  data3.map(parseString).sum
}

def day3: Unit = {
  println(day3a)
  println(day3b)
}
