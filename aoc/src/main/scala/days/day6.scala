package advent

import scala.annotation.tailrec
import scala.io.Source

val inputData: String = Source.fromResource("day6.csv").getLines.toList.head
val data = inputData.zipWithIndex.map(_.swap).toMap

@tailrec
def iteratorDay6(idx: Int, seqLength: Int): Int = {
  val chars =
    data.filterKeys(n => (n >= idx) & (n <= idx + seqLength - 1)).values.toSet
  if (chars.size == seqLength)
    idx + seqLength // i.e., it's zero based so we need a +1
  else iteratorDay6(idx + 1, seqLength)
}

def day6: Unit = {
  println(iteratorDay6(0, 4))
  println(iteratorDay6(0, 14))
}
