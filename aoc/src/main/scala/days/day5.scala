package advent

import scala.annotation.tailrec
import scala.io.Source

val initData = Map(
  1 -> List('P', 'L', 'M', 'N', 'W', 'V', 'B', 'H'),
  2 -> List('H', 'Q', 'M'),
  3 -> List('L', 'M', 'Q', 'F', 'G', 'B', 'D', 'N'),
  4 -> List('G', 'W', 'M', 'Q', 'F', 'T', 'Z'),
  5 -> List('P', 'H', 'T', 'M'),
  6 -> List('T', 'G', 'H', 'D', 'J', 'M', 'B', 'C'),
  7 -> List('R', 'V', 'F', 'B', 'N', 'M'),
  8 -> List('S', 'G', 'R', 'M', 'H', 'L', 'P'),
  9 -> List('N', 'C', 'B', 'D', 'P')
)

  val initData2 = Map(
1 -> List('D', 'N', 'Z'),
2 -> List('C', 'M'),
3 -> List('P'),
)

// move A from B to C
val moves: List[Array[Int]] = Source
  .fromResource("day5test.csv")
  .getLines
  .toList
  .map(_.replace("move ", "").replace("from", ",").replace("to ", ",").split(",").map(_.trim.toInt))

@tailrec
def moveCrates(
    data: Map[Int, List[Char]],
    move: Array[Int],
    ctr: Int
): Map[Int, List[Char]] = {
  if (ctr == 0) data
  else {
    val c: Char = data(move(1)).head
    val newData: Map[Int, List[Char]] =
      data + (move(1) -> data(move(1)).tail) + (move(2) -> (c :: data(move(2))))
    moveCrates(newData, move, ctr - 1)
  }
}

def day5a(): List[Char] = {
  val endData: Map[Int, List[Char]] =
  moves.foldLeft (initData2) ((acc, move) => moveCrates (acc, move, move(0)) )
  println(endData)
  val result = endData.values.map(arr => arr.head).toList
  result
  }

def day5: Unit = {
  println(moves.head) //.map(_(1)))
  println(day5a())
}