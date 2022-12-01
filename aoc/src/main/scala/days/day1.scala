package advent

import scala.annotation.tailrec
import scala.io.Source

val data1a: List[String] = Source.fromResource("day1.csv").getLines.toList

def caloriesPerElf(inputData: List[String]): List[Int] = {

  @tailrec
  def splitLists(data:List[String], collector: List[List[String]]): List[List[String]] = {
    val (preList, postList) = data.span(_ != "")
    val collNew = preList :: collector
    if (preList.isEmpty) collNew
    else splitLists(postList.drop(1), collNew)
  }
   splitLists(inputData, List(List.empty)).drop(1).map(sumCals)
  }

def sumCals(input: List[String]): Int = {
  input.map(_.toInt).sum
  }

def day1a: Int = {
    caloriesPerElf(data1a).max
  }

def day1b: Int = {
    caloriesPerElf(data1a).sortBy(c => -c).take(3).sum
  }

def day1: Unit = {
    println(day1a)
    println(day1b)
  }