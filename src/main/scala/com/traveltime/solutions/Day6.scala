package com.traveltime.solutions

import scala.annotation.tailrec

object Day6 {
  private def optionTraverse[A](input: List[Option[A]]): Option[List[A]] =
    input.foldRight[Option[List[A]]](Some(Nil))((oa, acc) =>
      for {
        a <- oa
        list <- acc
      } yield a :: list
    )
  private def parseIntList(input: String): Option[List[Int]] = {
    optionTraverse(input.split(",").map(_.toIntOption).toList)
  }
  def part1(fish: Option[List[Int]], n: Int): Option[Int] = {
    @tailrec
    def processFishList(fishList: List[Int], times: Int): List[Int] = {
      if (times <= 0) fishList
      else {
        val updatedList = fishList.foldLeft(List.empty[Int]) { (list, fish) =>
          if (fish == 0) {
            6 :: 8 :: list
          } else fish - 1 :: list
        }
        processFishList(updatedList, times - 1)
      }
    }

    fish.map { fishList =>
      val finalList = processFishList(fishList, n)
      finalList.length
    }
  }

  def main(args: Array[String]): Unit = {
    val input = "3,4,3,1,2"
    val myInput =
      "3,5,1,2,5,4,1,5,1,2,5,5,1,3,1,5,1,3,2,1,5,1,1,1,2,3,1,3,1,2,1,1,5,1,5,4,5,5,3,3,1,5,1,1,5,5,1,3,5,5,3,2,2,4,1,5,3,4,2,5,4,1,2,2,5,1,1,2,4,4,1,3,1,3,1,1,2,2,1,1,5,1,1,4,4,5,5,1,2,1,4,1,1,4,4,3,4,2,2,3,3,2,1,3,3,2,1,1,1,2,1,4,2,2,1,5,5,3,4,5,5,2,5,2,2,5,3,3,1,2,4,2,1,5,1,1,2,3,5,5,1,1,5,5,1,4,5,3,5,2,3,2,4,3,1,4,2,5,1,3,2,1,1,3,4,2,1,1,1,1,2,1,4,3,1,3,1,2,4,1,2,4,3,2,3,5,5,3,3,1,2,3,4,5,2,4,5,1,1,1,4,5,3,5,3,5,1,1,5,1,5,3,1,2,3,4,1,1,4,1,2,4,1,5,4,1,5,4,2,1,5,2,1,3,5,5,4,5,5,1,1,4,1,2,3,5,3,3,1,1,1,4,3,1,1,4,1,5,3,5,1,4,2,5,1,1,4,4,4,2,5,1,2,5,2,1,3,1,5,1,2,1,1,5,2,4,2,1,3,5,5,4,1,1,1,5,5,2,1,1"
    val fish = parseIntList(input)
    println(part1(fish, 256))
  }
}
