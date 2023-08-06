package com.traveltime.solutions

import scala.annotation.tailrec

object Day1 {
  @tailrec
  def part1(
      data: List[Int],
      count: Int = 0,
      previousValue: Int = Int.MaxValue
  ): Int = {
    data match {
      case Nil => count
      case head :: tail =>
        if (head > previousValue)
          part1(tail, count + 1, head)
        else
          part1(tail, count, head)
    }
  }

  def part2(data: List[Int]): Int = {
    val sums = data.sliding(3).map(_.sum).toList
    part1(sums)
  }
}
