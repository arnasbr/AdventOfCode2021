package com.traveltime.solutions

import scala.annotation.tailrec

object Day1 {
  case class Accumulator(counter: Int = 0, previous: Int)
  private def part1(
      data: List[Int]
  ): Int = {
    data
      .sliding(2, 1)
      .map { case a :: b :: Nil =>
        a > b
      }
      .count(x => x)
  }

  def part2(data: List[Int]): Int = {
    val sums = data.sliding(3).map(_.sum).toList
    part1(sums)
  }
}
