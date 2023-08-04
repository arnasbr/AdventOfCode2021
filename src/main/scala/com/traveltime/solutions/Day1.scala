package com.traveltime.solutions

object Day1 {
  def part1(
      data: List[Int],
      count: Int = 0,
      index: Int = 0,
      previousValue: Int = Int.MaxValue
  ): Int = {
    if (index == data.length) count
    else {
      if (data(index) > previousValue)
        part1(data, count + 1, index + 1, data(index))
      else part1(data, count, index + 1, data(index))
    }
  }
}
