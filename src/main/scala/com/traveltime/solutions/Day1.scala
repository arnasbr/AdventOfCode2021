package com.traveltime.solutions

object Day1 {
  def increasedDepthCountRec(
      data: List[Int],
      count: Int = 0,
      index: Int = 0,
      previousValue: Int = Int.MaxValue
  ): Int = {
    if (index == data.length) count
    else {
      if (data(index) > previousValue)
        increasedDepthCountRec(data, count + 1, index + 1, data(index))
      else increasedDepthCountRec(data, count, index + 1, data(index))
    }
  }
}
