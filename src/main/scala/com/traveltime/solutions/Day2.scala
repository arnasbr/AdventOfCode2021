package com.traveltime.solutions

object Day2 {
  def part1(data: List[(String, Int)]): Int = {
    def sumOfSubInstruction(subInstruction: List[(String, Int)]): Int = {
      subInstruction.foldLeft(0)((acc, current) => acc + current._2)
    }

    val forwardsSum = sumOfSubInstruction(data.filter(x => x._1 == "forward"))
    val upsSum = sumOfSubInstruction(data.filter(x => x._1 == "up"))
    val downsSum = sumOfSubInstruction(data.filter(x => x._1 == "down"))

    forwardsSum * (downsSum - upsSum)
  }
}
