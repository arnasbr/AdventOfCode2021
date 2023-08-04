package com.traveltime.solutions

object Day2 {
  def part1(data: List[(String, Int)]): Int = {
    def sumOfSubInstruction(subInstruction: List[(String, Int)]): Int = {
      subInstruction.foldLeft(0)((acc, current) => acc + current._2)
    }

    val forwards = data.filter(x => x._1 == "forward")
    val ups = data.filter(x => x._1 == "up")
    val downs = data.filter(x => x._1 == "down")
    val forwardsSum = sumOfSubInstruction(forwards)
    val upsSum = sumOfSubInstruction(ups)
    val downsSum = sumOfSubInstruction(downs)

    forwardsSum * (downsSum - upsSum)
  }
}
