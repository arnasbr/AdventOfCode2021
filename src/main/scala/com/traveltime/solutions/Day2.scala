package com.traveltime.solutions

import scala.annotation.tailrec

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
  @tailrec
  def part2(
      data: List[(String, Int)],
      aim: Int = 0,
      horizontal: Int = 0,
      depth: Int = 0
  ): Int = {
    data match {
      case Nil => horizontal * depth
      case head :: tail =>
        val instruction = head._1
        val x = head._2
        if (instruction == "forward")
          part2(tail, aim, horizontal + x, depth + aim * x)
        else if (instruction == "down")
          part2(tail, aim + x, horizontal, depth)
        else part2(tail, aim - x, horizontal, depth)
    }
  }
}
