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

  def part2(data: List[(String, Int)]): Int = {
    val (aim, horizontal, depth) = data.foldLeft((0, 0, 0)) {
      case ((aim, horizontal, depth), (instruction, x)) =>
        instruction match {
          case "forward" => (aim, horizontal + x, depth + aim * x)
          case "down"    => (aim + x, horizontal, depth)
          case _         => (aim - x, horizontal, depth)
        }
    }

    horizontal * depth
  }
}
