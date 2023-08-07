package com.traveltime.solutions

import com.traveltime.solutions.Day2.InstructionName._

import scala.annotation.tailrec

object Day2 {
  sealed trait InstructionName
  object InstructionName {
    final case object Forward extends InstructionName
    final case object Up extends InstructionName
    final case object Down extends InstructionName

    def toInstruction(s: String): InstructionName = s match {
      case "forward" => Forward
      case "up"      => Up
      case _         => Down
    }
  }

  case class Instruction(name: InstructionName, value: Int)

  private case class Results(aim: Int, horizontal: Int, depth: Int)

  def part1(data: List[Instruction]): Int = {
    def sumOfSubInstruction(
        subInstruction: List[Instruction]
    ): Int = {
      subInstruction.foldLeft(0)((acc, current) => acc + current.value)
    }

    val forwardsSum = sumOfSubInstruction(data.filter(_.name == Forward))
    val upsSum = sumOfSubInstruction(data.filter(_.name == Up))
    val downsSum = sumOfSubInstruction(data.filter(_.name == Down))

    forwardsSum * (downsSum - upsSum)
  }

  def part2(data: List[Instruction]): Int = {
    val results = data.foldLeft(Results(0, 0, 0)) { case (res, instruction) =>
      instruction.name match {
        case Forward =>
          Results(
            res.aim,
            res.horizontal + instruction.value,
            res.depth + res.aim * instruction.value
          )
        case Down =>
          Results(res.aim + instruction.value, res.horizontal, res.depth)
        case _ =>
          Results(res.aim - instruction.value, res.horizontal, res.depth)
      }
    }

    results.horizontal * results.depth
  }

  def main(args: Array[String]): Unit = {
    val input =
      "forward 4\ndown 9\nforward 6\ndown 5\nup 2\nforward 5\nforward 7\nup 5\ndown 9\nup 6\ndown 6\ndown 1\ndown 1\nup 2\ndown 3\nup 3\nforward 8\nforward 7\ndown 6\ndown 7\nforward 6\nforward 9\nforward 7\nup 9\ndown 4\ndown 6\ndown 5\ndown 9\nforward 8\ndown 9\nforward 9\nforward 4\nforward 4\nup 3\nup 8\ndown 9\ndown 8\ndown 4\nforward 5\nforward 4\nup 6\nforward 6\nup 3\nup 8\nup 3\nup 4\ndown 3\ndown 5\ndown 5\nup 1\nforward 9\ndown 4\nforward 6\ndown 6\nup 2\nup 9\nforward 1\nforward 2\nforward 7\ndown 6\nup 6\nforward 1\nforward 7\ndown 7\nforward 9\nforward 4\nforward 6\ndown 5\nup 9\ndown 1\nup 5\nup 5\nup 9\ndown 5\nforward 7\ndown 1\nup 9\ndown 7\nforward 2\ndown 4\ndown 4\nforward 8\nforward 8\ndown 6\ndown 3\nup 7\ndown 3\nforward 9\ndown 7\nforward 2\ndown 1\nforward 5\nup 9\ndown 2\nup 2\ndown 3\nup 7\nforward 9\nforward 7\ndown 4\ndown 5\nup 3\ndown 3\ndown 5\nforward 9\ndown 3\nforward 9\ndown 3\nup 9\ndown 5\nforward 4\ndown 4\nup 8\nforward 7\nup 1\ndown 2\nforward 4\ndown 7\ndown 9\ndown 4\ndown 4\nforward 6\ndown 7\ndown 2\ndown 1\nforward 1\ndown 2\nforward 1\ndown 7\nforward 5\nup 3\nforward 6\nup 9\ndown 3\ndown 3\ndown 9\nforward 4\ndown 4\nforward 9\nforward 6\ndown 7\nup 9\nup 6\nforward 4\ndown 5\nforward 2\ndown 7\ndown 7\nforward 4\nforward 5\ndown 8\ndown 5\nup 4\nforward 7\nup 8\ndown 8\nforward 4\nforward 5\ndown 6\ndown 1\ndown 1\ndown 9\nforward 4\nup 1\ndown 8\nup 7\ndown 1\nup 2\nforward 4\ndown 7\ndown 7\ndown 2\nforward 7\ndown 2\nup 1\nup 4\ndown 6\nforward 5\nforward 2\nup 1\nforward 2\nforward 9\nup 9\nup 7\nforward 9\ndown 8\nup 5\ndown 6\ndown 6\nup 8\ndown 1\nforward 6\ndown 5\nforward 2\ndown 9\ndown 9\nup 4\nforward 4\nforward 2\nforward 7\nforward 3\ndown 1\nforward 8\nup 9\ndown 7\nforward 9\nforward 1\nforward 5\nup 6\ndown 6\nforward 6\nup 3\nforward 9\ndown 3\nforward 2\ndown 7\ndown 3\nup 9\ndown 2\ndown 3\nforward 5\ndown 9\nforward 8\ndown 2\nforward 1\ndown 9\ndown 7\nforward 2\nforward 6\nforward 4\nforward 5\ndown 5\ndown 1\nforward 5\nup 4\ndown 4\nup 8\ndown 4\nup 4\ndown 1\ndown 2\ndown 9\ndown 2\nup 4\ndown 1\nforward 2\nforward 1\nforward 9\ndown 5\nup 4\nup 1\nforward 8\nforward 6\nforward 9\nup 9\nforward 4\nforward 4\ndown 1\nforward 6\nforward 7\nforward 3\nup 5\nup 7\ndown 1\nforward 4\ndown 3\ndown 5\nup 7\ndown 4\nup 9\ndown 3\ndown 5\nforward 7\nforward 8\nup 5\nup 1\nforward 3\nup 8\nforward 3\ndown 2\nforward 1\nforward 9\nforward 1\ndown 2\nforward 7\ndown 5\nforward 6\ndown 9\nup 9\nforward 5\nforward 7\nforward 6\ndown 2\nup 2\nforward 3\nforward 4\nforward 3\ndown 5\nforward 1\nforward 2\nforward 6\ndown 4\nforward 2\nforward 6\nup 8\nforward 2\nup 4\nforward 7\ndown 2\nforward 1\nforward 7\ndown 6\nforward 4\ndown 3\ndown 2\ndown 2\nforward 4\ndown 8\nforward 6\nforward 6\ndown 2\nup 3\nup 1\nforward 1\ndown 5\ndown 2\nforward 4\nforward 7\nforward 3\ndown 3\nforward 9\ndown 1\ndown 7\nforward 6\nforward 1\nup 6\nforward 7\nforward 1\ndown 5\ndown 4\nforward 6\nup 1\ndown 1\nup 9\ndown 2\ndown 2\nforward 3\nup 4\ndown 5\ndown 5\ndown 3\ndown 6\nup 8\nforward 2\nforward 2\ndown 6\ndown 1\nup 4\nup 1\ndown 5\nup 4\nup 2\nforward 4\nforward 6\nforward 3\ndown 7\nforward 8\nup 5\nforward 5\ndown 1\nforward 2\nforward 6\ndown 8\nup 6\ndown 1\ndown 7\nforward 4\nforward 2\nup 1\ndown 6\nforward 3\nforward 1\nforward 5\nforward 9\nforward 9\ndown 4\nforward 2\ndown 1\nforward 1\nforward 7\nforward 5\ndown 9\ndown 8\ndown 1\ndown 6\ndown 1\nup 7\ndown 3\nforward 3\nup 6\nup 4\ndown 7\ndown 7\nforward 6\nup 7\ndown 7\nforward 9\ndown 9\ndown 3\nforward 6\nforward 9\nforward 1\ndown 4\nforward 5\ndown 4\ndown 2\ndown 3\nup 3\nforward 9\nforward 7\nforward 5\ndown 5\nforward 7\nup 4\ndown 1\nforward 3\ndown 3\nforward 4\ndown 9\nforward 2\ndown 5\ndown 1\nforward 8\ndown 3\nforward 7\nup 1\ndown 3\nforward 2\nup 8\ndown 2\nforward 4\nforward 4\nforward 4\ndown 5\nup 6\ndown 3\nforward 5\ndown 4\nup 5\nforward 1\nforward 6\nup 1\ndown 3\nforward 2\nforward 9\ndown 7\ndown 4\nforward 5\nup 3\nup 6\nup 1\nforward 4\nforward 1\nforward 1\ndown 7\nup 4\ndown 3\ndown 8\ndown 3\nforward 8\nforward 3\ndown 6\ndown 9\nforward 3\nforward 9\nforward 7\ndown 8\ndown 6\ndown 4\nforward 2\nup 4\nforward 8\ndown 1\nforward 9\nforward 1\ndown 9\nforward 2\ndown 7\ndown 2\nup 7\ndown 1\nup 8\nforward 8\ndown 7\nforward 1\ndown 1\nforward 3\nforward 1\nup 2\ndown 7\ndown 5\nforward 5\ndown 8\nforward 4\ndown 1\nup 2\nup 8\ndown 8\ndown 1\ndown 5\nup 3\nforward 3\nforward 5\ndown 2\nup 4\ndown 2\nforward 7\nforward 9\nup 9\nup 7\nforward 1\nup 4\nforward 3\nup 5\nforward 9\nforward 9\nforward 6\nforward 2\ndown 7\nforward 8\nforward 4\nforward 7\ndown 8\ndown 5\ndown 6\nforward 6\ndown 4\ndown 1\ndown 9\ndown 1\nforward 3\nforward 5\ndown 6\ndown 7\ndown 9\ndown 8\ndown 4\nup 5\nforward 7\ndown 9\nforward 6\ndown 7\nforward 5\ndown 5\nforward 1\ndown 5\ndown 3\nup 9\nup 3\nforward 2\nup 9\nforward 6\ndown 1\ndown 5\ndown 9\ndown 4\nup 6\nforward 9\ndown 4\ndown 9\ndown 5\ndown 8\ndown 5\ndown 4\nup 5\ndown 8\nup 8\nforward 5\ndown 9\nforward 2\nup 2\ndown 6\nforward 2\nforward 4\nforward 6\ndown 6\ndown 1\nforward 8\ndown 5\ndown 5\nforward 2\ndown 7\ndown 5\ndown 6\ndown 9\nforward 4\nup 9\ndown 3\ndown 7\nforward 3\ndown 5\nup 1\nforward 5\nup 2\ndown 2\nforward 2\nup 3\nup 6\nforward 2\nforward 7\ndown 8\nforward 8\nforward 7\nforward 6\ndown 5\ndown 6\ndown 6\ndown 9\nup 5\ndown 3\nup 1\nup 9\nup 5\ndown 4\ndown 4\ndown 8\nforward 8\nup 5\ndown 9\nforward 1\nup 1\nforward 2\ndown 9\nforward 5\nup 9\nforward 7\ndown 7\ndown 5\nup 1\nup 2\ndown 8\ndown 7\nup 4\nforward 9\ndown 4\nup 8\ndown 5\ndown 1\nforward 9\ndown 6\nup 8\ndown 6\nforward 7\nup 6\nup 5\nforward 2\nup 7\nforward 7\nforward 5\ndown 1\nforward 9\ndown 8\nforward 9\ndown 3\ndown 3\nforward 9\nup 1\ndown 2\nforward 9\ndown 7\nforward 4\nforward 3\nforward 4\ndown 5\nforward 9\nforward 9\ndown 5\nforward 4\ndown 5\ndown 2\ndown 6\nforward 5\nforward 8\nforward 6\nup 9\ndown 9\nforward 7\ndown 6\ndown 7\ndown 4\nforward 1\nforward 3\nforward 6\nforward 4\nforward 3\nforward 4\ndown 1\nforward 2\nforward 3\nforward 9\nup 8\nforward 6\ndown 1\nup 5\ndown 1\ndown 4\ndown 7\ndown 5\ndown 9\ndown 2\ndown 9\nforward 2\ndown 2\nup 5\nforward 2\nforward 3\nforward 5\nup 8\nup 1\ndown 9\nforward 2\ndown 4\ndown 9\ndown 6\ndown 5\ndown 8\nforward 3\nforward 8\nforward 7\nup 3\nup 5\ndown 9\ndown 5\nup 6\nforward 4\nforward 4\nforward 4\ndown 9\ndown 2\ndown 7\ndown 1\ndown 2\ndown 4\nforward 7\ndown 9\nforward 4\nforward 5\nup 5\nforward 4\nforward 9\nforward 1\nforward 5\ndown 3\nforward 1\nforward 5\nup 9\ndown 7\nforward 7\nforward 6\ndown 2\ndown 3\nforward 9\ndown 1\nforward 4\nforward 9\nup 7\nforward 7\ndown 5\nforward 9\nforward 2\nup 3\ndown 3\ndown 7\ndown 5\nup 7\nup 9\nup 7\nforward 3\nforward 3\nforward 8\nup 9\nforward 8\nforward 9\nforward 4\ndown 2\nforward 7\ndown 6\nup 3\nup 9\nforward 8\nforward 2\ndown 9\ndown 7\nforward 1\nup 4\nup 7\nforward 2\nup 4\nforward 4\nup 1\nforward 3\ndown 7\nforward 5\ndown 4\nforward 2\nforward 7\nup 4\ndown 1\ndown 6\nforward 1\nforward 9\nup 6\nforward 7\nforward 7\ndown 8\nforward 7\ndown 8\ndown 9\nup 3\nforward 3\nforward 3\ndown 8\nup 2\ndown 2\ndown 4\nup 3\ndown 3\nforward 7\ndown 4\nup 8\ndown 9\ndown 9\nup 7\ndown 1\nforward 2\nup 1\ndown 3\nup 9\ndown 6\nup 2\nforward 6\nup 8\nup 1\ndown 6\ndown 1\nup 6\nup 4\nup 2\nforward 6\ndown 6\ndown 1\nforward 7\nup 9\nup 1\nforward 4\nforward 5\nup 6\nforward 9\ndown 1\ndown 9\ndown 3\ndown 7\nforward 7\ndown 1\ndown 4\nforward 6\ndown 5\nup 4\nforward 9\nup 5\ndown 1\ndown 2\ndown 2\nup 4\nforward 1\nforward 3\ndown 7\nforward 4\ndown 4\ndown 8\ndown 5\nforward 3\nup 4\nforward 5\ndown 2\ndown 4\ndown 4\ndown 1\nforward 2\nforward 1\nforward 8\nforward 4\nup 4\ndown 9\nup 6\nforward 9\nup 5\ndown 5\nforward 3\nup 1\nforward 7\ndown 4\nforward 7\ndown 9\nup 8\ndown 5\nforward 1\ndown 5\ndown 8\nforward 3\nup 6\nforward 3\nup 7\nforward 6\nforward 9\nup 1\ndown 3\ndown 9\nup 4\nup 6\nforward 5\ndown 6\ndown 3\ndown 4\nup 1\nforward 5\ndown 5\ndown 2\nforward 6\ndown 8\ndown 3\nup 8\nforward 5\nforward 6\ndown 6\ndown 6\ndown 6\nforward 7\nup 4\nforward 7\nup 4\ndown 2\nforward 4\nforward 2\ndown 6\nup 1\ndown 1\ndown 4\nup 8\ndown 6\nforward 3\nforward 6\ndown 6\nforward 5\ndown 4\nup 2\nup 3\ndown 3\nup 1\nforward 2\nup 1\nforward 4\nup 5\nup 2\ndown 7\nforward 3\nup 2\nforward 5\ndown 1\ndown 3\ndown 2\nforward 5\ndown 1\nup 5\nforward 4\ndown 7\nup 8\nup 3\ndown 7\ndown 7\nforward 9\nforward 1\nup 6\ndown 4\ndown 7\nforward 1\ndown 4\nforward 9\nup 1\nforward 3\ndown 1\nup 3\ndown 6\ndown 8\ndown 6\nforward 6\nforward 6\nup 2\ndown 8\nforward 5"

    val lines = input.split("\n")

    // Creating a list of tuples (String, Int)
    val list: List[Instruction] = lines.map { line =>
      val words = line.split(" ")
      Instruction(InstructionName.toInstruction(words(0)), words(1).toInt)
    }.toList

    println(Day2.part1(list))
    println(Day2.part2(list))
  }
}
