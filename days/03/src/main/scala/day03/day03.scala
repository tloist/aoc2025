package day03

import scala.annotation.tailrec

case class Bank(batteries: List[Int])
def parseInput(filename: String = "input.txt"): List[Bank] =
  os.read.lines(os.resource / filename).toList.map: line =>
    Bank(line.map(_.asDigit).toList)

@tailrec
def findMaxJoltage(res: String, batteries: List[Int], size: Int): String =
  if size <= 1 then res + batteries.max.toString
  else
    val minimumRemainingElements = size - 1
    val max = batteries.slice(0, batteries.length - minimumRemainingElements).max
    val idx = batteries.indexOf(max)
    findMaxJoltage(res + max.toString, batteries.slice(idx + 1, batteries.length), size - 1)
def findMaxJoltage_2(bank: Bank): BigInt = BigInt(findMaxJoltage("", bank.batteries, 2))
def findMaxJoltage_12(bank: Bank): BigInt = BigInt(findMaxJoltage("", bank.batteries, 12))

@main def total_output_joltage(): Unit =
  val totalJoltageSum = parseInput().map(findMaxJoltage_2).sum
  println(s"The total joltage is: $totalJoltageSum")

@main def total_output_joltage_12(): Unit =
  val totalJoltageSum = parseInput().map(findMaxJoltage_12).sum
  println(s"The total joltage is: $totalJoltageSum")