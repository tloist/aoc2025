package day03

case class Bank(batteries: List[Int])
def parseInput(filename: String = "input.txt"): List[Bank] =
  os.read.lines(os.resource / filename).toList.map: line =>
    Bank(line.map(_.asDigit).toList)

def findMaxJoltage(bank: Bank): Int =
  val max = bank.batteries.slice(0, bank.batteries.length - 1).max
  val idx = bank.batteries.indexOf(max)
  val max2 = bank.batteries.slice(idx + 1, bank.batteries.length).max
  max * 10 + max2

@main def total_output_joltage(): Unit =
  val totalJoltageSum = parseInput().map(findMaxJoltage).sum
  println(s"The total joltage is: $totalJoltageSum")

def findMaxJoltage(batteries: List[Int], size: Int): String =
  if size <= 1 then batteries.max.toString
  else
    val minimumRemainingElements = size - 1
    val max = batteries.slice(0, batteries.length - minimumRemainingElements).max
    val idx = batteries.indexOf(max)
    max.toString + findMaxJoltage(batteries.slice(idx + 1, batteries.length), size - 1)
def findMaxJoltage_12(bank: Bank): BigInt = BigInt(findMaxJoltage(bank.batteries, 12))


@main def total_output_joltage_12(): Unit =
  val totalJoltageSum = parseInput().map(findMaxJoltage_12).sum
  println(s"The total joltage is: $totalJoltageSum")