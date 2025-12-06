package day06.transpose

enum Calculation:
  case Addition(numbers: List[BigInt])
  case Multiplication(numbers: List[BigInt])
  def result: BigInt = this match
    case Addition(numbers) => numbers.sum
    case Multiplication(numbers) => numbers.product
  def append(number: BigInt): Calculation = this match
    case Addition(numbers) => Addition(numbers :+ number)
    case Multiplication(numbers) => Multiplication(numbers :+ number)
object Calculation:
  def parse(line: String): Calculation = line.charAt(line.length - 1) match
    case '+' => Addition(List(BigInt(line.init.trim)))
    case '*' => Multiplication(List(BigInt(line.init.trim)))
  def isNewCalculationLine(line: String): Boolean = line.endsWith("+") | line.endsWith("*")

def transpose(lines: List[String]): List[String] =
  require(lines.map(_.length).toSet.size == 1)
  lines.head.indices.map(i => lines.map(_.apply(i)).mkString).toList

def readInput(filename: String): Seq[String] =
  transpose(os.read.lines(os.resource / filename).toList)

def parseCalculations(input: Seq[String]): List[Calculation] =
  input.filterNot(_.isBlank).foldLeft(List.empty[Calculation]): (calcs, line) =>
    if !Calculation.isNewCalculationLine(line)
    then calcs.head.append(BigInt(line.trim)) +: calcs.tail
    else Calculation.parse(line) +: calcs

def solve(filename: String): BigInt =
  parseCalculations(readInput(filename))
    .map(_.result)
    .sum

@main def total_of_individual_cephalopod_solutions_transposed(): Unit =
  println(s"${solve("input.txt")} is the grand total in cephalopod")