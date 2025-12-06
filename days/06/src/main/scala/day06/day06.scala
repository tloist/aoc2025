package day06

import cats.kernel.Monoid

extension (str: String)
  def chatAtOrSpace(idx: Int): Char = if idx >= str.length then ' ' else str.charAt(idx)

enum Operator(m: Monoid[BigInt]) extends Monoid[BigInt]:
  case Plus extends Operator(Monoid.instance(0, _ + _))
  case Multiplication extends Operator(Monoid.instance(1, _ * _))
  export m.{empty, combine}
object Operator:
  def parse(char: Char): Operator = char match
    case '+' => Operator.Plus
    case '*' => Operator.Multiplication
    case _ => throw new IllegalArgumentException(s"Illegal operator '$char'")

def isOperatorLine: String => Boolean = line => line.startsWith("+") || line.startsWith("*")
def isNumberLine: String => Boolean = line => !isOperatorLine(line)

def parseOperatorLine(filename: String): Map[Range, Operator] =
  val lines = os.read.lines(os.resource / filename)
  val line = lines.dropWhile(isNumberLine).head
  val operatorIndices = line.toCharArray.zipWithIndex.collect {case (c, idx) if !Character.isWhitespace(c) => idx }.toList
  val columnRange: List[Range] = operatorIndices.sliding(2).toList.map(range => Range(range(0), range(1) - 1))
  val initColumns = columnRange.map(r => r -> Operator.parse(line(r.start))).toMap
  val lastColumn = operatorIndices.last.until(lines.map(_.length).max) -> Operator.parse(line(operatorIndices.last))
  initColumns + lastColumn

def parseColumns(filename: String): Map[List[String], Operator] =
  val lines = os.read.lines(os.resource / filename).takeWhile(isNumberLine)
  for {
    (range, op) <- parseOperatorLine(filename)
    numbers = lines.map(line => range.map(line.chatAtOrSpace).mkString).toList
  } yield (numbers, op)

def readNumberHuman(textual: List[String]): List[BigInt] = textual.map(str => BigInt(str.trim))
def readNumberCephalopod(textual: List[String]): List[BigInt] =
  textual.head.indices.map(i => BigInt(textual.map(_.apply(i)).mkString.trim)).toList
def calculate(howToReadNumbers: List[String] => List[BigInt]): Map[List[String], Operator] => BigInt = input => (
  for {
    (inputs, operator) <- input
    numbers = howToReadNumbers(inputs)
  } yield operator.combineAll(numbers)
).sum

val solutionA: String => BigInt = parseColumns andThen calculate(readNumberHuman)

@main def total_of_individual_solutions(): Unit =
  println(s"${solutionA("input.txt")} is the grand total")

val calculateCephalopod = parseColumns andThen calculate(readNumberCephalopod)

@main def total_of_individual_cephalopod_solutions(): Unit =
  println(s"${calculateCephalopod("input.txt")} is the grand total in cephalopod")