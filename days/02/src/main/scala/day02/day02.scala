package day02

val one: BigInt = BigInt(1)
case class ProductIdRange(from: BigInt, to: BigInt):
  def ids: Seq[BigInt] = LazyList.iterate(from)(_ + one).takeWhile(_ <= to)
object ProductIdRange:
  def of(from: String, to: String): ProductIdRange = ProductIdRange(BigInt(from), BigInt(to))

def parse(filename: String = "input.txt"): List[ProductIdRange] =
  os.read(os.resource / filename).split(",").toList.map: range =>
    val parts = range.split("-")
    ProductIdRange(BigInt(parts(0)), BigInt(parts(1)))

type Partitioner = String => Seq[Int]

def isInvalid(no: BigInt)(using partitioner: Partitioner): Boolean =
  val textual = no.toString
  partitioner(textual).exists: divider =>
    textual.grouped(divider).toSet.size == 1

val alwaysSplitInHalf: Partitioner = textual =>
  if textual.length % 2 == 0 then Seq(textual.length / 2) else Seq.empty

@main def summing_up_invalid_ids(): Unit =
  given Partitioner = alwaysSplitInHalf
  val sum = parse().flatMap(_.ids.filter(isInvalid)).sum
  println(s"The sum of all the invalid IDs is $sum")

val allDivider: Partitioner = textual =>
  LazyList.from(1)
    .takeWhile(_ <= textual.length / 2)
    .filter(textual.length % _ == 0)

@main def summing_up_invalid_ids_2(): Unit =
  given Partitioner = allDivider
  val sum = parse().flatMap(_.ids.filter(isInvalid)).sum
  println(s"The sum of all the invalid IDs now is $sum")
