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

def isInvalid(no: BigInt): Boolean =
  val textual = no.toString
  if textual.length % 2 != 0 then false
  else
    val (begin, end) = textual.splitAt(textual.length / 2)
    begin == end

@main def summing_up_invalid_ids(): Unit =
  val sum = parse().flatMap(_.ids.filter(isInvalid)).sum
  println(s"The sum of all the invalid IDs is $sum")

def isInvalid2(no: BigInt): Boolean =
  val textual = no.toString
  LazyList.from(1)
    .takeWhile(_ <= textual.length / 2)
    .filter(textual.length % _ == 0)
    .exists: divider =>
      textual.grouped(divider).toSet.size == 1

@main def summing_up_invalid_ids_2(): Unit =
  val sum = parse().flatMap(_.ids.filter(isInvalid2)).sum
  println(s"The sum of all the invalid IDs now is $sum")
