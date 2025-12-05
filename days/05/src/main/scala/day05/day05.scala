package day05

import com.google.common.collect.{Range, RangeSet, TreeRangeSet}
import scala.jdk.CollectionConverters.*
import scala.math.BigInt.javaBigInteger2bigInt
import java.math.BigInteger

type FreshIngredientIdRange = Range[BigInteger]
type AvailableIngredientId = BigInteger

def parseFreshIngredientRange(line: String): FreshIngredientIdRange =
  val boundaries = line.split("-").map(new BigInteger(_))
  Range.closed(boundaries(0), boundaries(1))
def parseAvailableIngredientId(line: String): AvailableIngredientId =
  BigInteger(line)

case class Ingredients(fresh: List[FreshIngredientIdRange], available: List[AvailableIngredientId]):
  def freshSet(): RangeSet[BigInteger] =
    val lookup = TreeRangeSet.create[BigInteger]()
    fresh.foreach(lookup.add)
    lookup

def parse(filename: String = "input.txt"): Ingredients =
  val lines = os.read.lines(os.resource / filename)
  Ingredients(
    fresh = lines.takeWhile(_.nonEmpty).map(parseFreshIngredientRange).toList,
    available = lines.dropWhile(_.nonEmpty).drop(1).map(parseAvailableIngredientId).toList
  )

def findAvailableFresh(ingredients: Ingredients): Set[AvailableIngredientId] =
  val fresh = ingredients.freshSet()
  ingredients.available.filter(fresh.contains).toSet

val ingredients = parse()

@main def count_available_fresh(): Unit =
  val result = findAvailableFresh(ingredients).size
  println(s"$result of the available ingredient IDs are fresh")

def totalNumberOfFreshIngredients(input: RangeSet[BigInteger]): BigInt =
  val ranges = input.asRanges().asScala.toList  // toList is important not to fold multiple same counts together
  ranges.map(r => r.upperEndpoint() - r.lowerEndpoint() + 1).sum


@main def all_possible_fresh_ids(): Unit =
  val total = totalNumberOfFreshIngredients(ingredients.freshSet())
  println(s"$total of the ingredient IDs are considered fresh")