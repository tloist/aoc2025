package day05

import com.google.common.collect.{Range, RangeSet, TreeRangeSet}
import munit.FunSuite

import scala.math.BigInt.javaBigInteger2bigInt
import day05.*

import java.math.BigInteger

class Test05 extends FunSuite:
  val example: Ingredients = parse("example.txt")

  test("Example"):
    assertEquals(findAvailableFresh(example).map(javaBigInteger2bigInt).map(_.toInt), Set(5, 11, 17))

  test("Example B"):
    assertEquals(totalNumberOfFreshIngredients(example.freshSet()), BigInt(14))