import munit.FunSuite
import day02.*

class Test02 extends FunSuite:

    test("example"):
      given Partitioner = alwaysSplitInHalf
      assertEquals(parse("example.txt").flatMap(_.ids.filter(isInvalid)).sum, BigInt("1227775554"))

    test("example2"):
      given Partitioner = allDivider
      assertEquals(parse("example.txt").flatMap(_.ids.filter(isInvalid)).sum, BigInt("4174379265"))
