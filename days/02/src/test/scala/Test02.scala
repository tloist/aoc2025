import munit.FunSuite
import day02.*

class Test02 extends FunSuite:

    test("example"):
      assertEquals(parse("example.txt").flatMap(_.ids.filter(isInvalid)).sum, BigInt("1227775554"))

    test("example2"):
      assertEquals(parse("example.txt").flatMap(_.ids.filter(isInvalid2)).sum, BigInt("4174379265"))
