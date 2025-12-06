package day06.transpose

import munit.FunSuite

class TestTranspose extends FunSuite:

  test("Example B"):
    assertEquals(solve("example.txt"), BigInt("3263827"))