package day06

import munit.FunSuite


class Test06 extends FunSuite:

  test("Example"):
    assertEquals(solutionA("example.txt"), BigInt("4277556"))

  test("Example Cephalopod"):
    assertEquals(calculateCephalopod("example.txt"), BigInt("3263827"))