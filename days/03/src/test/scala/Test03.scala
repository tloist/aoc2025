import munit.FunSuite
import day03.*;

class Test03 extends FunSuite {

  test("Initial Joltage example #1"):
    assertEquals(findMaxJoltage_2(Bank(List(9,8,7,6,5,4,3,2,1,1,1,1,1,1,1))), BigInt(98))

  test("Initial Joltage example #2"):
    assertEquals(findMaxJoltage_2(Bank(List(8,1,1,1,1,1,1,1,1,1,1,1,1,1,9))), BigInt(89))

  test("Initial Joltage example #3"):
    assertEquals(findMaxJoltage_2(Bank(List(2,3,4,2,3,4,2,3,4,2,3,4,2,7,8))), BigInt(78))

  test("Initial Joltage example #4"):
    assertEquals(findMaxJoltage_2(Bank(List(8,1,8,1,8,1,9,1,1,1,1,2,1,1,1))), BigInt(92))

  // -----------------------------

  test("Joltage example B #1"):
    assertEquals(findMaxJoltage_12(Bank(List(9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 1, 1, 1, 1))), BigInt("987654321111"))

  test("Joltage example B #2"):
    assertEquals(findMaxJoltage_12(Bank(List(8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9))), BigInt("811111111119"))

  test("Joltage example B #3"):
    assertEquals(findMaxJoltage_12(Bank(List(2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 3, 4, 2, 7, 8))), BigInt("434234234278"))

  test("Joltage example B #4"):
    assertEquals(findMaxJoltage_12(Bank(List(8, 1, 8, 1, 8, 1, 9, 1, 1, 1, 1, 2, 1, 1, 1))), BigInt("888911112111"))
}
