import common.dimension2.Board
import munit.FunSuite;
import day04.*

class Test04 extends FunSuite:
  val grid: Grid = Board.from(os.read(os.resource / "example.txt"), parseBoardSpace)

  test("Example"):
    val accessiblePositions = grid.findPositions((pos, _) => grid.isAccessible(pos))
    println(grid.asString(grid.boundingBox, (p, v) => if accessiblePositions.contains(p) then 'x' else v.char))
    assertEquals(accessiblePositions.size, 13)

  test("Example B"):
    assertEquals(grid.clearAccessibleRollsIteratively.sum, 43)



