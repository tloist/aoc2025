package day07

import munit.FunSuite

class Test07 extends FunSuite:
  /* example.txt / named.txt
   *
   *   The example contains 22 '^' splitter, but only 21 are actively reachable, because (10, 15) is directly below
   *   another splitter (10, 11) and there are no splitters in columns 9 or 11 in between the heights that would
   *   lead rays onto it.
   *
   *   named.txt is the same example, but instead of splitters it has characters to identify the splitters.
   *   The splitter which is not reachable above is named 'u' here
   */
  val named: Manifold = Manifold.parse("named.txt")
  val namedDeps: Dependencies = dependencyGraphFrom(named)
  extension (start: Char)
    infix def dependsOn(end: Char): Boolean = namedDeps.edges.exists: edge =>
      edge.source.char.exists(start.equals) && edge.target.char.exists(end.equals)

  test("Dependency Graph - Parsed correctly"):
    val byLineNumber = namedDeps.nodes.toSet.groupMap(_.coord.y + 1)(_.toString)
    assertEquals(byLineNumber( 3), Set("a"))
    assertEquals(byLineNumber( 5), Set("b", "c"))
    assertEquals(byLineNumber( 7), Set("d", "e", "f"))
    assertEquals(byLineNumber( 9), Set("g", "h", "i"))
    assertEquals(byLineNumber(11), Set("j", "k", "l", "m"))
    assertEquals(byLineNumber(13), Set("n", "o", "p"))
    assertEquals(byLineNumber(15), Set("q", "r", "s", "t", "v"))  // 'u' is not reachable and thus not present as a dep
    assertEquals(namedDeps.nodes.size, 21)

  test("Dependency Graph - Dependencies"):
    assert('a' dependsOn 'b')
    assert('a' dependsOn 'c')
    assert('j' dependsOn 'n')
    assert('n' dependsOn 'q')
    assert('n' dependsOn 'r')
    assert('h' dependsOn 'k')
    assert('h' dependsOn 't')  // Depends on splitter multiple level below
    assert('m' dependsOn 'p')
    assert('p' dependsOn 'v')
    assert('c' dependsOn 'f')

  test("Dependency Graph - Part 1"):
    assertEquals(namedDeps.nodes.size, 21)

  test("Dependency Graph - Total Number of timelines"):
    assertEquals(totalNumberOfPossibleTimelines(namedDeps), BigInt(40))

  test("Example B"):
    assertEquals(totalNumberOfPossibleTimelines(dependencyGraphFrom(Manifold.parse("example.txt"))), BigInt(40))



