import munit.FunSuite
import cats.syntax.all._

class Test01_Cats extends FunSuite {

    test("Turning right from 50") {
        val result = turn0x434C49434B(Rotation.RightTurn(1000)).runS(initial).value
        assertEquals(result.hits, 10)
    }

    test("Given example") {
        val rotations = List(
            Rotation.LeftTurn(68),
            Rotation.LeftTurn(30),
            Rotation.RightTurn(48),
            Rotation.LeftTurn(5),
            Rotation.RightTurn(60),
            Rotation.LeftTurn(55),
            Rotation.LeftTurn(1),
            Rotation.LeftTurn(99),
            Rotation.RightTurn(14),
            Rotation.LeftTurn(82)
        )

        def compute(mod: StateModification): Int = rotations.traverse(mod).runS(initial).value.hits
        assertEquals(compute(turn), 3)
        assertEquals(compute(turn0x434C49434B), 6)
    }
}