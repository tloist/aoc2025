import munit.FunSuite
import Day01.*

class Test01 extends FunSuite:

    test("Turning right from 50"):
        assertEquals(10, Rotation.right(1000).atomic.scanFrom(Dial.initial).count(_.isTwelveOClock))

    test("Given example"):
        val example = List(
            Rotation.left(68),
            Rotation.left(30),
            Rotation.right(48),
            Rotation.left(5),
            Rotation.right(60),
            Rotation.left(55),
            Rotation.left(1),
            Rotation.left(99),
            Rotation.right(14),
            Rotation.left(82)
        )
        def compute(rotations: Seq[Rotation]): Int = rotations.scanFrom(Dial.initial).count(_.isTwelveOClock)
        assertEquals(3, compute(example))
        assertEquals(6, compute(example.flatMap(_.atomic)))