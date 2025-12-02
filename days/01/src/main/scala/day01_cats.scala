import cats.syntax.all._
import scala.util.Try
import cats.data.State

trait FromString[A] {
    def parse(line: String): Either[String, A]
}

def parseLines[A](filename: String = "input.txt")(using converter: FromString[A]): Either[String, List[A]] =
    os.read.lines(os.resource / filename).toList.traverse(converter.parse)

enum Rotation {
    case LeftTurn(distance: Int)
    case RightTurn(distance: Int)
    def distance: Int
    def atomicStep: Rotation = this match {
        case _ : LeftTurn => LeftTurn(1)
        case _ : RightTurn => RightTurn(1)
    }
    def asAtomicSteps: Seq[Rotation] = LazyList.continually(atomicStep).take(distance)
}

given FromString[Rotation] with {
    def parse(line: String): Either[String, Rotation] =
        Try(line.substring(1).toInt).toEither.leftMap(_.toString).flatMap { distance =>
            line.charAt(0) match
                case 'L' => Right(Rotation.LeftTurn(distance))
                case 'R' => Right(Rotation.RightTurn(distance))
                case _ => Left(s"Invalid input for a rotation '$line'!")
        }
}

case class Computation(dial: Int, hits: Int)
val initial = Computation(50, 0)

type StateModification = Rotation => State[Computation, Unit]
def computeResult(turning: StateModification): Either[String, Int] = for {
    rotations <- parseLines()
    transformation = rotations.traverse(turning)
} yield transformation.runS(initial).value.hits


def normalizePosition(pos: Int): Int = pos % 100
def nextDialPosition(start: Int, rotation: Rotation): Int = normalizePosition(rotation match {
  case Rotation.LeftTurn(distance) => start - distance
  case Rotation.RightTurn(distance) => start + distance
})
val turn: StateModification = rotation => State.modify[Computation] { prev =>
    val nextDial = nextDialPosition(prev.dial, rotation)
    Computation(nextDial, if nextDial == 0 then prev.hits + 1 else prev.hits)
}

@main def secretEntrance_cats_count_zeroes(): Unit = {
    val result = computeResult(turn).getOrElse(throw new IllegalArgumentException(s"Error reading input"))
    println(s"The dial has reached twelve o'clock exactly $result times")
}


val turn0x434C49434B: StateModification = rotation => State.modify[Computation] { prev =>
    rotation.asAtomicSteps.traverse(turn).runS(prev).value
}

@main def secretEntrance_cats_0x434C49434B(): Unit = {
    val result = computeResult(turn0x434C49434B).getOrElse(throw new IllegalArgumentException(s"Error reading input"))
    println(s"The dial has reached twelve o'clock exactly $result times using method 0x434C49434B")
}