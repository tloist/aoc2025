
package Day01:
    opaque type Rotation = Int
    extension (rotation: Rotation)
        def atomic: List[Rotation] = List.fill(Math.abs(rotation))(if rotation < 0 then -1 else 1)
    object Rotation:
        def left(distance: Int): Rotation = -distance
        def right(distance: Int): Rotation = distance
        def fromLine(line: String): Rotation =
            def distance = line.substring(1).toInt
            line.charAt(0) match
                case 'L' => -distance
                case 'R' => distance
                case _ => throw new IllegalArgumentException(s"Invalid input for a rotation '$line'!")

    opaque type Dial = Int
    extension (dial: Dial)
        def rotate(rotation: Rotation): Dial = (dial + rotation) % 100
        def isTwelveOClock: Boolean = dial == 0
    object Dial:
        def initial: Dial = 50
    
    extension (rotations: Seq[Rotation])
        def scanFrom(initial: Dial): Seq[Dial] = rotations.scanLeft(Dial.initial)((state, rotation) => state.rotate(rotation))

import Day01.*
val rotations = os.read.lines(os.resource / "input.txt").map(Rotation.fromLine)

@main def secretEntrance_count_zeroes(): Unit =
    val result = rotations.scanFrom(Dial.initial).count(_.isTwelveOClock)
    println(s"The dial has reached twelve o'clock exactly $result times")

@main def secretEntrance_0x434C49434B(): Unit =
    val result = rotations.flatMap(_.atomic).scanFrom(Dial.initial).count(_.isTwelveOClock)
    println(s"The dial has reached twelve o'clock exactly $result times using method 0x434C49434B")