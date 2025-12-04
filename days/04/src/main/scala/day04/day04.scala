package day04

import cats.data.State
import common.dimension2.*
import day04.BoardSpace.{Empty, Paperroll}

enum BoardSpace(character: Char):
  case Empty extends BoardSpace('.')
  case Paperroll extends BoardSpace('@')
  def char: Char = character
def parseBoardSpace(c: Char): BoardSpace =
  BoardSpace.values.find(_.char == c).getOrElse(throw new IllegalArgumentException(s"Illegal character '$c'"))

type Grid = Board[Int, BoardSpace]
extension (grid: Grid)
  def isAccessible(pos: Point[Int]): Boolean =
    grid.content(pos) == Paperroll && pos.allSurrounding
      .count(pos => grid.content.getOrElse(pos, BoardSpace.Empty) == BoardSpace.Paperroll) < 4
  def clearAccessibleRollsIteratively: LazyList[Int] = LazyList.unfold(grid): prev =>
    val accessible = prev.findPositions((pos, _) => prev.isAccessible(pos))
    val next = prev.updated(accessible, BoardSpace.Empty)
    if accessible.nonEmpty then Some((accessible.size, next)) else None

val grid: Grid = Board.from(os.read(os.resource / "input.txt"), parseBoardSpace)

@main def accessible_paper_rolls(): Unit =
  println(grid.findPositions((pos, _) => grid.isAccessible(pos)).size)

@main def accessible_paper_rolls_iteratively(): Unit =
  println(grid.clearAccessibleRollsIteratively.sum)