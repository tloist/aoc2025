package common.dimension2

import scala.annotation.tailrec

case class Board[C: Numeric, V](content: Map[Point[C], V]) {
  export content.{get, getOrElse, keySet, size}

  def findPositions(pred: ((Point[C], V)) => Boolean): Set[Point[C]] = content.collect:
    case entry @ (pos, _) if pred(entry) => pos
  .toSet

  def boundingBox: Rectangle[C] = Rectangle(
    Point[C](
      content.keys.view.map(_.x).min,
      content.keys.view.map(_.y).min
    ), Point[C](
      content.keys.view.map(_.x).max,
      content.keys.view.map(_.y).max
    ))
  def updated(positions: Iterable[Point[C]], value: V): Board[C,V] = Board(positions.foldLeft(content) { (res, pos) =>
    res.updated(pos, value) })

  def asString(valuePrinter: V => Char): String = asString(boundingBox, valuePrinter)
  def asString(bbox: Rectangle[C], valuePrinter: V => Char): String = asString(bbox, (_, v) => valuePrinter(v))
  def asString(bbox: Rectangle[C], valuePrinter: (Point[C], V) => Char): String =
    val Rectangle(min, max) = bbox.minMaxRectangle
    min.y.to(max.y).map: y =>
      min.x.to(max.x).map: x =>
        val p = Point(x, y)
        content.get(p).map(valuePrinter(p, _)).getOrElse(' ')
      .mkString
    .mkString("\n")

  def environmentAsString(valuePrinter: V => Char, ps: Point[C]*): String =
    asString(Rectangle.boundingBox(ps.flatMap(_.andAllSurrounding)), valuePrinter)

  def crawl(start: Point[C], neighbors: Point[C] => Iterable[Point[C]], continue: V => Boolean): List[Point[C]] = {
    @tailrec
    def crawlRec(left: List[Point[C]], visited: Set[Point[C]], result: List[Point[C]]): List[Point[C]] = left match {
      case Nil => result
      case head :: rest =>
        if (visited contains head) crawlRec(rest, visited, result) else {
          val nexts = neighbors(head).filterNot(visited.contains).filter(p => content.get(p).exists(continue))
          crawlRec(left ++ nexts, visited + head, result :+ head)
        }
    }
    crawlRec(List(start), Set.empty, List.empty)
  }

}

object Board:
  def contentFrom[T](content: String, charInterpreter: Char => T): Map[Point[Int], T] =
    (for {
      (line, y) <- content.linesIterator.zipWithIndex
      (char, x) <- line.zipWithIndex
    } yield Point[Int](x, y) -> charInterpreter(char)
      ).toMap
  def from[T](content: String, charInterpreter: Char => T): Board[Int, T] = Board(contentFrom(content, charInterpreter))

