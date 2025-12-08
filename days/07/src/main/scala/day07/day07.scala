package day07

import common.dimension2.{Board, Point, Vector}
import Entry.*
import Vector.*
import scalax.collection.edges.DiEdge
import scalax.collection.mutable.Graph

import scala.annotation.tailrec

enum Entry(val char: Char):
  case Start extends Entry('S')
  case Empty extends Entry('.')
  case UnnamedSplitter extends Entry('^')
  case NamedSplitter(name: Char) extends Entry(name)
  case Beam extends Entry('|')
  def isSplitter: Boolean = this match
    case UnnamedSplitter | _: NamedSplitter => true
    case _ => false
object Entry:
  def parse(ch: Char): Entry = ch match
    case 'S' => Start
    case '.' => Empty
    case '^' => UnnamedSplitter
    case '|' => Beam
    case c => NamedSplitter(c)

type Coord = Point[Int]
type Manifold = Board[Int, Entry]
object Manifold:
  def parse(filename: String): Manifold = Board.from(os.read(os.resource / filename), Entry.parse)
extension (manifold: Manifold)
  def start: Coord = manifold.content.collectFirst({ case (p, Entry.Start) => p }).get
  def contains(c: Coord): Boolean = manifold.content.contains(c)

case class Splitter(coord: Coord, char: Option[Char]):
  override def toString: String = char.map(_.toString).getOrElse(coord.toString)
type Dependencies = Graph[Splitter, DiEdge[Splitter]]


def dependencyGraphFrom(manifold: Manifold): Dependencies =
  def splitterAt(c: Coord): Option[Splitter] = manifold.content.getOrElse(c, Empty) match
    case UnnamedSplitter => Some(Splitter(c, None))
    case NamedSplitter(name) => Some(Splitter(c, Option(name)))
    case _ => None
  def nextSplitterBelow(c: Coord): Option[Splitter] = c.ray(down)
    .takeWhile(c => manifold.contains(c) && !manifold.content(c).isSplitter)
    .lastOption.map(_ + down)
    .flatMap(splitterAt)
  @tailrec def traverse(splitters: List[Splitter], deps: Dependencies): Dependencies =
    if splitters.isEmpty then deps else
      val current = splitters.head
      val nexts = List(current.coord + left, current.coord + right).flatMap(nextSplitterBelow)
      traverse(nexts.filterNot(deps.contains) ++ splitters.tail, deps ++ nexts.map(DiEdge(current, _)))
  traverse(nextSplitterBelow(manifold.start).toList, Graph.empty)


val manifold: Manifold = Board.from(os.read(os.resource / "input.txt"), parse)
val dependencies: Dependencies = dependencyGraphFrom(manifold)

@main def total_of_individual_cephalopod_solutions(): Unit =
  println(s"The beam was split ${dependencies.nodes.size} times")

/*
 * timelinesOf(q) = 2                                         .......S.......
 * timelinesOf(r) = 2                                         ...............
 * timelinesOf(s) = 2                                         .......a.......
 * timelinesOf(t) = 2                                         ...............
 * timelinesOf(u) = 2                                         ......b.c......
 * timelinesOf(v) = 2                                         ...............
 *                                                            .....d.e.f.....
 * timelinesOf(n) = timelinesOf(q) timelinesOf f(r)           ...............
 * timelinesOf(o) = timelinesOf(s) + timelinesOf(t)           ....g.h...i....
 * timelinesOf(p) = 1 + timelinesOf(v)                        ...............
 *                                                            ...j.k...l.m...
 * timelinesOf(j) = timelinesOf(n) + 1                        ...............
 * timelinesOf(k) = 1 + timelinesOf(o)                        ..n...o.....p..
 * timelinesOf(l) = 2                                         ...............
 * timelinesOf(m) = 1 + timelinesOf(p)                        .q.r.s.t.u...v.
 *                                                            ...............
 * ... and so on ...                                         timelinesOf(a) = ?
 */
def totalNumberOfPossibleTimelines(dependencies: Dependencies): BigInt =
  @tailrec  // Dependencies#GraphLikeInnerNode is a path dependent type
  def calculateTimelines(order: List[Dependencies#GraphLikeInnerNode], points: Map[Dependencies#GraphLikeInnerNode, BigInt]): Map[Dependencies#GraphLikeInnerNode, BigInt] =
    if order.isEmpty then points else
      val current = order.head
      val newPoints = points + (current -> (current.diSuccessors.toList.map(points).sum + 2 - current.diSuccessors.size))
      calculateTimelines(order.tail, newPoints)
  val sorted = dependencies.topologicalSort.getOrElse(throw new IllegalArgumentException("Failed to topological sort dependencies!"))
  calculateTimelines(sorted.toList.reverse, Map.empty).values.max

@main def number_of_timelines(): Unit =
  println(s"There are ${totalNumberOfPossibleTimelines(dependencies)} total timelines")
