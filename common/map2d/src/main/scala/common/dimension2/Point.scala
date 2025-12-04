package common.dimension2

import scala.annotation.targetName
import scala.math.Numeric.Implicits.infixNumericOps

case class Point[C: Numeric](x: C, y: C):
  def between(that: Point[C]): Vector[C] = Vector(this.x - that.x, this.y - that.y)
  def manhattanDistance(that: Point[C]): C = (this.x - that.x).abs + (this.y - that.y).abs

  @targetName("plus")
  def +(that: Vector[C]): Point[C] =
    Point(this.x + that.x, this.y + that.y)

  def ray(vec: Vector[C]): LazyList[Point[C]] =
    LazyList.iterate(this) { p => p + vec }

  def edgeNeighbors: Set[Point[C]] = Set(
    Vector[C](zero,  one),
    Vector[C](zero, -one),
    Vector[C]( one, zero),
    Vector[C](-one, zero),
  ).map(this + _)

  def cornerNeighbors: Set[Point[C]] = Set(
    Vector[C](-one, -one),
    Vector[C]( one, -one),
    Vector[C](-one,  one),
    Vector[C]( one,  one),
  ).map(this + _)

  def allSurrounding: Set[Point[C]] = edgeNeighbors ++ cornerNeighbors
  def andAllSurrounding: Set[Point[C]] = allSurrounding + this

  def switchAxes: Point[C] = Point(y, x)

  override def toString: String = s"P($x/$y)"

  private def zero: C = summon[Numeric[C]].zero
  private def one: C = summon[Numeric[C]].one