package common.dimension2

import scala.annotation.targetName
import scala.math.Numeric.Implicits.infixNumericOps

case class Vector[C: Numeric](x: C, y: C):
  @targetName("plus")
  def +(that: Vector[C]): Vector[C] = Vector(this.x + that.x, this.y + that.y)

  @targetName("minus")
  def -(that: Vector[C]): Vector[C] =
    Vector(this.x - that.x, this.y - that.y)

  def sign: Vector[C] = Vector(x.sign, y.sign)

  override def toString: String = s"V($x/$y)"

object Vector:
  def up[C](using num: Numeric[C]): Vector[C] =    Vector( num.zero, -num.one)
  def down[C](using num: Numeric[C]): Vector[C] =  Vector( num.zero,  num.one)
  def left[C](using num: Numeric[C]): Vector[C] =  Vector(-num.one,   num.zero)
  def right[C](using num: Numeric[C]): Vector[C] = Vector( num.one,   num.zero)