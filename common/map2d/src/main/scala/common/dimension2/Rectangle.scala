package common.dimension2

case class Rectangle[C: Numeric](p1: Point[C], p2: Point[C]):
  def minMaxRectangle: Rectangle[C] = Rectangle.boundingBox(Seq(p1, p2))
  def points: Seq[Point[C]] = Seq(p1, p2)

  def enclosedPoints: Set[Point[C]] =
    val minMax = minMaxRectangle
    (for {
      x <- minMax.p1.x.to(minMax.p2.x)
      y <- minMax.p1.y.to(minMax.p2.y)
    } yield Point(x, y)).toSet

object Rectangle:
  def apply[C: Numeric](x1: C, y1: C, x2: C, y2: C): Rectangle[C] = new Rectangle[C](Point(x1, y1), Point(x2, y2))
  def apply[C: Numeric](p1: Point[C], p2: Point[C]): Rectangle[C] = new Rectangle[C](p1, p2)
  def boundingBox[N: Numeric](ps: Iterable[Point[N]]): Rectangle[N] =
    val xs = ps.map(_.x)
    val ys = ps.map(_.y)
    Rectangle(Point(xs.min, ys.min), Point(xs.max, ys.max))
