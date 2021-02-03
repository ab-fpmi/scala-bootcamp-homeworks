package basics

object ClassesAndTraits {
  sealed trait Shape extends Located with Bounded with Movable with HasArea

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  trait Movable {
    def move(dx: Double, dy: Double): Shape
  }

  trait HasArea {
    def area: Double
  }

  final case class Point(x: Double, y: Double) extends Shape {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y

    override def move(dx: Double, dy: Double): Point = copy(x = x + dx, y = y + dy)

    override def area: Double = 0.0
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = centerX - radius
    override def maxX: Double = centerX + radius
    override def minY: Double = centerY - radius
    override def maxY: Double = centerY + radius

    override def move(dx: Double, dy: Double): Circle = copy(centerX = centerX + dx, centerY = centerY + dy)

    override def area: Double = Math.PI * radius * radius
  }

  final case class Rectangle(left: Double, top: Double, width: Double, height: Double) extends Shape {
    override def x: Double = left
    override def y: Double = top

    override def minX: Double = left
    override def maxX: Double = top
    override def minY: Double = left + width
    override def maxY: Double = top + height

    override def move(dx: Double, dy: Double): Shape = copy(left = left + dx, top = top + dy)

    override def area: Double = width * height
  }

  final case class Triangle(a: Point, b: Point, c: Point) extends Shape {
    override def x: Double = points.map(_.x).sum / 3
    override def y: Double = points.map(_.y).sum / 3

    override def minX: Double = points.map(_.x).min
    override def maxX: Double = points.map(_.x).max

    override def minY: Double = points.map(_.y).min
    override def maxY: Double = points.map(_.y).max

    override def move(dx: Double, dy: Double): Triangle = Triangle(a.move(dx, dy), b.move(dx, dy), c.move(dx, dy))

    override def area: Double = Math.abs((a.x * (b.y - c.y) + b.x * (c.y - a.y) + c.x * (a.y - b.y)) / 2)

    private val points = Seq(a, b, c)
  }

  sealed trait Shape3d extends Located3d with Movable3d with HasArea with HasVolume

  sealed trait Located3d {
    def x: Double
    def y: Double
    def z: Double
  }

  trait Movable3d {
    def move(dx: Double, dy: Double, dz: Double): Shape3d
  }

  trait HasVolume {
    def volume: Double
  }

  case class Point3d(x: Double, y: Double, z: Double) extends Shape3d {
    override def move(dx: Double, dy: Double, dz: Double): Point3d = Point3d(x + dx, y + dy, z + dz)
    override def volume: Double = 0.0
    override def area: Double = 0.0
  }

  case class Sphere(center: Point3d, radius: Double) extends Shape3d {
    override def x: Double = center.x
    override def y: Double = center.y
    override def z: Double = center.z

    override def move(dx: Double, dy: Double, dz: Double): Sphere = copy(center = center.move(dx, dy, dz))

    override def area: Double = 4 * Math.PI * radius * radius

    override def volume: Double = 4/3 * Math.PI * Math.pow(radius, 3)
  }

  case class Cube(corner: Point3d, size: Double) extends Shape3d {
    override def x: Double = corner.x
    override def y: Double = corner.y
    override def z: Double = corner.z

    override def move(dx: Double, dy: Double, dz: Double): Shape3d = copy(corner = corner.move(dx, dy, dz))

    override def area: Double = size * size * 6

    override def volume: Double = Math.pow(size, 3)
  }

  case class Cuboid(corner: Point3d, width: Double, depth: Double, height: Double) extends Shape3d {
    override def x: Double = corner.x
    override def y: Double = corner.y
    override def z: Double = corner.z

    override def move(dx: Double, dy: Double, dz: Double): Shape3d = copy(corner = corner.move(dx, dy, dz))

    override def area: Double = (width * depth + width * height + depth * height) * 2

    override def volume: Double = width * depth * height
  }
}
