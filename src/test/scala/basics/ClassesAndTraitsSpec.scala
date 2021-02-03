package basics

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalacheck.Arbitrary._
import ClassesAndTraits._
import org.scalatest.matchers.must.Matchers._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._

class ClassesAndTraitsSpec extends AnyFlatSpec {
  val genSize = Arbitrary.arbitrary[Double].withFilter(_ > 0)

  val genPoint = Gen.resultOf(Point)
  implicit val arbitraryPoint = Arbitrary(genPoint)

  val genCircle = for {
    x <- Arbitrary.arbitrary[Double]
    y <- Arbitrary.arbitrary[Double]
    r <- genSize
  } yield Circle(x, y, r)

  val genRectangle = for {
    x <- Arbitrary.arbitrary[Double]
    y <- Arbitrary.arbitrary[Double]
    w <- genSize
    h <- genSize
  } yield Rectangle(x, y, w, h)

  val genTriangle = Gen.resultOf(Triangle)

  val genShape = Gen.oneOf(genPoint, genCircle, genRectangle)
  implicit val arbitraryShape = Arbitrary[Shape](genShape)

  val genPoint3d = Gen.resultOf(Point3d)
  implicit val arbitraryPoint3d = Arbitrary(genPoint3d)

  val genSphere = for {
    center <- genPoint3d
    r <- genSize
  } yield Sphere(center, r)

  val genCube = for {
    c <- genPoint3d
    size <- genSize
  } yield Cube(c, size)

  val genCuboid = for {
    c <- genPoint3d
    w <- genSize
    d <- genSize
    h <- genSize
  } yield Cuboid(c, w, d, h)

  val genShape3d = Gen.oneOf(genPoint3d, genSphere, genCube, genCuboid)
  implicit val arbitraryShape3d = Arbitrary[Shape3d](genShape3d)

  "Shape" should "has non-negative area" in {
    forAll { s: Shape => s.area should be >= (0.0) }
  }

  "Shape.move" should "preserve shape area" in {
    forAll { (s: Shape, v: Point) =>
      val area = s.area
      val s2 = s.move(v.x, v.y)
      s2.area shouldEqual area
    }
  }

  "Shape3d" should "has non-negative area" in {
    forAll { s: Shape3d => s.area should be >= (0.0) }
  }

  "Shape3d" should "has non-negative volume" in {
    forAll { s: Shape3d => s.volume should be >= (0.0) }
  }

  "Shape3d.move" should "preserve shape area and volume" in {
    forAll { (s: Shape3d, v: Point3d) =>
      val area = s.area
      val volume = s.volume
      val s2 = s.move(v.x, v.y, v.z)
      s2.area shouldEqual area
      s2.volume shouldEqual volume
    }
  }
}
