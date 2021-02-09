package basics

import org.scalatest.flatspec.AnyFlatSpec
import ControlStructures._
import ControlStructures.Command._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._

class ControlStructuresSpec extends AnyFlatSpec {
  "Divide" should "work" in {
    forAll(Arbitrary.arbitrary[Double], Arbitrary.arbitrary[Double]) { (a: Double, b: Double) =>
      val cmd = f"divide $a $b"
      parseCommand(cmd).flatMap(calculate) shouldEqual Right(Result(a / b))
    }
  }

  "Sum" should "work" in {
    forAll { l: List[Double] =>
      val cmd = f"sum ${l.mkString(" ")}"
      parseCommand(cmd).flatMap(calculate) shouldEqual Right(Result(l.sum))
    }
  }
}
