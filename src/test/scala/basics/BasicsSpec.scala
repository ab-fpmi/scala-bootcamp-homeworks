package basics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import Basics._

class BasicsSpec extends AnyFlatSpec {
  "lcm" should "work" in {
    lcm(6, 15) shouldEqual Some(30)
    lcm(8, 12) shouldEqual Some(24)
    lcm(0,0) shouldEqual None
  }

  "gcd" should "work" in {
    gcd(24, 18) shouldEqual 6
    gcd(1220, 516) shouldEqual 4
  }
}
